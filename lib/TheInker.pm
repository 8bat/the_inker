package TheInker;
#
# Copyright (C) 2017, Andrew Sayers.
#
# This file is part of The Inker
#
# The Inker is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, version 2.
#
# The Inker is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Foobar.  If not, see <http://www.gnu.org/licenses/>.

=head1 NAME

TheInker - Represents an Illustrator image

=head1 SYNOPSIS

    use TheInker;

    my @subroutines = TheInker::initialise( data => $data, pattern_template => 'doc', height => 96 );
    foreach my $subroutine ( @subroutines ) {
      mt $image = TheInker::compile($subroutine);
      print $image->{room};
      print $image->{pnm};
      print $image->{svg};
    }


=head1 DESCRIPTION

Build pictures based on commands from The Illustrator (a companion package to The Quill).

The Illustrator uses an odd mix of vector and bitmap images.
This package tries to reconstruct scalable versions of images.

=head2 IMAGE PARSING

Here is a rough overview of the way Illustrator images are parsed:

1. Illustrator commands ("FILL", "LINE" etc.) are converted to intermediate commands
   * this layer manages the parser state, and converts arguments to more useful values
2. Intermediate commands (fill(), line() etc.) are converted to bitmap and SVG commands
   * this layer contains most of the drawing logic
3. Bitmap commands (draw_point(), set_colour() etc.) modify the image in memory
4. SVG commands (svg_add_line(), svg_add_area() etc.) build a set of SVG elements
5. Renderers (render_pnm() and render_svg()) convert data to output formats

=cut

use warnings;
use strict;
use List::Util qw/ min max /;

no warnings 'recursion'; # fill and shade are deeply recursive

# ZX Spectrum graphics are a bit confusing:
#
# 1. the main image is a 256x176 grid of 1-bit colour, where the bit means "paper" or "ink"
# 2. colour for both layers ("paper" and "ink") is stored in a 32x22 grid of bytes:
#    3 bits of paper colour, a "bright" flag, 3 bits of ink colour, then a "flash" flag
# 3. each paper and colour value maps to an 8x8 grid in the actual bitmap,
#    so updating the colour of one pixel modifies the colour of all pixels in the block
#
# The Illustrator also has a few gotchas:
#
# 1. the (0,0) point for most commands is in the bottom-left
#    *except* the BLOCK command, where (0,0) is in the top-left
# 2. the "LINE" command does not fill in the pixel under the cursor when the line starts
#    (so e.g. a line from (x,y) to (x+10,y-10) will actually start at (x+1,y-1)
# 3. setting INK, PAPER, FLASH or BRIGHT to 8 means "do not overwrite current value"
#    so you can e.g. change the colour without changing the brightness
# 4. any pixel (or every pixel) of a fill'ed section could be overwritten by a later command,
#    so filled areas can only be detected by searching the whole completed board
# 8. the "fill" and "shade" commands ignore paper colours when filling
# 9. the "fill" and "shade" commands are actually two different algorithms:
#    * "fill" goes row-by-row, "shade" goes column-by-column
#    * if "fill" hits a wall, it will get around it above or below,
#      if "shade" hits a wall, it gives up
#    * if there is a wall to the right of the point where "shade" starts,
#      it fills in the wall but flips the pixels for all bytes below the wall
#      (this is hard to describe - try it and see)
#    * the "shade" command:
#      * can shade the top- and bottom-most rows
#      * can move into a column if both top- and bottom-most rows are the only unfilled pixels
#      * can move into a column if any one pixel other than the top- and bottom-most rows is unfilled
#      * will not move into a column if exactly one of the top- and bottom-most rows are the only unfilled pixels
#    * the "shade" command has several other odd behaviours.
#      These might be bugs, or tricks to work around various issues.
#      shade_column() below models all the behaviour I've been able to find
#
# Finally, some things you need to know when generating SVG images:
# 1. filling in the pixel at (x,y) effectively fills in a square (x,y)..(x+1,y+1),
#    so SVG lines must be drawn at [x+0.5,y+0.5] in order to neatly fill the specified pixel point.
# 2. commands like "MOVE" and "LINE" behave more like vector graphics,
#    but the "fill" and "shade" commands behave more like bitmap graphics.
# 3. because The Illustrator uses a different algorithm to calculate lengths,
#    we need to represent lines several different ways:
#    * as a straight line between the first and last pixel in the bitmap (rounded to the nearest pixel)
#    * as a straight line between the start and end point in the SVG (can be fractional due to scaling)
#    * as a series of pixels generated by The Illustrator's line-drawing algorithm

#
# GLOBAL VARIABLES
#

my $debug = 0;
my $room;

#
# Illustrator flags
#

my ($basecursor_x, $basecursor_y, $scale, $ink_colour, $paper_colour, $flash, $bright);

#
# Illustrator commands (initialised below)
#

my %illustrator_commands;

#
# List of subroutines (most of which represent rooms)
#

my $subroutines;

#
# Screen state
#

# The screen is a 256x176 grid, describing the state of each pixel on the screen.
# The viewbox is a 256x96 grid - the subset of the screen that is drawn on-screen
# Valid values include:
#
# undef - this pixel is not set
# 1     - this pixel is set
#
# The screen commands largely mirror the screen itself, but stores a list of commands
# that were applied to each pixel.
# Valid values include:
#
# undef - no commands for this pixel
# [ @commands ] - this pixel has been touched by the specified commands, in order
#
# Note: clear_point() will clear the value in @screen, and add a command to @screen_commands
#
# The attribute blocks are a 32x22 grid, describing the state of each 8x8 colour block.
# Valid values look like:
#
# { ink => 7, paper => 1, flash => 0, bright => 1 }
#
# See below for information about the format for SVG elements

my ( $screen_width, $screen_height ) = ( 256, 176 );
my ( $attribute_width, $attribute_height ) = ( $screen_width/8, $screen_height/8 );
my ( $viewbox_width, $viewbox_height ) = ( $screen_width );
my ( $location_height );
my $bottom_row; # = $screen_height - $viewbox_height;
my @attribute_blocks;
my @screen;
my @screen_commands;
my $command_id; # unique ID for each command on the screen


#
# SVG paths
#

# Objects describing SVG paths
#
# Each path is a list of lines.  Valid lines look like:
#
# { from => $from_coords, to => $to_coords, pixels => [...], fill => $pattern }
#
# Valid co-ordinates must be screen co-ordinates (upside-down).  They look like:
# [ $x, $y ]

my @svg_paths;
my @svg_areas;
my $svg_current_path;

# Shade patterns (0-255) describe the pixels in a 4x2 grid that should be filled in.
# Unfortunately, the pattern differs between versions of The Illustrator.
# See examine-patterns.sh for how to reverse-engineer a new pattern:
our @pattern_templates = (
    {
        id => 'vbca',
        desc => 'The pattern seen in The Very Big Cave Adventure',
        pattern => [qw/
            2  32  8 128
            1  16  4  64
        /],
    },
    {
        id => 'doc',
        desc => "The pattern described in The Illustrator's documentation",
        pattern => [qw/
          128  64  32  16
            8   4   2   1
        /],
    },
);

my @pattern_template;

# ZX Spectrum colours
#
# Note: the exact colour generated by a ZX spectrum differs from device to device
# (and from emulator to emulator).  These colours are based on those used by FUSE:
my @byte_colours = (
    # basic colours:
    [0x00,0x00,0x00], # black
    [0x00,0x00,0xC0], # blue
    [0xC0,0x00,0x00], # red
    [0xC0,0x00,0xC0], # magenta
    [0x00,0xC0,0x00], # green
    [0x00,0xC0,0xC0], # cyan
    [0xC0,0xC0,0x00], # yellow
    [0xC0,0xC0,0xC0], # white

    # bright colours:
    [0x00,0x00,0x00], # black
    [0x00,0x00,0xFF], # blue
    [0xFF,0x00,0x00], # red
    [0xFF,0x00,0xFF], # magenta
    [0x00,0xFF,0x00], # green
    [0x00,0xFF,0xFF], # cyan
    [0xFF,0xFF,0x00], # yellow
    [0xFF,0xFF,0xFF], # white
);

my @css_colours = map( { sprintf( "#%02x%02x%02x", @$_ ) } @byte_colours );

#
# BITMAP COMMANDS
#
# Update the screen state (similar to a ZX Spectrum graphics buffer)
#

# always round towards zero:
sub round { return int($_[0]) };

sub scale { return round( $_[0] * $scale / 8 ); }

sub current_colour {
    my ( $inverse, $overwrite ) = @_;
    return {
        flash  => $flash,
        bright => $bright,
        over   => $overwrite,
        $inverse
        ? ( paper => $ink_colour, ink   => $paper_colour )
        : ( ink   => $ink_colour, paper => $paper_colour )
    };
}

sub set_basecursor {
    ($basecursor_x, $basecursor_y) = @_;
    return;
}

sub update_basecursor {
    my ( $x, $y ) = @_;
    ( $basecursor_x += $x ) %= $screen_width;
    ( $basecursor_y += $y ) %= $screen_height;
    return;
}

sub set_colour {
    my ( $x, $y, $colour ) = @_;
    my $block = $attribute_blocks[$y][$x];
    $block->{flash } = $colour->{flash } unless $colour->{flash } == 8;
    $block->{bright} = $colour->{bright} unless $colour->{bright} == 8;
    $block->{paper } = $colour->{paper } unless $colour->{paper } == 8;
    $block->{ink   } = $colour->{ink   } unless $colour->{ink   } == 8;
    return;
}

sub draw_point {
    my ( %args ) = @_;
    my ( $x, $y, $command, $colour ) = @args{qw/ x y command colour /};
    $x %= $screen_width;
    $y %= $screen_height;
    $screen[$y][$x] = 1;
    push( @{$screen_commands[$y][$x]}, $command );
    push( @{$command->{pixels}}, [$x,$y] );
    set_colour( round($x/8), round($y/8), $colour );
    return $command;
}

sub clear_point {
    my ( %args ) = @_;
    my ( $x, $y, $command, $colour ) = @args{qw/ x y command colour /};
    $x %= $screen_width;
    $y %= $screen_height;
    undef $screen[$y][$x];
    push( @{$screen_commands[$y][$x]}, $command );
    set_colour( round($x/8), round($y/8), $colour );
    return $command;
}


#
# SVG COMMANDS
#
# Update the list of SVG paths to draw
# (includes some basic geometry for use in calculating filled areas)
#
# SVG lines are calculated with an absolute anchor point
# (where The Illustrator started drawing lines),
# but relative line lengths (as The Illustrator drew each line).
# This keeps the overall path close to its location in the original image,
# but minimises the effects of rounding errors when drawing scaled images.

sub equal {
    # do two pixels describe the same point?
    my ( $p1, $p2 ) = @_;
    return
        $p1->[0] == $p2->[0] &&
        $p1->[1] == $p2->[1]
        ;
}

sub adjacent {
    # do two pixels touch each horizontially, vertically or diagonally?
    my ( $p1, $p2 ) = @_;
    return
        abs($p1->[0]-$p2->[0]) < 2 &&
        abs($p1->[1]-$p2->[1]) < 2
        ;
}

#
# SVG line commands
# Create a path from plot and line intermediate commands
#

sub svg_add_line {
    my ( $command, $offset, $scale ) = @_;
    if ( defined($svg_current_path) ) {
        my $prev = $svg_current_path->{commands}->[-1];
        $command->{next} = $prev->{next};
        $command->{prev} = $prev;
        $prev->{next}->{prev} = $command;
        $prev->{next} = $command;
    } else {
        push( @svg_paths, $svg_current_path = {
            class    => 'line',
            from     => $command->{from},
            offset   => $offset,
            commands => [],
            fill     => 0,
            stroke   => 'ink',
            cursor   => [
                                 $command->{from}->[0] + $offset->[0] + 0.5,
                $screen_height - $command->{from}->[1] - $offset->[1] - 0.5,
            ],
        });
        $command->{from}->[0] += $offset->[0];
        $command->{from}->[1] += $offset->[1];
        $command->{length}->[0] -= $offset->[0];
        $command->{length}->[1] -= $offset->[1];
        $command->{next} = $command->{prev} = $command;
    }
    $command->{distance_to_anchor} = @{$svg_current_path->{commands}};
    $command->{length}->[0] *=  $scale;
    $command->{length}->[1] *= -$scale;
    $command->{path} = $svg_current_path;

    # start/end point represent the edges of the SVG line, whereas from/to represent the bitmap edges:
    $command->{start_point} = [@{$svg_current_path->{cursor}}];
    $svg_current_path->{cursor}->[0] += $command->{length}->[0];
    $svg_current_path->{cursor}->[1] += $command->{length}->[1];
    $command->{  end_point} = [@{$svg_current_path->{cursor}}];

    push( @{$svg_current_path->{commands}}, $command );
    return;
}

sub svg_finalise_path {
    if ( $svg_current_path ) {

        # close the path if the base cursor ends (approximately) where it began:
        $svg_current_path->{closed} |= $#{$svg_current_path->{commands}} && adjacent([$basecursor_x,$basecursor_y],$svg_current_path->{from});
        if ( $svg_current_path->{closed} ) {
            my @reverse_commands = reverse( @{$svg_current_path->{commands}} );
            for ( my $n=0; $n<$#reverse_commands/2; ++$n ) {
                $reverse_commands[$n]->{distance_to_anchor} = $n+1;
            }
        } else {
            delete $svg_current_path->{commands}->[ 0]->{prev};
            delete $svg_current_path->{commands}->[-1]->{next};
        }

        my ( $x, $y ) = @{$svg_current_path->{from}};
        $x += 0.5;
        $y = $screen_height-$y-0.5;
        $svg_current_path->{d} = join(
            ' ',
            "M $x,$y",
            map( { "l $_->{length}->[0],$_->{length}->[1]" } @{$svg_current_path->{commands}} ),
            $svg_current_path->{closed} ? 'Z' : ()
        );

        $svg_current_path->{stroke_pixels} = $svg_current_path->{pixels};

        undef $svg_current_path;

    }
}

#
# SVG area commands
# Create a path from fill and shade intermediate commands
#
# Note: only svg_add_area() is called directly.
#       Everything else is used by it to calculate paths
#

sub point_line_distance {
    # distance from a point to a line
    # taken from https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
    my ( $x0, $y0, $x1, $y1, $x2, $y2 ) = @_;
    return abs(($y2-$y1)*$x0-($x2-$x1)*$y0+$x2*$y1-$y2*$x1) / sqrt(($y2-$y1)**2+($x2-$x1)**2);
}

sub point_point_distance {
    # distance between two points
    my ( $x0, $y0, $x1, $y1 ) = @_;
    return ($x1-$x0)**2 + ( ($y1-$y0)**2 );
}

sub points_to_equation {
    my ( $x1, $y1, $x2, $y2 ) = @_;
    # Calculate the slope-intercept equation for a line (y = mx + b) given two points:
    my $m = ($y2-$y1) / ( $x2-$x1);
    my $b = $y1 - $m*$x1;
    return $m, $b;
}

sub point_within_line_segment {
    # Is a point within a rounding error of a line segment?
    my ( $x0, $y0, $x1, $y1, $x2, $y2 ) = @_;
    return
        # Point is within the bounds of the line on the X axis:
        ( ( $x0 <= $x1 ) ? ( $x0 >= $x2 ) : ( $x0 <= $x2 ) ) &&
        # Point is within the bounds of the line on the Y axis:
        ( ( $y0 <= $y1 ) ? ( $y0 >= $y2 ) : ( $y0 <= $y2 ) ) &&
        # Point is at most a rounding error away from the line:
        point_line_distance( $x0, $y0, $x1, $y1, $x2, $y2 ) < 0.001
        ;
}

# Line segment commands
# A line segment is a part of a line which touches a filled area.
# A line can have more than one segment

sub svg_segment_attach {
    # attach one segment to the end of another
    my ( $base, @next ) = @_;
    my $prev = $base;
    foreach my $next ( @next ) {
      die if $next->{prev};
      $base->{root} |= $next->{root};
      $next->{root} = 0;
      $next->{prev} = $prev->{last};
      $prev->{last}->{next} = $next;
      $base->{last} = $next->{last};
      $prev = $next;
    }
    return;
}

sub svg_segment_clone {
    # create a copy of a segment (and some later segments)
    my ( $segment, $max_nodes ) = @_;
    my %ret = (
        %$segment,
        prev => undef,
        root => 0,
        pixels => [@{$segment->{pixels}}], # deep copy
    );
    my $value = \%ret;
    while ( $value->{next} && $max_nodes-- ) {
        $value->{next} = { %{$value->{next}} };
        $value->{next}->{prev} = $value;
        $value = $value->{next};
    }
    $ret{last} = $value;
    $value->{next} = undef;
    return \%ret;
}

sub svg_segment_reverse {
    # reverse a list of segments, and the direction of each segment
    my ( $line_segments, $segment ) = @_;
    $segment->{root} = 0;
    for ( my $s=$segment; $s; $s=$s->{prev} ) {
        @{$s}{qw/ prev     next   /} = @{$s}{qw/ next   prev     /};
        @{$s}{qw/ from     to     /} = @{$s}{qw/ to     from     /};
        @{$s}{qw/ from_pos to_pos /} = @{$s}{qw/ to_pos from_pos /};
        @{$s->{pixels}} = reverse(@{$s->{pixels}});
        $s->{reversed} ^= 1;
    }
    my $first = $segment;
    $first = $first->{prev} while $first->{prev};
    $first->{root} = 1;
    $first->{last} = $segment;
    @$line_segments = map( { $_ == $segment ? $first : $_ } @$line_segments );
    return $first;
}

sub svg_segment_endpoints {
    my ( $segment ) = @_;
    if ( $segment->{root} ) {
        return [
            $segment        ->{from},
            $segment->{last}->{  to},
        ];
    } else {
        return [ ['NaN', 'NaN' ], ['NaN', 'NaN' ] ]
    }
}

sub svg_segment_rotate {
    my ( $segment, $n ) = @_;
    my $last = $segment->{last};
    while ( $n-- ) {
        $segment->{last} = undef;
        $segment->{root} = 0;
        $segment->{prev} = $last;
        $segment->{next}->{prev} = undef;
        $segment->{next}->{last} = $segment;
        $segment->{next}->{root} = 1;
        $last->{next} = $segment;
        $last = $segment;
        $segment = delete $segment->{next};
    }
    return $segment;
}

sub svg_segment_intersections_inner {
    # Intersection point(s) for a pair of line segments
    # The common case is just a line/line intersection.
    # (see https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection)
    # There are several edge cases too
    my ( $s1, $s2, $command ) = @_;
    my ( $x1, $y1 ) = @{$s1->{command}->{start_point}};
    my ( $x2, $y2 ) = @{$s1->{command}->{  end_point}};
    my ( $x3, $y3 ) = @{$s2->{command}->{start_point}};
    my ( $x4, $y4 ) = @{$s2->{command}->{  end_point}};

    # Special cases for points:
    if ( $x1 == $x2 && $y1 == $y2 ) {
        # line 1 is actually a point
        if ( $x3 == $x4 && $y3 == $y4 ) {
            # line 2 is also a point
            return [$x1,$y2] if $x1 == $x3 && $y1 == $y3;
        } else {
            return [$x1,$y2] if point_line_distance($x1,$y1,$x3,$y3,$x4,$y4) < 2;
        }
    } elsif ( $x3 == $x4 && $y3 == $y4 ) {
        # line 1 is a line, line 2 is a point
        return [$x3,$y3] if point_line_distance($x3,$y3,$x1,$y1,$x2,$y2) < 2;
    }

    # normal case:
    if ( ($x1-$x2)*($y3-$y4)-($y1-$y2)*($x3-$x4) ) {
        # lines are not parallel:
        return
            [
                 # x co-ordinate of intersection point:
                 (($x1*$y2-$y1*$x2)*($x3-$x4)-($x1-$x2)*($x3*$y4-$y3*$x4)) /
                 (($x1-$x2)*($y3-$y4)-($y1-$y2)*($x3-$x4)),
                 # y co-ordinate of intersection point:
                 (($x1*$y2-$y1*$x2)*($y3-$y4)-($y1-$y2)*($x3*$y4-$y3*$x4)) /
                 (($x1-$x2)*($y3-$y4)-($y1-$y2)*($x3-$x4))
            ];
    }

    # Special cases for adjacent points:
    return [ ($x1+$x3)/2, ($y1+$y3)/2 ] if adjacent( [$x1,$y1], [$x3,$y3] );
    return [ ($x1+$x4)/2, ($y1+$y4)/2 ] if adjacent( [$x1,$y1], [$x4,$y4] );
    return [ ($x2+$x3)/2, ($y2+$y3)/2 ] if adjacent( [$x2,$y2], [$x3,$y3] );
    return [ ($x2+$x4)/2, ($y2+$y4)/2 ] if adjacent( [$x2,$y2], [$x4,$y4] );

    # special cases for parallel lines:
    if ( $x1 == $x2 && $x2 == $x3 && $x3 == $x4 ) {
        # parallel vertical lines along the same X value
        return [$x1,$y1] if abs($y1-$y3)<1.5 || abs($y1-$y4)<1.5;
        return [$x1,$y2] if abs($y2-$y3)<1.5 || abs($y2-$y4)<1.5;
    } elsif ( $y1 == $y2 && $y2 == $y3 && $y3 == $y4 ) {
        # parallel horizontal lines along the same Y value
        return [$x1,$y1] if abs($x1-$x3)<1.5 || abs($x1-$x4)<1.5;
        return [$x1,$y2] if abs($x2-$x3)<1.5 || abs($x2-$x4)<1.5;
    }

    # parallel vertical lines at most one pixel apart:
    if ( $x1 == $x2 && $x3 == $x4 ) {
        return ( [$x2,$y2], [$x3,$y3] ) if abs( $x1 - $x3 ) <= 1;
    } elsif ( $y1 == $y2 && $y3 == $y4 ) {
        # parallel horizontal lines at most one pixel apart:
        return ( [$x2,$y2], [$x3,$y3] ) if abs( $y1 - $y3 ) <= 1;
    } elsif (
        # points can't be parallel lines:
        ( $x1 != $x2 || $y1 != $y2 ) &&
        ( $x3 != $x4 || $y3 != $y4 )
        ) {
        my ( $m1, $b1 ) = points_to_equation( $x1, $y1, $x2, $y2 );
        my ( $m2, $b2 ) = points_to_equation( $x3, $y3, $x4, $y4 );
        if ( abs( $m2 - $m1 ) < 0.001 ) { # parallel, give or take a rounding error
            my $distance = ($b2-$b1) / sqrt( $m1**2 + 1 );
            return ( [$x2,$y2], [$x3,$y3] ) if $distance <= 1;
        }
    }

    warn sprintf(
        "Could not fill area $command->{id} in room $room.  See fill-errors.md for more information.\n" .
        "Tried to find intersection between parallel lines:\n" .
        "Line 1: %3.1f,%3.1f -> %3.1f,%3.1f (type: %s)\n" .
        "Line 2: %3.1f,%3.1f -> %3.1f,%3.1f (type: %s)\n" .
        "(0,0 is the top-left pixel)",
        $x1, $y1, $x2, $y2, $s1->{command}->{type},
        $x3, $y3, $x4, $y4, $s2->{command}->{type}
    );

    return undef;

}

sub svg_segment_intersections {
    # Intersection point(s) for a pair of line segments
    # Most of the work is done in svg_segment_intersections_inner().
    # This just handles some special cases
    my ( $s1, $s2, $command ) = @_;
    my @points = svg_segment_intersections_inner($s1,$s2,$command);
    unless ( $#points ) {
        my ( $x1, $y1 ) = @{$s1->{command}->{start_point}};
        my ( $x2, $y2 ) = @{$s1->{command}->{  end_point}};
        my ( $x3, $y3 ) = @{$s2->{command}->{start_point}};
        my ( $x4, $y4 ) = @{$s2->{command}->{  end_point}};
        my $left   = min( $x1, $x2, $x3, $x4 );
        my $right  = max( $x1, $x2, $x3, $x4 );
        my $top    = min( $y1, $y2, $y3, $y4 );
        my $bottom = max( $y1, $y2, $y3, $y4 );
        if ( $left - $points[0][0]   > 2 ||
             $points[0][0] - $right  > 2 ||
             $top - $points[0][1]    > 2 ||
             $points[0][1] - $bottom > 2
            ) {
            my ( $x, $y ) = @{$points[0]};
            return (
                (
                 $s1->{reversed}
                 ? [$x1,$y1]
                 : [$x2,$y2]
                ),
                (
                 $s2->{reversed}
                 ? [$x4,$y4]
                 : [$x3,$y3]
                )
            );
        }
    }
    return @points;
}

sub svg_segments_stringify {
    # debugging use only
    my ( @segments ) = @_;
    my @ret;
    my @errors;
    my $node_count;
    foreach my $segment ( @segments ) {
        my @waypoints;
        my @types;
        my @pixels;
        push( @errors, "\$segment->{last} not at end" ) if $segment->{last}->{next};
        for ( my $s=$segment; $s; $s=$s->{next} ) {
            push( @errors, "line segment root == $s->{root}" ) if $s->{root} != ( $s == $segment ) ? 1 : 0;
            push( @errors, "invalid linked list: segment->prev->next != segment" ) if $s->{prev} && $s->{prev}->{next} != $s;
            push( @errors, "invalid linked list: segment->next->prev != segment" ) if $s->{next} && $s->{next}->{prev} != $s;
            push( @waypoints, '[' . join( ',', @{$s->{from}} ) . "],[" . join( ',', @{$s->{  to}} ) . "]" );
            push( @pixels, "$s->{from_pos}..$s->{to_pos}" );
            push( @types, "$s->{command}->{type}" );
        }
        push( @ret,
              scalar(@ret) . ": { " .
              'closed => ' . ( ( $segment->{closed} ? '1' : '0' ) ) .
              ', node_count => ' . scalar(@waypoints) .
              ', waypoints => [ ' . join( ', ', @waypoints ) . ' ]' .
              ', pixels => [ ' . join( ', ', @pixels ) . ' ]' .
              ', types => [ ' . join( ', ', @types ) . ' ]' .
              " }"
        );
        $node_count += @waypoints;
    }
    warn join( "\n", @errors ) if @errors;
    return join( "\n", @ret ) . "\n(total: $node_count node(s) in " . @ret . " segment(s))";
}

sub svg_segment_reduce {
    my ( $segment ) = @_;
    while (
        $segment &&
        $segment->{next} &&
        $segment->{from_pos} == $segment->{to_pos} &&
        equal( $segment->{to}, $segment->{next}->{from} )
        ) {
        $segment = $segment->{next};
    }
    $segment->{prev} = undef;
    for ( my $s = $segment->{next}; $s; $s = $s->{next} ) {
        if (
            $s->{from_pos} == $s->{to_pos} &&
            equal( $s->{from}, $s->{prev}->{to} )
            ) {
            $s->{prev}->{next} = $s->{next};
            $s->{next}->{prev} = $s->{prev} if $s->{next};
        }
    }
    return $segment;
}

sub svg_area_to_pnm {
    # Debugging use only
    my ( $command ) = @_;

    my $area = $command->{area};

    my @border;
    foreach my $pixel ( @{$command->{border_pixels}} ) {
        $border[$pixel->{coords}->[1]][$pixel->{coords}->[0]] = 1
            if $pixel->{coords}->[1] >= 0 && $pixel->{coords}->[0] >= 0;
    }

    my $ret = "P6\n$screen_width $screen_height\n255\n";

    foreach my $y ( 0..($screen_height-1) ) {
        foreach my $x ( 0..($screen_width-1) ) {
            if ( $border[$y][$x] ) {
                $ret .= $area->[$y][$x] ? "\xFF\x00\x00" : "\xFF\xFF\xFF";
            } else {
                $ret .= $area->[$y][$x] ? "\x7F\x7F\x7F" : "\x00\x00\x00";
            }
        }
    }

    return $ret;
}

sub svg_area_to_debugging_svg {
    # generates an SVG that can be loaded by GIMP.
    # See the end of svg_add_area() for the real SVG-generating code
    my ( @line_segments ) = @_;

    my $ret =
        q{<?xml version="1.0" encoding="UTF-8" standalone="no"?>} . "\n" .
        q{<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 20010904//EN" "http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd">} . "\n" .
        q{<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 256 96">} . "\n"
    ;
    foreach my $n ( 0..$#line_segments ) {
        my $segment = $line_segments[$n];
        $ret .=
            " <path id=\"path-$n\" class=\"path\" fill=\"none\" stroke=\"black\" stroke-width=\"1\" " . "d=\"M";
        for ( my $s=$segment; $s; $s=$s->{next} ) {
            $ret .= ' ' . join(',',@{$s->{from}}) . ' ' . join(',',@{$s->{to}});
        }
        $ret .= "\" />\n";
    }
    return $ret . "</svg>\n";

}

sub svg_area_dump {
    my ( $command, @line_segments ) = @_;
    mkdir('error');
    open( my $pnm_fh, '>', "error/$room.pnm" );
    print( $pnm_fh svg_area_to_pnm( $command ) );
    open( my $vectors_fh, '>', "error/$room.svg" );
    print( $vectors_fh svg_area_to_debugging_svg(@line_segments) );
    close($vectors_fh);
    return;
}

sub svg_edge_offset {
    # when calculating special lines for edges of the image,
    # we need to offset the start/end points depending on
    # which side of the image they're on:
    my ( $coords ) = @_;
    return [
                           ( $coords->[0] > 0           ) ? $coords->[0] : 0,
        $screen_height - ( ( $coords->[1] > $bottom_row ) ? $coords->[1] : $bottom_row )
    ];
}

# the "fill" and "shade" commands fill in an area defined by:
# 1. the edges of the screen
# 2. lines that have already been drawn
# 3. shades that have already been drawn
#    (because of the way shading works, complex areas often need multiple shades)
#
# this calculates the area touched by "fill" and "shade" commands,
# based on the list of pixels filled in by the algorithm.
sub svg_add_area {
    my ( $command ) = @_;
    my $local_debug = $command->{id} == 'NaN'; # change this to a number during debugging
    warn "debugging enabled for command $command->{id} in room $room" if $local_debug;

    my @pixels = @{$command->{pixels}};

    #
    # PART ONE: Calculate required information
    #
    # We need:
    # 1. the pixels inside the area
    # 2. the pixels at the boundaries of the area
    # 3. the set of lines where at least one segment of the line touches a boundary pixel
    # 4. the line segments which are actually on the boundary
    #

    # special lines describing the edges of the image:
    my ( $left_edge, $right_edge, $bottom_edge, $top_edge ) =
        map( {
            my ( $from, $to, $id ) = @$_;
            [{
                id => $id,
                type => 'image-edge',

                from        => $from,
                to          => $to,
                start_point => svg_edge_offset($from),
                  end_point => svg_edge_offset(  $to),

                length => [ $to->[0] - $from->[0], $from->[1] - $to->[1] ],
                pixels => [
                    map(
                        { my $x=$_; map( { [$x,$_] } $from->[1]..$to->[1] ) }
                        $from->[0]..$to->[0]
                    )
                ],

                distance_to_anchor => 256,
            }]}
             [[            -1,     $bottom_row],[              -1,$screen_height  ], 'left-edge' ],
             [[$viewbox_width,   $bottom_row-1],[$viewbox_width  ,$screen_height-1], 'right-edge' ],
             [[            -1,   $bottom_row-1],[$viewbox_width-1,   $bottom_row-1], 'bottom-edge' ],
             [[             0,  $screen_height],[$viewbox_width  ,$screen_height  ], 'top-edge' ],
        );

    # Combine the edges into a single closed path:
    foreach my $edge ( $right_edge->[0], $bottom_edge->[0] ) {
        @{$edge->{pixels}} = reverse @{$edge->{pixels}};
    }
    my @edges = ( $bottom_edge, $left_edge, $top_edge, $right_edge, $bottom_edge, $left_edge );
    foreach my $n ( 1..4 ) {
        $edges[$n]->[0]->{next} = $edges[$n+1]->[0];
        $edges[$n]->[0]->{prev} = $edges[$n-1]->[0];
    }

    # Grid of pixels within the current area:
    my @area;
    $area[$_->[1]][$_->[0]] = 1 foreach @pixels;

    # build a list of pixels that touch the border of the area:
    my @border_pixels;
    foreach my $pixel ( @{$command->{pixels}} ) {
        my ( $x, $y ) = @$pixel;
        # check the left edge:
        if ( $x == 0 ) {
            push( @border_pixels, { coords => [$x-1,$y], commands => $left_edge } );
        } elsif ( !$area[$y][$x-1] ) {
            push( @border_pixels, { coords => [$x-1,$y] } )
        }
        if ( $x == $viewbox_width-1 ) {
            push( @border_pixels, { coords => [$x+1,$y], commands => $right_edge } );
        } elsif ( !$area[$y][$x+1] ) {
            push( @border_pixels, { coords => [$x+1,$y] } )
        }
        if ( $y == $bottom_row ) {
            push( @border_pixels, { coords => [$x,$y-1], commands => $bottom_edge } );
        } elsif ( !$area[$y-1][$x] ) {
            push( @border_pixels, { coords => [$x,$y-1] } )
        }
        if ( $y == $screen_height-1 ) {
            push( @border_pixels, { coords => [$x,$y+1], commands => $top_edge } );
        } elsif ( !$area[$y+1][$x] ) {
            push( @border_pixels, { coords => [$x,$y+1] } )
        }
    }

    # debugging use only:
    $command->{area} = \@area;
    $command->{border_pixels} = \@border_pixels;

    # add commands for border pixels not at the edges of the viewbox:
    foreach my $pixel ( @border_pixels ) {
        unless ( $pixel->{commands} ) {
            my ( $x, $y ) = @{$pixel->{coords}};
            $pixel->{commands} = [ @{$screen_commands[$y][$x]//[]}, ($command->{lines}->[$y][$x]//()) ];
        }
    }

    # invert @border_pixels:
    # convert the list of pixels (which contain lists of commands)
    # to a set of lines (which contain lists of pixels)
    my %border_lines;
    my $path = 0;
    if ( @{$border_pixels[0]->{commands}//[]} && $border_pixels[0]->{commands}->[0]->{path} ) {
        $path = $border_pixels[0]->{commands}->[0]->{path};
    }
    foreach my $pixel ( @border_pixels ) {
        my ( $coords, $commands ) = @{$pixel}{qw/ coords commands /};
        # the "shade" command creates a line for each point it colours in - we ignore those lines:
        my @commands = grep( { $_ != $command } @$commands );
        if ( $#$commands > 0 ) {
            my @not_shade = grep( { !exists($_->{pattern}) } @commands );
            @commands = @not_shade if @not_shade;
        }
        foreach my $command ( @commands ) {
            $path = 0 unless $path == ($command->{path}//0);
            my $border_line = $border_lines{$command->{id}} //= {
                command       => $command,
                pixels        => [],
                intersections => [],
            };
            push( @{$border_line->{pixels}}, $pixel );
        }
    }

    # Special case: if an area touches every point in a path, and only points in that path,
    # then fill the area inside that path.
    if ( $path && @{$path->{commands}} == keys(%border_lines) ) {
        unless ( $path->{closed} ) {
             $path->{d} .= ' Z';
             $path->{closed} = 1;
        }
        $path->{fill} = $command->{pattern};
        $path->{fill_pixels} = $command->{pixels};
        return;
    }

    # separate %border_lines:
    # convert the set of commands to a list of line segments
    # (we might need e.g. the first and last third of a line but not the middle segment)
    my @line_segments;
    foreach my $key ( sort keys %border_lines ) {
        # note: we use sort() to avoid intermittent bugs, we don't actually care about the order
        my $line = $border_lines{$key};
        my $source_pixels = $line->{command}->{pixels};
        my %pixel_positions;
        my $max_position = -1;
        foreach my $pixel ( @{$line->{pixels}} ) {
            for ( my $position=0; $position!=@$source_pixels; ++$position ) {
                if (
                    $source_pixels->[$position][0] == $pixel->{coords}->[0] &&
                    $source_pixels->[$position][1] == $pixel->{coords}->[1]
                    ) {
                    $pixel_positions{$position} = 1;
                    $max_position = $position if $max_position < $position;
                    last;
                }
            }
        }
        my $state = 'closed';
        foreach my $n ( 0..$max_position ) {
            if ( $pixel_positions{$n} ) {
                if ( $state eq 'closed' ) {
                    my $segment = {
                        line     => $line,
                        command  => $line->{command},
                        pixels   => undef, # initialised below
                        from_pos => $n,
                          to_pos => $n,
                        reversed => 0,
                        prev     => undef,
                        next     => undef,
                        root     => 1,
                        closed   => 0,
                        last     => undef,
                    };
                    $segment->{last} = $segment;
                    push( @line_segments, $segment );
                    $state = 'open';
                } else {
                    ++$line_segments[-1]->{to_pos};
                }
            } else {
                $state = 'closed';
            }
        };
    }
    foreach my $segment ( @line_segments ) {
        $segment->{pixels} = [@{$segment->{command}->{pixels}}[$segment->{from_pos}..$segment->{to_pos}]];
        @{$segment}{qw/ from to /} = @{$segment->{pixels}}[0,-1];
    }

    #
    # PART TWO: join the line segments into a single boundary path
    #
    # This is a set of heuristics to guess how lines join up.
    # Most cases are fairly simple, but it can't always produce the right result,
    # and in some cases won't be ably to produce *any* result.
    #
    # If you came here trying to make the program handle a new edge case,
    # add another heuristic to the list.
    #

    # Lines that were generated one after the other already have "previous" and "next" lines:
    foreach my $segment ( @line_segments ) {
        my $command = $segment->{command};
        if ( $segment->{from_pos} == 0 ) {
            if ( my @prev = grep(
                     {
                         $_ != $segment &&
                         $_->{root} &&
                         $_->{last}->{command} == ($command->{prev}//-1) &&
                         $_->{last}->{to_pos} == $#{$_->{last}->{command}->{pixels}}
                     } @line_segments ) ) {
                die if $#prev;
                svg_segment_attach($prev[0],$segment);
            }
        }
    }
    @line_segments = grep( { !$_->{prev} } @line_segments );

    #warn svg_segments_stringify(@line_segments) if $local_debug;

    # sometimes, a path can be removed because it touches a subset of the points in another path:
    if ( $#line_segments > 0 ) {

        my @segment_pixels;
        foreach my $segment ( @line_segments ) {
            my @pixels;
            for ( my $s=$segment; $s; $s=$s->{next} ) {
                push( @pixels, @{$s->{command}->{pixels}}[$s->{from_pos}..$s->{to_pos}] );
            }
            push( @segment_pixels, \@pixels );
        }

        my @sorted_segments = sort( { $#{$segment_pixels[$a]} <=> $#{$segment_pixels[$b]} } 0..$#segment_pixels );
        while ( @sorted_segments ) {
            # delete this path if it is contained within a larger path
            my $self = shift @sorted_segments;
            next unless $line_segments[$self]->{root};
            my $pixel_positions = $segment_pixels[$self];
            foreach my $other ( @sorted_segments ) {
                next unless $line_segments[$other]->{root};
                my $other_positions = $segment_pixels[$other];
                # skip this other unless every pixel in the current path also exists in that path...
                my $all_pixels_overlap = 1;
                foreach my $pixel ( @$pixel_positions ) {
                    $all_pixels_overlap &=
                        grep( { $pixel->[0] == $_->[0] && $pixel->[1] == $_->[1] } @$other_positions );
                }
                if ( $all_pixels_overlap ) {
                    $line_segments[$self]->{root} = 0;
                    last;
                }
            }
        }

        @line_segments = grep( { $_->{root} } @line_segments );

    }


    # often, the endpoints of paths will be adjacent to one another:
    while ( $#line_segments ) { # note: usually exits early from a "last" command, below

        my @segment_endpoints = map( { svg_segment_endpoints($_) } @line_segments );

        # This can be quite complicated in some cases.
        # For example, imagine three lines like this:
        #
        #     |
        #     |
        #     v
        # -->   -->
        #
        # the horizontal lines are each adjacent to the end of one line,
        # the vertical line is adjacent to the start of one line and the end of another.
        #
        # We build a list of possible actions, then pick the best one.

        my @possible_actions;

        # find start -> end matches:
        foreach my $n ( 0..$#segment_endpoints ) {
            my $start = $segment_endpoints[$n]->[0];
            my @other = grep(
                {
                    $_ != $n &&
                    $line_segments[$_]{root} &&
                    adjacent($segment_endpoints[$_]->[1],$start)
                }
                0..$#segment_endpoints
            );
            unless ( $#other ) {
                my $action = { type => 0, from => $other[0], to => $n };
                push( @{$possible_actions[($n       *2)  ]}, $action );
                push( @{$possible_actions[($other[0]*2)+1]}, $action );
            }
        }

        # find start -> start matches:
        foreach my $n ( 0..$#segment_endpoints ) {
            my $start = $segment_endpoints[$n]->[0];
            my @other = grep(
                {
                    $line_segments[$_]->{root} &&
                    adjacent($segment_endpoints[$_]->[0],$start) &&
                    !equal($segment_endpoints[$_]->[0],$segment_endpoints[$_]->[1])
                }
                ($n+1)..$#segment_endpoints
            );
            unless ( $#other ) {
                my $action = { type => 1, from => $other[0], to => $n };
                push( @{$possible_actions[($n       *2)  ]}, $action );
                push( @{$possible_actions[($other[0]*2)  ]}, $action );
            }
        }

        # find end -> end matches:
        foreach my $n ( 0..$#segment_endpoints ) {
            next if equal($segment_endpoints[$n]->[0],$segment_endpoints[$n]->[1]);
            my $end = $segment_endpoints[$n]->[1];
            my @other = grep(
                {
                    $line_segments[$_]->{root} &&
                    adjacent($segment_endpoints[$_]->[1],$end) &&
                    !equal($segment_endpoints[$_]->[0],$segment_endpoints[$_]->[1])
                }
                ($n+1)..$#segment_endpoints
            );
            unless ( $#other ) {
                my $action = { type => 2, from => $n, to => $other[0] };
                push( @{$possible_actions[($n       *2)+1]}, $action );
                push( @{$possible_actions[($other[0]*2)+1]}, $action );
            }
        }

        # don't look for end -> start matches - would be redundant (see start -> end matches)

        my ( $chosen_action ) =
            sort(
                # prefer start->end actions, then start->start, then end->end:
                { $a->[0]->{type} <=> $b->[0]->{type} }
                # choose actions for nodes that have exactly one possible action:
                grep( { $_ && !$#$_ } @possible_actions )
            );

        last unless $chosen_action;

        my $type = $chosen_action->[0]->{type};
        my $from = $line_segments[$chosen_action->[0]->{from}];
        my $to   = $line_segments[$chosen_action->[0]->{to  }];
        if ( $type == 0 ) { # start -> end
            svg_segment_attach($from,$to);
        } elsif ( $type == 1 ) { # start -> start
            svg_segment_attach( # attach starts
                svg_segment_reverse(\@line_segments,$from), # prev's start is now its end
                $to,
            );
        } else { # end -> end
            svg_segment_attach( # attach ends
                $from,
                svg_segment_reverse(\@line_segments,$to), # segment's end is now its start
            );
        }

        @line_segments = grep( { $_->{root} } @line_segments );

    }


    # occasionally, a shade will leave two paths separated by a vertical line.
    # Imagine a situation where a shade surrounds two unrelated lines:
    #
    # SSS
    # S|S
    # SvS
    # S S
    # S^S
    # S|S
    # SSS
    #
    # We would want the shaded area to join the two lines
    #
    if ( $#line_segments ) {
        my $n = 0;
        while ( $n <= $#line_segments ) {
            my @segment_endpoints = map( { svg_segment_endpoints($_) } @line_segments );
            my $segment = $line_segments[$n];
            my ( $from, $to ) = @{$segment_endpoints[$n]};
            my @others = ($n+1)..$#line_segments;
            my $other;
            if      ( ( $other ) = grep( { $segment_endpoints[$_][1][0] == $from->[0] } @others ) ) {
                # other's "to" -> our "from"
                svg_segment_attach( $line_segments[$other], $segment );
            } elsif ( ( $other ) = grep( { $segment_endpoints[$_][0][0] ==   $to->[0] } @others ) ) {
                # other's "from" -> our "to"
                svg_segment_attach( $segment, $line_segments[$other] );
            } elsif ( ( $other ) = grep( { $segment_endpoints[$_][0][0] == $from->[0] } @others ) ) {
                # other's "from" -> our "from"
                svg_segment_attach(
                    svg_segment_reverse(\@line_segments,$line_segments[$other]),
                    $segment,
                );
            } elsif ( ( $other ) = grep( { $segment_endpoints[$_][1][0] ==   $to->[0] } @others ) ) {
                # other's "to" -> our "to"
                svg_segment_attach(
                    $line_segments[$other],
                    svg_segment_reverse(\@line_segments,$segment)
                );
            } else {
                ++$n;
            }
            @line_segments = grep( { $_->{root} } @line_segments );
        }
    }

    # occasionally, the end of one path will connect to a point in the middle of another path:
    if ( $#line_segments ) {
      LINE_SEGMENT:
        foreach my $n ( 0..$#line_segments ) {
            my $segment = $line_segments[$n];
            next unless $segment->{root};
            my ( $from, $to ) = @{svg_segment_endpoints($segment)};
            foreach my $other ( @line_segments[($n+1)..$#line_segments] ) {
                my $count = -1;
                for ( my $s=$other; $s; $s=$s->{next} ) { ++$count; }

                my $pos = 0;
                for ( my $s=$other; $s; $s=$s->{next} ) {
                    if      ( adjacent(   $to, $s->{  to} ) ) {
                        # $other is [ $a .. $match .. $z ]
                        if ( $pos < $count/2 ) {
                            # create [ $segment .. $match..$a .. $a..$z ]:
                            svg_segment_attach( $segment, svg_segment_reverse( [], svg_segment_clone( $other    , $pos+1 ) ), $other );
                        } else {
                            # create [ $segment .. $match..$z .. $z..$a ]:
                            svg_segment_attach( $segment,                          svg_segment_clone( $s->{next}, 'Inf'  )  , svg_segment_reverse( \@line_segments, $other ) );
                        }
                        next LINE_SEGMENT;
                    } elsif ( adjacent( $from, $s->{  to} ) ) {
                        # other is [ $a .. $match .. $z ]
                        if ( $pos < $count / 2 ) {
                           # create [ $z..$a .. $a..$match .. $segment ]:
                           svg_segment_attach( svg_segment_reverse( \@line_segments, $other ), svg_segment_clone( $other, $pos+1 ), $segment );
                        } else {
                           # create [ $a..$z .. $z..$match .. $segment ]
                           svg_segment_attach( $other, svg_segment_reverse( [], svg_segment_clone( $s->{next}, 'Inf' ) ), $segment );
                        }
                    }
                    next LINE_SEGMENT;
                    ++$pos;
                }

            }
        }
        @line_segments = grep( { $_->{root} } @line_segments );
    }

    # mark closed segments:
    if ( $#line_segments ) {
        foreach my $segment ( @line_segments ) {
            my $from = $segment->{from};
            my $first_non_adjacent=0;
            my $s = $segment;
            $s = $s->{next} while $s && adjacent( $from, $s->{from} ) && adjacent( $from, $s->{to} );
            $segment->{closed} = $s && adjacent( $from, $segment->{last}->{to} );

            # Occasionally (e.g. in room 20 of the Very Big Cave Adventure),
            # a line will be spuriously cut into two segments:
            $segment->{closed} |= $segment->{command} == $segment->{last}->{command};

        }
    } else {
            # if only one segment remains, we assume it's closed:
            $line_segments[0]->{closed} = 1;
    }

    if ( grep( { !$_->{closed} } @line_segments ) ) {

        # Error: we only handle areas that can be described with closed paths

        svg_area_dump($command,@line_segments);

        warn
            "\nCould not fill area $command->{id} in room $room.  See fill-errors.md for more information.\n" .
            "\t";

        @line_segments = grep( { $_->{closed} } @line_segments );
    }

    #
    # Build SVG path
    #

    my @d;
    foreach my $segment ( @line_segments ) {

        # Move the anchor point of the path to match the anchor point of one of the matching SVG paths:
        my $anchor = 0;
        my ( $anchor_from, $anchor_distance ) = ( 'Inf', 'Inf' );
        my $n = 0;
        for ( my $s=$segment; $s; $s=$s->{next} ) {
            if ( $anchor_from     > $s->{from_pos} ||
                 $anchor_distance > $s->{command}->{distance_to_anchor}
                ) {
                $anchor = $n;
                $anchor_from     = $s->{from_pos};
                $anchor_distance = $s->{command}->{distance_to_anchor};
            }
            ++$n;
        }
        $segment = svg_segment_rotate( $segment, $anchor );

        # very rarely, points can be removed from a line:
        $segment = svg_segment_reduce( $segment );

        # Build the line description:
        my @segment_lines = svg_segment_intersections( $segment->{last}, $segment, $command );
        for ( my $s=$segment; $s != $segment->{last}; $s = $s->{next} ) {
            push( @segment_lines, svg_segment_intersections( $s, $s->{next}, $command ) );
        }
        if ( grep( { !defined($_) } @segment_lines ) ) {

            svg_area_dump($command,@line_segments);

        } else {

            # remove duplicate points:
            my $n=1;
            while ( $n <= $#segment_lines ) {
                if ( equal( $segment_lines[$n], $segment_lines[$n-1] ) ) {
                    splice( @segment_lines, $n, 1 );
                } else {
                    ++$n;
                }
            }

            # remove redundant points:
            $n=1;
            while ( $n < $#segment_lines ) {
                if ( point_within_line_segment(map( { @$_ } @segment_lines[$n,$n-1,$n+1] )) ) {
                    splice( @segment_lines, $n, 1 );
                } else {
                    ++$n;
                }
            }

            # convert points to relative:
            foreach my $n ( reverse(1..$#segment_lines) ) {
                $segment_lines[$n][0] -= $segment_lines[$n-1][0];
                $segment_lines[$n][1] -= $segment_lines[$n-1][1];
            }

            push( @d, 'M ' . join( ' l ', map( { "$_->[0],$_->[1]" } @segment_lines ) ), 'Z' );
        }

    }

    push(
        @svg_areas,
        {
             class => 'area',
             closed => 1,
               fill_pixels => $command->{pixels},
             stroke_pixels => $command->{stroke_pixels},
             fill => $command->{pattern},
             stroke => $command->{stroke_pixels} ? 'paper' : 'none',
             d => join( "\n", @d ),
        }
    );

    return;

}


#
# INTERMEDIATE COMMANDS
#
# translate Illustrator commands to bitmap/SVG commands
#

sub plot {
    my ( $colour, $dx, $dy ) = @_;
    # draw a line at the specified point

    my $line = {
        type   => 'plot',
        stroke => 1,
        id     => $command_id++,
        from   => [$basecursor_x-($dx//0),$basecursor_y-($dy//0)],
        to     => [$basecursor_x         ,$basecursor_y         ],
        length => ( defined($dx) ? [$dx,$dy] : undef ),
        pixels => [[$basecursor_x,$basecursor_y]],
    };

    draw_point(
        x => $basecursor_x,
        y => $basecursor_y,
        colour  => $colour,
        command => $line,
    );

    return svg_add_line($line,[0,0],1);
}

sub line {
    my ( $colour, $unscaled_x, $unscaled_y ) = @_;

    my ( $x, $y ) = ( scale($unscaled_x), scale($unscaled_y) );

    my $distance = max(abs($x),abs($y));

    # The Illustrator never draws the point under the base cursor:
    return unless $distance;

    my ( @from_coords, @to_coords, @length );
    foreach my $coord ( [$basecursor_x,$x,$unscaled_x], [$basecursor_y,$y,$unscaled_y] ) {
        my ( $start, $rc, $unscaled ) = @$coord; # base cursor, rubber cursor
        push( @length, $unscaled );
        push( @from_coords, $start );
        push(   @to_coords, $start + $rc );
    }

    my $line = {
        type    => 'line',
        stroke => 1,
        id     => $command_id++,
        from   => \@from_coords,
        to     => \@to_coords,
        length => \@length,
        pixels => [],
    };

    # based on the algorithm in pawgr 0.9

    my $sign_x = ( $x < 0 ) ? -1 : 1; $x = abs($x);
    my $sign_y = ( $y < 0 ) ? -1 : 1; $y = abs($y);

    my ( $inc, $max, $step_x, $step_y )
        = ( $y >= $x )
        ? ( $x, $y, 0, $sign_y )
        : ( $y, $x, $sign_x, 0 )
        ;

    my $cur = round($max / 2);
    my $pas = 0;

    my ( $pos_x, $pos_y ) = ( $basecursor_x, $basecursor_y );

    # The Illustrator never draws the point under the base cursor.
    # We record the value of that point for later deletion:
    my $offset;

    while ($pas++ < $max) {
        $cur += $inc;
        if ($cur >= $max) {
            $pos_y += $sign_y;
            $pos_x += $sign_x;
            $cur -= $max;
        } else {
            $pos_y += $step_y;
            $pos_x += $step_x;
        }
        draw_point(
            x => $pos_x,
            y => $pos_y,
            colour  => $colour,
            command => $line,
        );
        $offset //= [ $pos_x - $basecursor_x, $pos_y - $basecursor_y ];
    }

    return svg_add_line($line,$offset,$scale/8);
}

sub rectangle {
    my ( $x, $y, $w, $h ) = @_;

    my $colour = current_colour;

    $x = round($x/8);
    $y = round($y/8);
    $w = round($w/8);
    $h = round($h/8);
    for ( my $yc = 0; $yc != $h; ++$yc ) {
        for ( my $xc = 0; $xc != $w; ++$xc ) {
            set_colour( $x+$xc, $y+$yc, $colour );
        }
    }

    svg_finalise_path;

    return;
}

# fill a single pixel
#
# To simplify svg_add_area(), we treat each pixel like a separate "plot" command.
sub fill_pixel {
    my ( $command, $colour, $x, $y ) = @_;
    draw_point( x => $x, y => $y, colour => $colour, command => {
        type   => 'pixel',
        stroke => 1,
        id     => $command_id++,
        pixels => [],
        distance_to_anchor => 256,
        start_point => [ $x+0.5, $screen_height-$y-0.5 ],
          end_point => [ $x+0.5, $screen_height-$y-0.5 ],
    });
    push( @{$command->{pixels}}, [$x,$y] );
    return;
}

# fill pixels in the current row,
# then search the row(s) above/below for blank spaces to continue,
# then call itself recursively on those rows.
#
# Returns the x coordinate of the right wall of the current row
sub fill_row {
    my ( $fill, $colour, $x, $y ) = @_;

    # originally based on the algorithm in pawgr 0.9,
    # with modifications to match The Illustrator's algorithm

    die if $screen[$y][$x];

    my $left = $x;
    while ( $left && !$screen[$y][$left-1] ) {
        --$left;
    }

    # Fill row:
    my $right = $left;
    do {
        fill_pixel( $fill, $colour, $right++, $y );
    } while ( $right<$viewbox_width && !$screen[$y][$right] );
    --$right;

    # Search adjacent row(s):
    if ( $y > 0 ) {
        for ( my $x = $left; $x <= $right; ++$x ) {
            if ( !$screen[$y-1][$x] ) {
                $x = fill_row( $fill, $colour, $x, $y-1, 'down' );
            }
        }
    }

    if ( $y < $screen_height-1 ) {
        for ( my $x = $left; $x <= $right; ++$x ) {
            if ( !$screen[$y+1][$x] ) {
                $x = fill_row( $fill, $colour, $x, $y+1, 'up' );
            }
        }
    }

    return $right+1;
}

sub fill {
    my ( $x, $y ) = @_;

    svg_finalise_path;

    my $colour = current_colour;

    $x = ( $basecursor_x+scale($x) ) % $screen_width;
    $y = ( $basecursor_y+scale($y) ) % $screen_height;

    return if $screen[$y][$x];

    my $fill = {
        type    => 'fill',
        id      => $command_id++,
        pattern => 255,
        pixels  => [],
        lines   => [], # not used by fill (only by shade)
    };

    fill_row( $fill, $colour, $x, $y, 'down-then-up' );

    return svg_add_area($fill);
}

# Returns true if a location should be flled in:
sub shade_pixel_at {
    my ( $pattern, $x, $y ) = @_;
    return $pattern & $pattern_template[$y%2][$x%4];
}

# shade pixels in the current column,
# then shade at most one column to the left and/or right
sub shade_column {
    my ( $shade, $colour, $x, $y, $pattern, $border, $direction, $touched ) = @_;

    # originally based on the algorithm in pawgr 0.9,
    # with modifications to match The Illustrator's algorithm

    die if $screen[$y][$x];

    my $bottom = $y;
    while ( $bottom && !$screen[$bottom-1][$x] ) {
        --$bottom;
    }

    # shade column:
    my $top = $bottom;
    do {
        if ( shade_pixel_at( $pattern, $x, $top ) ) {
            fill_pixel( $shade, $colour, $x, $top );
        } else {
            push( @{$shade->{pixels}}, [$x,$top] );
        }
        $touched->[$top][$x] = 1;
        ++$top;
    } while ( $top<$screen_height && !$screen[$top][$x] );
    --$top;

    # The Illustrator sets the colour of adjacent pixels:
    set_colour( round($x/8), round(($bottom-1)/8), $colour ) if $bottom && $x != $screen_width-1;
    set_colour( round($x/8), round((   $top+1)/8), $colour ) if $top < $screen_height-1;

    if ( $border ) {
        if ( $bottom > 0 && !shade_pixel_at( $pattern, $x, $bottom-1 ) ) {
            push( @{$shade->{stroke_pixels}}, [$x,$y] );
            clear_point( x => $x, y => $bottom-1, command => $shade, colour => $colour );
        }
        if ( $top < $screen_height-1 && !shade_pixel_at( $pattern, $x, $top+1 ) ) {
            push( @{$shade->{stroke_pixels}}, [$x,$y] );
            clear_point( x => $x, y => $top+1, command => $shade, colour => $colour );
        }
    }

    # Search adjacent column(s):
    my @full_columns_to_search = ( $y..$top, reverse($bottom..$y) );
    my @columns_to_search;
    if ( grep( { $_ == $bottom_row } @columns_to_search ) && grep( { $_ == $screen_height-1 } @columns_to_search ) ) {
        @columns_to_search = @full_columns_to_search;
    } else {
        # replicate a weird bug in The Illustrator's shade algorithm
        @columns_to_search = grep( { $_ > $bottom_row && $_ < $screen_height - 1 } @full_columns_to_search );
    }

    if ( $direction =~ /right/ && $x < $screen_width-1 ) {
        foreach my $y ( @columns_to_search ) {
            if ( !$screen[$y][$x+1] ) {
                shade_column( $shade, $colour, $x+1, $y, $pattern, $border, 'right', $touched );
                last;
            }
        }
    }

    if ( $direction =~ /left/ && $x > 0 ) {
        foreach my $y ( @columns_to_search ) {
            if ( !$screen[$y][$x-1] ) {
                shade_column( $shade, $colour, $x-1, $y, $pattern, $border, 'left', $touched );
                last;
            }
        }
    }

    # Games will often apply multiple shades to the same area,
    # in order to fill in the parts that were blocked by imaginary lines.
    # Generate overlapping shades, rather than a shade full of holes.
    foreach my $range ( [reverse($bottom_row..($bottom-1))], [($top+1)..($screen_height-1)] ) {
        # if we have touched the adjacent pixel: touch the current pixel
        # else: draw a line here
        next unless @$range;

        # pretend to touch pixels if we have already touched both sides:
        my $line = {
            type   => 'touch',
            stroke => 0,
            id     => $command_id++,
            pixels => [],
            distance_to_anchor => 256,
            start_point => [ $x+0.5, $screen_height-$range->[0]-0.5 ],
              end_point => [ $x+0.5, 0 ],
        };

        my $y;
        my $mode = 'touch';
        my $line_start;
        while ( @$range ) {
            $y = shift @$range;
            last if grep( { $_->{type} ne 'pixel' } @{$screen_commands[$y][$x]//[]} );
            $mode = 'line' if
                ( $x > 0               && !$touched->[$y][$x-1] ) ||   # untouched pixel on the left
                ( $x < $screen_width-1 && !$touched->[$y][$x+1] ) ||   # untouched pixel on the right
                ( $screen[$y][$x] && !shade_pixel_at($pattern,$x,$y) ) # shouldn't touch this pixel
            ;
            if ( $mode eq 'touch' ) {
                if ( shade_pixel_at( $pattern, $x, $y ) ) {
                    fill_pixel( $shade, $colour, $x, $y );
                } else {
                    push( @{$shade->{pixels}}, [$x,$y] );
                }
                $touched->[$y][$x] = 1;
            } else {
                $line_start //= $y;
                push( @{$line->{pixels}}, [$x,$y] );
                $shade->{lines}[$y][$x] = $line;
            }
        }
        $line->{end_point}->[1] = $screen_height-$y+0.5;
        $line->{length} = [ 0, $y - $line_start ] if defined $line_start;

    }

    # Because the shade algorithm only recurses into a single adjacent point,
    # we can think of it as a fill that's blocked by imaginary lines.
    # Find those lines, so the SVG fill can use them:
    foreach my $x_offset ( -1, +1 ) {
        my $_x = $x + $x_offset;
        my @y_values = sort( { $a <=> $b } grep( { !$touched->[$_][$_x] && !$screen[$_][$_x] } @full_columns_to_search ) );
        while ( @y_values ) {
            my $bottom = shift @y_values;
            my $top = $bottom;
            my @pixels = ( [$_x,$top] );
            while ( @y_values && $y_values[0] == $top+1 ) {
                $top = shift @y_values;
                push( @pixels, [$_x,$top] );
            }
            my $point_x = ( $x_offset == -1 ) ? $x : $x+1;
            my $line = {
                type   => 'imaginary',
                stroke => 0,
                id     => $command_id++,
                length => [0,$top-$bottom+1  ],
                pixels => \@pixels,
                distance_to_anchor => 256,
                start_point => [ $point_x, $screen_height-$top   -0.5 ],
                  end_point => [ $point_x, $screen_height-$bottom+0.5 ],
            };
            foreach my $y ( $bottom..$top ) {
                $shade->{lines}[$y][$_x] = $line;
            }
        }
    }

    return;
}

sub shade {
    my ( $x, $y, $pattern, $border ) = @_;

    svg_finalise_path;

    my $colour = current_colour;

    $x = ( $basecursor_x+scale($x) ) % $screen_width;
    $y = ( $basecursor_y+scale($y) ) % $screen_height;

    return if $screen[$y][$x];

    my $shade = {
        type    => 'shade',
        id      => $command_id++,
        pattern => $pattern,
        border  => $border,
        lines   => [],
        pixels  => [],
        ( $border ? ( stroke_pixels => [] ) : () )
    };

    shade_column( $shade, $colour, $x, $y, $pattern, $border, 'right-then-left', [] );

    return svg_add_area($shade);
}

sub compute_sub {
    my ( $subroutine ) = @_;
    foreach my $command ( @{$subroutine->{commands}} ) {
        warn join( "\t", $command->{command}, @{$command->{arguments}} ) if $debug > 1;
        $illustrator_commands{$command->{command}}(@{$command->{arguments}});
        warn "$basecursor_x $basecursor_y" if $debug > 1;
    }
}

sub noop {
    svg_finalise_path;
}

#
# ILLUSTRATOR COMMANDS
#

%illustrator_commands = (

    #
    # Attribute commands
    #

    'INK' => sub { # Set ink colour for drawing
        ($ink_colour) = @_;
        noop;
    },

    'PAPER' => sub { # Set paper colour for drawing.
        ($paper_colour) = @_;
        noop;
    },

    'BRIGHT' => sub {
        # Set bright attribute for drawing.
        # Illustrator accepts values of '0', '1', and '8', but '8' seems to be the same as '0'
        ($bright) = $_[0];
        noop;
    },

    'FLASH' => sub {
        # Set flash attribute for drawing.
        # Illustrator accepts values of '0', '1', and '8', but '8' seems to be the same as '0'
        ($flash) = $_[0];
        noop;
    },

    #
    # Move commands
    #

    'AMOVE' => sub {
        # Move the drawing position to the absolute coordinates (x,y).
        my ( $x, $y ) = @_;
        noop;
        set_basecursor($x,$y);
    },

    'MOVE' => sub {
        # Move by a number of pixels relative to the current drawing position.
        my ($x, $y) = @_;
        noop;
        update_basecursor(scale($x),scale($y));
    },

    #
    # Draw commands
    #

    'PLOT' => sub {
        # As AMOVE, but plots a point at the coordinates as well.  The flags (i and o) are for Inverse and Over respectively.
        my ($inverse, $overwrite, $x, $y) = @_;
        noop;
        set_basecursor($x,$y);
        plot( current_colour($inverse,$overwrite) );
    },

    'RPLOT' => sub {
        # Move by 0 or 1 pixels along each axis and plot a point.  Note: this is immune to scaling
        my ($overwrite, $inverse, $dx, $dy) = @_;
        update_basecursor($dx,$dy);
        plot( current_colour($inverse,$overwrite), $dx, $dy );
    },

    'LINE' => sub {
        # As MOVE, but draws a line. The flags (i and o) are for Inverse and Over.
        my ($inverse, $overwrite, $x, $y) = @_;
        line( current_colour($inverse,$overwrite), $x, $y );
        update_basecursor(scale($x),scale($y));
    },

    #
    # Fill commands
    #

    'BLOCK' => sub {
        # set screen attributes in a block (w*8, h*8) in size at (x*8, y*8).
        # * the man page talks about "moving to the point", but that doesn't mean updating the base cursor
        # * block co-ordinates start at the *top-left*, not the bottom-left like other co-ordinates
        my ($h, $w, $x, $y) = @_;
        $w = ($w+1)*8;
        $h = ($h+1)*8;
        rectangle( $x*8, $screen_height-$h-($y*8), $w, $h );
    },

    'SHADE' => sub {
        # do a shaded flood fill in a texture given by "f".
        # Note: the man page likens this to MOVE, but $basecursor_x and $basecursor_y aren't updated
        my ($x, $y, $f) = @_;
        shade($x, $y, $f);
    },

    'BSHADE' => sub {
        # As SHADE, but overwrites the borders of the shaded area.
        my ($x, $y, $f) = @_;
        shade($x, $y, $f, 1);
    },

    'FILL' => sub {
        # As SHADE, but does a solid fill rather than shaded.
        my ($x, $y) = @_;
        fill($x, $y);
    },

    #
    # Meta commands
    #

    'GOSUB' => sub {
        # Draw the image for another subroutine at the current coordinates, with scale "sc".  Scale 0 is full-size; Scale 1 is 1/8, Scale 2 is 2/8, ...
        my ($s, $subroutine) = @_;
        ( $s, $scale ) = ( $scale, $s || 8 );
        noop;
        compute_sub($subroutines->[$subroutine]);
        noop;
        $scale = $s;
    },

    'END' => sub { # End of drawing opcodes.
        noop;
    },

);

#
# CONVERSION FUNCTIONS
#

sub render_pnm {

    die $#attribute_blocks if $#attribute_blocks >= $attribute_height;
    my $pnm_paper = "P6\n$attribute_width $attribute_height\n255\n";
    my $pnm_ink   = "P6\n$attribute_width $attribute_height\n255\n";
    foreach my $y ( reverse 0..($attribute_height-1) ) {
        my $row_attribute_blocks = $attribute_blocks[$y];
        die $#$row_attribute_blocks if $#$row_attribute_blocks >= $attribute_width;
        foreach my $x ( 0..($attribute_width-1) ) {
            my $colour = $row_attribute_blocks->[$x];
            $colour->{pnm_paper} = pack( 'C*', @{$byte_colours[$colour->{paper} + ($colour->{bright}?8:0)]} );
            $colour->{ink} = $colour->{paper} if $colour->{ink} == 8; # transparent
            $colour->{pnm_ink} = pack( 'C*', @{$byte_colours[$colour->{ink  } + ($colour->{bright}?8:0)]} );
            $pnm_paper .= $colour->{pnm_paper};
            $pnm_ink   .= $colour->{pnm_ink  };
        }
    }

    # generate image:
    my $pnm_a         = "P6\n$viewbox_width $viewbox_height\n255\n";
    my $pnm_b         = "P6\n$viewbox_width $viewbox_height\n255\n";
    my $pnm_selection = "P1\n$viewbox_width $viewbox_height\n";
    my $has_flash;
    foreach my $y ( reverse( ($screen_height-$viewbox_height)..($screen_height-1) ) ) {
        my $row = $screen[$y] // [];
        foreach my $x ( 0..($viewbox_width-1) ) {
            my $attribute = $attribute_blocks[round($y/8)][round($x/8)];
            if ( $attribute->{flash} ) {
                $has_flash = 1;
                $pnm_b .= $attribute->{$row->[$x] ? 'pnm_paper' : 'pnm_ink' };
            } else {
                $pnm_b .= $attribute->{$row->[$x] ? 'pnm_ink' : 'pnm_paper' };
            }
            $pnm_a .= $attribute->{$row->[$x]      ? 'pnm_ink' : 'pnm_paper' };
            $pnm_selection .= $row->[$x] ? 0 : 1;
        }
    }

    return (
        $pnm_paper,
        $pnm_ink,
        $pnm_selection,
        $pnm_a,
        ( $has_flash ? $pnm_b : () ),
    );

}

sub pixels_to_paths {
    # Combine a matrix of pixels into a list of identically-coloured polygons
    my ( $area, $multiplier, $class, $colour_name1, $colour_name2, $fill ) = @_;

    # Step one: build polygons:
    my @polygons;
    my %colours;

    foreach my $y ( ($bottom_row/$multiplier)..$#$area ) {
        my $row = $area->[$y];
        foreach my $x ( 0..$#$row ) {
            my $colour1 = $row->[$x]->{$colour_name1} + ($row->[$x]->{bright}?8:0);
            my $colour2 = $row->[$x]->{$colour_name2} + ($row->[$x]->{bright}?8:0);
            my $colour = $row->[$x]->{flash} ? "$colour1 $colour2" : $colour1;
            $colours{$colour}++;
            my ( $polygon ) = grep(
                {
                  (($_->{colour}//-1) eq $colour ) && # colour matches
                  (
                   $_->{screen}[$y-1][$x] || # existing element above
                   $_->{screen}[$y][$x-1]    # existing element to the left
                  )
                } @polygons
                );
            push( @polygons, $polygon = { colour => $colour, screen => [] } )
                unless $polygon;
            $polygon->{screen}[$y][$x] = 1;
        }
    }

    return unless @polygons;

    my @path_data;

    # Step two: if the image is mainly one colour, use it as a default:
    my ( $default_colour, $next_colour ) = sort( { $colours{$b} <=> $colours{$a} } keys %colours );
    if ( (!$next_colour) || ( $colours{$default_colour} > $colours{$next_colour} * 5 ) ) {
        my ( $top, $bottom, $left, $right ) = ( ($screen_height-$viewbox_height)/$multiplier, $screen_height/$multiplier, 0, $screen_width/$multiplier );
        push(
            @path_data,
            {
                colour => $default_colour,
                lines  => [
                    { from => [ $left,$bottom], to => [ $left,$top   ] },
                    { from => [ $left,$top   ], to => [$right,$top   ] },
                    { from => [$right,$top   ], to => [$right,$bottom] },
                    { from => [$right,$bottom], to => [ $left,$bottom] },
                    ]
            }
            );
    } else {
        $default_colour = -1;
    }

    # Step three: calculate lines:
    foreach my $polygon ( @polygons ) {
        my ( @top_lines, @bottom_lines, @left_lines, @right_lines );
        my ( $colour, $screen ) = @{$polygon}{qw/ colour screen /};
        next if $colour eq $default_colour;
        foreach my $y ( 0..$#$screen ) {
            my $row = $screen->[$y];
            foreach my $x ( 0..$#$row ) {
                if ( $row->[$x] ) {
                    if ( $y == 0 || !$screen->[$y-1][$x] ) {
                        # draw top border
                        my ( $line ) = grep( { $_->{y} == $y && $_->{right} == $x } @top_lines );
                        push( @top_lines, $line = { y => $y, left => $x, right => $x } )
                            unless $line;
                        ++$line->{right};
                    }
                    if ( $x == 0 || !$screen->[$y][$x-1] ) {
                        # draw left border
                        my ( $line ) = grep( { $_->{x} == $x && $_->{bottom} == $y } @left_lines );
                        push( @left_lines, $line = { x => $x, top => $y, bottom => $y } )
                            unless $line;
                        ++$line->{bottom};
                    }
                    if ( $y == $screen_width/$multiplier-1 || !$screen->[$y+1][$x] ) {
                        # draw bottom border
                        my ( $line ) = grep( { $_->{y} == $y+1 && $_->{right} == $x } @bottom_lines );
                        push( @bottom_lines, $line = { y => $y+1, left => $x, right => $x } )
                            unless $line;
                        ++$line->{right};
                    }
                    if ( $x == $screen_width/$multiplier-1 || !$screen->[$y][$x+1] ) {
                        # draw right border
                        my ( $line ) = grep( { $_->{x} == $x+1 && $_->{bottom} == $y } @right_lines );
                        push( @right_lines, $line = { x => $x+1, top => $y, bottom => $y } )
                            unless $line;
                        ++$line->{bottom};
                    }
                }
            }
        }
        push(
            @path_data,
            {
                colour => $colour,
                lines => [
                    map( {{ from => [$_->{left},$_->{y}], to => [$_->{right},$_->{y}] }} @top_lines ),
                    map( {{ from => [$_->{right},$_->{y}], to => [$_->{left},$_->{y}] }} @bottom_lines ),
                    map( {{ from => [$_->{x},$_->{bottom}], to => [$_->{x},$_->{top}] }} @left_lines ),
                    map( {{ from => [$_->{x},$_->{top}], to => [$_->{x},$_->{bottom}] }} @right_lines ),
                    ]
            }
        );
    }

    # Step four: create SVG elements:
    my ( @defs, @paths );

    my $path_id = 1;
    foreach my $path ( @path_data ) {
        my ( $colour, $lines ) = @{$path}{qw/ colour lines /};
        my ( $pos_x, $pos_y ) = ( 'Inf', 'Inf' );

        my $id = "pattern-$class-$room-$path_id";
        if ( $colour =~ /^(\d+) (\d+)$/ ) {
            # flashing
            my ( $colour1, $colour2 ) = ( $css_colours[$1], $css_colours[$2] );
            if ( $fill == 255 ) {
                push( @defs, {
                    id => $id,
                    text =>
                        "    <pattern id=\"$id\" width=\"$viewbox_width\" height=\"$viewbox_height\" patternUnits=\"userSpaceOnUse\">\n" .
                        "      <rect x=\"0\" y=\"0\" width=\"$viewbox_width\" height=\"$viewbox_height\">\n" .
                        "        <animate attributeType=\"XML\" attributeName=\"fill\" from=\"$colour1\" to=\"$colour2\" calcMode=\"discrete\" dur=\"0.64s\" repeatCount=\"indefinite\"/>\n" .
                        "      </rect>\n" .
                        "    </pattern>\n"
                });
            } else {
                my $dots = '';
                foreach my $x ( 0..3 ) {
                    foreach my $y ( 0..1 ) {
                        $dots .=
                            "      <rect x=\"$x\" y=\"$y\" width=\"1\" height=\"1\">\n" .
                            "        <animate attributeType=\"XML\" attributeName=\"fill\" from=\"$colour1\" to=\"$colour2\" calcMode=\"discrete\" dur=\"0.64s\" repeatCount=\"indefinite\"/>\n" .
                            "      </rect>\n"
                            if shade_pixel_at($fill, $x, 1-$y);
                    }
                }
                push( @defs, {
                    id => $id,
                    text =>
                        "    <pattern id=\"$id\" width=\"4\" height=\"2\" patternUnits=\"userSpaceOnUse\">\n" .
                        $dots .
                        "    </pattern>\n"
                });
            }
            $colour = "url(#$id)"
        } elsif ( $fill == 255 ) {
            $colour = $css_colours[$colour];
        } else {
            my $dots = '';
            foreach my $x ( 0..3 ) {
                foreach my $y ( 0..1 ) {
                    $dots .=
                        "      <rect fill=\"$css_colours[$colour]\" x=\"$x\" y=\"$y\" width=\"1\" height=\"1\" />\n"
                        if shade_pixel_at($fill, $x, 1-$y);
                }
            }
            push( @defs, {
                id => $id,
                text =>
                    "    <pattern id=\"$id\" width=\"4\" height=\"2\" patternUnits=\"userSpaceOnUse\">\n" .
                    $dots .
                    "    </pattern>\n"
            });
            $colour = "url(#$id)"
        }

        $id = "$class-$room-$path_id";
        if ( @$lines == 4 ) {
            # rectangle
            my ( $min_x, $min_y, $max_x, $max_y ) = ( 'Inf', 'Inf', 0, 0 );
            foreach my $point ( map( { ( $_->{from}, $_->{to} ) } @$lines ) ) {
                $min_x = $point->[0] if $min_x > $point->[0];
                $min_y = $point->[1] if $min_y > $point->[1];
                $max_x = $point->[0] if $max_x < $point->[0];
                $max_y = $point->[1] if $max_y < $point->[1];
            }
            push( @paths, {
                id => $id,
                text => "  <rect id=\"$class-$room-$path_id\" class=\"$class\" fill=\"$colour\" " .
                    "x=\"" . ($min_x*$multiplier) . "\" y=\"" . ($screen_height-$max_y*$multiplier) .
                    "\" width=\"" . (($max_x-$min_x)*$multiplier) . "\" height=\"" . (($max_y-$min_y)*$multiplier) .
                    "\" />\n"
            });
        } else {
            # complex path
            push( @paths, {
                id => $id,
                text => "  <path id=\"$class-$room-$path_id\" class=\"$class\" fill=\"$colour\" d=\""
            });
            while ( @$lines ) {
                my ( $line ) = grep( { $lines->[$_]->{from}->[0] == $pos_x && $lines->[$_]->{from}->[1] == $pos_y } 0..$#$lines );
                if ( defined($line) ) {
                    # continue a previous connected path
                    $line = splice( @$lines, $line, 1 );
                } else {
                    # start a new connected path
                    $line = shift @$lines;
                    $paths[-1]{text} .= " M " . ($line->{from}->[0]*$multiplier) . ',' . ($screen_height-($line->{from}->[1]*$multiplier));
                    ( $pos_x, $pos_y ) = @{$line->{from}};
                }
                my ( $new_x, $new_y ) = @{$line->{to}};
                $paths[-1]{text} .= " l " . (($new_x-$pos_x)*$multiplier) . ',' . (($pos_y-$new_y)*$multiplier);
                ( $pos_x, $pos_y ) = @{$line->{to}};
            }
            $paths[-1]{text} .= "\" />\n";
        }
        ++$path_id;
    }

    return ( \@defs, \@paths );
}

sub render_svg {

    my ( $room ) = @_;

    my ( $defs, $paths ) = ( "", "" );

    #
    # Step one: draw background rectangles
    #

    my ( $block_defs, $block_paths ) = pixels_to_paths( \@attribute_blocks, 8, "block", 'paper', 'ink', 255 );
    $defs  .= $_->{text} foreach @$block_defs;
    $paths .= $_->{text} foreach @$block_paths;

    #
    # Step two: draw foreground shapes
    #

    my $line_id = 1;
    foreach my $path ( @svg_areas, @svg_paths ) {

        # Calculate line colour (or colour gradient):
        my $multicoloured = 0;
        my $has_animation = 0;
        my $first_colour;
        my @colours = ();

        foreach my $pixel ( map( { @{$_->{pixels}} } @{$path->{commands}} ), @{$path->{fill_pixels}} ) {
            my ( $x, $y ) = @$pixel;

            my $colour = $attribute_blocks[round($y/8)][round($x/8)];
            push( @colours, [ $x, $y, $colour ] );

            $has_animation |= $colour->{flash};
            if ( defined($first_colour) ) {
                $multicoloured |=
                    $colour->{ink   } != $first_colour->{ink   } ||
                    $colour->{bright} != $first_colour->{bright} ||
                    $colour->{flash } != $first_colour->{flash } ||
                    ( $colour->{flash} && $colour->{paper} != $first_colour->{paper} )
                    ;
            } else {
                $first_colour = $colour;
            }

        }

        my $colour; # single-colour mode

        if ( $multicoloured || $has_animation ) {

            # step one: expand colour blocks until they fill the screen:
            my @area;
            while ( @colours ) {
                my ( $x, $y, $colour ) = @{shift @colours};
                next if defined($area[$y][$x]);
                $area[$y][$x] = $colour;
                if ( $x > 0 ) {
                    push( @colours, [ $x-1, $y-1, $colour ] ) if $y >= $bottom_row;
                    push( @colours, [ $x-1, $y  , $colour ] );
                    push( @colours, [ $x-1, $y+1, $colour ] ) if $y < $screen_height;
                }

                push(     @colours, [ $x  , $y-1, $colour ] ) if $y >= $bottom_row;

                push(     @colours, [ $x  , $y+1, $colour ] ) if $y < $screen_height;

                if ( $x < $viewbox_width-1 ) {
                    push( @colours, [ $x+1, $y-1, $colour ] ) if $y >= $bottom_row;
                    push( @colours, [ $x+1, $y  , $colour ] );
                    push( @colours, [ $x+1, $y+1, $colour ] ) if $y < $screen_height;
                }
            }

            my $id = "pattern-$path->{class}-$line_id";
            my ( $line_defs, $line_paths ) = pixels_to_paths( \@area, 1, $id, 'ink', 'paper', $path->{fill} || 255 );

            if ( $multicoloured ) {
                $defs .= $_->{text} foreach @$line_defs;
                $defs .= "    <pattern id=\"$id\" width=\"256\" height=\"96\" patternUnits=\"userSpaceOnUse\">\n";
                foreach my $path ( @$line_paths ) {
                    $path->{text} =~ s/^/    /mg;
                    $defs .= $path->{text};
                }
                $defs .= "    </pattern>\n";
                $colour = "url(#$id)";
            } else {
                # must be animated
                die if $#$line_defs;
                $defs .= $line_defs->[0]->{text};
                $colour = "url(#$line_defs->[0]->{id})";
            }

        } elsif ( $path->{stroke} eq 'none' ) {
            $colour = $css_colours[($first_colour->{bright}?8:0) + $first_colour->{ink}];
        } else {
            $colour = $css_colours[($first_colour->{bright}?8:0) + $first_colour->{$path->{stroke}}];
        }

        my $stroke = ( $path->{stroke} eq 'none' ) ? 'none': $colour;

        my $fill = 'none';
        if ( $path->{fill} ) {

            if ( $path->{fill} == 255 ) {
                $fill = $colour;
            } elsif ( $multicoloured ) {
                $fill = $colour;
            } else {
                $defs .= "    <pattern id=\"fill-$path->{class}-$room-$line_id\" x=\"0\" y=\"0\" width=\"4\" height=\"2\" patternUnits=\"userSpaceOnUse\">\n";
                foreach my $x ( 0..3 ) {
                    foreach my $y ( 0..1 ) {
                        $defs .= "      <rect fill=\"$colour\" x=\"$x\" y=\"$y\" width=\"1\" height=\"1\" />\n" if shade_pixel_at($path->{fill}, $x, 1-$y);
                    }
                }
                $defs .= "    </pattern>\n";
                $fill = "url(#fill-$path->{class}-$room-$line_id)";
            }

        }

        my $class = $path->{class};
        $class .= ' closed' if $path->{closed};
        $paths .= "  <path id=\"$path->{class}-$room-$line_id\" class=\"$class\" fill=\"$fill\" stroke=\"$stroke\" d=\"$path->{d}\" />\n";
        ++$line_id;

    }

    $defs = "  <defs>\n$defs  </defs>\n" if $defs;

    return <<END
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.0//EN" "http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd">
<svg id="room-$room" viewBox="0 0 $viewbox_width $viewbox_height" style="fill-rule:evenodd;stroke-linecap:square;stroke-linejoin:round" height="100%" width="100%" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
$defs$paths</svg>
END
;

}

#
# PUBLIC FUNCTIONS
#

sub initialise {
    my %args = @_;
    $subroutines = $args{data}->{graphics};
    my ( $template ) = grep( { $_->{id} eq $args{pattern_template} } @pattern_templates );
    @pattern_template = (
        # convert values to numbers, and convert for upside-down Y co-ordinates:
        [map( { 0+$_ } @{$template->{pattern}}[4..7] )],
        [map( { 0+$_ } @{$template->{pattern}}[0..3] )],
    );
    my $all_images = $args{all_images};
    $location_height = $viewbox_height = $args{height} // 96;

    return grep( { $all_images || $_->{is_location} } @$subroutines );

}

sub compile {

    my ( $image ) = @_;

    $room = $image->{room};

    if ( $image->{is_location} ) {
        $viewbox_height = $location_height;
        $bottom_row = $screen_height - $viewbox_height;
    } else {
        $viewbox_height = $screen_height;
        $bottom_row = 0;
    }

    #
    # Initialise the state
    #
    ($basecursor_x, $basecursor_y, $scale, $flash, $bright) = ( 0, 0, 8, 0, 0 );
    ($ink_colour, $paper_colour) = ( $image->{ink}, $image->{paper} );
    foreach my $y ( 0..($attribute_height-1) ) {
        foreach my $x ( 0..($attribute_width-1) ) {
            $attribute_blocks[$y][$x] = current_colour;
        }
    }
    @svg_areas = ();
    @svg_paths = ();
    undef $svg_current_path;

    #
    # Run the commands
    #
    @screen = ();
    @screen_commands = ();
    $command_id = 0;
    compute_sub($image);

    #
    # Finalise the state
    #
    my ( $pnm_paper, $pnm_ink, $pnm_selection, @pnm ) = render_pnm;
    my ( $svg ) = render_svg($room);

    return {
        source => $image,
        room   => $room++,
        pnm    => \@pnm,
        svg    => $svg,
        # debugging use only:
        pnm_paper     => $pnm_paper,
        pnm_ink       => $pnm_ink,
        pnm_selection => $pnm_selection
    };

}

1;
