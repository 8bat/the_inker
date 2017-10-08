#!/usr/bin/perl
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

use warnings;
use strict;
use Getopt::Long;

use File::Spec;

BEGIN {
    # find The Inker's library directory:
    push( @INC, "$1/../lib" ) if __FILE__ =~ m{^(.*)[\\/]};;
}

use TheScribe;
use TheInker;

my $out = "images";
my $debug = 0;
my $pattern_template = 'vbca';
my $height = 96;
my $help = 0;

GetOptions(
    "debug" => \$debug,
    "out=s" => \$out,
    "pattern-template=s" => \$pattern_template,
    "height=i" => \$height,
    "help"     => \$help,
) or exit 1;

sub to_text {
    my ( $markup ) = @_;
    return $markup unless defined $markup;
    $markup = join( '', map( { $_->{contents}//'' } @$markup ) );
    $markup =~ s/\\n/\n/g;
    return $markup;
}

sub write_file {
    my ( $filename, $data ) = @_;
    open( my $fh, '>', $filename );
    print( $fh $data );
    close( $fh );
    return;
}

if ( @ARGV && !$help ) {

    my $exit = 0;

    mkdir $out unless -d $out;

    open( my $index_fh, '>', "$out/index.html" ) or die "$!: $out/index.html";
    print( $index_fh <<END
<html>
 <head>
  <title>Gallery of rooms</title>
 </head>
 <body>
  <!-- Note: run convert-to-gif.sh to create GIF images -->
  <ol start="0">
END
    );

    foreach my $arg ( @ARGV ) {
        unless ( -f $arg ) {
            print "Skipping $arg (file not found)\n";
            next;
        }
        my $data = TheScribe::load($arg,'spectrum');
        unless ( $data ) {
            print "Unknown file format: $arg\n";
            $exit = 1;
            next;
        }
        my @subroutines = TheInker::initialise(
            data             => $data,
            pattern_template => $pattern_template,
            height           => $height,
            all_images       => $debug,
        );
        my $n = -1;
        my $list_id = -1;
        foreach my $subroutine ( @subroutines ) {
            ++$n;
            printf( STDERR "\r$arg -> $out: %d%%", 100 * $n / @subroutines );
            my $image = TheInker::compile($subroutine);
            write_file( "$out/$image->{room}-a.pnm"  , $image->{pnm}[0] );
            write_file( "$out/$image->{room}-b.pnm"  , $image->{pnm}[1] ) if $image->{pnm}[1];
            write_file( "$out/$image->{room}.svg"    , $image->{svg} );
            if ( $debug ) {
                write_file( "$out/$image->{room}-paper.pnm"    , $image->{pnm_paper} );
                write_file( "$out/$image->{room}-ink.pnm"      , $image->{pnm_ink} );
                write_file( "$out/$image->{room}-selection.pnm", $image->{pnm_selection} );
            }
            while ( ++$list_id < $image->{room} ) {
                my $description = to_text($data->{room}->[$list_id]->{description}) // '(not a location image)';
                print( $index_fh "<li title=\"$description\">(no image for this location)\n" );
            }
            my $description = to_text($data->{room}->[$list_id]->{description}) // '';
            print( $index_fh "<li title=\"$description\"><img width=512 height=192 src=\"$list_id.gif\"><img width=512 height=192 src=\"$list_id.svg\">\n" );
        }
        printf( STDERR "\r$arg -> $out: done.\n" );
    }

    print( $index_fh <<END
  </ol>
 </body>
</html>
END
    );

    print "Gallery saved to file://" . File::Spec->rel2abs($out) . "/index.html\n";

    exit $exit;

} else {

    print <<END
Usage: $0 [ options ] <unquill-filename>

Convert a set of outputs from Unquill to images.
options:
  --debug                    Save debugging images (colour,ink images)
  --out=<directory>          Output directory (default: "$out")
  --height=<0-255>           Height of the output image in pixels (default: $height)
  --pattern-template=<0-255> Template for generating shade patterns (default: $pattern_template)

DEBUGGING IMAGES

When debugging The Inker itself, it can be useful to examine:

* paper and ink images - 32x22 images describing the image colour
* selection images - 256x172 images describing whether to select paper or ink colour
* non-location images - sub-images drawn within other images

The "--debug" flag enables all of these.

PATTERN TEMPLATES

The Quill describes patterns using numbers from 0 to 255,
However, these numbers have different meanings in different games.
The following pattern templates are supported by The Inker:
END
;
    foreach my $pattern ( @TheInker::pattern_templates ) {
        printf "  %4s %s\n", $pattern->{id}, $pattern->{desc};
    }
    print "\n";
;
    exit 1;

}
