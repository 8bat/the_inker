#!/usr/bin/perl

use warnings;
use strict;

open( my $fh, '<:bytes', $ARGV[0] );

my $format = <$fh>;
die $format unless $format eq "P6\n";

my $size = <$fh>;
while ( $size =~ /^#/ ) {
    $size = <$fh>;
}
die unless $size eq "320 240 255\n";

undef $/;

my @data = split( '', <$fh> );

my @blocks;

for ( my $y=0; $y!=24; ++$y ) {
    push( @blocks, [ $y, 0, 320 ] );
}

for ( my $y=24; $y!=216; ++$y ) {
    push( @blocks, [ $y, 0, 32 ] );
    push( @blocks, [ $y, 288, 320 ] );
}

for ( my $y=216; $y!=240; ++$y ) {
    push( @blocks, [ $y, 0, 320 ] );
}


# Calculate styles:
my %styles;
foreach my $block ( @blocks ) {
    my ( $y, $x_min, $x_max ) = @$block;
    my @row_lines;
    for ( my $x=$x_min; $x!=$x_max; ++$x ) {
        my $offset = ($y*320+$x)*3;
        my $colour = sprintf( "#%02x%02x%02x", map( { ord($_) } @data[$offset..($offset+2)] ) );
        push( @row_lines, { colour => $colour, left => $x-$x_min, right => $x-$x_min } )
            unless @row_lines && $row_lines[-1]->{colour} eq $colour;
        ++$row_lines[-1]{right};
    }
    my $width = ($x_max-$x_min);
    my $id = "#block-$y-$x_min";
    if ( $#row_lines ) {
        my $style = "linear-gradient(to right";
        foreach my $line ( @row_lines ) {
            $style .=
                ', ' . $line->{colour} . ' ' . ($line->{left }*100/$width) . '%' .
                ', ' . $line->{colour} . ' ' . ($line->{right}*100/$width) . '%'
        }
        push( @{$styles{$style.')'}}, $id );
    } else {
        push( @{$styles{$row_lines[0]->{colour}}}, $id );
    }
}


# Print:
print "<style type=\"text/css\">\n";
foreach my $style ( sort keys %styles ) {
    print join( ', ', @{$styles{$style}} ), ' { background: ', $style, " }\n";
}
print "</style>\n";
print "<div id=\"flash-block\">";
foreach my $block ( @blocks ) {
    my ( $y, $x_min, $x_max ) = @$block;
    my $width = ($x_max-$x_min);
    print "<div id=\"block-$y-$x_min\" style=\"position: absolute; left: ", $x_min/3.2, "%; width: ", $width/3.2, "%; top: ", $y/2.4, "%; height: 0.5vh;\"></div>\n";
}
print "</div>";
