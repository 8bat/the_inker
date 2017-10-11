#!/usr/bin/perl
#
# Calculate absolute co-ordinates along a path

use warnings;
use strict;

use Getopt::Long;

my $help = 0;

GetOptions(
    "help" => \$help,
) or exit 1;

if ( $help ) {
    print <<END
Usage: $0 [ files ]

Calculate absolute co-ordinates along an SVG path
(e.g. M 0,10 l 10,0 v -10 h -10)

Each calculation should be on one line,
reads from standard input if no files are given.
END
;
    exit 0;
}

while ( <> ) {
    next unless /\S/;
    print $_;
    my ( $pos_x, $pos_y ) = ( 0, 0 );
    my @lines;
    my $max_length_t = 7;
    my $max_length_x = 0;
    my $max_length_y = 0;
    while ( /(([MLlhvc]) *((?:-?[0-9.]+)(?:[, ]+-?[0-9.]+)*))/g ) {
        my ( $text, $command, $args ) = ( $1, $2, $3 );
        my @args = split( /[, ]+/, $args );
        if ( $command eq 'H' ) {
            ( $pos_x ) = @args;
        } elsif ( $command eq 'V' ) {
            ( $pos_y ) = @args;
        } elsif ( $command eq 'h' ) {
            $pos_x += $args[0];
        } elsif ( $command eq 'v' ) {
            $pos_y += $args[0];
        } elsif ( $command =~ /[A-Z]/ ) {
            ( $pos_x, $pos_y ) = @args;
        } elsif ( $command eq 'c' ) {
            $pos_x += $args[4];
            $pos_y += $args[5];
        } else {
            $pos_x += $args[0];
            $pos_y += $args[1];
        }
        push( @lines, [ $text, $pos_x, $pos_y ] );
        $max_length_t = length($text)  if $max_length_t < length($text);
        $max_length_x = length($pos_x) if $max_length_x < length($pos_x);
        $max_length_y = length($pos_y) if $max_length_y < length($pos_y);
    }
    printf "ID\t%-${max_length_t}s -> %s\n", qw/ Command co-ordinates /;
    foreach my $n ( 0..$#lines ) {
        printf "%d\t%-${max_length_t}s -> %${max_length_x}s,%${max_length_y}s\n", $n+1, @{$lines[$n]};
    }
}
