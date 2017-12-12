#!/usr/bin/perl

use warnings;
use strict;

use File::Slurp;

if ( @ARGV < 3 || ! -f $ARGV[0] ) {
    print <<END
Usage: $0 <filename> <offset> <byte1> [byte2] ...

Find locations in the file that match the specified pattern.

Useful for finding the file offset associated with a Quill file.

Example usage:

    Given this UnQuill output:
        1c2b:               AT       28
        1c2d:               EQ       16  255

    Run this command:
        $ $0 cpc.sna 1c26 . 28 . 16 255
        Possible offset: Quill location 0000 == file byte 266

    Note: an argument of "." matches any value.

END
;
    exit 1;
}

my $data = read_file(shift @ARGV, binmode => ':raw');

my $offset = hex(shift @ARGV);

my $regexp = join( '', map( { $_ eq '.' ? '.' : sprintf('\x%02x',$_) } @ARGV ) );

print "Hex string: ", join( ' ', map( { $_ eq '.' ? '..' : sprintf('%02x',$_) } @ARGV ) ), "\n";
while ( $data =~ /($regexp)/sg ) {
    printf "Pattern found at location %08x\n", pos($data) - length($1);
    printf "Possible offset: Quill location 0000 == file byte %d\n", pos($data) - length($1) - $offset;
}
