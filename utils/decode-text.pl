#!/usr/bin/perl
#
# Quill obfuscates text in .SNA files, this utility reverses that process.
# It converts every byte in the file, so the output is only useful for
# finding the byte locations of strings in a snapshot.

use warnings;
use strict;

use File::Slurp;

my $data = read_file(shift @ARGV, binmode => ':raw');

$data =~ s/(.)/chr(0xFF-ord($1))/ges;

print $data;
