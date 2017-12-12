#!/usr/bin/perl -Ilib
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
use utf8;
use Getopt::Long;
use File::Copy;

BEGIN {
    # find The Inker's library directory:
    push( @INC, "$1/../lib" ) if __FILE__ =~ m{^(.*)[\\/]};;
}

use TheScribe;

my $out = "adventure";
my $images = "images";
my $help = 0;
my $platform = 'spectrum';

GetOptions(
    "out=s" => \$out,
    "images=s" => \$images,
    "help"     => \$help,
    "platform=s" => \$platform,
) or exit 1;
$out =~ s/\/+$//;
$platform = 'spectrum' if $platform eq 'spec';
$platform = 'spectrum' if $platform eq 'spec+3';

my $output;
my $data;
my ( %nouns, %verbs ) = @_;

my @flag_names = (
    qw(
        yesno_is_dark
        total_carried
        countdown_location_description
        countdown_location_description_dark
        countdown_object_description_unlit
        countdown_player_input_1
        countdown_player_input_2
        countdown_player_input_3
        countdown_player_input_4
        countdown_player_input_dark
        countdown_player_input_unlit
    ),
    map( { "game_flag_$_" } 11..28 ),
    qw(
        bitset_graphics_status
        total_player_score
        total_turns_lower
        total_turns_higher
        word_verb
        word_noun
        room_number
    )
);

my @object_names = map( { "object_$_" } 0..255 );
my @room_names = (
    map( { "room_$_" } 0..251 ),
    'room_noncreated',
    'room_worn',
    'room_carried',
    'room_here',
);


sub print_text {
    my ( $n, $text, $comment ) = @_;
    return unless defined $text;
    print $output '/', $n;
    print $output " ; $comment" if defined $comment;
    print $output "\n", $text, "\n";
}

sub print_condacts {
    my ( $process ) = @_;
    print $output " ; ID: $process->{id}\n";
    foreach my $section ( @{$process}{qw/ conditions actions /} ) {
        foreach my $condition ( @$section ) {
            my ( $command, @args ) = @$condition;
            if ( $command =~ /^(?:at|notat|atgt|atlt|goto)$/ ) {
                @args = map( { "room_$_" } @args );
            } elsif ( $command =~ /^(?:present|absent|worn|notworn|carried|notcarr|remove|get|drop|wear|destroy|create|swap)$/ ) {
                @args = map( { $object_names[$_] } @args );
            } elsif ( $command =~ /^(?:zero|notzero|eq|gt|lt|set|clear|plus|minus|let)$/ ) {
                $args[0] = $flag_names[$args[0]];
            } elsif ( $command eq 'place' ) {
                $args[0] = $object_names[$args[0]];
                $args[1] = "room_$args[1]";
            } elsif ( $command eq 'beep' ) {
                $command = 'beep2';
            }
            if ( @args ) {
                printf( $output " %-7s %s\n", $command, join( ' ', @args ) );
            } else {
                print $output " $command\n"; # do not print trailing spaces
            }
        }
    }
}

sub print_section_header {
    my ( $key, $name, $extra ) = @_;
    my $text = uc($name) . ' SECTION' . ( $extra//'');
    print $output "\n\n;\n; $text\n;\n\n/$key\n";
}

sub compare_commands {
    my ( $a, $b ) = @_;
    return (
         $a->{input}->[0]       cmp  $b->{input}->[0]       ||
        ($a->{input}->[1]//'_') cmp ($b->{input}->[1]//'_') ||
        $a->{id}         <=> $b->{id}
    );
}

sub print_command {
    my ( $verb, $noun ) = @_;
    print(
        $output
        ( ($verb//'_') eq '_' ) ? '_' : $verb,
        ' ',
        ( ($noun//'_') eq '_' ) ? '_' : $noun,
        "\n"
    );
}

sub to_sequence_tags {
    my ( $markup ) = @_;
    my $text = join(
        '',
        map(
            {
                if ( $_->{type} eq 'open-tag' || $_->{type} eq 'close-tag' ) {
                    "";
                } else {
                    $_->{contents};
                }
            }
            @$markup
        )
    );
    $text =~ s/^(?:\s|\\n)*//g;
    $text =~ s/(?:\s|\\n)*$//g;
    $text =~ s/(?:\s|\\n)+/ /g;
    $text =~ s/\&#92;/&pound;/g;
    return $text;
}

if ( @ARGV && !$help ) {

    mkdir $out unless -d $out;
    mkdir "$out/dat" unless -d "$out/dat";

    my $input;
    if ( $ARGV[0] eq '-' ) {
        $input = \*STDIN;
        $output = \*STDOUT;
    } else {
        open(  $input, '<', $ARGV[0] ) or die "$!: $ARGV[0]";
        open( $output, '>:utf8', "$out/code.txp" ) or die "$!: $out/code.txp";
    }

    my $pause_parameter = TheScribe::platform_data($platform)->{pause_parameter};
    if ( $pause_parameter > -1 ) {
        $flag_names[$pause_parameter] = 'pause_parameter';
    }

    $data = TheScribe::load($input,$platform);

    #
    # Parse descriptions
    #

    foreach my $section ( qw/ object room / ) {
        foreach my $value ( @{$data->{$section}} ) {
            $value->{description} = to_sequence_tags($value->{description});
        }
    }
    foreach my $section ( qw/ message system_message / ) {
        @{$data->{$section}} = map( { to_sequence_tags($_) } @{$data->{$section}} );
    }

    #
    # Try to find nouns for objects
    #

    my @object_nouns;
    foreach my $response ( @{$data->{response}} ) {
        next unless $response->{input}->[1];
        next if grep( { $_->[0] !~ /^(?:get|ok|done)$/ } @{$response->{actions}} );
        my @messages = grep( { $_->[0] eq 'get' } @{$response->{actions}} );
        next if $#messages;
        $object_nouns[$messages[0]->[1]] = $data->{vocabulary}->{$response->{input}->[1]};
        $object_names[$messages[0]->[1]] = "object_$messages[0]->[1]_$response->{input}->[1]";
    }

    foreach my $n ( 0..$#{$data->{object}} ) {
        my $object = $data->{object}->[$n];
        my $name = $object->{name} // ''; # unquill for C64 doesn't have a section for object names
        $object_names[$n] = "object_${n}_$name" if $name ne '';
    }

    #
    # Create definitions for later use
    #

    foreach my $n ( 0..$#{$data->{object}} ) {
        print $output "#define const $object_names[$n] = $n\n";
    }

    print $output "\n; Room ID constants:\n";
    foreach my $n ( 0..$#{$data->{room}} ) {
        print $output "#define const room_$n = $n\n";
    }

    #
    # Convert messages to writelns
    #

    foreach my $table ( qw/ response process / ) {
        foreach my $condacts ( @{$data->{$table}} ) {
            foreach my $action ( @{$condacts->{actions}} ) {
                if ( $action->[0] eq 'message' ) {
                    $action->[0] = 'writeln';
                    $action->[1] = '"' . $data->{message}->[$action->[1]] . '"';
                }
            }
        }
    }

    #
    # Control section (always empty)
    #

    print_section_header "CTL", "Control", " (deprecated)";

    #
    # Vocabulary
    #

    print_section_header "VOC", "Vocabulary";

    my @words;
    while ( my ( $word, $value ) = each(%{$data->{vocabulary}}) ) {
        push( @{$words[$value]}, $word ) if defined $value;
    }
    @words = map( { defined($_) ? join( "\t", sort @$_ ) : '' } @words );
    foreach my $n ( sort( { $words[$a] cmp $words[$b] } 0..$#words ) ) {
        print $output "$n\t$words[$n]\n" if $words[$n] ne '';
    }


    #
    # System messages
    #

    print_section_header "STX", "system messages";

    foreach my $n ( 0..$#{$data->{system_message}} ) {
        if ( ($data->{system_message}->[$n]//'') =~ /./ ) {
            print_text( $n, $data->{system_message}->[$n] );
        }
    }


    #
    # Object text
    #

    print_section_header "OTX", "object text";
    foreach my $n ( 0..$#{$data->{object}} ) {
        my $object = $data->{object}->[$n];
        my $description = $object->{description};
        print_text( $object_names[$n], $description );
    }


    #
    # Location text
    #

    print_section_header "LTX", "location text";

    my @room_specific_responses;
    foreach my $condacts ( @{$data->{response}} ) {
        my @at = grep( { $_->[0] eq 'at' } @{$condacts->{conditions}} );
        next if $#at;
        $room_specific_responses[$at[0]->[1]]{
            join(
                ' ',
                $condacts->{input}->[0],
                $condacts->{input}->[1]//()
            )
        } = 1;
    }

    foreach my $n ( 0..$#{$data->{room}} ) {
        my $vocabulary = $data->{vocabulary};
        my $description = $data->{room}->[$n]->{description};
        if ( my @unlinked = map( { lc $_ } sort keys %{$room_specific_responses[$n]//{}} ) ) {
            print_text( "room_$n", $description, join("\n; ", '', @unlinked ) );
        } else {
            print_text( "room_$n", $description );
        }
    }


    #
    # Connections
    #

    print_section_header "CON", "connections";
    foreach my $n ( 0..$#{$data->{room}} ) {
        print $output "/room_$n\n";
        my $to = $data->{room}->[$n]->{to};
        foreach my $key ( sort keys %$to ) {
            printf $output "vb_%s room_%d\n", $key, $to->{$key};
        }
        print $output "\n";
    }


    #
    # Objects
    #

    print_section_header "OBJ", "objects";
    my $object_name_length = 0;
    foreach my $name ( @object_names ) {
        $object_name_length = length($name) if $object_name_length < length($name);
    }
    print $output ";object#" . (' 'x($object_name_length-7)) . " loc             weight    noun adjective attributes\n";
    foreach my $n ( 0..$#{$data->{object}} ) {
        my $object = $data->{object}->[$n];
        next unless defined $object->{initial_location};
        my $id = $data->{vocabulary}->{$object->{name}//''}//0;
        my $attrs = '';
        $attrs .= ' aLight' if $n == 0;
        $attrs .= ' aWearable' if $id >= 200;
        print $output "$object_names[$n]\tSTARTS AT $room_names[$object->{initial_location}]\n";
        print $output "$object_names[$n]\tNOUN      $object->{name}\n" if length($object->{name});
        print $output "$object_names[$n]\tATTRS     $attrs\n";
    }

    #
    # Responses
    #


    print_section_header "PRO 0", "responses", " (process table 0)";

    my @non_default_responses = @{$data->{response}};
    my @default_responses;

    while ( @non_default_responses && !$#{$non_default_responses[-1]->{input}} ) {
        unshift( @default_responses, pop(@non_default_responses ) )
    }

    my $response_id = 0;
    foreach my $response ( @non_default_responses, @default_responses ) {
        $response->{id} = $response_id++;
    }

    foreach my $response ( sort( { compare_commands($a,$b) } @non_default_responses, @default_responses ) ) {
        print $output $response->{comment}//'';
        print_command( @{$response->{input}} );
        print_condacts($response);
        print $output "\n";
    }

    #
    # Statuses
    #

    print_section_header "PRO 2", "statuses", " (process table 2)";

    my $process_id = 0;
    foreach my $process ( @{$data->{process}} ) {
        $process->{id} = $process_id++;
    }

    foreach my $process ( sort( { compare_commands($a,$b) } @{$data->{process}} ) ) {
        print $output "\n";
        print $output $process->{comment}//'';
        print_command( @{$process->{input}} );
        print_condacts($process);
    }

} else {

    print <<END
Usage: $0 [ options ] <unquill-filename>

Convert UnQuill output to a diff-friendly format

Options:
  --out=<directory>     Output directory (default: "$out")
  --images=<directory>  Input images directory (default: "$images")
  --platform=<platform> Platform the game was written for (default: "$platform")

The first argument specifies the UnQuill text file to process
(or standard input if the argument is missing)

The second argument specifies the .txp file to save
(or standard output if the argument is missing)

END
    ;
    exit 1;
}
