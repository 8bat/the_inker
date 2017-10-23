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

my $pause_parameter;

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
my @pause_parameter_names = qw(
    pause_unknown_value_0
    pause_sound_effect_tone_increasing
    pause_sound_effect_telephone
    pause_sound_effect_tone_decreasing
    pause_flash
    pause_sound_effect_white_noise
    pause_sound_effect_machine_gun
    pause_font_default
    pause_font_alternate
    pause_box_wipe
    pause_change_youcansee
    pause_ability
    pause_restart
    pause_reboot
    pause_ability_plus
    pause_ability_minus
    pause_click_effect_1
    pause_click_effect_2
    pause_click_effect_3
    pause_toggle_graphics
    pause_unknown_value_20
    pause_ram_save_load
    pause_set_identity_byte
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
sub print_barrier_text {
    my ( $n ) = @_;
    print_text(
        $n,
        "(extra message added so the newlines below aren't included in the message above)",
    );
}

sub print_condacts {
    my ( $process ) = @_;
    foreach my $section ( @{$process}{qw/ conditions actions /} ) {
        foreach my $condition ( @$section ) {
            my ( $command, @args ) = @$condition;
            if ( $command =~ /^(?:at|notat|atgt|atlt|goto)$/ ) {
                @args = map( { "room_$_" } @args );
            } elsif ( $command =~ /^(?:present|absent|worn|notworn|carried|notcarr|remove|get|drop|wear|destroy|create|swap)$/ ) {
                @args = map( { $object_names[$_] } @args );
            } elsif ( $command eq 'let' && $args[0] == $pause_parameter ) {
                $args[0] = 'pause_parameter';
                $args[1] = $pause_parameter_names[$args[1]] // "pause_unknown_value_$args[1]";
            } elsif ( $command =~ /^(?:zero|notzero|eq|gt|lt|set|clear|plus|minus|let)$/ ) {
                $args[0] = $flag_names[$args[0]];
            } elsif ( $command eq 'place' ) {
                $args[0] = $object_names[$args[0]];
                $args[1] = "room_$args[1]";
            } elsif ( $command eq 'beep' ) {
                if ( $platform eq 'c64' ) {
                    # the C64 has a "SID" command that works on the SID chip:
                    $command = 'sid';
                } else {
                    # ngPAWS has a "beep" command that's incompatible with the old command:
                    $command = 'beep2';
                }
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

sub print_words {
    my ( $header, @words ) = @_;
    return unless @words;
    print $output $header;
    foreach my $key ( @words ) {
        my $value = $data->{vocabulary}->{$key};
        printf $output "%-10s %3d    %s\n", "nn_$key", $value, 'noun' if $nouns{$value};
        printf $output "%-10s %3d    %s\n", "vb_$key", $value, 'verb' if $verbs{$value};
    }
}

sub print_command {
    my ( $verb, $noun ) = @_;
    print(
        $output
        ( ($verb//'_') eq '_' ) ? '_' : "vb_$verb",
        ' ',
        ( ($noun//'_') eq '_' ) ? '_' : "nn_$noun",
        "\n"
    );
}

sub to_sequence_tags {
    my ( $markup ) = @_;
    # Remove redundant tags:
    for ( my $n=0; $n<$#$markup; ++$n ) {
        if ( $markup->[$n  ]->{type} eq 'open-tag' &&
             $markup->[$n+1]->{type} eq 'close-tag'
            ) {
            splice( @$markup, $n--, 2 );
        }
    }
    return join(
        '',
        map(
            {
                if ( $_->{type} eq 'open-tag' ) {
                    "{CLASS|$_->{contents}|";
                } elsif ( $_->{type} eq 'close-tag' ) {
                    "}";
                } else {
                    $_->{contents};
                }
            }
            @$markup
        )
    );
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

    $pause_parameter = TheScribe::platform_data($platform)->{pause_parameter};
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
    # Try to find long descriptions for objects
    #

    # find the verb with the highest number of actions that look like "examine":
    my %examine_verbs;
    foreach my $response ( @{$data->{response}} ) {
        my $verb = $response->{input}->[0];
        next if $verb eq '_';
        next unless $response->{input}->[1];
        next if grep( { $_->[0] ne 'at' } @{$response->{conditions}} );
        next if grep( { $_->[0] !~ /^(?:message|done)$/ } @{$response->{actions}} );
        ++$examine_verbs{$verb};
    }
    my @examine_verbs = sort( { $examine_verbs{$b} <=> $examine_verbs{$a} } keys %examine_verbs );

    my @long_description;
    if ( @examine_verbs &&
         ( @examine_verbs == 1 ||
           $examine_verbs{$examine_verbs[0]} > $examine_verbs{$examine_verbs[1]}*3
         )
        ) {
        my $examine_verb = $examine_verbs[0];
        foreach my $response ( @{$data->{response}} ) {
            next if $response->{input}->[0] ne $examine_verb;
            next unless $response->{input}->[1];
            next if grep( { $_->[0] ne 'at' } @{$response->{conditions}} );
            next if grep( { $_->[0] !~ /^(?:message|done)$/ } @{$response->{actions}} );
            my @messages = grep( { $_->[0] eq 'message' } @{$response->{actions}} );
            next if $#messages;
            $long_description[$data->{vocabulary}->{$response->{input}->[1]}] = '#'.$data->{message}->[$messages[0]->[1]];
        }
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
    # Print header
    #

    print $output <<END
; ngPAWS output created by The Inker
;
; Note: Adventures created with The Quill need to be cleaned up manually
; before they will work properly with ngPAWS.
; Please search for "TODO" lines below.

; Object attributes constants:
#define const aLight       = 0
#define const aWearable    = 1
#define const aContainer   = 2
#define const aNPC         = 3
#define const aConcealed   = 4
#define const aEdible      = 5
#define const aDrinkable   = 6
#define const aEnterable   = 7
#define const aFemale      = 8
#define const aLockable    = 9
#define const aLocked      = 10
#define const aMale        = 11
#define const aNeuter      = 12
#define const aOpenable    = 13
#define const aOpen        = 14
#define const aPluralName  = 15
#define const aTransparent = 16
#define const aScenery     = 17
#define const aSupporter   = 18
#define const aSwitchable  = 19
#define const aOn          = 20
#define const aStatic      = 21

; Object name constants:
END
    ;

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
    print $output <<END
; Special Room ID constants:
#define const room_noncreated = 252 ; destroying an object moves it to this room
#define const room_worn = 253 ; wearing an object moves it to this room
#define const room_carried = 254 ; carrying an object moves it to this room
#define const room_here = 255 ; not used by any Quill condacts
END
        ;

    print $output q{
; Flag IDs:
; Flags beginning "yesno_" are yes/no values (e.g. "is it dark?")
; Flags beginning "total_" represent a count (e.g. "how many objects are carried?")
; Flags beginning "countdown_" are decremented when certain events occur
; Flags beginning "bitset_" are a set of 8 yes/no values
; Flags beginning "word_" represent a word ID in the vocabulary table
; Flags beginning "game_flag_" have game-specific meanings
; Flags ending "_dark" refer to the room being dark
; Flags ending "_unlit" refer to the room being dark *and* the lamp being absent
};
    my $flag_name_length = 0;
    foreach my $name ( @flag_names ) {
        $flag_name_length = length($name) if $flag_name_length < length($name);
    }
    foreach my $n ( 0..$#flag_names ) {
        if ( $n == 35 ) {
            printf $output "#define const %-${flag_name_length}s =  38 ; (35 in The Quill, but 38 in ngPAWS)\n", $flag_names[$n];
        } elsif ( $n >= 11 && $n <= 16 ) {
            printf $output "#define const %-${flag_name_length}s = 1%2d ; ngPAWS assigns a special meaning to flag $n\n", $flag_names[$n], $n;
        } else {
            printf $output "#define const %-${flag_name_length}s =  %2d\n", $flag_names[$n], $n;
        }
    }
    print $output q{
; PAUSE subfunctions:
; Setting the pause_parameter flag will change the behaviour of the PAUSE command:
; sound effects:
#define const pause_sound_effect_tone_increasing = 1 ; increasing tone (value is the duration)
#define const pause_sound_effect_telephone       = 2 ; ringing telephone (value is the number of rings)
#define const pause_sound_effect_tone_decreasing = 3 ; decreasing tone (opposite of effect 1)
#define const pause_sound_effect_white_noise     = 5 ; white noise (resembles an audience clapping, value is the number of repetitions)
#define const pause_sound_effect_machine_gun     = 6 ; machine gun noise (value is the duration)
; visual effects:
#define const pause_flash = 4 ; flash the screen and the border (value is the number of flashes)
#define const pause_box_wipe = 9 ; a series of coloured boxes expands out of the centre of the screen at speed
; change font:
#define const pause_font_default = 7 ; switch to the default font
#define const pause_font_alternate = 8 ; switch to the alternative font.
; change messages:
#define const pause_change_youcansee = 10 ; Change the "You can also see" message to the message number passed to PAUSE
; Control commands:
#define const pause_restart = 12 ; Restart the game without warning.
#define const pause_reboot = 13 ; Reboot the Spectrum without warning
#define const pause_ability = 11 ; Set the maximum number of objects carried at once to the value passed to PAUSE
#define const pause_ability_plus = 14 ; Increase number of objects that can be carried at once by the amount passed to PAUSE
#define const pause_ability_minus = 15 ; Decrease number of objects that can be carried at once by the amount passed to PAUSE
#define const pause_toggle_graphics = 19 ; switch graphics off (for PAUSE 255) or on (for any other value)
#define const pause_ram_save_load = 21 ; RAMload (for PAUSE 50) or RAMsave (for any other value)
#define const pause_set_identity_byte = 22 ; Set the identity in saved games to the value passed to PAUSE
; change the keyboard click sound:
#define const pause_click_effect_1 = 16
#define const pause_click_effect_2 = 17
#define const pause_click_effect_3 = 18
} if $pause_parameter != -1;

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
    # Graphics
    #

    print $output "\n; Location graphics:\n";
    foreach my $n ( 0..$#{$data->{graphics}} ) {
        if ( $data->{graphics}->[$n]->{is_location} ) {
            # ngPAWS looks for the files in $out/dat
            # by default, txtpaws checks there is a file with the same name in the current directory.
            # (there is a command-line option to look elsewhere, but it doesn't seem to work)
            open( my $fh, '>', "$out/location$n.svg" );
            copy( "$images/$n.svg", "$out/dat/location$n.svg" ) or die "$!: $out/dat/location$n.svg";
            printf( $output "#define pic %-16s = %3d\n", "location$n.svg", $n );
        }
    }
    print $output "\n";

    #
    # Control section (always empty)
    #

    print_section_header "CTL", "Control", " (deprecated)";

    #
    # Vocabulary
    #

    print_section_header "VOC", "Vocabulary";

    %nouns = map( { $data->{vocabulary}->{$_->{name}//''} ? ( $data->{vocabulary}->{$_->{name}} => 1 ) : () } @{$data->{object}} );
    foreach my $response ( @{$data->{response}} ) {
        my $input = $response->{input};
        next if $input->[0] eq '_';
        $verbs{$data->{vocabulary}->{$input->[0]}} = 1;
        $nouns{$data->{vocabulary}->{$input->[1]}} = 1 if $#$input;
    }

    my @words = (
        sort(
            { $data->{vocabulary}->{$a} <=> $data->{vocabulary}->{$b} || $a cmp $b }
            grep( { defined $data->{vocabulary}->{$_} } keys %{$data->{vocabulary}} )
        )
    );

    print_words q{
; -- Movements, verbs and nouns --
; words with value less than 14 are treated as requests to go in that direction.
; This is the same in both PAW and Quill, so these should behave properly.
;
;word    value    type
}, grep( { $data->{vocabulary}->{$_} < 14 } @words );

    print_words q{
;-- verbs and proper nouns --
; nouns less than 50 are proper nouns and ignored by the pronoun system.
; The Quill didn't have a pronoun system, so these should behave properly.
;
;word    value    type
}, grep( { $data->{vocabulary}->{$_} >= 14 && $data->{vocabulary}->{$_} < 50 } @words );

    print_words q{
;-- verbs and other nouns --
; nouns 50 and above can be used by the pronoun system.
; We don't define any pronouns by default, so these should behave properly.
;
;word    value    type
}, grep( { $data->{vocabulary}->{$_} >= 50 } @words );

    print $output q{
; -- Standard ngPAWS conjuntions (taken from the ngPAWS "start" database) --
;
;word    value    type
AND          2    conjunction
THEN         2    conjunction
};

    #
    # System messages
    #

    print_section_header "STX", "system messages";

    my @default_messages = (
        # taken from the start database
        "It's too dark to see anything.",
        "You can see:",
        "What do you want to do now?",
        "{EXITS|\@38|1000}",
        "Please enter your next order",
        "Please type in your orders",
        "Sorry? Please try other words.",
        "You can't go in that direction.",
        "Sorry?",
        "You are carrying:",
        " (worn)<br>",
        "nothing at all.<br>",
        "Are you sure?<br>",
        "Play again?<br>",
        "Good bye...<br>",
        "OK.<br>",
        "Press any key to continue.<br>",
        "You have typed<br>",
        " turn<br>",
        "s<br>",
        ".<br>",
        "Your score is ",
        "%.<br>",
        "You are not wearing that.<br>",
        "You are already wearing that.<br>",
        "You already have {REF}.<br>",
        "You can't see that here.<br>",
        "You can't carry any more things.<br>",
        "You don't have that.<br>",
        "You are already wearing {OREF}.<br>",
        "Y<br>",
        "N<br>",
        "More...<br>",
        "><br>",
        "<br>",
        "Time passes...<br>",
        "",
        "",
        "",
        "",
        ( $data->{system_message}->[ 8] // "You can't wear {OREF}.<br>" ) . "<br>",
        ( $data->{system_message}->[ 8] // "You can't remove {OREF}.<br>" ) . "<br>",
        ( $data->{system_message}->[24] // "You can't remove {OREF}. Your hands are full!<br>" ) . "<br>",
        ( $data->{system_message}->[27] // "{OREF} weighs too much.<br>" ) . "<br>",
        "You put {OREF} into<br>",
        "{OREF} is not into<br>",
        "<br>",
        "<br>",
        "<br>",
        ( $data->{system_message}->[28] // "You don't have {OREF}." ) . "<br>",
        ( $data->{system_message}->[ 8] // "You are not wearing {OREF}.<br>" ) . "<br>",
        ".<br>",
        "That is not into<br>",
        "nothing at all<br>",
        "File not found.<br>",
        "File corrupt.<br>",
        "I/O error. File not saved.<br>",
        "Directory full.<br>",
        "Please enter savegame name you used when saving the game status.",
        "Invalid savegame name. Please check the name you entered is correct, and make sure you are trying to load the game from the same browser you saved it.<br>",
        "Please enter savegame name. Remember to note down the name you choose, as it will be requested in order to restore the game status.",
        "<br>",
        "Sorry? Please try other words.<br>",
        "Here<br>",
        "you can see<br>",
        "you can see<br>",
        "inside you see<br>",
        "on top you see<br>",
    );

    if ( grep( { ($_//'') !~ /./ } @{$data->{system_message}} ) ) {
        print $output q{;
; ngPAWS defines several extra system messages that didn't exist in The Quill.
; Default messages have been added below, but they might not match the tone
; of your game (too serious, too silly, etc.).
; TODO: check the text of these messages match the tone of your game
};
    }

    foreach my $n ( 0..$#{$data->{system_message}} ) {
        my @message = (
            $default_messages[$n],
            "TODO: check the following message matches the tone of your game:"
        );
        if ( ($data->{system_message}->[$n]//'') =~ /./ ) {
            @message = (
                $data->{system_message}->[$n]
            );
            $message[0] .= '<br>' unless $n =~ /^(?:9|10|17|18|19|21)$/;
        }
        print_text( $n, @message );
    }

    foreach my $n ( @{$data->{system_message}}..$#default_messages ) {
        print_text(
            $n,
            $default_messages[$n],
            "TODO: check the following message matches the tone of your game:"
        );
    }
    print_barrier_text(scalar(@default_messages));

    #
    # User messages
    #

    print_section_header "MTX", "Messages";

    print $output q{
; Messages used by the EXITS condact and sequence tag:
/1000 ; TODO: check the following message matches the tone of your game:
Exits: 
/1001 ; TODO: check the following message matches the tone of your game:
You can't see any exits
; TODO: Make sure the text for each exit looks right:
};
    my %exits;
    foreach my $room ( @{$data->{room}} ) {
        @exits{map( { $data->{vocabulary}->{$_} } keys %{$room->{to}} )} = ();
    }

    foreach my $key ( @words ) {
        $exits{$data->{vocabulary}->{$key}} = $key
            if exists($exits{$data->{vocabulary}->{$key}});
    }
    my $max_exit_id = 0;
    foreach my $id ( sort( { $a <=> $b } keys %exits ) ) {
        $max_exit_id = $id + 1002;
        print $output
            '/', $max_exit_id, "\n",
            "{ACTION|$exits{$id}|$exits{$id}}\n"
            ;
    }
    print_barrier_text($max_exit_id+1);

    #
    # Object text
    #

    print_section_header "OTX", "object text";
    foreach my $n ( 0..$#{$data->{object}} ) {
        my $object = $data->{object}->[$n];
        my $description = $object->{description};
        if ( ($object->{name}//'') ne '' ) {
            $description .= $long_description[$data->{vocabulary}->{$object->{name}}]//'';
        } elsif ( defined($object_nouns[$n]) && defined($long_description[$object_nouns[$n]]) ) {
            $description .= $long_description[$object_nouns[$n]];
        }
        print_text( $object_names[$n], $description );
    }
    print_barrier_text(scalar(@{$data->{object}}));


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
        my @unlinked;
        foreach my $word ( sort keys %{$room_specific_responses[$n]//{}} ) {
            push( @unlinked, '{ACTION|'.lc($word).'|'.lc($word).'}' );
        }
        foreach my $to ( sort keys %{$data->{room}->[$n]->{to}} ) {
            my @words;
            my $action_word = '';
            foreach my $word ( grep( { ($vocabulary->{$_}//-1) == $vocabulary->{$to} } sort keys %$vocabulary ) ) {
                $action_word = $word if length($action_word) < length($word);
                if ( length($word) == 4 ) {
                    push( @words, quotemeta($word) . '.*?' );
                } else {
                    push( @words, quotemeta($word)         );
                }
            }
            my $to_names = join('|',@words);
            unless ( $description =~ s/(^|[^\\])\b($to_names)\b/$1\{ACTION|$2|$2}/gi ) {
                push( @unlinked, '{ACTION|'.lc($action_word).'|'.lc($action_word).'}' );
            }
        }
        if ( @unlinked ) {
            print_text( "room_$n", $description, "TODO: consider adding relevant links: " . join(', ', @unlinked ) );
        } else {
            print_text( "room_$n", $description );
        }
    }
    print_barrier_text(scalar(@{$data->{room}}));


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
        printf(
            $output
            "/%-${object_name_length}s %15s      1 %-7s _       ATTR%s\n",
            $object_names[$n],
            $room_names[$object->{initial_location}],
            length($object->{name}//'')?"nn_$object->{name}":'   _',
            $attrs,
        );
    }

    #
    # Responses
    #


    print_section_header "PRO 0", "responses", " (process table 0)";

    print $output q{;
; ngPAWS includes a system to hook in extra functionality.
; For hooks to work properly, they must be fired in the right order.
; TODO: move responses around so the default responses
;       ("you can't do that", "I see no X here" etc.)
;       are grouped together between the hooks titled
;       "RESPONSE_DEFAULT_START" and "RESPONSE_DEFAULT_END"
};

    my @non_default_responses = @{$data->{response}};
    my @default_responses;

    while ( @non_default_responses && !$#{$non_default_responses[-1]->{input}} ) {
        unshift( @default_responses, pop(@non_default_responses ) )
    }

    foreach my $response (
        {
            comment => "; Callback to hook system, do not delete:\n",
            input => [ '_', '_' ],
            conditions => [],
            actions => [[ hook => '"RESPONSE_START"' ]],
        },
        {
            comment => "; Callback to hook system, do not delete:\n",
            input => [ '_', '_' ],
            conditions => [],
            actions => [[ hook => '"RESPONSE_USER"' ]],
        },
        @non_default_responses,
        ) {
        print $output $response->{comment}//'';
        print_command( @{$response->{input}} );
        print_condacts($response);
        print $output "\n";
    }

    foreach my $response (
        {
            comment => "; TODO: move all default responses below this, and everything else above it:\n",
            input => [ '_', '_' ],
            conditions => [],
            actions => [[ hook => '"RESPONSE_DEFAULT_START"' ]],
        },
        @default_responses,
        {
            comment => "; Callback to hook system, do not delete:\n",
            input => [ '_', '_' ],
            conditions => [],
            actions => [[ hook => '"RESPONSE_DEFAULT_END"' ]],
        },
        ) {
        print $output $response->{comment}//'';
        print_command( @{$response->{input}} );
        print_condacts($response);
        print $output "\n";
    }


    #
    # Post-description actions
    #


    print_section_header "PRO 1", "post-description action", " (process table 1)";

    print $output q{
; Callback to hook system, do not delete:
_ _
 hook "PRO1"

_ _
 at     0
 bclear 12 5                      ; Set language to English

_ _
 islight
 listobj                        ; Lists present objects
 listnpc @38                    ; Lists present NPCs

};

    #
    # Statuses
    #

    print_section_header "PRO 2", "statuses", " (process table 2)";
    foreach my $process (
        {
            comment => "; set carrying capacity at game start:\n",
            input => [ '_', '_' ],
            conditions => [
                [qw/ eq 31 0 /],
                [qw/ eq 32 0 /],
            ],
            actions => [[ 'ability', $data->{max_objects}, $data->{max_objects} ]],
        },
        {
            comment => "; Callback to hook system, do not delete:\n",
            input => [ '_', '_' ],
            conditions => [],
            actions => [[ hook => '"PRO2"' ]],
        },
        @{$data->{process}}
        ) {
        print $output "\n";
        print $output $process->{comment}//'';
        print $output "_ _\n";
        print_condacts($process);
    }

} else {

    print <<END
Usage: $0 [ options ] <unquill-filename>

Convert UnQuill output to ngPAWS format.

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
