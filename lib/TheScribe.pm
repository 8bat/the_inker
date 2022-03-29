package TheScribe;
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

TheScribe - Load UnQuill output

=head1 SYNOPSIS

    use TheScribe;

    my $data = TheScribe::load($fh,'spectrum');


=head1 DESCRIPTION

Convert from UnQuill's text format to a more useable data format.

=cut

use warnings;
use strict;
use utf8;

my $location = qr/^([[:xdigit:] ]{4})/o;

my %platform_data = (
    spectrum => {
        colours      => [qw( black blue red magenta lime cyan yellow white )],
        screen_width => 32,
        pause_parameter => 28,
    },
    c64 => {
        # colours taken from QuickRef_Illustrator.png in http://cl.8-bit.info/3P12293X3O0d/download/CPC_QA04_IA02_disk.zip
        colours      => [ 'black', 'white', '#880000', '#aaffee', '#cc44cc', '#00cc55', '#0000aa', '#eeee77', '#dd8855', '#664400', '#ff7777', '#333333', '#777777', '#aaff66', '#0088ff', '#bbbbbb' ],
        screen_width => 40,
        pause_parameter => -1,
    },
    cpc => {
        # In graphics mode, The CPC uses a per-room palette.
        # The default palette in text mode is qw( #0000 #7b67fb #efc34f #eb67eb )
        # The colours defined below should be more compatible with Spectrum colours
        # TODO: define a proper colour palette
        #colours      => [qw( pallete-1 pallete-2 palette-3 palette-4 )]
        colours      => [qw( black blue yellow magenta )],
        screen_width => 40,
        pause_parameter => -1,
    },
);

sub known_platforms { return sort keys %platform_data };

sub platform_data {
    return $platform_data{$_[0]};
}

my $platform = 'spectrum';

=head2 parse_zxml

    my $data = parse_zxml("<INK=6><BLINK>foo<,>bar")

Quill descriptions have several issues:

1. a Spectrum screen is only 32 characters wide
2. Quill doesn't have an automatic word-break
3. lines are often "centred" by prepending spaces
4. Quill control codes aren't nested like HTML tags
5. The <INVERSE> command doesn't have a CSS equivalent
6. UnQuill's <,> adds spaces up to a 16-character
   boundary, which is hard to implement in HTML.
7. UnQuill's <TABn> adds spaces up to an arbitrary boundary.

This clears all that up, and produces data like:

[
 { type => 'open-tag', contents => "classes" },
 { type => 'text',     contents => "html-escaped text" },
 { type => 'close-tag' },
]

=cut

sub parse_zxml {

    my ( $text ) = @_;

    my ( $screen_width, $colours ) = @{$platform_data{$platform}}{qw/ screen_width colours /};

    my $screen_width_2 = $screen_width / 2;

    $text =~ s/<BR>/\n/g; # Unquill for C64 and CPC has explicit newlines

    # STEP ONE: split the text into characters (and tags):
    # convert <,> to an appropriate number of spaces
    my @characters;
    my $character_position = 0;
    while ( $text =~ m{<(,)>|<TAB(\d)>|</([A-Z]+)>|<([A-Z]+)>|<([A-Z]+)=(\d+)>|(&[^;]*;)|(%[[:xdigit:]]+;)|([^%<&])}g ) {
        if ( defined($1) ) {
            my $length = $screen_width_2 - ($character_position%$screen_width_2);
            push( @characters, { type => 'space', length => 1, text => ' ' } )
                foreach 1..$length;
            $character_position += $length;
        } elsif ( defined($2) ) {
            if ( ($character_position%$screen_width) > $2 ) {
                push( @characters, { type => 'newline', length => 0, text => "\n" } );
                $character_position = 0;
            }
            while ( $character_position < $2 ) {
                push( @characters, { type => 'space', length => 1, text => ' ' } );
                ++$character_position;
            }
        } elsif ( defined($3) ) {
            push( @characters, { type => 'tag', length => 0, tag => lc($3), close => 1 } );
        } elsif ( defined($4) ) {
            push( @characters, { type => 'tag', length => 0, tag => lc($4), value => '' } );
        } elsif ( defined($5) ) {
            if ( $platform eq 'c64' && $5 eq 'PAPER' && $6 > 15 ) {
                # UnQuill 0.11.0 prints e.g. <PAPER=16> instead of <INK=0>
                push( @characters, { type => 'tag', length => 0, tag => 'ink', value => '-'.$colours->[$6-16] } );
            } elsif ( $5 eq 'INK' || $5 eq 'PAPER' ) {
                push( @characters, { type => 'tag', length => 0, tag => lc($5), value => '-'.$colours->[$6] } );
            } else {
                push( @characters, { type => 'tag', length => 0, tag => lc($5), value => "-$6" } );
            }
        } elsif ( defined($7) ) {
            push( @characters, { type => 'word', length => 1, text => $7 } );
            ++$character_position;
        } elsif ( defined($8) ) {
            die $8 unless $8 =~ /^%([[:xdigit:]]+);$/;
            my $value = hex($1);
            if ( $platform eq 'cpc' && $value >= 1 && $value <= 8 ) {
                # UnQuill 0.11.0 treats CPC colour codes as normal characters
                if ( $value <= 4 ) {
                    push( @characters, { type => 'tag', length => 0, tag => 'INK'  , value => '-'.$colours->[$value-1] } );
                } else {
                    push( @characters, { type => 'tag', length => 0, tag => 'PAPER', value => '-'.$colours->[$value-5] } );
                }
            } else {
                push( @characters, { type => 'word', length => 1, text => "&#x$1;" } );
            }
            ++$character_position;
        } elsif ( $9 eq "\n" ) {
            push( @characters, { type => 'newline', length => 1, text => '\n' } );
            $character_position = 0;
        } elsif ( $9 eq " " ) {
            push( @characters, { type => 'space', length => 1, text => $9 } );
            ++$character_position;
        } else {
            # escape quotes and curly brackets (ngPAWS changes quotes to apostrophes, uses curly brackets for sequence tags, and uses # for long descriptions):
            push( @characters, { type => 'word', length => 1, text => $9 } );
            $characters[-1]->{text} =~ s/([|"{}&#;\\])/sprintf('&#%d;',ord($1))/ge;
            ++$character_position;
        }
    }

    # STEP TWO: handle the INVERSE tag
    # note: we guesss the initial paper/ink colour - could well be wrong
    my ( $inversed, $paper, $ink ) = ( 0, '-black', '-white' );
    for ( my $n=0; $n!=@characters; ++$n ) {
        next unless $characters[$n]->{type} eq 'tag';
        my $tag = $characters[$n];
        if ( $inversed ) {
            if ( $tag->{tag} eq 'paper' ) {
                $paper = $tag->{value};
                $tag->{tag} = 'ink';
            } elsif ( $tag->{tag} eq 'ink' ) {
                $ink = $tag->{value};
                $tag->{tag} = 'paper';
            } elsif ( $tag->{tag} eq 'inverse' ) {
                $inversed = 0;
                splice(
                    @characters, $n, 1,
                    { type => 'tag', length => 0, tag => 'paper', value => $paper },
                    { type => 'tag', length => 0, tag => 'ink', value => $ink },
                );
                ++$n;
            }
        } else {
            if ( $tag->{tag} eq 'paper' ) {
                $paper = $tag->{value};
            } elsif ( $tag->{tag} eq 'ink' ) {
                $ink = $tag->{value};
            } elsif ( $tag->{tag} eq 'inverse' ) {
                $inversed = 1;
                splice(
                    @characters, $n, 1,
                    { type => 'tag', length => 0, tag => 'paper', value => $ink },
                    { type => 'tag', length => 0, tag => 'ink', value => $paper },
                );
                ++$n;
            }
        }
    }

    # STEP THREE: split characters into lines
    my $previous_line_type = 'left';
    my @lines;
  LINE:
    while ( @characters ) {
        my @leading_tags;
        my @line;
        my $character_position = 0;
        # process leading space and tags:
        while ( @characters ) {
            if ( $characters[0]->{type} eq 'space' ) {
                push( @line, shift(@characters) );
                ++$character_position;
                if ( $character_position == $screen_width ) {
                    push( @lines, { type => 'break', text => [ @leading_tags, { type => 'newline', length => 0, text => '\n' } ] } );
                    @line = ();
                    $character_position = 0;
                }
            } elsif ( $characters[0]->{type} eq 'tag' && $characters[0]->{tag} ne 'paper' ) {
                push( @leading_tags, shift(@characters) );
            } elsif ( $characters[0]->{type} eq 'newline' ) {
                push( @lines, { type => 'break', text => [ @leading_tags, { type => 'newline', length => 0, text => '\n' } ] } );
                @line = ();
                $character_position = 0;
                shift(@characters)
            } else {
                last;
            }
        }
        unshift( @line, @leading_tags );
        my $leading_space = $character_position;
        my $trailing_space = 0;
        # process remaining characters:
        while ( my $character = shift(@characters) ) {
            push( @line, $character );
            if ( $character->{type} eq 'newline' ) {
                $character_position = $screen_width;
            } else {
                $character_position += $character->{length};
                if ( $character->{type} eq 'space' ) {
                    ++$trailing_space;
                } elsif ( $character->{type} eq 'word' || $character->{tag} eq 'paper' ) {
                    $trailing_space = 0;
                }
            }
            if ( $character_position == $screen_width ) {
                push( @lines, { type => 'needs-alignment', text => [ @line ], leading_space => $leading_space, trailing_space => $trailing_space } );
                next LINE;
            }
        }

        if ( !@characters ) {
            # end of final line
            if ( $character_position == $leading_space ) {
                push( @lines, { type => 'break', text => [ @line ] } );
            } else {
                $trailing_space += $screen_width - $character_position;
                push( @lines, { type => 'needs-alignment', text => [ @line ], leading_space => $leading_space, trailing_space => $trailing_space } );
            }
        }

    }

    # STEP FOUR: decide alignment and trim space in centred lines:
    foreach my $line ( grep( { $_->{type} eq 'needs-alignment' } @lines ) ) {

        my ( $leading_space, $trailing_space ) = @{$line}{qw/ leading_space trailing_space /};

        if ( $leading_space + $trailing_space == $screen_width ) {
            $line->{type} = 'break';
            @{$line->{text}} = grep( { $_->{type} ne 'space' } @{$line->{text}} );
            push( @{$line->{text}}, { type => 'newline', length => 0, text => '\n' } );
        } elsif ( abs($leading_space-$trailing_space) < 2 && $leading_space && $trailing_space ) {
            $line->{type} = 'centre';
        } elsif ( $leading_space > 3 && $trailing_space > 3 ) {
            $line->{type} = 'centre';
        } else {
            $line->{type} = 'left';
        }

        if ( $line->{type} eq 'centre' ) {
            my $text = $line->{text};
            my $space;
            if ( $line->{leading_space} < $line->{trailing_space} ) {
                $space = $line->{leading_space};
            } else {
                $space = $line->{trailing_space};
            }
            my ( $leading_space, $trailing_space ) = ( $space, $space );
            for ( my $n=0; $n<@$text; ++$n ) {
                if ( $text->[$n]->{type} eq 'space' ) {
                    splice( @$text, $n--, 1 );
                    last unless --$leading_space;
                }
            }
            for ( my $n=$#$text; $n>=0; --$n ) {
                if ( $text->[$n]->{type} eq 'space' ) {
                    splice( @$text, $n, 1 );
                    last unless --$trailing_space;
                } elsif ( $text->[$n]->{type} ne 'tag' ) {
                    # this can happen if the last line of a message isn't a full line
                    last;
                }
            }
        }
    }

    # STEP FIVE: merge compatible lines:
    for ( my $n=0; $n<$#lines; ++$n ) {
        my ( $current, $next ) = @lines[$n,$n+1];
        my $splice_next = 0;
        if ( $current->{type} eq 'break' ) {
            $current->{type} = $next->{type};
            $splice_next = 1;
        } elsif ( $next->{type} eq 'break' ) {
            pop(@{$current->{text}}) while ( $current->{text}->[-1]->{type} eq 'space' );
            push( @{$current->{text}}, { type => 'newline', length => 0, text => '\n' } )
                if $current->{text}->[-1]->{type} ne 'newline';
            $splice_next = 1;
        } elsif ( $current->{type} eq $next->{type} ) {
            if ( $current->{type} eq 'left' ) {
                # decide what to do
                my $next_word_length = 0;
                foreach my $letter ( @{$next->{text}} ) {
                    last if $letter->{type} eq 'space';
                    $next_word_length += $letter->{length};
                }
                pop(@{$current->{text}}) while ( $current->{text}->[-1]->{type} eq 'space' );
                if ( $current->{trailing_space} > $next_word_length ) {
                    push( @{$current->{text}}, { type => 'newline', length => 0, text => '\n' } );
                } elsif ( $current->{text}->[-1]->{type} ne 'newline' ) {
                    push( @{$current->{text}}, { type => 'space', length => 1, text => ' ' } );
                }
            } else {
                push( @{$current->{text}}, { type => 'newline', length => 0, text => '\n' } );
            }
            $splice_next = 1;
        }
        if ( $splice_next ) {
            push( @{$current->{text}}, @{$next->{text}} );
            $current->{trailing_space} = $next->{trailing_space}//0;
            splice( @lines, $n+1, 1 );
            --$n;
        }
    }

    # STEP SIX: convert lines to open tags, text and close tags:
    my %properties;
    my @ret;
    foreach my $line ( @lines ) {
        my ( $type, $text ) = @{$line}{qw/ type text /};
        while ( @$text && $text->[0]->{type} eq 'tag' ) {
            my $tag = shift(@$text);
            if ( $tag->{close} ) {
                delete $properties{$tag->{tag}};
            } else {
                $properties{$tag->{tag}} = $tag->{value};
            }
        }
        my @class;

        if ( $type eq 'centre' ) {
            push( @class, "center" );
            if ( $text->[-1]->{type} eq 'newline' ) {
                pop(@$text);
            }
        }
        my @open_tags;
        foreach my $tag ( sort keys %properties ) {
            if ( $properties{$tag} eq '' && grep( { ($_->{tag}//'') eq $tag } @$text ) ) {
                unshift( @$text, { type => 'tag', tag => $tag, value => '' } );
            } elsif ( $tag eq 'paper' ) {
                unshift( @$text, { type => 'tag', tag => $tag, value => $properties{$tag} } );
            } else {
                push( @class, $tag . $properties{$tag} );
            }
        }
        if ( @class ) {
            push( @ret, { type => 'open-tag', contents => join(' ', @class) } );
            push( @open_tags, join(' ', @class) );
        }

        while ( @$text ) {

            my $character = shift(@$text);
            if ( $character->{type} eq 'tag' ) {
                my $tag = $character; # renamed for readability
                if ( $tag->{close} ) {
                    next unless defined($properties{$tag->{tag}});
                    delete $properties{$tag->{tag}};
                    my $value = $tag->{value};
                    my @reopen_tags;
                    for (;;) {
                        push( @ret, { type => 'close-tag' } );
                        if ( $open_tags[-1] =~ s/\b$tag->{tag}// ) {
                            if ( $open_tags[-1] =~ /\S/ ) {
                                # several tags in this block
                                $open_tags[-1] =~ s/  / /;
                                push( @reopen_tags, shift(@open_tags) );
                            }
                            last;
                        };
                        push( @reopen_tags, shift(@open_tags) );
                    }
                    foreach my $tag ( @reopen_tags ) {
                        push( @ret, { type => 'open-tag', contents => $_ } );
                        push( @open_tags, $tag );
                    }
                } else {
                    my %tags = ( $tag->{tag} => $tag->{value} );
                    $properties{$tag->{tag}} = $tag->{value};
                    while ( @$text ) {
                        my $tag = $text->[0];
                        last if $tag->{type} ne 'tag';
                        last if $tag->{close};
                        shift(@$text);
                        $tags{$tag->{tag}} = $tag->{value};
                        $properties{$tag->{tag}} = $tag->{value};
                    }
                    my $tag_class = join( ' ', map( { $_ . $tags{$_} } sort keys %tags ) );
                    push( @ret, { type => 'open-tag', contents => $tag_class } );
                    push( @open_tags, $tag_class );
                }
            } else {
                die $character->{type} unless $character->{type} =~ /^(?:word|space|newline)$/;
                my $contents = $character->{text};
                while ( @$text ) {
                    my $character = $text->[0];
                    last if $character->{type} !~ /^(?:word|space|newline)$/;
                    shift(@$text);
                    $contents .= $character->{text};
                }
                push( @ret, { type => 'text', contents => $contents } );
            }

        }

        foreach my $tag ( @open_tags ) {
            push( @ret, { type => 'close-tag' } );
        }

    }

    return \@ret;

}

=head2 load

    my $data = load( $filename, 'spectrum' );
    my $data = load( $fh, 'cpc' );

Load data in UnQuill format.

See %platform_data for a list of known platforms.

Returns nothing if the input is formatted incorrectly.

=cut
sub load {

    my ( $fh, $_platform ) = @_;

    die "Please specify recognised platform, not '$_platform'" unless exists $platform_data{$_platform};
    $platform = $_platform;

    unless ( ref($fh) ) {
       open( my $fh1, '<', $fh );
       $fh = $fh1;
    }

    return unless <$fh> =~ /^      ; Generated by UnQuill/;

    #
    # STEP ONE: PARSE INPUT
    #

    my %data;

    my $current;

    my $type;
    my $subsection;

    while (<$fh>) {
        chomp;
        if ( /^\s*;.*/ ) { # comment line
            #warn $_;
        } elsif ( /^$/ ) { # empty line - usually indicates the start of a new section
            #warn;
        } elsif ( /$location: Player can carry (\d+) objects\.$/ ) {
            die $2 if exists($data{max_objects});
            $data{max_objects} = $2;
            #warn $max_objects;
        } elsif ( /$location: Response \[Event\] table$/ ) {
            $type = 'table';
            $current = $data{responses} = [];
            $subsection = '';
        } elsif ( /$location: Process \[Status\] table$/ ) {
            $type = 'table';
            $current = $data{processes} = [];
            $subsection = '';
        } elsif ( /^There are (\d+) (Object|Location|Message|System message)s\.$/ ) {
            $type = 'list';
            $subsection = $2;
            my $name = lc($2);
            $name =~ s/ /_/g;
            $current = $data{"${name}s"} = [];
        } elsif ( /$location: User-defined graphics$/ ) {
            $type = 'udg';
        } elsif ( /$location: User-defined font$/ ) {
            $type = 'font';
        } elsif ( /$location: There are (\d+) graphics records\.$/ ) {
            $type = 'graphics';

        } elsif ( /$location: Connections from\s+(\d+):$/ ) {
            my $connections = $data{connections}[$2] = { location => $1, to => [] };
            while ( ( $_ = scalar(<$fh>) ) =~ /\S/ ) {
                die "{$_}" unless /^\s*(\w+)\s+to\s+(\d+)\n$/;
                push( @{$connections->{to}}, { direction => $1, to => $2 } );
            };

        } elsif ( /$location: Word\s+(\d+): (\w+|\*)\s*$/ ) {
            die $3 if exists $data{vocabulary}{$3};
            $data{vocabulary}{$3} = { location => $1, id => $2 };

        } elsif ( /$location: Object\s+(\d+) is initially not created\.$/ ) {
            $data{objects}[$2]{initial_status} = { location => $1, room => 252 };
        } elsif ( /$location: Object\s+(\d+) is initially in room\s+(\d+)\.$/ ) {
            $data{objects}[$2]{initial_status} = { location => $1, room => $3 };
        } elsif ( /$location: Object\s+(\d+) named (\w+)\s*$/ ) {
            $data{objects}[$2]{name} = { location => $1, text => $3 };

        } elsif ( $type eq 'list' ) {
            if ( /$location: $subsection (\d+):$/ ) {
                my ( $location, $id ) = ( $1, $2 );
                my $description = '';
                while ( <$fh> ) {
                    last unless s/^      //;
                    chomp;
                    $description .= $_;
                }
                die $id if defined $current->[$id];
                $current->[$id] = { location => $location, description => $description };
                if ( defined $_ ) {
                    redo;
                } else {
                    last;
                }
            } else {
                die $_;
            }

        } elsif ( $type eq 'table' ) {
            if ( /$location: (\w+|\*)\s*(\w+|\*)\s*Conditions:$/ ) {
                push( @$current, {
                    location => $1,
                    verb => ( $2 eq '*' ? '_' : $2 ),
                    noun => ( $3 eq '*' ? '_' : $3 ),
                    conditions => [],
                    actions => [],
                });
                $subsection = 'conditions';
            } elsif ( /^\s*Actions:$/ ) {
                $subsection = 'actions';
            } elsif ( /^$location:\s*(\w+)\s*(\d+(?:\s+\d+)*)?\s*$/ ) {
                my $subtable = $current->[-1]->{$subsection};
                if ( defined $3 ) {
                    push( @$subtable, { location => $1, verb => $2, noun => [split( ' ', $3 )] } );
                } else {
                    push( @$subtable, { location => $1, verb => $2 } );
                }
            } else {
                die $_;
            }


        } elsif ( $type eq 'udg' ) {
            if ( /^#define (\w+) (\d+)$/ ) {
                die $1 if exists($data{udgs}{$1});
                $data{udgs}{$1} = $2;
            } elsif ( /^static char udg_bits\[\] = \{$/ ) {
                die 'bits' if exists($data{udgs}{bits});
                my $bits = $data{udgs}{bits} = [];
                do {
                    $_ = <$fh>;
                    push( @$bits, map( { hex($_) } /0x([[:xdigit:]]{2})/g ) );
                } until /};/;
            } else {
                die $_;
            }

        } elsif ( $type eq 'font' ) {
            if ( /^#define (\w+) (\d+)$/ ) {
                die $1 if exists($data{fonts}{$1});
                $data{fonts}{$1} = $2;
            } elsif ( /^static char font(\d+)_bits\[\] = \{$/ ) {
                die $1 if exists($data{fonts}{"font-$1"});
                my $bits = $data{fonts}{"font-$1"} = [];
                do {
                    $_ = <$fh>;
                    push( @$bits, map( { hex($_) } /0x([[:xdigit:]]{2})/g ) );
                } until /};/;
            } else {
                die $_;
            }

        } elsif ( $type eq 'graphics' ) {
            if ( /$location: Location (\d+) graphics flag:\s*(Not location graphic|Location graphic)\.$/ ) {
                my $graphic = $data{graphics}[$2] = {
                    location => $1,
                    room     => $2,
                    is_location => ($3 eq 'Location graphic') ? 1 : 0,
                    commands => []
                };
                $_ = <$fh>;
                chomp;
                die $_ unless /^      Ink=(\d+) Paper=(\d+) Bit6=(\d+)$/;
                @{$graphic}{qw/ ink paper bit6 /} = ( 0+$1, 0+$2, 0+$3 );
                while ( ( $_ = scalar(<$fh>) ) !~ /$location: END\s*$/ ) {
                    chomp;
                    if ( /$location: (LINE |PLOT |RPLOT) (.)(.)\s*(-?)0*(\d+)\s*(-?)0*(\d+)\s*$/ ) {
                        push(
                            @{$graphic->{commands}},
                            {
                                command => $2,
                                arguments => [
                                    ($3 eq 'o') ? 1 : 0,
                                    ($4 eq 'i') ? 1 : 0,
                                    0+"$5$6",
                                    0+"$7$8"
                                ],
                            }
                            );
                        $graphic->{commands}->[-1]->{command} =~ s/ $//;
                    } elsif ( /$location: GOSUB\s+sc=0*(\d+)\s+0*(\d+)\s*$/ ) {
                        push(
                            @{$graphic->{commands}},
                            {
                                command => 'GOSUB',
                                arguments => [ 0+$2, 0+$3 ],
                            }
                        );
                    } elsif ( /$location: (\w+)\s+(.*)$/ ) {
                        die $_ if /LINE|GOSUB/;
                        push(
                            @{$graphic->{commands}},
                            {
                                command => $2,
                                arguments => [map( { s/^((?:-)?)0+/$1/; 0+($_||0) } split( /\s+/, $3 ) )],
                            }
                        );
                    } else {
                        die "{$_}";
                    }

                };
                push(
                    @{$graphic->{commands}},
                    {
                        command => 'END',
                        arguments => [],
                    }
                );

            } else {
                die $_;
            }

        } else {
            die $_;
        }

    }

    #
    # STEP TWO: print output
    #


    my @objects = map(
        {
            if ($_->{description} eq '') {
                undef;
            } else {
                {
                    initial_location => 0+$_->{initial_status}->{room},
                    (
                     defined($_->{name})
                     ? ( name => $_->{name}->{text} eq '_' ? '' : lc($_->{name}->{text}) )
                     : ()
                    ),
                    description => parse_zxml($_->{description})
                };
            };
        } @{$data{objects}}
        );
    pop(@objects) while $#objects && !$objects[-1];

    my @rooms = map(
        {{
            description => parse_zxml($data{locations}->[$_]->{description}),
            to          => {map( {( lc($_->{direction}) => 0+$_->{to} )} @{$data{connections}->[$_]->{to}} )},
        }}
        0..$#{$data{locations}}
    );

    my        @messages = map( { parse_zxml($_->{description}) } @{$data{       messages}} );
    my @system_messages = map( { parse_zxml($_->{description}) } @{$data{system_messages}} );

    my @algorithms = (
        map(
            {[map(
                  {
                      my $response = $_;
                      {
                          input => [ lc($_->{verb}), $_->{noun} eq '_' ? () : lc($_->{noun}) ],
                          map(
                              {
                                  $_ => [
                                      map( {
                                          [ lc($_->{verb}), map({0+$_} @{$_->{noun}}) ]
                                           } @{$response->{$_}} )
                                      ],
                              } qw/ conditions actions /
                              ),
                      }
                  }
                  @$_
              )]}
            @data{qw/ responses processes /}
        )
    );


    my %vocabulary;
    while ( my ( $text, $word_data ) = each(%{$data{vocabulary}}) ) {
        $vocabulary{lc($text)} = 0+$word_data->{id};
    }
    $vocabulary{'_'} = undef;
    delete $vocabulary{'*'};

    return {
        graphics        => $data{graphics},
        max_objects     => 0+$data{max_objects},
        message         => \@messages,
        object          => \@objects,
        process         => $algorithms[1],
        response        => $algorithms[0],
        room            => \@rooms,
        system_message  => \@system_messages,
        vocabulary      => \%vocabulary,
    };

}

1;
