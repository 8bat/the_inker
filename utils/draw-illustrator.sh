#!/bin/bash
#
# Send keys to The Illustrator's graphics program

# commands:
MARK=m
LINE=l

# directions:
UP_LEFT=q
UP=w
UP_RIGHT=e
LEFT=a
RIGHT=d
DOWN_LEFT=z
DOWN=x
DOWN_RIGHT=c

# directions:
UP_LEFT8=Q
UP8=W
UP_RIGHT8=E
LEFT8=A
RIGHT8=D
DOWN_LEFT8=Z
DOWN8=X
DOWN_RIGHT8=C

repeat() {
          for n in $( seq 1 $2 )
          do echo $1
          done
}

build_line() {
    echo $MARK $( repeat $LEFT $1 ) $LINE
    [ "$1" != 8 ] && echo $( repeat $RIGHT $1 ) $UP8
}

for N in $( seq 0 8 )
do
    xdotool key --delay 200 $( build_line $N )
    sleep 0.2
done
