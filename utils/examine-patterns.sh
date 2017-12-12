#!/bin/sh

# To examine the patterns in a snapshot:

# 1. create a snapshot of a room with the shade you're interested in, with the "LOOK" command typed in:
SNAPSHOT="my-snapshot.sna"

# 2. Search for "Location <number> in the decompiled source:
./unquill-0.11.0/unquill -TZ "$SNAPSHOT" | less -i

# 3. find the equivalent location in the snapshot

# 3a. (untested) you should be able to calculate it from the location specified by unquill:
SNAPSHOT_BYTE="$( perl -e 'printf "%04x", 0x<location> - 0x3fe5' )"

# 3b. Create a search pattern for the binary values you're looking for ("22" is the code for "SHADE"):
printf "/ 22 *%02x *%02x *%02x\n" <arg1> <arg2> <arg3>
hexdump -C "$SNAPSHOT" | less
SNAPSHOT_BYTE="$( printf %d 0x<address> )"

# 4. set each bit in that byte:
for N in 1 2 4 8 16 32 128
do
    cp "$SNAPSHOT" "$N-$SNAPSHOT"
    printf "\\x$N" | dd of="$N-$SNAPSHOT" bs=1 seek="$SNAPSHOT_BYTE" count=1 conv=notrunc
done

# 5. load each snapshot in turn, create a screenshot for each one

# 6. load all the snapshots in GIMP and see which pixels are set
