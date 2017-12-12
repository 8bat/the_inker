#!/bin/bash
#
# Transcode movies created by fuse-gtk
#
# Create movies with: File > Movie > Record

extract_frames() {
    FILE="$1"
    mkdir "$FILE-frames"
    fmfconv -P "$FILE" "$FILE-frames/frame.pnm" || exit 1
}

deduplicate() {
    # TODO: remove duplicate frames at the start and end, but not in the middle
    # (e.g. when one SHADE overwrites the contents of another)
    FILE="$1"
    FORMAT="$2"
    PREV=
    sha1sum "$FILE-frames"/frame-*.$FORMAT | \
        while read SHA NAME
        do
            [ "$PREV" = "$SHA" ] && rm -f "$NAME"
            PREV="$SHA"
        done
}

crop_to_graphics_area() {
    FILE="$1"
    for FRAME in $FILE-frames/frame-*.pnm
    do
        echo -n "\rCropping $FRAME... "
        convert -crop 256x96+32+24 "$FRAME" "$FRAME.png"
    done
}

frames_to_video() {
    FILE="$1"
    FORMAT="$2"
    ffmpeg \
        -hide_banner \
        -framerate 50 \
        -pattern_type glob -i "$FILE-frames/*$FORMAT" \
        -c:v libx264 -crf 0 \
        "$FILE.mkv"
}

graphics_video() {
    # Good for showing graphics being drawn, to study details of the algorithm
    FILE="$1"

    extract_frames "$FILE"

    # Remove duplicate PNM images (redundant, but improves performance):
    deduplicate "$FILE" pnm

    crop_to_graphics_area "$FILE"

    # Proper deduplication, removing e.g. areas where text is being printed
    deduplicate "$FILE" png

    frames_to_video "$FILE" png

    # Delete temporary files:
    rm -rf "$FILE-frames"

    echo "Now do: mpv --geometry=1536x576 -vo opengl:scale=nearest $FILE"
}

normal_video() {
    # lossless MKV of the video
    FILE="$1"
    fmfconv "$FILE" "$FILE.flac"
    ffmpeg -i <( fmfconv "$FILE" ) -i "$FILE.flac" -crf 0 -acodec copy "$FILE.mkv"
    rm -f "$FILE.flac"
    echo "Now do: mpv $FILE.mkv"
}

normal_audio() {
    # lossless MKV of the video
    FILE="$1"
    fmfconv "$FILE" "$FILE.flac"
    echo "Now do: mpv $FILE.flac"
}

frames() {
    extract_frames "$FILE"
}

COMMAND="$1" ; shift
case "$COMMAND" in

    --graphics)
        for FILE in "$@"
        do graphics_video "$FILE"
        done
        ;;

    --video)
        for FILE in "$@"
        do normal_video "$FILE"
        done
        ;;

    --audio)
        for FILE in "$@"
        do normal_audio "$FILE"
        done
        ;;

    --frames)
        for FILE in "$@"
        do frames "$FILE"
        done
        ;;

    *)
        cat <<END
Usage: $0 <--graphics|--video|--frames> [ files ]
END
        ;;

esac
