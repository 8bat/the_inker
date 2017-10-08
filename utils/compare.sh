#!/bin/bash
#
# Numerically and graphically compare differences between image versions
#
# Note: the bitmap and SVG comparison images are generated differently:
#
# The bitmap should be pixel-for-pixel identical to the original,
# so the comparison image is the output of `compare`
#
# But the SVG only needs to look like the original,
# so the comparison image is generated using `composite`
#

TOTAL=0
FAILED=0

[ -d comparison ] || mkdir comparison

cat <<EOF > comparison.html
<table>
 <h1>Original <-> bitmap differences</h1>
 <thead>
  <tr>
   <td>ID
   <td>Original
   <td>Generated
   <td>comparison
  </tr>
 </thead>
 <tbody>
EOF
for N in $( seq 0 256 )
do
    if [ -e "look/$N.png" ]
    then
	DIFFERENCE="$( compare -metric AE "look/$N.png" "images/$N-a.pnm" "comparison/$N.png" 2>&1 )"
	case "$DIFFERENCE" in
	    0)
	    #echo "look/$N.png images/$N-a.pnm: identical"
	    #echo "<tr><td>$N<td><img src=\"look/$N.png\"><td><img src=\"images/$N-a.pnm\"><td><img title=\"identical\" src=\"comparison/$N.png\">" >> comparison.html
	    ;;
	    [0-9]*)
		TOTAL=$(( TOTAL + DIFFERENCE ))
		echo "<tr><td>$N<td><img src=\"look/$N.png\"><td><img src=\"images/$N-a.pnm\"><td><img title=\"$DIFFERENCE\" src=\"comparison/$N.png\">" >> comparison.html
		echo "look/$N.png images/$N-a.pnm: $DIFFERENCE"
		;;
	    *)
		FAILED=$(( FAILED + 1 ))
		echo "<tr><td>$N<td><img src=\"look/$N.png\"><td><img src=\"images/$N-a.pnm\"><td><img title=\"$DIFFERENCE\" src=\"comparison/$N.png\">" >> comparison.html
		echo "$DIFFERENCE"
		rm -f "comparison/$N.png"
		;;
	esac
    fi
done
cat <<EOF >> comparison.html
 </tbody>
</table>
EOF

if [ -z "$( find images -maxdepth 1 -name '*.svg.png' -print -quit )" ]
then
    echo "(SVG images not compared - please run svg-export.sh first)"
else
    cat <<EOF >> comparison.html
<table>
 <h1>Original <-> SVG differences</h1>
 <thead>
  <tr>
   <td>ID
   <td>Original
   <td>SVG
   <td>comparison
  </tr>
 </thead>
 <tbody>
EOF
    for N in $( seq 0 256 )
    do
        if [ -e "look/$N.png" ] && [ -e "images/$N.svg.png" ]
        then
	    DIFFERENCE="$( compare -metric AE "look/$N.png" "images/$N.svg.png" "null:" 2>&1 )"
	    case "$DIFFERENCE" in
	        0)
	        ;;
	        [0-9]*)
		    TOTAL=$(( TOTAL + DIFFERENCE ))
                    composite "look/$N.png" "images/$N.svg.png" -compose difference "comparison/$N.svg.png"
		    echo "<tr><td>$N<td><img src=\"look/$N.png\"><td><img src=\"images/$N.svg.png\"><td><img title=\"$DIFFERENCE\" src=\"comparison/$N.svg.png\">" >> comparison.html
		    echo "look/$N.png images/$N.svg.png: $DIFFERENCE"
		    ;;
	        *)
		    FAILED=$(( FAILED + 1 ))
		    echo "<tr><td>$N<td><img src=\"look/$N.png\"><td><img src=\"images/$N.svg.png\"><td><img title=\"$DIFFERENCE\" src=\"comparison/$N.svg.png\">" >> comparison.html
		    echo "$DIFFERENCE"
		    rm -f "comparison/$N.svg.png"
		    ;;
	    esac
        fi
    done
    cat <<EOF >> comparison.html
 </tbody>
</table>
EOF
fi

echo "Total pixels difference: $TOTAL"
echo "Total images failed: $FAILED"
