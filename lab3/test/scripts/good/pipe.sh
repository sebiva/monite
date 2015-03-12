echo "123456" | wc | wc | wc | wc ;
echo "123456" > tempfile ;
# Possible grammar error: (<) should probably bind harder than (|), but it works
# with parenthesis
(wc < tempfile) | wc | wc ;

rm tempfile ;
