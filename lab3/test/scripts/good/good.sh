# -----------------------------------------------------------------------------
# remember where we begin
let x = pwd ;
# -----------------------------------------------------------------------------
cd test/ ;                      # test dir
mkdir tmp;
cd tmp ;
touch test1;
ls ;                            # stdout: test1
echo piped > pipe;              # files: test1 pipe
cat pipe;                       # stdout: piped
ls ;                            # stdout: test1 piped
# -----------------------------------------------------------------------------
# cleanup after run
cd $x ;
#rm -r files ;
# -----------------------------------------------------------------------------
