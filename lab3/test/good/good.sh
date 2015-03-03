cd test/ ;                       # test dir
mkdir files;
cd files/ ;
touch test1;
ls ;                            # stdout: test1
echo piped > pipe;              # files: test1 pipe
cat pipe;                       # stdout: piped
ls ;                            # stdout: test1 piped
