# Testing the change directory command
let x = pwd ;

cd ../../../../../../../../../../../../../../../../../../ ;
pwd ;           # stdout: /

cd ../.././../////./////..//////.///.///////..//////../////..// ;
pwd ;           # stdout: /

cd $x/../ ;
cd $x ;
