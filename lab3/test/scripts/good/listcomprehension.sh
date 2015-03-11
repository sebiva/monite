# stdout: x\ny\n\z
{ echo $i : i <- [x, y, z] } ;
# stdout: x\ny\n\z
{ echo ($i) : i <- [echo x, echo y, echo z] } ;
# stdout: x\n
{ echo $i : i <- [x] } ;
# stdout: x\n
{ echo $i : i <- echo x } ;

#{ let $i = $i : i <- [x, y, z] } ; Not possible, probably shouldn't
# stdout: xyz
#echo $x$y$z ;
# stdout: For example:    6     47     257\n/home/or3x/Documents....
{ (($i)) : i [ls -l | wc, pwd] }

