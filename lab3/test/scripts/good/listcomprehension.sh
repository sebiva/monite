# stdout: x\ny\n\z
{ echo $i : i <- [x, y, z] } ;
# stdout: x\ny\n\z
{ (($i)) : i <- [echo x, echo y, echo z] } ;
# stdout: x\n
{ echo $i : i <- [x] } ;
# stdout: x\n
{ echo $i : i <- echo x } ;

# stdout: For example:    6     47     257\n/home/or3x/Documents....
#{ (($i)) : i [ls -l | wc, pwd] } ## TODO: Different depending on system : 2015-03-11 - 22:47:13 (John)
