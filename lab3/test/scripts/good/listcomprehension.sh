# stdout: x\ny\n\z
{ echo $i : i <- [x, y, z] } ;
# stdout: x\ny\n\z
{ (($i)) : i <- [echo x, echo y, echo z] } ;
# stdout: x\n
{ echo $i : i <- [x] } ;
# stdout: x\n
{ echo $i : i <- echo x } ;
