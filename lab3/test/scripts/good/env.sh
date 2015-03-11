let x = x ;
let x = ((echo z)) in let y = ($x) ;

echo $x ;    # stdout: x
echo $y ;    # stdout: z

{let x = (($i)) : i <- [echo x, echo y, echo z]} ;

echo $x ;    # stdout: z
echo $i ;    # stdout:

let y = y ;
let x = $y ;

echo $x ;    # stdout: y

let x = x ;
let y = y ;
let z = z ;

echo $x/$y/$z ; # stdout: x/y/z

let xyz = $x$y$z ;

echo $xyz ; # stdout: xyz
