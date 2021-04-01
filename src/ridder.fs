\ Using Ridder's method, return the root of a function f(x)
\ known to lie between x1 and x2.

\ See Ridders, C. (1979). "A new algorithm for computing a 
\     single root of a real continuous function". 
\     IEEE Transactions on Circuits and Systems. 26: 979–980. 
\     doi:10.1109/TCS.1979.1084580

s" vars.fs" required

defer f(x)

 variables( MAXIT )
fvariables( UNLIKELY-VALUE ACCURACY
            x1 x2 xm xnew
            fl fm fh fnew
            s result )

\ default values
-1.11e30 (UNLIKELY-VALUE)
 1.0e-09 (ACCURACY)
 50      (MAXIT)

: fsign ( r1 -- r1 ) f0< dup invert - s>f ;

: ridder (x2) (x1) ( xt ) is f(x)
  x1 f(x) (fl) fl f0= if x1 exit then
  x2 f(x) (fh) fh f0= if x2 exit then
  fh f0< fl f0> and fh f0> fl f0< and or invert
  if ." root must be bracketed within [" x1 f. ." ," x2 f. ." ]" then
  UNLIKELY-VALUE (result)
  MAXIT 0
  do x1 x2 f+ f2/ (xm)  xm f(x) (fm)
     fm fm f* fl fh f* f- fsqrt (s)
     s f0=
     if result unloop exit then
     fl fh f- fsign xm x1 f- f* fm f* s f/ xm f+ (xnew)
     xnew result ACCURACY f~
     if result unloop exit then
     xnew (result)  xnew f(x) (fnew)
     fnew f0=
     if result unloop exit then
     fnew fsign fm f* fm f<>
     if xm (x1) fm (fl)  result (x2) fnew (fh)
     else
        fnew fsign fl f* fl f<>
        if result (x2)  fnew (fh) then
        fnew fsign fh f* fh f<>
        if result (x1)  fnew (fl) then
     then
     x1 x2 ACCURACY f~
     if result unloop exit then
  loop
  ." exceed maximum number ( " MAXIT . ." ) of iterations" ;

