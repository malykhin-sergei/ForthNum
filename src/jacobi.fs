s" compat.fs"   required
s" vars.fs"     required
s" arrays.fs"   required
s" sort.fs"     required
s" matrices.fs" required

: fsign ( r1 -- r1 ) f0< dup invert - s>f ;

variables( M )

: matrix-set-identity! (M)
  M .matrix-data @ M .matrix-cols @ M .matrix-rows @ min 0
  do   1.0e dup f! M .matrix-cols @ 1+ floats +
  loop drop ;

\ Calculate off-diagonal matrix norm. It's prone to precision loss,
\ but it doesn't need to be accurate, only provides an estimate
\ for matrix diagonality.

variables( M )

: off-diagonal-norm (M)
  0.0e
  M .matrix-rows @ 0
  do M .matrix-cols @ i 1+
     ?do i j M fm@ f**2 f+ loop
  loop
  fsqrt ;

\ Algorithm 8.5.1. Calculate symmetric 2x2 Schur decomposition.
\ Golub & Van Loan, Matrix Computations

fvariables( t tau cos sin )
 variables( M p q )

: sym.schur2 (M) (q) (p)
  p q M matrix-element f@ f0<>
  if q q M fm@  p p M fm@ f-
     p q M fm@  2.0e f* f/ (tau)
     tau fsign tau fabs tau f**2 1.0e f+ fsqrt f+ f/ (t)
     t f**2 1.0e f+ fsqrt 1/f (cos)
     t cos f* (sin)
  else 1.0e (cos) 0.0e (sin)
  then cos sin ;

\ Apply Jacobi rotation to matrix A,
\ A' = A*J(p,q,theta)

fvariables( cos sin )
 variables( M p q )

: jacobi.rot (M) (q) (p) (sin) (cos)
  M .matrix-rows @ 0
  do p i M fm@ cos f*
     q i M fm@ sin f* f-
     p i M fm@ sin f*
     q i M fm@ cos f* f+
     q i M fm!
     p i M fm!
  loop ;

\ Apply Jacobi rotation to matrix A,
\ A' = J(p,q,theta)^T*A

fvariables( cos sin )
 variables( M p q )

: jacobi.rot' (M) (q) (p) (sin) (cos)
  M .matrix-cols @ 0
  do i p M fm@ cos f*
     i q M fm@ sin f* f-
     i p M fm@ sin f*
     i q M fm@ cos f* f+
     i q M fm!
     i p M fm!
  loop ;

\ Cyclic Jacobi. Algorithm 8.5.3
\ Golub & Van Loan, Matrix Computations

fvariables( cos sin EPS )
 variables( M EV MAXROT )

1.0e-10 (EPS)
     50 (MAXROT)

: eig! (EV) (M)
  EV matrix-set-identity!
  MAXROT 0
  do
    M off-diagonal-norm EPS f<
    if unloop exit then
    M .matrix-rows @ 0
    do M .matrix-cols @ i 1+
       ?do i j M sym.schur2 (sin) (cos)
        cos sin i j  M jacobi.rot'
        cos sin i j  M jacobi.rot
        cos sin i j EV jacobi.rot
      loop
    loop
  loop
  ." jacobi not converged" ;

variables( M Evec Eval Ival )

: eigsort! (M) (Evec) (Eval) (Ival)
\ copy diagonal elements as eigenvalues
  M .matrix-rows @ 0
  do i i M fm@  i Eval fa!  i i Ival ca! loop
\ sort eigenvalues
  ['] fa@ is i@   ['] fa! is i!  [']  f> is >?
  ['] ca@ is ix@  ['] ca! is ix!  
  Eval .array-size @ Eval Ival rank
\ re-order eigenvectors and move them to matrix M
  M .matrix-rows @ 0
  do M .matrix-cols @ 0
     do j Ival ca@ i Evec fm@  j i M fm! loop
  loop ;

[ifdef] TESTING-JACOBI

: array-print 
  dup  .array-data @ 
  swap .array-size @ 0 
  do dup f@ f. float+ loop drop ;

: matrix-print
  cr
  dup .matrix-rows @ 0
  do dup .matrix-cols @ 0
     do dup i j rot fm@ f.
     loop cr
  loop drop ;

: | dup f! float+ ;

4 4 float-size matrix: A    A  matrix-allocate
4 4 float-size matrix: EV   EV matrix-allocate

4 float-size array:  eigenvalues   eigenvalues array-allocate
4  cell-size array: ieigenvalues  ieigenvalues array-allocate

A .matrix-data @
  10.0e |  1.0e |  4.0e |  0.0e |
   1.0e | 10.0e |  5.0e | -1.0e |
   4.0e |  5.0e | 10.0e |  7.0e |
   0.0e | -1.0e |  7.0e |  9.0e |
drop

A matrix-print
A EV eig!
A matrix-print
ieigenvalues eigenvalues EV A eigsort!
eigenvalues array-print

 A matrix-free
EV matrix-free

ieigenvalues array-free
 eigenvalues array-free

[then]
