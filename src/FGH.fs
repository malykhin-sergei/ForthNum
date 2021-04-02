\ C. Clay Marston and Gabriel G. BalintKurti. 
\ The Fourier grid Hamiltonian method for bound state 
\ eigenvalues and eigenfunctions // J. Chem. Phys. 91, 3571 (1989); 
\ doi: 10.1063/1.456888

s" compat.fs"   required
s" vars.fs"     required
s" arrays.fs"   required
s" sort.fs"     required
s" matrices.fs" required
s" jacobi.fs"   required

: array-print 
  >r 
  r@ .array-data @
  r> .array-size @ 0 
  do dup f@ f. float+ loop drop ;

: matrix-print
  cr
  dup .matrix-rows @ 0
  do dup .matrix-cols @ 0
     do dup i j rot fm@ f.
     loop cr
  loop drop ;

: matrix-scale!
  >r
  r@ .matrix-data @ 
  r@ .matrix-cols @ r> .matrix-rows @ * 0
  do dup f@ fover f* dup f! float+
  loop drop fdrop ;

\ Toeplitz matrix

variables( M diags )

: toeplitz! (M) (diags)
  M .matrix-rows @ 0
  do M .matrix-cols @ i
     do i j - dup diags fa@
        if   fdup i j M fm!  j i M fm!
        else      i i M fm!
        then
     loop
  loop ;

\ Equation 26

fvariables( l d/N )

: sum (d/N)
  1.0e (l)
  0.0e ( N ) 1 rshift 0
  do [ 2.0e fpi f* ] fliteral
     l d/N  f*  f* fcos l f**2 f* f+
     l 1.0e f+ (l)
  loop ;

 variables( diags n )
fvariables( dx 1/n )

: FGH! (diags) (dx)
  diags .array-size @ (n)
  n s>f 1/f (1/n)
  [ -8.0e fpi f**2 f* ] fliteral
  1/n fdup fdup f* f* f* dx f**2 1/f f*
  n 0
  do i s>f 1/n f* n sum fover f*
     i diags fa!
  loop
  fdrop ;

\ Make uniform mesh

 variables( grid )
fvariables( start stop dx )

: mesh! (grid) (stop) (start)
  stop start f- grid .array-size @ 1- s>f f/ (dx)
  start grid .array-size @ 0
  do fdup i grid fa! dx f+ loop
  fdrop dx ;

\ Take potential V(x) on the mesh

defer V(x)
variables( xgrid vgrid )

: tabulate! (vgrid) (xgrid) ( xt ) is V(x)
  grid .array-size @ 0
  do i xgrid fa@  V(x) i vgrid fa! loop ;

\ Modify the main diagonal of a matrix

variables( M diag )

: +diag! (M) (diag)
  diag .array-size @ 0
  do i diag fa@  i i M fm@ f+ i i M fm! loop ;

\ Calculate reduced mass of two atoms

: reduced-mass
  fover fover f* frot frot f+ f/ ;

\ Atomic units

: units: 
  create  f, does> f@ f* ;

1822.8885e units: amu

\ Dimension of the eigenproblem. NB: it should be an odd number!

101 constant N

N N float-size matrix: H                            H matrix-allocate
N N float-size matrix: PSI                        PSI matrix-allocate
  N float-size  array: T                            T array-allocate
  N float-size  array: xgrid                    xgrid array-allocate
  N float-size  array: Vx                          Vx array-allocate
  N float-size  array: energy-levels    energy-levels array-allocate
  N  cell-size  array: levels-order      levels-order array-allocate

\ Morse potential parameters for the OH-bond are:

fvariables( mu x0 D beta dx )

1.821e  (x0)    \ is the equilibrium bond distance
0.1994e (D)     \ is the well depth (defined relative to the dissociated atoms)
1.189e  (beta)  \ controls the 'width' of the potential

( Hydrogen atom mass is ) 1.00794e amu ( Oxygen atom mass is )  15.9994e  amu 
( and their ) reduced-mass (mu)

\ E(x) = D*(exp(-β*(x-x₀))-1)^2 - D

: morse-potential
  x0 fswap f- beta f* fexp 1.0e f- f**2 1.0e f- D f* ;

( Take x from ) 2.0e x0 f- ( to ) 12.0e x0 f+ 

( Tabulate ) xgrid mesh! ( with step size ) (dx) 
\ Than calculate matrix elements using eq. 26, 27

dx T FGH! 

\ Build the hamiltonian matrix H, known it is Toeplitz matrix, from the array 

T H toeplitz!

( Calculate ) ' morse-potential ( on ) xgrid ( then ) Vx tabulate!

( Kinetic energy operator is ) -1.0e mu 2.0e f* f/ H matrix-scale! 

\ Add potential to the diagonal of the hamiltonian:

Vx H +diag!

\ Solve eigenproblem

H PSI eig!
 
\ Sort eigenvalues

levels-order energy-levels PSI H eigsort!

\ Analytic solution for the Morse potential is

fvariables( omega delta )
 variables( level )

: exact-solution (level)
  2.0e D f* mu f/ fsqrt beta f* (omega)
  omega f**2 4.0e D f* f/       (delta)
  level  s>f 0.5e f+      omega f*
  level  s>f 0.5e f+ f**2 delta f* fnegate 
  D f- f+ ;

: compare-results ( n )
  0 do ." level = "     i . 
       ." exact = "     i exact-solution f. 
       ." numerical = " i energy-levels fa@ f.
       ." error = "     i exact-solution i energy-levels fa@ f- f.
       cr
  loop ;

cr
." ================================================================== " cr
." Results of the Fourier grid Hamiltonian method applied to the      " cr
." Morse potential of the OH-bond                                     " cr cr
."            E(x) = D*(exp(-β*(x-x₀))-1)^2 - D,                      " cr cr 
." where D = " D f. ." β = " beta f. ." x₀ = " x0 f.                    cr
." The grid is taken from " 0 xgrid fa@ f. ." to " N 1- xgrid fa@ f.    
." with " N . ." points and step size " dx f.                           cr cr

10 compare-results

            H matrix-free
          PSI matrix-free
            T  array-free
        xgrid  array-free
           Vx  array-free
energy-levels  array-free
 levels-order  array-free

bye
