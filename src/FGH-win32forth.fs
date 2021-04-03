variable W32F

\ Provides compatibility for some common but non-standard words.
\ See ANSI document X3.215-1994 (i.e., the ANS Forth standard).

: [defined] bl word find nip 0<> ; immediate
: [ifdef]   POSTPONE [defined]    POSTPONE [if] ; immediate
: [ifundef] POSTPONE [defined] 0= POSTPONE [if] ; immediate

\ Strings words

[ifundef] +place
: +place ( c-addr1 u c-addr2 )
  2dup >r >r dup c@ char+ + swap move r> r> dup c@ rot + swap c! ;
[then]

[ifundef] place
: place ( c-addr1 u c-addr2 )
  2dup c! char+ swap move ;
[then]

\ Floats

[ifundef] f0<>  : f0<>  ( r1    -- flag ) f0= 0= ;          [then]
[ifundef] f>=   : f>=   ( r1 r2 -- flag ) f< 0= ;           [then]
[ifundef] f>    : f>    ( r1 r2 -- flag ) fswap f< ;        [then]
[ifundef] f<>   : f<>   ( r1 r2 -- flag ) f- f0= 0= ;       [then]

[ifundef] s>f   : s>f   ( n1 -- r1 ) s>d d>f ;              [then]

[ifundef] 1/f   : 1/f   ( r1 -- r2 ) 1.0e fswap f/ ;        [then]
[ifundef] f2/   : f2/   ( r1 -- r2 ) 2.0e f/ ;              [then]
[ifundef] f**2  : f**2  ( r1 -- r1 ) fdup f* ;              [then]

\ Constants

[ifundef] fpi  1.0e fatan 4.0e f* fconstant fpi  [then]
[ifundef] f1.0 1.0e               fconstant f1.0 [then]

1 floats  constant float-size
1 cells   constant  cell-size

\ Variables

\ macro
: ++ s" pad +place" evaluate ; immediate

: (variable)
  s" variable &" pad place 2dup ++
  s"  : (" ++ 2dup ++ s" ) &" ++   2dup   ++ s"  ! ;" ++
  s"  : "  ++ 2dup ++ s"   &" ++ ( name ) ++ s"  @ ;" ++
  pad count evaluate ;

: (fvariable)
  s" fvariable &" pad place 2dup ++
  s"  : (" ++ 2dup ++ s" ) &" ++   2dup   ++ s"  f! ;" ++
  s"  : "  ++ 2dup ++ s"   &" ++ ( name ) ++ s"  f@ ;" ++
  pad count evaluate ;

: refill-at-eol? ( S: -- flag )
  source nip >in @ > dup 0= if drop refill then ;

: variables(
  begin bl word count 2dup s" )" compare
  while refill-at-eol?
  while (variable)
  repeat then 2drop ; immediate

: fvariables(
  begin bl word count 2dup s" )" compare
  while refill-at-eol?
  while (fvariable)
  repeat then 2drop ; immediate

\ Structures

: structure: create ( name -- ) here 0 0 , does> @ ;
: +field:    create ( name -- ) over , +   does> @ + ;
: (cell)   aligned 1 cells  +field: ;
: (float) faligned 1 floats +field: ;
: structure-end ( addr size -- ) swap ! ;

\ Arrays

structure: array-structure
  (cell) .array-data
  (cell) .array-type
  (cell) .array-size
structure-end

: array: ( size type name -- )
  create
  0 here .array-data ! here .array-type ! here .array-size !
  array-structure allot ;

: array-allocate ( arr -- )
  >r r@ .array-size @ r@ .array-type @ * allocate throw r> .array-data ! ;

: array-free ( arr -- )
  >r r@ .array-data @ free throw 0 r> .array-data ! ;

: array-element ( i arr -- *arr[i] )
  >r r@ .array-type @ * r> .array-data @ + ;

[ifdef] W32F

code fs-array-element
  pop eax
  mov ebx, [ebx]
  lea ebx, [ebx] [eax*8]
next c;

code cs-array-element
  pop eax
  mov ebx, [ebx]
  lea ebx, [ebx] [eax*4]
next c;

[else]

: fs-array-element ( i vec -- *arr[i] )
  >r floats r> .array-data @ + ;

: cs-array-element ( i vec -- *arr[i] )
  >r cells  r> .array-data @ + ;

[then]

: fa@ fs-array-element f@ ;   : fa! fs-array-element f! ;
: ca@ cs-array-element  @ ;   : ca! cs-array-element  ! ;

\ Matrices

structure: matrix-structure
  (cell) .matrix-data
  (cell) .matrix-cols
  (cell) .matrix-rows
  (cell) .matrix-type
structure-end

: matrix: ( cols rows type -- )
  create
  0 here .matrix-data !
    here .matrix-type !
    here .matrix-rows !
    here .matrix-cols !
  matrix-structure allot ;

: matrix-allocate ( matrix -- )
  >r r@ .matrix-cols @
     r@ .matrix-rows @
     r@ .matrix-type @ * *
     allocate throw
  r> .matrix-data ! ;

: matrix-free ( matrix -- )
  >r r@ .matrix-data @ free throw 0 r> ! ;

\ FORTH: inner loop is for I: I j A matrix-element

: matrix-element ( column=j row=i matrix -- *element )
  >r r@ .matrix-cols @ * + r@ .matrix-type @ * r> .matrix-data @ + ;

[ifdef] W32F

code fs-matrix-element ( column=j row=i matrix=A -- Aij )
  mov ecx, edx             \ EDX is USER pointer, so EDX must be saved before MUL
  pop eax                  \ EAX = i
  mul 4 [ebx]              \ EAX = i*A.cols
  pop edx                  \ EDX = j
  add eax, edx             \ EAX = offset = A.cols*i + j
  mov ebx, [ebx]           \ EBX = A.data
  lea ebx, [ebx] [eax*8]   \ EBX = addr of A[row][col] = A.data + offset
  mov edx, ecx             \ restore EDX
next c;

code cs-matrix-element ( column=j row=i matrix=A -- Aij )
  mov ecx, edx             \ EDX is USER pointer, so EDX must be saved before MUL
  pop eax                  \ EAX = i
  mul 4 [ebx]              \ EAX = i*A.cols
  pop edx                  \ EDX = j
  add eax, edx             \ EAX = offset = A.cols*i + j
  mov ebx, [ebx]           \ EBX = A.data
  lea ebx, [ebx] [eax*4]   \ EBX = addr of A[row][col] = A.data + offset
  mov edx, ecx             \ restore EDX
next c;

[else]

: fs-matrix-element ( column=j row=i matrix -- *element )
  >r r@ .matrix-cols @ * + floats r> .matrix-data @ + ; \ offset = A.cols*row + col

: cs-matrix-element ( column=j row=i matrix -- *element )
  >r r@ .matrix-cols @ * + cells r> .matrix-data @ + ; \ offset = A.cols*row + col

[then]

: fm@ fs-matrix-element f@ ;  : fm! fs-matrix-element f! ;
: cm@ cs-matrix-element @ ;   : cm! cs-matrix-element ! ;

[ifdef] TESTING-MATRICES

: matrix-print
  cr
  dup .matrix-rows @ 0
  do dup .matrix-cols @ 0
     do dup i j rot fm@ f.
     loop cr
  loop drop ;

: | dup f! float+ ;

( cols = ) 5 ( rows = ) 4 float-size matrix: A  A matrix-allocate

A .matrix-data @
  10.0e |  1.0e |  4.0e |  0.0e | -11.1e |
   1.0e | 10.0e |  5.0e | -1.0e | -11.1e |
   4.0e |  5.0e | 10.0e |  7.0e | -11.1e |
   0.0e | -1.0e |  7.0e |  9.0e | -11.1e |
drop

A matrix-print
A .matrix-cols @ A .matrix-rows @  A .matrix-cols ! A .matrix-rows !
A matrix-print
A matrix-free

[then]

\ Sorting

defer  i@ defer  i! defer >?
defer ix@ defer ix!

variables( n p q r s arr )

: maxi (r) (q) (p) (n) (arr)
  p (s)
  q n < if q arr i@ s arr i@ >? if q (s) then then
  r n < if r arr i@ s arr i@ >? if r (s) then then
  s ;

variables( p q n arr )

: downheap (p) (n) (arr)
  0 1
  do arr n p p 2* 1+ p 2* 2 + maxi (q)
     q p = if leave then
     p arr i@ q arr i@ p arr i! q arr i!
     q (p)
  loop ;

variables( n arr )

: sort (arr) (n)
  0 n 2 - 2/
  do arr n i downheap -1 +loop
  n 1
  do n i - arr i@  0 arr i@
     n i - arr i!  0 arr i!
     arr n i - 0 downheap
  loop ;

variables( p q n index arr )

: downheapi (p) (n) (index) (arr)
  0 1 do
    arr n p p 2* 1+ p 2* 2 + maxi (q)
    q p = if leave then
    p arr i@ q arr i@  p arr i! q arr i!
    p index ix@ q index ix@
    p index ix! q index ix!
    q (p)
  loop ;

variables( n index arr )

: rank (index) (arr) (n)
  n 0 do i i index ix! loop
  0 n 2 - 2/ do arr index n i downheapi -1 +loop
  n 1 do
    n i - arr i@ 0 arr i@ n i - arr i! 0 arr i!
    n i - index ix@ 0 index ix@
    n i - index ix! 0 index ix!
    arr index n i - 0 downheapi
  loop ;

[ifdef] VFX

: farray-print swap 0 do dup f@ f. float+ loop drop ;
: array-print  swap 0 do dup  @  .  cell+ loop drop ;

: | dup f! float+ ;

5 farray data
5  array indx

0 data -1.0e | 3.0e | 0.0e | 1.5e | 2.0e | drop

cr ." unsorted data = " 5 0 data farray-print

:noname >r floats r> + f@ ; is i@
:noname >r floats r> + f! ; is i!
' f> is >?

5 0 data sort

cr ." sorted data = " 5 0 data farray-print

' f< is >? 5 0 data sort
cr ." reverse order = " 5 0 data farray-print

:noname >r cells r> + @ ; is ix@
:noname >r cells r> + ! ; is ix!
' f> is >?

5 0 data 0 indx rank
cr ." order of items = " 5 0 indx array-print

[then]

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

\ C. Clay Marston and Gabriel G. BalintKurti.
\ The Fourier grid Hamiltonian method for bound state
\ eigenvalues and eigenfunctions // J. Chem. Phys. 91, 3571 (1989);
\ doi: 10.1063/1.456888

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

151 constant N

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

\ E(x) = D*(exp(-beta*(x-x0))-1)^2 - D

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

cls timer-reset H PSI eig! .elapsed

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
       ." error = "     i exact-solution i energy-levels fa@ f- fabs f.
       cr
  loop ;

cr

: report
 ." ================================================================== " cr
 ." Results of the Fourier grid Hamiltonian method applied to the      " cr
 ." Morse potential of the OH-bond                                     " cr cr
 ."            E(x) = D*(exp(-beta*(x-x0))-1)^2 - D,                   " cr cr
 ." where D = " D f. ." beta = " beta f. ." x0 = " x0 f.                    cr
 ." The grid is taken from x = " 0 xgrid fa@ f. ." to " N 1- xgrid fa@ f. cr
 ." with " N . ." points and step size dx = " dx f.                       cr cr
;

report 10 compare-results

            H matrix-free
          PSI matrix-free
            T  array-free
        xgrid  array-free
           Vx  array-free
energy-levels  array-free
 levels-order  array-free

0 [if] \ Output
Elapsed time: 00:00:06.547
==================================================================
Results of the Fourier grid Hamiltonian method applied to the
Morse potential of the OH-bond

           E(x) = D*(exp(-beta*(x-x0))-1)^2 - D,

where D = .199400 beta = 1.18900 x0 = 1.82100
The grid is taken from x = .179000 to 13.8210
with 151 points and step size dx = .090947

level = 0 exact = -.190472 numerical = -.190472 error = .000000
level = 1 exact = -.173229 numerical = -.173229 error = .000000
level = 2 exact = -.156805 numerical = -.156805 error = .000000
level = 3 exact = -.141198 numerical = -.141198 error = .000000
level = 4 exact = -.126409 numerical = -.126409 error = .000000
level = 5 exact = -.112438 numerical = -.112438 error = .000000
level = 6 exact = -.099285 numerical = -.099285 error = .000000
level = 7 exact = -.086950 numerical = -.086950 error = .000000
level = 8 exact = -.075433 numerical = -.075433 error = .000000
level = 9 exact = -.064734 numerical = -.064734 error = .000000
 ok
[then]
