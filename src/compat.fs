\ Provides compatibility for some common but non-standard words.
\ See ANSI document X3.215-1994 (i.e., the ANS Forth standard).

: [ifundef] bl word find nip 0= postpone [if] ; immediate

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
