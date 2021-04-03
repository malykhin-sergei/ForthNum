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

[ifundef] required
\ Taken from https://forth-standard.org/standard/file/REQUIRED

: save-mem ( addr1 u -- addr2 u ) \ gforth
\ copy a memory block into a newly allocated region in the heap
   swap >r
   dup allocate throw
   swap 2dup r> rot rot move ;

: name-add ( addr u listp -- )
   >r save-mem ( addr1 u )
   3 cells allocate throw \ allocate list node
   r@ @ over ! \ set next pointer
   dup r> ! \ store current node in list var
   cell+ 2! ;

: name-present? ( addr u list -- f )
   rot rot 2>r begin ( list r: addr u )
     dup
   while
     dup cell+ 2@ 2r@ compare 0= if
       drop 2r> 2drop true exit
     then
     @
   repeat
   ( drop 0 ) 2r> 2drop ;

: name-join ( addr u list -- )
   >r 2dup r@ @ name-present? if
     r> drop 2drop
   else
     r> name-add
   then ;

variable included-names 0 included-names !

: included ( i*x addr u -- j*x )
   2dup included-names name-join
   included ;

: required ( i*x addr u -- i*x )
   2dup included-names @ name-present? 0= if
     included
   else
     2drop
   then ;
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
