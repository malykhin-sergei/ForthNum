\ Sorting

s" compat.fs" required
s" vars.fs"   required

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
