\ Arrays

s" compat.fs" required
s" struct.fs" required

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
