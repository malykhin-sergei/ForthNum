\ Matrices

s" compat.fs" required
s" struct.fs" required

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
