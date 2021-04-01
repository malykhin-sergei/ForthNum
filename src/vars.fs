s" compat.fs" required

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

