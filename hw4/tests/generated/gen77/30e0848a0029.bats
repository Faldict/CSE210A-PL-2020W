load ../../harness

@test "30e0848a0029" {
  check 'while z    *     -1 <  1*   2∧   -4     +    -2<y     -     0     do 

y  :=   4*   -2 ' '⇒ y := (4*-2); while (((z*-1)<(1*2))∧((-4+-2)<(y-0))) do { y := (4*-2) }, {}
⇒ skip; while (((z*-1)<(1*2))∧((-4+-2)<(y-0))) do { y := (4*-2) }, {y → -8}
⇒ while (((z*-1)<(1*2))∧((-4+-2)<(y-0))) do { y := (4*-2) }, {y → -8}
⇒ skip, {y → -8}'
}
