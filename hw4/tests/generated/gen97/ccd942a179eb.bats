load ../../harness

@test "ccd942a179eb" {
  check 'f    := -123075152219064618405049489844667796489    *     z   ;  x :=    -139711176290197972045722320579754263691   +67942307717520085198474418810671648117     ' '⇒ skip; x := (-139711176290197972045722320579754263691+67942307717520085198474418810671648117), {f → 0}
⇒ x := (-139711176290197972045722320579754263691+67942307717520085198474418810671648117), {f → 0}
⇒ skip, {f → 0, x → -71768868572677886847247901769082615574}'
}