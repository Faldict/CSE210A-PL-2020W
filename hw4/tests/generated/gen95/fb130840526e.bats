load ../../harness

@test "fb130840526e" {
  check 'if (¬(131122720176069847404715275132686814477     + x  =   79990982773656082517309962158868184516 -    59024340341942602787011949594981781279))      then A    :=x  -     x      else  

skip  ' '⇒ A := (x-x), {}
⇒ skip, {A → 0}'
}
