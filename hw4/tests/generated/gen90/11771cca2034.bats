load ../../harness

@test "11771cca2034" {
  check 'hB     :=     x ;z    :=     z   *     -167021685662028152230133317207630343039    ' '⇒ skip; z := (z*-167021685662028152230133317207630343039), {hB → 0}
⇒ z := (z*-167021685662028152230133317207630343039), {hB → 0}
⇒ skip, {hB → 0, z → 0}'
}
