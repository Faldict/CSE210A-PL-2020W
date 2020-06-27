load harness

@test "fact-5" {
  check 'i:=5; fact := 1; while 0<i do { fact := fact * i; i := i - 1 }' '{fact → 120, i → 0}'
}

@test "fact-3" {
  check 'i:=3; fact := 1; while 0<i do { fact := fact * i; i := i - 1 }' '{fact → 6, i → 0}'
}

@test "fact--1" {
  check 'i:=-1; fact := 1; while 0<i do { fact := fact * i; i := i - 1 }' '{fact → 1, i → -1}'
}
