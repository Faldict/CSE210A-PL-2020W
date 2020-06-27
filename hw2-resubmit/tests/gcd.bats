load harness

@test "gcd-98-76" {
  check 'a := 98;
           b := 76;
           while ¬(a=b) do {
             if a < b then
                b := b - a
             else
                a := a - b
           }' '{a → 2, b → 2}'
}

@test "gcd-369-1107" {
  check 'a := 369;
           b := 1107;
           while ¬(a=b) do {
             if a < b then
                b := b - a
             else
                a := a - b
           }' '{a → 369, b → 369}'
}

@test "gcd-369-1108" {
  check 'a := 369;
           b := 1108;
           while ¬(a=b) do {
             if a < b then
                b := b - a
             else
                a := a - b
           }' '{a → 1, b → 1}'
}
