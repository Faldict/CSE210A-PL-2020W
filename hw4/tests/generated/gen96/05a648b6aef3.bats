load ../../harness

@test "05a648b6aef3" {
  check 'if (false∨  false)      then  skip      else   while true     ∧  33678425345665000105671298738961903022*x     <     -141376263610172720528733266355616103202    +    114786426218836309709956577555295917199      do  

skip' '⇒ while (true∧((33678425345665000105671298738961903022*x)<(-141376263610172720528733266355616103202+114786426218836309709956577555295917199))) do { skip }, {}
⇒ skip, {}'
}
