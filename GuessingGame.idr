data Last: List a -> a -> Type where
  LastOne: Last [val] val
  NotLast: (prf: Last xs value) -> Last (x :: xs) value

test: Last [1,2] 3
test = NotLast LastOne
