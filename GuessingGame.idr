import Data.Vect

data WordState : (guesses_remaining: Nat) -> (letters_remaining:Nat) -> Type where
  MkWordState: (word:String)->(missing:Vect letters_remaining Char) ->
                WordState guesses_remaining letters_remaining

data Finished: Type where
  Lost: (game: WordState 0 (S letters_remaining)) -> Finished
  Won: (game: WordState (S guesses_reaming) 0) -> Finished

data ValidInput: List Char -> Type where
  Letter: (c:Char)->ValidInput [c]

isValidString: (s:String) -> Dec (ValidInput (unpack s))

validChar: (c:Char) -> ValidInput [c]
validChar c = Letter c

unvalidCharList: (cs = []) -> Void


isValidInput:(cs: List Char) -> Dec (ValidInput cs)
isValidInput (c::[]) = Yes (validChar c)


readGuess: IO (x**ValidInput x)
readGuess = do putStr "Guess: "
               x <- getLine
               case isValidString (toUpper x) of
                 Yes prf => pure (_**prf)
                 No contra => do putStrLn "invalid guess"
                                 readGuess
