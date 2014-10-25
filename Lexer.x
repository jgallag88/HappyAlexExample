{ module Lexer where }

-- monadUserState wrapper gives us a continuation-based lexer, with
-- state as well.
%wrapper "monadUserState"

tokens :-
    $white+ ;
    "["     { mkTok TokOBrack }
    "]"     { mkTok TokCBrack }
    [a-z]+  { mkTokWStr TokS }
{

-- When using the monadUserState wrapper, Alex requires us to define the
-- AlexUserState type. Without it, Alex will run fine, but the code it produces
-- won't compile.
data AlexUserState = AlexUserState { counter :: Int }

-- ... and we need to define alexInitUserState too
alexInitUserState = AlexUserState { counter = 0 }

data Tok = TokOBrack
         | TokCBrack
         | TokS String
         | TokEOF -- when using a threaded lexer, Happy will need an EOF token

-- Another definition we must provide. Produce the EOF token when Alex reaches
-- the end of its stream.
alexEOF = return TokEOF

-- Actions in lexer must have type
--  AlexInput -> Int -> Alex LocTok
--  Although we aren't using it, the source position is right there for us
--  if we wanted it to store with the token
mkTokWStr :: (String -> Tok) -> AlexInput -> Int -> Alex Tok
mkTokWStr strToTok (posn,_,_,str) len = return $ strToTok tokStr
    where tokStr = take len str

mkTok :: Tok -> AlexInput -> Int -> Alex Tok
mkTok tok = mkTokWStr (\_ -> tok)

}
