{
module Parser where 

import Lexer

}

%name parser
%tokentype { Tok }
%error { error "Parse error" }
%monad { Alex } -- run everything inside Alex monad

-- Happy wants a lexer of type (Token -> Alex a) -> Alex a, rather than type
-- Alex a. I guess this is a performance thing.
%lexer { alexMonadScan >>= } { TokEOF } 

%token
    '['     { TokOBrack }
    ']'     { TokCBrack }
    s       { TokS $$ }

%%

brack   : s                             { S $1 } -- Normal action has type 'Brack'
        | '[' brack brack brack ']'     {% monadicAction $2 $3 $4 } -- Monadic action has type 'Alex Brack'

{
data Brack = S String | Brack Brack Brack Brack deriving Show

parse input = runAlex input parser

-- Example monadic action, just increases a counter stored in the state
-- whenever the rule is applied.
monadicAction :: Brack -> Brack -> Brack -> Alex Brack
monadicAction b1 b2 b3 = do
    c <- alexGetUserState
    alexSetUserState $ c { counter = (counter c) + 1 }
    return $ Brack b1 b2 b3
}
