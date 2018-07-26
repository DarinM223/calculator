module Calculator
    ( Operator (..)
    , Precedence (..)
    , Calculator (..)
    , newCalculator
    , pressNumber  -- Number key button
    , deleteNumber -- 'Del' button
    , evaluate     -- '=' button
    , setOp        -- '+', '-', '*', '/' buttons
    ) where

data Operator = Plus | Minus | Times | Div
    deriving (Show, Eq)

newtype Precedence = Precedence { unPrecedence :: Int }
    deriving (Eq, Ord, Num, Show)

opPrec :: Operator -> Precedence
opPrec Plus  = 1
opPrec Minus = 1
opPrec Times = 2
opPrec Div   = 2

highestPriority :: Precedence
highestPriority = 3

data Calculator n = Calculator
    { _number   :: n
    , _opStack  :: [Operator]
    , _numStack :: [n]
    } deriving (Show, Eq)

newCalculator :: (Num n) => Calculator n
newCalculator = Calculator { _number   = 0
                           , _opStack  = []
                           , _numStack = []
                           }

pressNumber :: (Num n) => n -> Calculator n -> Calculator n
pressNumber num c = c { _number = _number c * 10 + num }

deleteNumber :: (RealFrac n) => Calculator n -> Calculator n
deleteNumber c = c { _number = fromIntegral $ truncate $ _number c / 10 }

evaluate :: (Fractional n) => Calculator n -> Calculator n
evaluate c@Calculator { _number = num } = popResult
                                        . collapseStack highestPriority
                                        . pushNum num
                                        $ c

setOp :: (Fractional n) => Operator -> Calculator n -> Calculator n
setOp op c@Calculator{ _number = num, _opStack = (op':_) }
    | opPrec op' < opPrec op = pushNum num . pushOp op . clearNumber $ c
    | otherwise              = pushOp op
                             . collapseStack (opPrec op)
                             . pushNum num
                             . clearNumber
                             $ c
setOp op c@Calculator{ _number = num, _opStack = [] } =
    pushNum num . pushOp op . clearNumber $ c

pushOp :: Operator -> Calculator n -> Calculator n
pushOp op c = c { _opStack = op:_opStack c }

pushNum :: n -> Calculator n -> Calculator n
pushNum num c = c { _numStack = num:_numStack c }

popResult :: Calculator n -> Calculator n
popResult c@Calculator{ _numStack = num:[] } = c
    { _number   = num
    , _numStack = []
    }
popResult c = c

clearNumber :: (Num n) => Calculator n -> Calculator n
clearNumber c = c { _number = 0 }

collapseStack :: (Fractional n) => Precedence -> Calculator n -> Calculator n
collapseStack prec c@Calculator{ _opStack = op:ops, _numStack = num1:num2:nums }
    | opPrec op <= prec = collapseStack prec c
        { _opStack  = ops
        , _numStack = (opFn op) num1 num2:nums
        }
    | otherwise = c
  where
    opFn Plus  = (+)
    opFn Minus = subtract
    opFn Times = (*)
    opFn Div   = (/)
collapseStack _ c = c
