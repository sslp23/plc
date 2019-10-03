data Ops =
    SUM
  | MUL
  | SUB
  deriving ( Show, Eq )

data IntTree = Nilt Int
             | Node Ops IntTree IntTree
             deriving (Show)

evalTree :: IntTree -> Int
evalTree (Node o (Nilt x) (Nilt y)) | o == SUM = x+y
                                    | o == MUL = x*y
                                    | o == SUB = x-y
evalTree (Node o x y) | o == SUM = (evalTree x)+(evalTree y)
                      | o == MUL = (evalTree x)*(evalTree y)
                      | o == SUB = (evalTree x)-(evalTree y)