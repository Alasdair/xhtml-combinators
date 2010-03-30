{-# LANGUAGE PatternGuards #-}
-- |
-- Module      : DTD2Combinators.SrcGen
-- Copyright   : (c) Alasdair Armstrong 2010
-- License     : BSD-style
-- Maintainer  : alasdair.armstrong@googlemail.com
-- Stability   : experimental
-- Portability : GHC

module DTD2Combinators.SrcGen where

import Control.Applicative
import Data.Char (toLower)
import Data.List (intersperse)

decorate True s = s ++ "'"
decorate False s = s

-- -----------------------------------------------------------------------------
-- Combinator Pretty Printing
-- -----------------------------------------------------------------------------

data Combinator = Combinator
    { name :: String
    , prefix :: String
    , typeSig :: (TypeG, TypeR)
    , required :: [String]
    , fixed :: [(String, String)]
    }

data TypeR = Root
           | Concrete String
           | Class String

data TypeG = Group String
           | Text
           | Empty

combinatorTypeSig :: Bool -> Combinator -> String
combinatorTypeSig prime comb = concat 
    [ decorate prime (name comb)
    , " :: (Functor t, Monad t", cls
    , reqs
    , if prime then arrow "Attrs" else ""
    , typeG, typeR
    ]
  where 
    cls | Class c <- snd (typeSig comb) = ", " ++ c ++ " c)"
        | otherwise = ")"

    typeG | Empty <- fst (typeSig comb) = ""
          | Text <- fst (typeSig comb) = arrow "Text"
          | Group s <- fst (typeSig comb) = arrow (prefix comb ++ "T t " ++ s ++ "Content")

    typeR | Root <- snd (typeSig comb) = prefix comb ++ "T t Root"
          | Concrete s <- snd (typeSig comb) = prefix comb ++ "T t " ++ s ++ "Content"
          | Class _ <- snd (typeSig comb) = prefix comb ++ "T t c"

    pad = "\n" ++ map (const ' ') (decorate prime (name comb)) ++ " "

    reqs | null (required comb) = " => "
         | (r:rs) <- required comb = rdarrow r ++ (rarrow =<< rs) ++ pad ++ "-> "

    rdarrow s = concat [pad, "=> Text -- ^ Required ", s, " attribute."]
    rarrow s = concat [pad, "-> Text -- ^ Required ", s, " attribute."]
    
    arrow s = s ++ " -> "

combinatorBody :: Bool -> Combinator -> String
combinatorBody prime comb = concat
    [ decorate prime (name comb), if rlen == 0 then "" else " "
    , intersperse ' ' (take rlen fargs)
    , " = ", tell, " \"", name comb, "\" "
    , "["
    , concat (intersperse ", " (zipWith attr (required comb) (map pure fargs)))
    , "]"
    , if prime then "" else " []"
    ]
  where
    tellType | Empty <- fst (typeSig comb) = "Empty"
             | Text <- fst (typeSig comb) = "Text"
             | Group _ <- fst (typeSig comb) = ""

    tell | Root <- snd (typeSig comb) = "tell" ++ tellType ++ "Node Root"
         | Concrete s <- snd (typeSig comb) = "tell" ++ tellType ++ "Node " ++ s
         | Class c <- snd (typeSig comb) = "tell" ++ tellType ++ "Node " ++ map toLower c

    attr a b = "A." ++ a ++ " " ++ b

    fargs = ['a'..'z']

    rlen = length (required comb)

combinator :: Combinator -> String
combinator comb = concat
    [ combinatorTypeSig True comb, "\n"
    , combinatorBody True comb, "\n\n"
    , combinatorTypeSig False comb, "\n"
    , combinatorBody False comb
    ]

combinators = concat . intersperse "\n\n" . map combinator

-- -----------------------------------------------------------------------------
-- Group Pretty Printing
-- -----------------------------------------------------------------------------

data Group = JustCombinators [Combinator]
           | ContentType String Bool [Combinator]

group :: Group -> String
group (JustCombinators cs) = combinators cs
group (ContentType n allowText cs) = concat
    [ "newtype ", n, "Content = ", n, " { ", map toLower n, "ToNode :: Node }\n"
    , "\n"
    , "instance Content ", n, "Content where\n"
    , "    toContent = ", map toLower n, "ToNode\n"
    , "\n"
    , if allowText
      then concat [ "instance CData ", n, "Content where\n"
                  , "    cdata = ", n, " . TextNode\n\n"
                  ]
      else ""
    , combinators cs
    ]
    

classAndInstances :: (String, [String]) -> String
classAndInstances (className, instances) = unlines
    [ "class " ++ className ++ " c where"
    , "    " ++ map toLower className ++ " :: Node -> c"
    , ""
    , unlines (map instanceDec instances)
    ]
  where instanceDec i = "instance " ++ className ++ " " ++ i 
                        ++ "Content where " ++ map toLower className
                        ++ " = " ++ i

classes :: [(String, [String])] -> String
classes = concatMap classAndInstances                  