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
import Data.Char (toLower, isUpper)
import Data.List (intersperse, nub)

import DTD2Combinators.Clash (clash, keywords)

decorate :: Bool -> String -> String
decorate True s = fmt s ++ "'"
decorate False s 
    | decorateAttr s `elem` clash = fmt s ++ "_"
    | otherwise = fmt s

decorateAttr :: String -> String
decorateAttr attrName
    | fmt attrName `elem` keywords = fmt attrName ++ "_"
    | otherwise = fmt attrName

fmt = map (\c -> if c `elem` ":-" then '_' else c)

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

    attr a b = "A." ++ (decorateAttr a) ++ " " ++ b

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
    
-- -----------------------------------------------------------------------------
-- Classes Pretty Printing
-- -----------------------------------------------------------------------------

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

-- -----------------------------------------------------------------------------
-- File Pretty Printing
-- -----------------------------------------------------------------------------

data Module = Module
    { modPath :: String
    , modName :: String
    , exports :: [String]
    }

combinatorModule :: Module -> String
combinatorModule (Module p n (e:es)) = concat
    [ "{-# LANGUAGE OverloadedStrings #-}\n"
    , "module ", p, ".", n, "\n"
    , "    ( ", names e, "\n"
    , concatMap (\e -> "    , " ++ names e ++ "\n") es
    , "    ) where\n"
    , "\n"
    , "import Data.Text (Text)\n"
    , "\n"
    , "import ", p, ".Internal\n"
    , "import qualified ", p, ".", n, ".Attributes as A"
    ]
  where
    names e 
        | isUpper (head e) = e 
        | otherwise = decorate True e ++ ", " ++ decorate False e

data CombinatorFile = CombinatorFile
    { fileMod :: Module
    , fileClasses :: [(String, [String])]
    , fileGroups :: [Group]
    }

combinatorFile :: CombinatorFile -> String
combinatorFile (CombinatorFile m cls groups) = concat
    [ combinatorModule m, "\n\n"
    , classes cls
    , concat (intersperse "\n\n" (map group groups))
    ]

-- -----------------------------------------------------------------------------
-- Attribute Pretty Printing
-- -----------------------------------------------------------------------------

attrModule :: String -> String -> String
attrModule p n = concat
    [ "module ", p, ".", n, ".Attributes where\n"
    , "\n"
    , "import Data.Text (Text)\n"
    , "import ", p, ".Internal\n\n"
    ]

data AttrFile = AttrFile String String [String]

attrFile :: AttrFile -> String
attrFile (AttrFile p n attrs) = concat
    [ attrModule p n
    , attr =<< (nub attrs)
    ]

attr :: String -> String
attr name = decorateAttr name ++ " t = Attr \"" ++ name ++ "\" t\n"