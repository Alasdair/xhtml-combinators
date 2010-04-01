{-# LANGUAGE PatternGuards #-}
module Main where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad
import Data.Char (toUpper, toLower)
import Data.Function (on)
import Data.List (find, nub, nubBy, intersperse)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing)
import System.IO

import DTD2Combinators.HaXml
import qualified DTD2Combinators.SrcGen as Src

allAttrs :: [Element] -> [Attr]
allAttrs = nub . concatMap (\(Elem _ _ attrs) -> attrs)

data ElemGroup e = ElemGroup 
    { groupAllowsText :: Bool
    , groupName :: GroupName 
    , groupElems :: [e] 
    } deriving (Show)

elemNames :: [ElemGroup Element] -> [String]
elemNames groups = map elemName $ concatMap groupElems groups

instance Functor ElemGroup where
    fmap f group = group { groupElems = map f $ groupElems group }

data GroupName = TextGroup
               | EmptyGroup
               | NamedGroup String
               deriving (Eq, Show)

groupNames :: [ElemGroup e] -> [String]
groupNames = mapMaybe gname . map groupName
    where gname (NamedGroup n) = Just n
          gname _ = Nothing

mkGroups :: [Element] -> IO [ElemGroup Element]
mkGroups elems = do
    let uniqueContentSpecs = elemContentSpec <$> nubBy ((==) `on` elemContentSpec) elems
        equalsSpec spec = filter (\elem -> elemContentSpec elem == spec) elems 
        groups = map equalsSpec uniqueContentSpecs

    groups' <- forM groups $ \elems -> do
        case elems of
            [elem] -> return $ ElemGroup (allowsText elem) (mkName elem) elems
            _ | isTextGroup elems -> return $ ElemGroup True TextGroup elems
              | isEmptyGroup elems -> return $ ElemGroup False EmptyGroup elems
              | otherwise -> do
                  putStrLn "Group contains the following:"
                  mapM_ (putStrLn . elemName) elems
                  putStr "What do you want to call it?\n> "
                  name <- capitalize <$> getLine
                  return $ ElemGroup (allowsText (head elems)) (NamedGroup name) elems

    return groups'
  where
    allowsText (Elem _ (CS b _) _) = b

    mkName = NamedGroup . capitalize . elemName

isTextGroup ((Elem _ (CS True set) _):_) = Set.null set
isTextGroup _ = False

isEmptyGroup ((Elem  _ Empty _):_) = True
isEmptyGroup _ = False

capitalize :: String -> String
capitalize (c:cs) = toUpper c : cs

groupContentSpec :: ElemGroup Element -> ContentSpec
groupContentSpec = elemContentSpec . head . groupElems

canBeNestedIn :: String -> [ElemGroup Element] -> [GroupName]
canBeNestedIn elem = map groupName . filter inContentSpec
    where inContentSpec group 
              | CS _ set <- groupContentSpec group = elem `Set.member` set
              | otherwise = False

data Nested c = Root Element
              | Concrete Element String
              | Class Element c
              deriving (Show)

element (Root e) = e
element (Concrete e _) = e
element (Class e _) = e

nest :: [ElemGroup Element] -> Element -> Nested [String]
nest groups elem = case elemName elem `canBeNestedIn` groups of
                       [] -> Root elem
                       [NamedGroup groupName] -> Concrete elem groupName
                       groupNames -> Class elem $ map (\(NamedGroup n) -> n) groupNames

nesting :: [ElemGroup Element] -> [ElemGroup (Nested [String])]
nesting groups = fmap (nest groups) <$> groups

classes :: String -> [ElemGroup (Nested [String])] -> [(String, [String])]
classes prefix = zip classNames . nub . mapMaybe nestedClasses . concatMap groupElems
    where nestedClasses (Class _ c) = Just c
          nestedClasses _ = Nothing

          classNames = map (\n -> prefix ++ show n) [1..]

singularize :: String -> [ElemGroup (Nested [String])] -> [ElemGroup (Nested String)]
singularize prefix groups = fmap singularizeClass <$> groups
    where cls = map (\(a,b) -> (b,a)) $ classes prefix groups 

          singularizeClass (Class elem strs) = Class elem (fromJust $ lookup strs cls)
          singularizeClass (Concrete elem str) = Concrete elem str
          singularizeClass (Root elem) = Root elem

toCombinator :: String -> ElemGroup (Nested String) -> Nested String -> Src.Combinator
toCombinator prefix group comb = Src.Combinator
    { Src.name = elemName . element $ comb
    , Src.prefix = prefix
    , Src.typeSig = (typeG, typeR)
    , Src.required = required
    , Src.fixed = [] -- TODO: handle fixed attrs
    }
  where typeG | TextGroup <- groupName group = Src.Text
              | EmptyGroup <- groupName group = Src.Empty
              | NamedGroup n <- groupName group = Src.Group n

        typeR | Root _ <- comb = Src.Root
              | Concrete _ t <- comb = Src.Concrete t
              | Class _ c <- comb = Src.Class c

        required = attrName <$> filter isRequired (elemAttrs (element comb))

toGroup :: String -> ElemGroup (Nested String) -> Src.Group
toGroup prefix group
    | (NamedGroup n) <- groupName group = Src.ContentType n (groupAllowsText group) cs
    | otherwise = Src.JustCombinators cs
  where 
    cs = map (toCombinator prefix group) (groupElems group)

toFile :: String -> String -> String -> [ElemGroup Element] -> Src.CombinatorFile
toFile prefix modPath modName groups = Src.CombinatorFile
    { Src.fileMod = Src.Module modPath modName exports
    , Src.fileClasses = classes prefix nst
    , Src.fileGroups = map (toGroup prefix) sng
    }
  where
    nst = nesting groups
    sng = singularize prefix nst

    classNames = fst <$> classes prefix nst
    
    exports = classNames ++ map (++ "Content") (groupNames groups) ++ elemNames groups

toAttrFile :: String -> String -> [Element] -> Src.AttrFile
toAttrFile modPath modName elems = 
    Src.AttrFile modPath modName (attrName <$> allAttrs elems)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    (dtd:_) <- getArgs

    putStr "Enter the class prefix (usually XHtml or XML)\n> "
    prefix <- getLine
    putStr "Input module path (e.g. Text.XHtmlCombinators)\n> "
    modPath <- getLine
    putStr "Enter module name\n> "
    modName <- getLine

    (Right elems) <- readDTD dtd
    groups <- mkGroups elems

    let cfile = Src.combinatorFile $ toFile prefix modPath modName groups
        afile = Src.attrFile $ toAttrFile modPath modName elems

    writeFile (modName ++ ".hs") cfile
    createDirectoryIfMissing False modName
    writeFile (modName ++ "/Attributes.hs") afile