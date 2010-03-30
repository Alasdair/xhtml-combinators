{-# LANGUAGE PatternGuards #-}
module Main where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad
import Data.Char (toUpper, toLower)
import Data.Function (on)
import Data.List (find, nub, nubBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Text.XML.HaXml as XML
import qualified Text.XML.HaXml.Parse as XML (dtdParse') 

data Element = Elem 
    { name :: String
    , contentSpec :: ContentSpec
    , attrs :: [Attr]
    } deriving (Show)

data Attr = Attr String Default deriving (Eq, Show)

data Default = Implied
             | Required
             | Fixed String
             | Default String
             deriving (Eq, Show)

data ContentSpec = Empty 
                 | Any
                 | CS Bool (Set String) 
                 deriving (Eq, Show)

toElement :: XML.DocTypeDecl -> XML.MarkupDecl -> Maybe Element
toElement dtd (XML.Element (XML.ElementDecl name xmlcs)) =
    Just $ Elem name (toContentSpec xmlcs) (findAttrs dtd name)
toElement _ _ = Nothing

-- | Convert a HaXml ContentSpec type to our ContentSpec type.
toContentSpec :: XML.ContentSpec -> ContentSpec
toContentSpec XML.EMPTY = Empty
toContentSpec XML.ANY = Any
toContentSpec (XML.Mixed XML.PCDATA) = CS True (Set.empty)
toContentSpec (XML.Mixed (XML.PCDATAplus names)) = CS True (Set.fromList names)
toContentSpec (XML.ContentSpec cp) = CS False (Set.fromList (cpToList cp))
    where cpToList (XML.TagName name _) = [name]
          cpToList (XML.Choice cps _) = cpToList =<< cps
          cpToList (XML.Seq cps _) = cpToList =<< cps

findAttrs :: XML.DocTypeDecl -> String -> [Attr]
findAttrs (XML.DTD _ _ decls) name
    | Just (XML.AttList attList) <- find attrByName decls
    = toAttrs attList
    | otherwise = []
    where attrByName :: XML.MarkupDecl -> Bool
          attrByName (XML.AttList (XML.AttListDecl n _)) = n == name
          attrByName _ = False

toAttrs :: XML.AttListDecl -> [Attr]
toAttrs (XML.AttListDecl _ attrDefs) = map toAttr attrDefs 
    where toAttr :: XML.AttDef -> Attr
          toAttr (XML.AttDef name _ XML.IMPLIED) = Attr name Implied
          toAttr (XML.AttDef name _ XML.REQUIRED) = Attr name Required
          toAttr (XML.AttDef name _ (XML.DefaultTo attVal (Just XML.FIXED))) = 
              Attr name (Fixed (show attVal)) 
              -- Not sure just showing attVal is really the right thing to do...
          toAttr (XML.AttDef name _ (XML.DefaultTo attVal Nothing)) = 
              Attr name (Default (show attVal))

canAppearIn :: String -> [Element] -> [Element]
canAppearIn = filter . contentSpecContains
    where contentSpecContains str (Elem _ (CS _ set) _) = str `Set.member` set
          contentSpecContains str _ = False

elements :: XML.DocTypeDecl -> [Element]
elements dtd@(XML.DTD _ _ decls) = mapMaybe (toElement dtd) decls

readDTD :: FilePath -> IO (Either String [Element])
readDTD path = do
    dtd <- readFile path
    return $ case XML.dtdParse' path dtd of
        Right Nothing  -> Left ("No DTD found at " ++ path)
        Right (Just d) -> Right (elements d)
        Left str       -> Left str

allAttrs :: [Element] -> [Attr]
allAttrs = nub . concatMap (\(Elem _ _ attrs) -> attrs)

data ElemGroup e = ElemGroup 
    { groupAllowsText :: Bool
    , groupName :: GroupName 
    , groupElems :: [e] 
    } deriving (Show)

instance Functor ElemGroup where
    fmap f group = group { groupElems = map f $ groupElems group }

data GroupName = TextGroup
               | EmptyGroup
               | NamedGroup String
               deriving (Eq, Show)

ppName group = (\(NamedGroup str) -> str) $ groupName group

mkGroups :: [Element] -> IO [ElemGroup Element]
mkGroups elems = do
    let uniqueContentSpecs = contentSpec <$> nubBy ((==) `on` contentSpec) elems
        equalsSpec spec = filter (\elem -> contentSpec elem == spec) elems 
        groups = map equalsSpec uniqueContentSpecs

    groups' <- forM groups $ \elems -> do
        case elems of
            [elem] -> return $ ElemGroup (allowsText elem) (mkName elem) elems
            _ | isTextGroup elems -> return $ ElemGroup True TextGroup elems
              | isEmptyGroup elems -> return $ ElemGroup False EmptyGroup elems
              | otherwise -> do
                  putStrLn "Group contains the following:"
                  mapM_ (putStrLn . name) elems
                  putStr "What do you want to call it?\n> "
                  name <- capitalize <$> getLine
                  return $ ElemGroup (allowsText (head elems)) (NamedGroup name) elems

    return groups'
  where
    allowsText (Elem _ (CS b _) _) = b

    mkName = NamedGroup . capitalize . name

isTextGroup ((Elem _ (CS True set) _):_) = Set.null set
isTextGroup _ = False

isEmptyGroup ((Elem  _ Empty _):_) = True
isEmptyGroup _ = False

capitalize :: String -> String
capitalize (c:cs) = toUpper c : cs

groupContentSpec :: ElemGroup Element -> ContentSpec
groupContentSpec = contentSpec . head . groupElems

canBeNestedIn :: String -> [ElemGroup Element] -> [GroupName]
canBeNestedIn elem = map groupName . filter inContentSpec
    where inContentSpec group 
              | CS _ set <- groupContentSpec group = elem `Set.member` set
              | otherwise = False

data Nested c = Root Element
              | Concrete Element String
              | Class Element c
              deriving (Show)

nest :: [ElemGroup Element] -> Element -> Nested [String]
nest groups elem = case name elem `canBeNestedIn` groups of
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

-- The following is pretty ugly, there must be a better way of doing it.

ppClass :: (String, [String]) -> String
ppClass (className, instances) = unlines
    [ "class " ++ className ++ " c where"
    , "    " ++ map toLower className ++ " :: Node -> c"
    , ""
    , unlines (map instanceDec instances)
    ]
  where instanceDec i = "instance " ++ className ++ " " ++ i 
                        ++ "Content where " ++ map toLower className
                        ++ " = " ++ i

ppClasses :: [(String, [String])] -> String
ppClasses = concatMap ppClass

ppCombinator :: ElemGroup (Nested String) -> Nested String -> String
ppCombinator group (Root elem) = unlines
    [ name elem ++ "' :: (Functor t, Monad t) => Attrs -> XHtmlT t " ++ ppName group ++ "Content -> XHtmlT t Root"
    , name elem ++ "' = tellNode Root \"" ++ name elem ++ "\" []"
    , ""
    , name elem ++ " :: (Functor t, Monad t) => XHtmlT t " ++ ppName group ++ "Content -> XHtmlT t Root"
    , name elem ++ " = " ++ name elem ++ "' []"
    ]
ppCombinator group (Concrete elem conc) = unlines
    [ name elem ++ "' :: (Functor t, Monad t) => Attrs -> XHtmlT t " ++ ppName group ++ "Content -> XHtmlT t " ++ conc ++ "Content"
    , name elem ++ "' = tellNode " ++ conc ++ " \"" ++ name elem ++ "\" []"
    , ""
    , name elem ++ " :: (Functor t, Monad t) => XHtmlT t " ++ ppName group ++ "Content -> XHtmlT t " ++ conc ++ "Content"
    , name elem ++ " = " ++ name elem ++ "' []"
    ]
ppCombinator group (Class elem cls) = unlines
    [ name elem ++ "' :: (Functor t, Monad t, " ++ cls ++ " c) => Attrs -> XHtmlT t " ++ ppName group ++ "Content -> XHtmlT t c"
    , name elem ++ "' = tellNode " ++ map toLower cls ++ " \"" ++ name elem ++ "\" []"
    , ""
    , name elem ++ " :: (Functor t, Monad t, " ++ cls ++ " c) => XHtmlT t " ++ ppName group ++ "Content -> XHtmlT t c"
    , name elem ++ " = " ++ name elem ++ "' []"
    ]

ppElemGroup :: ElemGroup (Nested String) -> String
ppElemGroup group
    | groupName group == TextGroup = ""
    | groupName group == EmptyGroup = ""
    | otherwise = unlines
        [ "newtype " ++ ppName group ++ "Content = " ++ ppName group ++ " { " ++ map toLower (ppName group) ++ "ToNode :: Node }"
        , ""
        , "instance Content " ++ ppName group ++ "Content where"
        , "    toContent = " ++ map toLower (ppName group) ++ "ToNode"
        , if groupAllowsText group 
          then unlines [ ""
                       , "instance CData " ++ ppName group ++ "Content where"
                       , "    cdata = " ++ ppName group ++ " . TextNode"
                       ]
          else ""
        , unlines . map (ppCombinator group) $ groupElems group
        ]
