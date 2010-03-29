{-# LANGUAGE PatternGuards #-}
module Main where

import Control.Applicative
import Control.Arrow ((&&&))
import Data.List (find, nub)
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
    , canContain :: [Element]
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
    Just $ Elem name (toContentSpec xmlcs) (findAttrs dtd name) []
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
allAttrs = nub . concatMap (\(Elem _ _ attrs _) -> attrs)

clash = ["head"]

inners :: [Element] -> [ContentSpec]
inners = nub . map (\(Elem _ cspec _ _) -> cspec)

canAppearIn :: String -> [Element] -> [Element]
canAppearIn = filter . contentSpecContains
    where contentSpecContains str (Elem _ (CS _ set) _ _) = str `Set.member` set
          contentSpecContains str _ = False

main :: IO ()
main = putStrLn "Not finished..."