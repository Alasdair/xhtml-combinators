{-# LANGUAGE PatternGuards #-}
-- |
-- Module      : DTD2Combinators.HaXml
-- Copyright   : (c) Alasdair Armstrong 2010
-- License     : BSD-style
-- Maintainer  : alasdair.armstrong@googlemail.com
-- Stability   : experimental
-- Portability : GHC

module DTD2Combinators.HaXml
    ( Element (..)
    , Attr (..), isRequired
    , Default (..)
    , ContentSpec (..)
    , readDTD
    ) where

import Data.List (find)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Text.XML.HaXml as XML
import qualified Text.XML.HaXml.Parse as XML (dtdParse') 

-- -----------------------------------------------------------------------------
-- DTD Element Type
-- -----------------------------------------------------------------------------

-- | HaXml's DTD type is much more complicated than what we need. Instead, we
-- choose to represent a DTD as a list of elements. Each element has a name 
-- (for example in the XHTML DTD, head, body and title) a contentspec, which
-- tells us what elements our element can have nested within it and a list of
-- attributes it supports.
data Element = Elem
    { elemName :: String
    , elemContentSpec :: ContentSpec
    , elemAttrs :: [Attr]
    } deriving (Show)

data Attr = Attr 
    { attrName :: String
    , attrDefault :: Default
    } deriving (Eq, Show)

isRequired :: Attr -> Bool
isRequired (Attr _ Required) = True
isRequired _ = False

data Default = Implied
             | Required
             | Fixed String
             | Default String
             deriving (Eq, Show)

data ContentSpec = Empty 
                 | Any
                 | CS Bool (Set String)
                   -- ^ Bool represents whether or not we allow text data.
                 deriving (Eq, Show)

-- -----------------------------------------------------------------------------
-- HaXml Conversion
-- -----------------------------------------------------------------------------

-- | Given an XML DTD, and a toplevel markup declaration, turn it into
-- a value of type 'Element'. If the toplevel declaration does not
-- represent an element 'toElement' returns 'Nothing'.
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

-- | Given the DTD and name of an element, find it's attributes.
findAttrs :: XML.DocTypeDecl -> String -> [Attr]
findAttrs (XML.DTD _ _ decls) name
    | Just (XML.AttList attList) <- find attrByName decls
    = toAttrs attList
    | otherwise = []
    where attrByName :: XML.MarkupDecl -> Bool
          attrByName (XML.AttList (XML.AttListDecl n _)) = n == name
          attrByName _ = False

-- | Turn a HaXml 'AttListDecl' into a list of our simpler 'Attr' types
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

-- | Turn a HaXml DTD into a list of our simpler element types.
elements :: XML.DocTypeDecl -> [Element]
elements dtd@(XML.DTD _ _ decls) = mapMaybe (toElement dtd) decls

-- -----------------------------------------------------------------------------
-- DTD Input
-- -----------------------------------------------------------------------------

-- | Read a DTD from a file.
readDTD :: FilePath -> IO (Either String [Element])
readDTD path = do
    dtd <- readFile path
    return $ case XML.dtdParse' path dtd of
        Right Nothing  -> Left ("No DTD found at " ++ path)
        Right (Just d) -> Right (elements d)
        Left str       -> Left str