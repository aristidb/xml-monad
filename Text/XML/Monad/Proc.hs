{-# LANGUAGE FlexibleContexts #-}
module Text.XML.Monad.Proc
where
  
import           Text.XML.Monad.Core
import qualified Text.XML.Light      as L
import           MonadLib

strContent :: ReaderM m L.Element => m String
strContent = asks L.strContent

onlyElems :: ReaderM m [L.Content] => m [L.Element]
onlyElems = asks L.onlyElems

elChildren :: ReaderM m L.Element => m [L.Element]
elChildren = asks L.elChildren

onlyText :: ReaderM m [L.Content] => m [L.CData]
onlyText = asks L.onlyText

findChildren :: ReaderM m L.Element => L.QName -> m [L.Element]
findChildren = asks . L.findChildren

filterChildren :: ReaderM m L.Element => (L.Element -> Bool) -> m [L.Element]
filterChildren = asks . L.filterChildren

filterChildrenName :: ReaderM m L.Element => (L.QName -> Bool) -> m [L.Element]
filterChildrenName = asks . L.filterChildrenName

findChild :: (ReaderM m L.Element, ExceptionM m ParseError) => L.QName -> m L.Element
findChild name = asksMaybe (XmlChildNotFoundQ name) (L.findChild name)

filterChild :: (ReaderM m L.Element, ExceptionM m ParseError) => (L.Element -> Bool) -> m L.Element
filterChild = asksMaybe XmlChildNotFound . L.filterChild

filterChildName :: (ReaderM m L.Element, ExceptionM m ParseError) => (L.QName -> Bool) -> m L.Element
filterChildName = asksMaybe XmlChildNotFound . L.filterChildName

findElement :: (ReaderM m L.Element, ExceptionM m ParseError) => L.QName -> m L.Element
findElement name = asksMaybe (XmlElementNotFoundQ name) (L.findElement name)

filterElement :: (ReaderM m L.Element, ExceptionM m ParseError) => (L.Element -> Bool) -> m L.Element
filterElement = asksMaybe XmlElementNotFound . L.filterElement

filterElementName :: (ReaderM m L.Element, ExceptionM m ParseError) => (L.QName -> Bool) -> m L.Element
filterElementName = asksMaybe XmlElementNotFound . L.filterElementName

findElements :: ReaderM m L.Element => L.QName -> m [L.Element]
findElements = asks . L.findElements

filterElements :: ReaderM m L.Element => (L.Element -> Bool) -> m [L.Element]
filterElements = asks . L.filterElements

filterElementsName :: ReaderM m L.Element => (L.QName -> Bool) -> m [L.Element]
filterElementsName = asks . L.filterElementsName

findAttr :: (ReaderM m L.Element, ExceptionM m ParseError) => L.QName -> m String
findAttr name = asksMaybe (XmlAttributeNotFoundQ name) (L.findAttr name)

lookupAttr :: (ReaderM m [L.Attr], ExceptionM m ParseError) => L.QName -> m String
lookupAttr name = asksMaybe (XmlAttributeNotFoundQ name) (L.lookupAttr name)

findAttrBy :: (ReaderM m L.Element, ExceptionM m ParseError) => (L.QName -> Bool) -> m String
findAttrBy = asksMaybe XmlAttributeNotFound . L.findAttrBy

lookupAttrBy :: (ReaderM m [L.Attr], ExceptionM m ParseError) => (L.QName -> Bool) -> m String
lookupAttrBy = asksMaybe XmlAttributeNotFound . L.lookupAttrBy