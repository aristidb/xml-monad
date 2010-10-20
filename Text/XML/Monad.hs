{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, FunctionalDependencies, FlexibleContexts #-}

module Text.XML.Monad
where
  
import           Text.XML.Monad.Core
import qualified Text.XML.Light       as L
import qualified Text.XML.Light.Lexer as LL
import           MonadLib

elName :: ReaderM m L.Element => m L.QName
elName = asks L.elName

elAttribs :: ReaderM m L.Element => m [L.Attr]
elAttribs = asks L.elAttribs

elContent :: ReaderM m L.Element => m [L.Content]
elContent = asks L.elContent

elLine :: ReaderM m L.Element => m (Maybe L.Line)
elLine = asks L.elLine

attrKey :: ReaderM m L.Attr => m L.QName
attrKey = asks L.attrKey

attrVal :: ReaderM m L.Attr => m String
attrVal = asks L.attrVal

cdVerbatim :: ReaderM m L.CData => m L.CDataKind
cdVerbatim = asks L.cdVerbatim

cdLine :: ReaderM m L.CData => m (Maybe L.Line)
cdLine = asks L.cdLine

cdData :: ReaderM m L.CData => m String
cdData = asks L.cdData

qName :: ReaderM m L.QName => m String
qName = asks L.qName

qURI :: ReaderM m L.QName => m (Maybe String)
qURI = asks L.qURI

qPrefix :: ReaderM m L.QName => m (Maybe String)
qPrefix = asks L.qPrefix

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

showTopElement :: ReaderM m L.Element => m String
showTopElement = asks L.showTopElement

showContent :: ReaderM m L.Content => m String
showContent = asks L.showContent

showElement :: ReaderM m L.Element => m String
showElement = asks L.showElement

showCData :: ReaderM m L.CData => m String
showCData = asks L.showCData

showAttr :: ReaderM m L.Attr => m String
showAttr = asks L.showAttr

ppTopElement :: ReaderM m L.Element => m String
ppTopElement = asks L.ppTopElement

ppContent :: ReaderM m L.Content => m String
ppContent = asks L.ppContent

ppElement :: ReaderM m L.Element => m String
ppElement = asks L.ppElement

ppcTopElement :: ReaderM m L.Element => L.ConfigPP -> m String
ppcTopElement = asks . L.ppcTopElement

ppcContent :: ReaderM m L.Content => L.ConfigPP -> m String
ppcContent = asks . L.ppcContent

ppcElement :: ReaderM m L.Element => L.ConfigPP -> m String
ppcElement = asks . L.ppcElement
