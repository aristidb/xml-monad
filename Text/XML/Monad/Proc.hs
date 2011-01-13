{-# LANGUAGE FlexibleContexts #-}
module Text.XML.Monad.Proc
where
  
import           Control.Monad.Error.Class
import           Control.Monad.Reader.Class
import           Text.XML.Monad.Error
import qualified Text.XML.Light             as L

strContent :: MonadReader L.Element m => m String
strContent = asks L.strContent

readContent :: (MonadReader L.Element m, Read a) => m a
readContent = asks (read . L.strContent)

onlyElems :: MonadReader [L.Content] m => m [L.Element]
onlyElems = asks L.onlyElems

elChildren :: MonadReader L.Element m => m [L.Element]
elChildren = asks L.elChildren

onlyText :: MonadReader [L.Content] m => m [L.CData]
onlyText = asks L.onlyText

findChildren :: MonadReader L.Element m => L.QName -> m [L.Element]
findChildren = asks . L.findChildren

filterChildren :: MonadReader L.Element m => (L.Element -> Bool) -> m [L.Element]
filterChildren = asks . L.filterChildren

filterChildrenName :: MonadReader L.Element m => (L.QName -> Bool) -> m [L.Element]
filterChildrenName = asks . L.filterChildrenName

findChild :: (MonadReader L.Element m, MonadError e m, FromXmlError e) => L.QName -> m L.Element
findChild name = asksMaybeXml (XmlChildNotFoundQ name) (L.findChild name)

filterChild :: (MonadReader L.Element m, MonadError e m, FromXmlError e) => (L.Element -> Bool) -> m L.Element
filterChild = asksMaybeXml XmlChildNotFound . L.filterChild

filterChildName :: (MonadReader L.Element m, MonadError e m, FromXmlError e) => (L.QName -> Bool) -> m L.Element
filterChildName = asksMaybeXml XmlChildNotFound . L.filterChildName

findElement :: (MonadReader L.Element m, MonadError e m, FromXmlError e) => L.QName -> m L.Element
findElement name = asksMaybeXml (XmlElementNotFoundQ name) (L.findElement name)

filterElement :: (MonadReader L.Element m, MonadError e m, FromXmlError e) => (L.Element -> Bool) -> m L.Element
filterElement = asksMaybeXml XmlElementNotFound . L.filterElement

filterElementName :: (MonadReader L.Element m, MonadError e m, FromXmlError e) => (L.QName -> Bool) -> m L.Element
filterElementName = asksMaybeXml XmlElementNotFound . L.filterElementName

findElements :: MonadReader L.Element m => L.QName -> m [L.Element]
findElements = asks . L.findElements

filterElements :: MonadReader L.Element m => (L.Element -> Bool) -> m [L.Element]
filterElements = asks . L.filterElements

filterElementsName :: MonadReader L.Element m => (L.QName -> Bool) -> m [L.Element]
filterElementsName = asks . L.filterElementsName

findAttr :: (MonadReader L.Element m, MonadError e m, FromXmlError e) => L.QName -> m String
findAttr name = asksMaybeXml (XmlAttributeNotFoundQ name) (L.findAttr name)

lookupAttr :: (MonadReader [L.Attr] m, MonadError e m, FromXmlError e) => L.QName -> m String
lookupAttr name = asksMaybeXml (XmlAttributeNotFoundQ name) (L.lookupAttr name)

findAttrBy :: (MonadReader L.Element m, MonadError e m, FromXmlError e) => (L.QName -> Bool) -> m String
findAttrBy = asksMaybeXml XmlAttributeNotFound . L.findAttrBy

lookupAttrBy :: (MonadReader [L.Attr] m, MonadError e m, FromXmlError e) => (L.QName -> Bool) -> m String
lookupAttrBy = asksMaybeXml XmlAttributeNotFound . L.lookupAttrBy
