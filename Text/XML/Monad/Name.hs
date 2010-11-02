{-# LANGUAGE FlexibleContexts #-}
module Text.XML.Monad.Name
where
  
import           Control.Monad.Error.Class
import           Control.Monad.Reader.Class
import           Data.Char
import           Data.Function
import           Text.XML.Monad.Core
import           Text.XML.Monad.Error
import qualified Text.XML.Light             as L

findElementNameG :: (MonadReader L.Element m, MonadError e m, FromXmlError e) => (L.QName -> L.QName -> Bool) -> L.QName -> m L.Element
findElementNameG cmp name = asksMaybe (fromXmlError $ XmlElementNotFoundQ name) $ L.filterElementName (cmp name)

findElementName :: (MonadReader L.Element m, MonadError e m, FromXmlError e) => L.QName -> m L.Element
findElementName = findElementNameG (==)

findElementNameU :: (MonadReader L.Element m, MonadError e m, FromXmlError e) => String -> m L.Element
findElementNameU = findElementNameG unqualEq . L.unqual

findElementNameUI :: (MonadReader L.Element m, MonadError e m, FromXmlError e) => String -> m L.Element
findElementNameUI = findElementNameG unqualEqI . L.unqual

findElementsNameG :: (MonadReader L.Element m) => (L.QName -> L.QName -> Bool) -> L.QName -> m [L.Element]
findElementsNameG cmp name = asks $ L.filterElementsName (cmp name)

findElementsName :: (MonadReader L.Element m) => L.QName -> m [L.Element]
findElementsName = findElementsNameG (==)

findElementsNameU :: (MonadReader L.Element m) => String -> m [L.Element]
findElementsNameU = findElementsNameG unqualEq . L.unqual

findElementsNameUI :: (MonadReader L.Element m) => String -> m [L.Element]
findElementsNameUI = findElementsNameG unqualEqI . L.unqual

testElementNameG :: (MonadReader L.Element m, MonadError e m, FromXmlError e) => (L.QName -> L.QName -> Bool) -> L.QName -> m ()
testElementNameG cmp expectedName = do
  actualName <- elName
  case expectedName `cmp` actualName of
    True -> return ()
    False -> raise (fromXmlError $ UnexpectedElementNameQ actualName expectedName)
    
testElementName :: (MonadReader L.Element m, MonadError e m, FromXmlError e) => L.QName -> m ()
testElementName = testElementNameG (==)

testElementNameU :: (MonadReader L.Element m, MonadError e m, FromXmlError e) => String -> m ()
testElementNameU = testElementNameG unqualEq . L.unqual

testElementNameUI :: (MonadReader L.Element m, MonadError e m, FromXmlError e) => String -> m ()
testElementNameUI = testElementNameG unqualEqI . L.unqual

unqualEq :: L.QName -> L.QName -> Bool
unqualEq = (==) `on` L.qName

unqualEqI :: L.QName -> L.QName -> Bool
unqualEqI = (==) `on` (map toLower . L.qName)
