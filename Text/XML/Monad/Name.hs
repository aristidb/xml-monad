{-# LANGUAGE FlexibleContexts #-}
module Text.XML.Monad.Name
where
  
import           Text.XML.Monad.Core
import           Text.XML.Monad.Error
import qualified Text.XML.Light       as L
import           Data.Char
import           Data.Function
import           MonadLib

findElementNameG :: (ReaderM m L.Element, ExceptionM m e, FromXmlError e) => (L.QName -> L.QName -> Bool) -> L.QName -> m L.Element
findElementNameG cmp name = asksMaybe (fromXmlError $ XmlElementNotFoundQ name) $ L.filterElementName (cmp name)

findElementName :: (ReaderM m L.Element, ExceptionM m e, FromXmlError e) => L.QName -> m L.Element
findElementName = findElementNameG (==)

findElementNameU :: (ReaderM m L.Element, ExceptionM m e, FromXmlError e) => String -> m L.Element
findElementNameU = findElementNameG unqualEq . L.unqual

findElementNameUI :: (ReaderM m L.Element, ExceptionM m e, FromXmlError e) => String -> m L.Element
findElementNameUI = findElementNameG unqualEqI . L.unqual

findElementsNameG :: (ReaderM m L.Element) => (L.QName -> L.QName -> Bool) -> L.QName -> m [L.Element]
findElementsNameG cmp name = asks $ L.filterElementsName (cmp name)

findElementsName :: (ReaderM m L.Element) => L.QName -> m [L.Element]
findElementsName = findElementsNameG (==)

findElementsNameU :: (ReaderM m L.Element) => String -> m [L.Element]
findElementsNameU = findElementsNameG unqualEq . L.unqual

findElementsNameUI :: (ReaderM m L.Element) => String -> m [L.Element]
findElementsNameUI = findElementsNameG unqualEqI . L.unqual

testElementNameG :: (ReaderM m L.Element, ExceptionM m e, FromXmlError e) => (L.QName -> L.QName -> Bool) -> L.QName -> m ()
testElementNameG cmp expectedName = do
  actualName <- elName
  case expectedName `cmp` actualName of
    True -> return ()
    False -> raise (fromXmlError $ UnexpectedElementNameQ actualName expectedName)
    
testElementName :: (ReaderM m L.Element, ExceptionM m e, FromXmlError e) => L.QName -> m ()
testElementName = testElementNameG (==)

testElementNameU :: (ReaderM m L.Element, ExceptionM m e, FromXmlError e) => String -> m ()
testElementNameU = testElementNameG unqualEq . L.unqual

testElementNameUI :: (ReaderM m L.Element, ExceptionM m e, FromXmlError e) => String -> m ()
testElementNameUI = testElementNameG unqualEqI . L.unqual

unqualEq :: L.QName -> L.QName -> Bool
unqualEq = (==) `on` L.qName

unqualEqI :: L.QName -> L.QName -> Bool
unqualEqI = (==) `on` (map toLower . L.qName)
