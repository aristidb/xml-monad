{-# LANGUAGE FlexibleContexts #-}
module Text.XML.Monad.Name
where
  
import           Text.XML.Monad.Core
import qualified Text.XML.Light      as L
import           Data.Char
import           Data.Function
import           MonadLib

findElementNameG :: (ReaderM m L.Element, ExceptionM m e, FromParseError e) => (L.QName -> L.QName -> Bool) -> L.QName -> m L.Element
findElementNameG cmp name = asksMaybe (fromParseError $ XmlElementNotFoundQ name) $ L.filterElementName (cmp name)

findElementName :: (ReaderM m L.Element, ExceptionM m e, FromParseError e) => L.QName -> m L.Element
findElementName = findElementNameG (==)

findElementNameU :: (ReaderM m L.Element, ExceptionM m e, FromParseError e) => String -> m L.Element
findElementNameU = findElementNameG unqualEq . L.unqual

findElementNameUI :: (ReaderM m L.Element, ExceptionM m e, FromParseError e) => String -> m L.Element
findElementNameUI = findElementNameG unqualEqI . L.unqual

testElementNameG :: (ReaderM m L.Element, ExceptionM m e, FromParseError e) => (L.QName -> L.QName -> Bool) -> L.QName -> m ()
testElementNameG cmp expectedName = do
  actualName <- elName
  case expectedName `cmp` actualName of
    True -> return ()
    False -> raise (fromParseError $ UnexpectedElementNameQ actualName expectedName)
    
testElementName :: (ReaderM m L.Element, ExceptionM m e, FromParseError e) => L.QName -> m ()
testElementName = testElementNameG (==)

testElementNameU :: (ReaderM m L.Element, ExceptionM m e, FromParseError e) => String -> m ()
testElementNameU = testElementNameG unqualEq . L.unqual

testElementNameUI :: (ReaderM m L.Element, ExceptionM m e, FromParseError e) => String -> m ()
testElementNameUI = testElementNameG unqualEqI . L.unqual

unqualEq :: L.QName -> L.QName -> Bool
unqualEq = (==) `on` L.qName

unqualEqI :: L.QName -> L.QName -> Bool
unqualEqI = (==) `on` (map toLower . L.qName)
