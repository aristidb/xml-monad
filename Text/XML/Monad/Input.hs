{-# LANGUAGE FlexibleContexts #-}

module Text.XML.Monad.Input
where
  
import           Text.XML.Monad.Core
import           Data.Maybe
import           MonadLib
import qualified Text.XML.Light       as L
import qualified Text.XML.Light.Lexer as LL

parseXML :: (ReaderM m s, LL.XmlSource s) => m [L.Content]
parseXML = asks L.parseXML

parseXMLDoc :: (ReaderM m s, ExceptionM m ParseError, LL.XmlSource s) => m L.Element
parseXMLDoc = do
  x <- ask
  when (isNothing $ LL.uncons x) $ raise EmptyDocument
  maybeRaise InvalidXml $ L.parseXMLDoc x