{-# LANGUAGE FlexibleContexts #-}

module Text.XML.Monad.Input
where
  
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.Reader.Class
import           Data.Maybe
import           Text.XML.Monad.Error
import qualified Text.XML.Light             as L
import qualified Text.XML.Light.Lexer       as LL

parseXML :: (MonadReader s m, LL.XmlSource s) => m [L.Content]
parseXML = asks L.parseXML

parseXMLDoc :: (MonadReader s m, MonadError e m, FromXmlError e, LL.XmlSource s) => m L.Element
parseXMLDoc = do
  x <- ask
  when (isNothing $ LL.uncons x) $ raise (fromXmlError EmptyDocument)
  maybeRaise (fromXmlError InvalidXml) $ L.parseXMLDoc x
