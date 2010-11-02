{-# LANGUAGE FlexibleContexts #-}

module Text.XML.Monad.Output
where
  
import           Control.Monad.Reader.Class
import qualified Text.XML.Light             as L

showTopElement :: MonadReader L.Element m => m String
showTopElement = asks L.showTopElement

showContent :: MonadReader L.Content m => m String
showContent = asks L.showContent

showElement :: MonadReader L.Element m => m String
showElement = asks L.showElement

showCData :: MonadReader L.CData m => m String
showCData = asks L.showCData

showAttr :: MonadReader L.Attr m => m String
showAttr = asks L.showAttr

ppTopElement :: MonadReader L.Element m => m String
ppTopElement = asks L.ppTopElement

ppContent :: MonadReader L.Content m => m String
ppContent = asks L.ppContent

ppElement :: MonadReader L.Element m => m String
ppElement = asks L.ppElement

ppcTopElement :: MonadReader L.Element m => L.ConfigPP -> m String
ppcTopElement = asks . L.ppcTopElement

ppcContent :: MonadReader L.Content m => L.ConfigPP -> m String
ppcContent = asks . L.ppcContent

ppcElement :: MonadReader L.Element m => L.ConfigPP -> m String
ppcElement = asks . L.ppcElement

