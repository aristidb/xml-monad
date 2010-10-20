{-# LANGUAGE FlexibleContexts #-}

module Text.XML.Monad.Output
where
  
import qualified Text.XML.Light      as L
import           MonadLib

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

