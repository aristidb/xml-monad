{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, FunctionalDependencies, FlexibleContexts #-}
module Text.XML.Monad.Core
(
  -- * XML monad (transformer) types
  XmlT(..)
, Xml
, runXmlT
, runXml
  -- * XML access.
, elName
, elAttribs
, elContent
, elLine
, attrKey
, attrVal
, cdVerbatim
, cdLine
, cdData
, qName
, qURI
, qPrefix
)
where

import           Control.Applicative
import qualified Text.XML.Light       as L
import           MonadLib
import           MonadLib.Derive
import           MonadLib.Compose

-- | Standard Xml reader + exception transformer type.
newtype XmlT e s m a = XmlT { fromXmlT :: ExceptionT e (ReaderT s m) a }
    deriving (Functor, Monad, Applicative)

-- | Standard Xml reader + exception monadic type.
type Xml e s a = XmlT e s Id a

isoXmlT :: Iso (ExceptionT e (ReaderT s m)) (XmlT e s m)
isoXmlT = Iso XmlT fromXmlT

instance BaseM m n => BaseM (XmlT e s m) n where
    inBase = derive_inBase isoXmlT

instance MonadT (XmlT e s) where
    lift = XmlT . lift . lift

instance Monad m => ReaderM (XmlT e s m) s where
    ask = derive_ask isoXmlT

instance Monad m => ExceptionM (XmlT e s m) e where
    raise = derive_raise isoXmlT

instance Monad m => RunExceptionM (XmlT e s m) e where
    try = derive_try isoXmlT

instance Monad m => ComposeM (XmlT e s m) (XmlT e t m) s t where
    mcompose = derive_mcompose isoXmlT isoXmlT
    mapply = derive_mapply isoXmlT isoXmlT

instance (RunM m (Either e a) r) => RunM (XmlT e s m) a (s -> r) where
    runM = derive_runM isoXmlT

-- | Run an 'XmlT'.
runXmlT :: s -> XmlT e s m a -> m (Either e a)
runXmlT r = runReaderT r . runExceptionT . fromXmlT

-- | Run an 'Xml'.
runXml :: s -> Xml e s a -> Either e a
runXml r = runId . runXmlT r

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
