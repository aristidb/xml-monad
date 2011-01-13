{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts #-}
module Text.XML.Monad.Core
(
  -- * XML monad (transformer) types
  XmlT(..)
, Xml
, runXmlT
, runXml
  -- * Monad functions
, inList
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
import           Control.Monad.Compose.Class
import           Control.Monad.Error
import           Control.Monad.Reader
import           Data.Functor.Identity
import qualified Text.XML.Light              as L

-- | Standard Xml reader + exception transformer type.
newtype XmlT e s m a = XmlT { fromXmlT :: ErrorT e (ReaderT s m) a }
    deriving (Functor, Monad, Applicative, MonadPlus, Alternative)

-- | Standard Xml reader + exception monadic type.
type Xml e s a = XmlT e s Identity a

instance Error e => MonadTrans (XmlT e s) where
    lift = XmlT . lift . lift

instance (Monad m, Error e) => MonadReader s (XmlT e s m) where
    ask = XmlT ask
    local f m = XmlT $ local f (fromXmlT m)

instance (Monad m, Error e) => MonadError e (XmlT e s m) where
    throwError e = XmlT $ throwError e
    catchError m f = XmlT $ catchError (fromXmlT m) (fromXmlT . f)

instance (Monad m, Error e) => MonadCompose (XmlT e s m) (XmlT e t m) s t where
    mcompose m n = XmlT $ mcompose (fromXmlT m) (fromXmlT n)

instance (MonadIO m, Error e) => MonadIO (XmlT e s m) where
    liftIO = XmlT . liftIO

-- | Run an 'XmlT'.
runXmlT :: XmlT e s m a -> s -> m (Either e a)
runXmlT = runReaderT . runErrorT . fromXmlT

-- | Run an 'Xml'.
runXml :: Xml e s a -> s -> Either e a
runXml = runIdentity .: runXmlT
    where (.:) = (.).(.)

-- | Run a reader inside a list.
inList :: (MonadCompose m n s t, MonadReader [s] n) => m a -> n [a]
inList m = ask >>= mapM (mapply m)

elName :: MonadReader L.Element m => m L.QName
elName = asks L.elName

elAttribs :: MonadReader L.Element m => m [L.Attr]
elAttribs = asks L.elAttribs

elContent :: MonadReader L.Element m => m [L.Content]
elContent = asks L.elContent

elLine :: MonadReader L.Element m => m (Maybe L.Line)
elLine = asks L.elLine

attrKey :: MonadReader L.Attr m => m L.QName
attrKey = asks L.attrKey

attrVal :: MonadReader L.Attr m => m String
attrVal = asks L.attrVal

cdVerbatim :: MonadReader L.CData m => m L.CDataKind
cdVerbatim = asks L.cdVerbatim

cdLine :: MonadReader L.CData m => m (Maybe L.Line)
cdLine = asks L.cdLine

cdData :: MonadReader L.CData m => m String
cdData = asks L.cdData

qName :: MonadReader L.QName m => m String
qName = asks L.qName

qURI :: MonadReader L.QName m => m (Maybe String)
qURI = asks L.qURI

qPrefix :: MonadReader L.QName m => m (Maybe String)
qPrefix = asks L.qPrefix
