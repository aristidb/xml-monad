{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, FunctionalDependencies, FlexibleContexts #-}
module Text.XML.Monad.Core
where
  
import           Control.Applicative
import qualified Text.XML.Light      as L
import           MonadLib
import           MonadLib.Derive
import           MonadLib.Compose

data ParseError
    = EmptyDocument
    | InvalidXml
    | XmlChildNotFound
    | XmlChildNotFoundQ L.QName
    | XmlElementNotFound
    | XmlElementNotFoundQ L.QName
    | XmlAttributeNotFound
    | XmlAttributeNotFoundQ L.QName
    | UnexpectedElementNameQ L.QName L.QName
    | XmlError String
    | OtherError String
    deriving (Show)

newtype XmlT s m a = XmlT { fromXmlT :: ExceptionT ParseError (ReaderT s m) a }
    deriving (Functor, Monad, Applicative)

type Xml s a = XmlT s Id a

isoXmlT :: Iso (ExceptionT ParseError (ReaderT s m)) (XmlT s m)
isoXmlT = Iso XmlT fromXmlT

instance BaseM m n => BaseM (XmlT s m) n where
    inBase = derive_inBase isoXmlT

instance MonadT (XmlT s) where
    lift = XmlT . lift . lift

instance Monad m => ReaderM (XmlT s m) s where
    ask = derive_ask isoXmlT

instance Monad m => ExceptionM (XmlT s m) ParseError where
    raise = derive_raise isoXmlT

instance Monad m => RunExceptionM (XmlT s m) ParseError where
    try = derive_try isoXmlT

instance Monad m => ComposeM (XmlT s m) (XmlT t m) s t where
    mcompose = derive_mcompose isoXmlT isoXmlT
    mapply = derive_mapply isoXmlT isoXmlT

instance (RunM m (Either ParseError a) r) => RunM (XmlT s m) a (s -> r) where
    runM = derive_runM isoXmlT

runXmlT :: s -> XmlT s m a -> m (Either ParseError a)
runXmlT r = runReaderT r . runExceptionT . fromXmlT

runXml :: s -> Xml s a -> Either ParseError a
runXml r = runId . runXmlT r

maybeRaise :: ExceptionM m i => i -> Maybe a -> m a
maybeRaise err Nothing  = raise err
maybeRaise _   (Just x) = return x

asksEither :: (ReaderM m s, ExceptionM m e) => (s -> Either e a) -> m a
asksEither f = ask >>= raises . f

asksMaybe :: (ReaderM m s, ExceptionM m e) => e -> (s -> Maybe a) -> m a
asksMaybe err f = ask >>= maybeRaise err . f

tryMaybe :: RunExceptionM m i => m a -> m (Maybe a)
tryMaybe a = either (const Nothing) Just `liftM` try a

tryBool :: RunExceptionM m i => m a -> m Bool
tryBool a = either (const False) (const True) `liftM` try a

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
