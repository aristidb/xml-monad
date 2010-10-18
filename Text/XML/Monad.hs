{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, NoMonomorphismRestriction #-}

module Text.XML.Monad
where
  
import qualified Text.XML.Light       as L
import qualified Text.XML.Light.Lexer as LL
import           Control.Applicative
import           MonadLib
import           MonadLib.Derive

data ParseError
    = EmptyDocument
    | InvalidXml
    | XmlElementNotFound String
    | UnexpectedElementName String String
    deriving (Show)

newtype XmlMonadT s m a = XmlMonadT { fromXmlMonadT :: ExceptionT ParseError (ReaderT s m) a }
    deriving (Functor, Monad, Applicative)

isoXmlMonadT :: Iso (ExceptionT ParseError (ReaderT s m)) (XmlMonadT s m)
isoXmlMonadT = Iso XmlMonadT fromXmlMonadT

instance MonadT (XmlMonadT s) where
    lift = XmlMonadT . lift . lift

instance Monad m => ReaderM (XmlMonadT s m) s where
    ask = derive_ask isoXmlMonadT

instance Monad m => ExceptionM (XmlMonadT s m) ParseError where
    raise = derive_raise isoXmlMonadT

runXmlMonadT :: s -> XmlMonadT s m a -> m (Either ParseError a)
runXmlMonadT r = runReaderT r . runExceptionT . fromXmlMonadT

returnEither :: Monad m => Either ParseError a -> XmlMonadT s m a
returnEither (Left err) = raise err
returnEither (Right x)  = return x

maybeRaise :: Monad m => ParseError -> Maybe a -> XmlMonadT s m a
maybeRaise err Nothing  = raise err
maybeRaise _   (Just x) = return x

asksEither :: Monad m => (s -> Either ParseError a) -> XmlMonadT s m a
asksEither f = ask >>= returnEither . f

asksMaybe :: Monad m => ParseError -> (s -> Maybe a) -> XmlMonadT s m a
asksMaybe err f = ask >>= maybeRaise err . f

with :: Monad m => XmlMonadT t m a -> XmlMonadT s m t -> XmlMonadT s m a
with inner outer = do
  s <- outer
  u <- lift (runXmlMonadT s inner)
  returnEither u

(>>>) = flip with

(<<<) = with

parseXML :: (Monad m, LL.XmlSource s) => XmlMonadT s m [L.Content]
parseXML = asks L.parseXML

parseXMLDoc :: (Monad m, LL.XmlSource s) => XmlMonadT s m L.Element
parseXMLDoc = asksMaybe InvalidXml L.parseXMLDoc

elName :: Monad m => XmlMonadT L.Element m L.QName
elName = asks L.elName

elAttribs :: Monad m => XmlMonadT L.Element m [L.Attr]
elAttribs = asks L.elAttribs

elContent :: Monad m => XmlMonadT L.Element m [L.Content]
elContent = asks L.elContent

elLine :: Monad m => XmlMonadT L.Element m (Maybe L.Line)
elLine = asks L.elLine

attrKey :: Monad m => XmlMonadT L.Attr m L.QName
attrKey = asks L.attrKey

attrVal :: Monad m => XmlMonadT L.Attr m String
attrVal = asks L.attrVal

cdVerbatim :: Monad m => XmlMonadT L.CData m L.CDataKind
cdVerbatim = asks L.cdVerbatim

cdLine :: Monad m => XmlMonadT L.CData m (Maybe L.Line)
cdLine = asks L.cdLine

cdData :: Monad m => XmlMonadT L.CData m String
cdData = asks L.cdData

qName :: Monad m => XmlMonadT L.QName m String
qName = asks L.qName

qURI :: Monad m => XmlMonadT L.QName m (Maybe String)
qURI = asks L.qURI

qPrefix :: Monad m => XmlMonadT L.QName m (Maybe String)
qPrefix = asks L.qPrefix

strContent :: Monad m => XmlMonadT L.Element m String
strContent = asks L.strContent

onlyElems :: Monad m => XmlMonadT [L.Content] m [L.Element]
onlyElems = asks L.onlyElems

elChildren :: Monad m => XmlMonadT L.Element m [L.Element]
elChildren = asks L.elChildren
