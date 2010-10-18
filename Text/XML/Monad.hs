{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, FunctionalDependencies, FlexibleContexts #-}

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
    | XmlChildNotFound
    | XmlChildNotFoundQ L.QName
    | XmlElementNotFound
    | XmlElementNotFoundQ L.QName
    | XmlAttributeNotFound
    | XmlAttributeNotFoundQ L.QName
    | UnexpectedElementName String String
    | OtherParseError
    deriving (Show)

newtype XmlMonadT s m a = XmlMonadT { fromXmlMonadT :: ExceptionT ParseError (ReaderT s m) a }
    deriving (Functor, Monad, Applicative)

type XmlMonad s a = XmlMonadT s Id a

isoXmlMonadT :: Iso (ExceptionT ParseError (ReaderT s m)) (XmlMonadT s m)
isoXmlMonadT = Iso XmlMonadT fromXmlMonadT

instance BaseM m n => BaseM (XmlMonadT s m) n where
    inBase = derive_inBase isoXmlMonadT

instance MonadT (XmlMonadT s) where
    lift = XmlMonadT . lift . lift

instance Monad m => ReaderM (XmlMonadT s m) s where
    ask = derive_ask isoXmlMonadT

instance Monad m => ExceptionM (XmlMonadT s m) ParseError where
    raise = derive_raise isoXmlMonadT

instance Monad m => RunExceptionM (XmlMonadT s m) ParseError where
    try = derive_try isoXmlMonadT

class (ReaderM m s, ExceptionM m ParseError) => MonadXml m s | m -> s

instance Monad m => MonadXml (XmlMonadT s m) s

runXmlMonadT :: s -> XmlMonadT s m a -> m (Either ParseError a)
runXmlMonadT r = runReaderT r . runExceptionT . fromXmlMonadT

runXmlMonad :: s -> XmlMonad s a -> Either ParseError a
runXmlMonad r = runId . runXmlMonadT r

returnEither :: ExceptionM m i => Either i a -> m a
returnEither (Left err) = raise err
returnEither (Right x)  = return x

maybeRaise :: ExceptionM m i => i -> Maybe a -> m a
maybeRaise err Nothing  = raise err
maybeRaise _   (Just x) = return x

asksEither :: MonadXml m s => (s -> Either ParseError a) -> m a
asksEither f = ask >>= returnEither . f

asksMaybe :: MonadXml m s => ParseError -> (s -> Maybe a) -> m a
asksMaybe err f = ask >>= maybeRaise err . f

with :: Monad m => XmlMonadT t m a -> XmlMonadT s m t -> XmlMonadT s m a
with inner outer = do
  s <- outer
  u <- lift (runXmlMonadT s inner)
  returnEither u

(>>>) = flip with
infixl 1 >>>

(<<<) = with
infixr 1 <<<

tryMaybe :: RunExceptionM m i => m a -> m (Maybe a)
tryMaybe a = either (const Nothing) Just `liftM` try a

tryBool :: RunExceptionM m i => m a -> m Bool
tryBool a = either (const False) (const True) `liftM` try a

parseXML :: (ReaderM m s, LL.XmlSource s) => m [L.Content]
parseXML = asks L.parseXML

parseXMLDoc :: (MonadXml m s, LL.XmlSource s) => m L.Element
parseXMLDoc = asksMaybe InvalidXml L.parseXMLDoc

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

strContent :: ReaderM m L.Element => m String
strContent = asks L.strContent

onlyElems :: ReaderM m [L.Content] => m [L.Element]
onlyElems = asks L.onlyElems

elChildren :: ReaderM m L.Element => m [L.Element]
elChildren = asks L.elChildren

onlyText :: ReaderM m [L.Content] => m [L.CData]
onlyText = asks L.onlyText

findChildren :: ReaderM m L.Element => L.QName -> m [L.Element]
findChildren = asks . L.findChildren

filterChildren :: ReaderM m L.Element => (L.Element -> Bool) -> m [L.Element]
filterChildren = asks . L.filterChildren

filterChildrenName :: ReaderM m L.Element => (L.QName -> Bool) -> m [L.Element]
filterChildrenName = asks . L.filterChildrenName

findChild :: MonadXml m L.Element => L.QName -> m L.Element
findChild name = asksMaybe (XmlChildNotFoundQ name) (L.findChild name)

filterChild :: MonadXml m L.Element => (L.Element -> Bool) -> m L.Element
filterChild = asksMaybe XmlChildNotFound . L.filterChild

filterChildName :: MonadXml m L.Element => (L.QName -> Bool) -> m L.Element
filterChildName = asksMaybe XmlChildNotFound . L.filterChildName

findElement :: MonadXml m L.Element => L.QName -> m L.Element
findElement name = asksMaybe (XmlElementNotFoundQ name) (L.findElement name)

filterElement :: MonadXml m L.Element => (L.Element -> Bool) -> m L.Element
filterElement = asksMaybe XmlElementNotFound . L.filterElement

filterElementName :: MonadXml m L.Element => (L.QName -> Bool) -> m L.Element
filterElementName = asksMaybe XmlElementNotFound . L.filterElementName

findElements :: ReaderM m L.Element => L.QName -> m [L.Element]
findElements = asks . L.findElements

filterElements :: ReaderM m L.Element => (L.Element -> Bool) -> m [L.Element]
filterElements = asks . L.filterElements

filterElementsName :: ReaderM m L.Element => (L.QName -> Bool) -> m [L.Element]
filterElementsName = asks . L.filterElementsName

findAttr :: MonadXml m L.Element => L.QName -> m String
findAttr name = asksMaybe (XmlAttributeNotFoundQ name) (L.findAttr name)

lookupAttr :: MonadXml m [L.Attr] => L.QName -> m String
lookupAttr name = asksMaybe (XmlAttributeNotFoundQ name) (L.lookupAttr name)

findAttrBy :: MonadXml m L.Element => (L.QName -> Bool) -> m String
findAttrBy = asksMaybe XmlAttributeNotFound . L.findAttrBy

lookupAttrBy :: MonadXml m [L.Attr] => (L.QName -> Bool) -> m String
lookupAttrBy = asksMaybe XmlAttributeNotFound . L.lookupAttrBy

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

ppContent :: ReaderM m L.Content => XmlMonadT L.Content m String
ppContent = asks L.ppContent

ppElement :: ReaderM m L.Element => XmlMonadT L.Element m String
ppElement = asks L.ppElement

ppcTopElement :: ReaderM m L.Element => L.ConfigPP -> m String
ppcTopElement = asks . L.ppcTopElement

ppcContent :: ReaderM m L.Content => L.ConfigPP -> m String
ppcContent = asks . L.ppcContent

ppcElement :: ReaderM m L.Element => L.ConfigPP -> m String
ppcElement = asks . L.ppcElement
