{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

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

runXmlMonadT :: s -> XmlMonadT s m a -> m (Either ParseError a)
runXmlMonadT r = runReaderT r . runExceptionT . fromXmlMonadT

runXmlMonad :: s -> XmlMonad s a -> Either ParseError a
runXmlMonad r = runId . runXmlMonadT r

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
infixl 1 >>>

(<<<) = with
infixr 1 <<<

tryMaybe :: Monad m => XmlMonadT s m a -> XmlMonadT s m (Maybe a)
tryMaybe a = either (const Nothing) Just <$> try a

tryBool :: Monad m => XmlMonadT s m a -> XmlMonadT s m Bool
tryBool a = either (const False) (const True) <$> try a

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

onlyText :: Monad m => XmlMonadT [L.Content] m [L.CData]
onlyText = asks L.onlyText

findChildren :: Monad m => L.QName -> XmlMonadT L.Element m [L.Element]
findChildren = asks . L.findChildren

filterChildren :: Monad m => (L.Element -> Bool) -> XmlMonadT L.Element m [L.Element]
filterChildren = asks . L.filterChildren

filterChildrenName :: Monad m => (L.QName -> Bool) -> XmlMonadT L.Element m [L.Element]
filterChildrenName = asks . L.filterChildrenName

findChild :: Monad m => L.QName -> XmlMonadT L.Element m L.Element
findChild name = asksMaybe (XmlChildNotFoundQ name) (L.findChild name)

filterChild :: Monad m => (L.Element -> Bool) -> XmlMonadT L.Element m L.Element
filterChild = asksMaybe XmlChildNotFound . L.filterChild

filterChildName :: Monad m => (L.QName -> Bool) -> XmlMonadT L.Element m L.Element
filterChildName = asksMaybe XmlChildNotFound . L.filterChildName

findElement :: Monad m => L.QName -> XmlMonadT L.Element m L.Element
findElement name = asksMaybe (XmlElementNotFoundQ name) (L.findElement name)

filterElement :: Monad m => (L.Element -> Bool) -> XmlMonadT L.Element m L.Element
filterElement = asksMaybe XmlElementNotFound . L.filterElement

filterElementName :: Monad m => (L.QName -> Bool) -> XmlMonadT L.Element m L.Element
filterElementName = asksMaybe XmlElementNotFound . L.filterElementName

findElements :: Monad m => L.QName -> XmlMonadT L.Element m [L.Element]
findElements = asks . L.findElements

filterElements :: Monad m => (L.Element -> Bool) -> XmlMonadT L.Element m [L.Element]
filterElements = asks . L.filterElements

filterElementsName :: Monad m => (L.QName -> Bool) -> XmlMonadT L.Element m [L.Element]
filterElementsName = asks . L.filterElementsName

findAttr :: Monad m => L.QName -> XmlMonadT L.Element m String
findAttr name = asksMaybe (XmlAttributeNotFoundQ name) (L.findAttr name)

lookupAttr :: Monad m => L.QName -> XmlMonadT [L.Attr] m String
lookupAttr name = asksMaybe (XmlAttributeNotFoundQ name) (L.lookupAttr name)

findAttrBy :: Monad m => (L.QName -> Bool) -> XmlMonadT L.Element m String
findAttrBy = asksMaybe XmlAttributeNotFound . L.findAttrBy

lookupAttrBy :: Monad m => (L.QName -> Bool) -> XmlMonadT [L.Attr] m String
lookupAttrBy = asksMaybe XmlAttributeNotFound . L.lookupAttrBy

showTopElement :: Monad m => XmlMonadT L.Element m String
showTopElement = asks L.showTopElement

showContent :: Monad m => XmlMonadT L.Content m String
showContent = asks L.showContent

showElement :: Monad m => XmlMonadT L.Element m String
showElement = asks L.showElement

showCData :: Monad m => XmlMonadT L.CData m String
showCData = asks L.showCData

showAttr :: Monad m => XmlMonadT L.Attr m String
showAttr = asks L.showAttr

ppTopElement :: Monad m => XmlMonadT L.Element m String
ppTopElement = asks L.ppTopElement

ppContent :: Monad m => XmlMonadT L.Content m String
ppContent = asks L.ppContent

ppElement :: Monad m => XmlMonadT L.Element m String
ppElement = asks L.ppElement

ppcTopElement :: Monad m => L.ConfigPP -> XmlMonadT L.Element m String
ppcTopElement = asks . L.ppcTopElement

ppcContent :: Monad m => L.ConfigPP -> XmlMonadT L.Content m String
ppcContent = asks . L.ppcContent

ppcElement :: Monad m => L.ConfigPP -> XmlMonadT L.Element m String
ppcElement = asks . L.ppcElement
