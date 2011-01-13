{-# LANGUAGE DeriveDataTypeable #-}
module Text.XML.Monad.Error
(
  -- * Error types
  XmlError(..)
, FromXmlError(..)
  -- * Error handling
, raise
, raiseXml
, maybeRaise
, maybeRaiseXml
, raises
, raisesXml
, asksEither
, asksEitherXml
, asksMaybe
, asksMaybeXml
, try
, tryMaybe
, tryBool
)
where

import           Control.Monad.Error
import           Control.Monad.Reader
import           Data.Typeable
import qualified Control.Exception    as C
import qualified Text.XML.Light       as L
  
-- | XML error type.
data XmlError
    = EmptyDocument                           -- ^ An (invalid) empty input document was observed.
    | InvalidXml                              -- ^ Invalid XML, general parse error.
    | XmlChildNotFound                        -- ^ An immediate child element in an XML tree was not found.
    | XmlChildNotFoundQ L.QName               -- ^ An immediate child element in an XML tree was not found, with name.
    | XmlElementNotFound                      -- ^ An element in an XML tree was not found.
    | XmlElementNotFoundQ L.QName             -- ^ An element in an XML tree was not found, with name.
    | XmlAttributeNotFound                    -- ^ An XML element attribute was not found.
    | XmlAttributeNotFoundQ L.QName           -- ^ An XML element attribute was not found, with name.
    | UnexpectedElementNameQ L.QName L.QName  -- ^ An XML element name was different than expected, with actual and expected names.
    | XmlError String                         -- ^ A general XML error occured.
    | OtherError String                       -- ^ A general error occured.
    | UnspecifiedError                        -- ^ An unspecified general error occured.
    deriving (Show, Typeable)

instance Error XmlError where
    noMsg = UnspecifiedError
    strMsg = OtherError

instance C.Exception XmlError

-- | An error type that can be constructed from 'XmlError'.
class FromXmlError a where
    -- | Construct error value.
    fromXmlError :: XmlError -> a

instance FromXmlError XmlError where
    fromXmlError = id

-- | Raise a defined exception for 'Nothing', return 'Just' values.
maybeRaise :: MonadError i m => i -> Maybe a -> m a
maybeRaise err Nothing  = throwError err
maybeRaise _   (Just x) = return x

-- | Raise a defined XML exception for 'Nothing', return 'Just' values.
maybeRaiseXml :: (MonadError i m, FromXmlError i) => XmlError -> Maybe a -> m a
maybeRaiseXml = maybeRaise . fromXmlError

-- | Raise an exception.
raise :: MonadError i m => i -> m a
raise = throwError

-- | Raise an XML exception.
raiseXml :: (MonadError i m, FromXmlError i) => XmlError -> m a
raiseXml = raise . fromXmlError

-- | Raise an exception for 'Left', return 'Right' values.
raises :: MonadError i m => Either i a -> m a
raises (Left err) = throwError err
raises (Right x)  = return x

-- | Raise an exception for 'Left', return 'Right' values.
raisesXml :: (MonadError i m, FromXmlError i) => Either XmlError a -> m a
raisesXml (Left err) = raiseXml err
raisesXml (Right x)  = return x

-- | Like 'asks' for a function that can return an error, as 'Left'.
asksEither :: (MonadReader s m, MonadError e m) => (s -> Either e a) -> m a
asksEither f = ask >>= raises . f

-- | Like 'asks' for a function that can return an XML error, as 'Left'.
asksEitherXml :: (MonadReader s m, MonadError e m, FromXmlError e) => (s -> Either XmlError a) -> m a
asksEitherXml f = ask >>= raisesXml . f

-- | Like 'asks' for a function that can return an error, as 'Nothing'.
asksMaybe :: (MonadReader s m, MonadError e m) => e -> (s -> Maybe a) -> m a
asksMaybe err f = ask >>= maybeRaise err . f

-- | Like 'asks' for a function that can return an error, as 'Nothing'.
asksMaybeXml :: (MonadReader s m, MonadError e m, FromXmlError e) => XmlError -> (s -> Maybe a) -> m a
asksMaybeXml err f = ask >>= maybeRaiseXml err . f

-- | Catch errors, and return an 'Left' for errors, 'Right' otherwise.
try :: MonadError e m => m a -> m (Either e a)
try m = catchError (liftM Right m) (return . Left)

-- | Catch errors (like 'try'), and return 'Nothing' for errors.
tryMaybe :: MonadError e m => m a -> m (Maybe a)
tryMaybe a = either (const Nothing) Just `liftM` try a

-- | Catch errors (like 'try'), and return 'False' for errors and 'True' for success.
tryBool :: MonadError e m => m a -> m Bool
tryBool a = either (const False) (const True) `liftM` try a
