module Text.XML.Monad.Error
(
  -- * Error types
  XmlError(..)
, FromXmlError(..)
  -- * Error handling
, maybeRaise
, asksEither
, asksMaybe
, tryMaybe
, tryBool
)
where

import           MonadLib
import qualified Text.XML.Light as L
  
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
    deriving (Show)

-- | An error type that can be constructed from 'XmlError'.
class FromXmlError a where
    -- | Construct error value.
    fromXmlError :: XmlError -> a

instance FromXmlError XmlError where
    fromXmlError = id

-- | Raise a defined exception for 'Nothing', return 'Just' values.
maybeRaise :: ExceptionM m i => i -> Maybe a -> m a
maybeRaise err Nothing  = raise err
maybeRaise _   (Just x) = return x

-- | Like 'asks' for a function that can return an error, as 'Left'.
asksEither :: (ReaderM m s, ExceptionM m e) => (s -> Either e a) -> m a
asksEither f = ask >>= raises . f

-- | Like 'asks' for a function that can return an error, as 'Nothing'.
asksMaybe :: (ReaderM m s, ExceptionM m e) => e -> (s -> Maybe a) -> m a
asksMaybe err f = ask >>= maybeRaise err . f

-- | Catch errors (like 'try'), and return 'Nothing' for errors.
tryMaybe :: RunExceptionM m i => m a -> m (Maybe a)
tryMaybe a = either (const Nothing) Just `liftM` try a

-- | Catch errors (like 'try'), and return 'False' for errors and 'True' for success.
tryBool :: RunExceptionM m i => m a -> m Bool
tryBool a = either (const False) (const True) `liftM` try a
