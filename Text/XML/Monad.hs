{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.XML.Monad
where
  
import Text.XML.Light
import Control.Applicative
import MonadLib

data ParseError
    = EmptyDocument
    | InvalidXml
    | XmlElementNotFound String
    | UnexpectedElementName String String
    deriving (Show)

newtype XmlMonadT s m a = XmlMonadT { fromXmlMonadT :: ExceptionT ParseError (ReaderT s m) a }
    deriving (Functor, Monad, Applicative)

runXmlMonadT :: s -> XmlMonadT s m a -> m (Either ParseError a)
runXmlMonadT r = runReaderT r . runExceptionT . fromXmlMonadT