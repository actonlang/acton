{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FlexibleInstances #-}
module Acton where

import Control.Monad.Except

data ST s a     = ST a

instance Functor (ST s) where
    fmap = undefined

instance Applicative (ST s) where
    pure = undefined
    (<*>) = undefined
    
instance Monad (ST s) where
    return = undefined
    (>>=) = undefined

instance MonadError String (ST s) where
    throwError = undefined
    catchError = undefined

data Msg t      = Msg t

data Var s t    = Var t

data Async a b  = Async a b

data Actor a    = Actor a




actor           :: (forall s . ST s t) -> Actor t
_new            :: Actor a -> a -> ST s a

_asyn           :: (a -> ST s b) -> Async a b
_snd            :: Async a b -> a -> ST s (Msg b)
await           :: Msg a -> ST s a

after           :: Int -> ST s a -> ST s ()

var             :: a -> ST s (Var s a)
readv           :: Var s a -> ST s a
writev          :: Var s a -> a -> ST s ()

prints          :: String -> ST s ()
pass            :: ST s ()
msg             :: a -> ST s (Msg a)


actor a         = undefined
await m         = undefined
after t p       = undefined

var e           = undefined
readv v         = undefined
writev v e      = undefined

prints s        = undefined
pass            = undefined
msg             = undefined

_asyn p         = undefined
_snd            = undefined
_new            = undefined

pingpong        :: Int -> Actor (Async Int ())
pingpong i      = actor $ do
                    count <- var 0
                    let ping q = do
                            c <- readv count
                            writev count (c+1)
                            prints ("Ping " ++ show (c*q))
                            after 1 (pong c (-q))
                        pong n q = do
                            prints ("Pong " ++ show (n*q))
                            after 2 (ping (-q))
                    ping i
                    return (_asyn ping)

session         :: Conn -> Async String () -> Actor (Async (String, Async Int ()) (), Async (String,String) (), Async () ())
session conn error_cb = actor $ do
                message_id <- var 1
                responder <- var Nothing
                let get (path, reply_cb) = do
                        m_id <- readv message_id
                        _snd (deliver conn) (print_rpc m_id "get" (print_path path ""))
                        writev responder $ Just $ \text -> _snd reply_cb (xml_parse $ strip_path path $ strip_reply m_id "data" text)
                    edit_config (path, value) = do
                        m_id <- readv message_id
                        _snd (deliver conn) (print_rpc m_id "edit-config" (print_path path $ xml_print value))
                        writev responder $ Just $ \text -> msg $ xml_parse_empty (strip_reply m_id "ok" text)
                    abort () = do
                        _snd (close conn) ()
                        return ()
                    _receive text = do
                        r <- readv responder
                        case r of
                          Just resp -> resp text `catchError` \ex -> _snd error_cb (show ex)
                          _ -> undefined
                        m_id <- readv message_id
                        writev message_id (m_id + 1)
                    _error what = do
                        _snd error_cb what
                        writev responder Nothing
                _snd (receive_on conn) (_asyn _receive, _asyn _error)
                return (_asyn get, _asyn edit_config, _asyn abort)

data Conn       = Conn

deliver         :: Conn -> Async String ()
close           :: Conn -> Async () ()
receive_on      :: Conn -> Async (Async String (), Async String ()) ()


deliver         = undefined
close           = undefined
receive_on      = undefined

print_rpc       :: Int -> String -> String -> String
print_path      :: String -> String -> String
strip_reply     :: Int -> String -> String -> String
strip_path      :: String -> String -> String
xml_print       :: Show a => a -> String
xml_parse       :: Read a => String -> a
xml_parse_empty :: String -> ()

print_rpc       = undefined
print_path      = undefined
strip_reply     = undefined
strip_path      = undefined
xml_print       = undefined
xml_parse       = undefined
xml_parse_empty = undefined

