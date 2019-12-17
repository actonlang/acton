{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FlexibleInstances #-}
module ActonLevels where

import Control.Monad.Except

data ST s l a   = ST a

instance Functor (ST s l) where
    fmap = undefined

instance Applicative (ST s l) where
    pure = undefined
    (<*>) = undefined
    
instance Monad (ST s l) where
    return = undefined
    (>>=) = undefined

instance MonadError String (ST s l) where
    throwError = undefined
    catchError = undefined

data Msg l t    = Msg t

data Var s l t  = Var t             -- A Var mentions a level l simply to connect the type of methods sharing a state

data Async l a b = Async a b

data Actor a    = Actor a

class Less l l' where
    less        :: l -> l' -> ()

actor           :: (forall s . ST s l t) -> Actor t
_new            :: Actor a -> a -> ST s l a

_asyn           :: (a -> ST s l b) -> Async l a b
_snd            :: Async l' a b -> a -> ST s l (Msg l' b)
await           :: Less l l' => Msg l' a -> ST s l a

after           :: Int -> ST s l a -> ST s l ()

var             :: a -> ST s l (Var s l a)
readv           :: Var s l a -> ST s l a
writev          :: Var s l a -> a -> ST s l ()

prints          :: String -> ST s l ()
pass            :: ST s l ()
msg             :: a -> ST s l (Msg l' a)


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

pingpong        :: Int -> Actor (Async l Int ())
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

session         :: (Less l_conn l_me) => Conn l_conn -> Async l_cb String () -> Actor (Async l_me (String, Async l_cb Int ()) (), Async l_me (String,String) (), Async l_me () ())
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

data Conn l     = Conn

deliver         :: Conn l -> Async l String ()
close           :: Conn l -> Async l () ()
receive_on      :: (Less l l1, Less l l2) => Conn l -> Async l (Async l1 String (), Async l2 String ()) ()
-- The Less constraints above make no obvious sense here, they are only added for testing purposes.

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

