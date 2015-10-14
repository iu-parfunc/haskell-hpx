{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers #-}
module Main (main) where

import Control.Exception

import Data.Binary
import Data.Typeable.Internal

import GHC.Fingerprint
import GHC.Generics
import GHC.StaticPtr

-------------------------------------------------------------------------------
-- Backported Binary Fingerprint instance

instance Binary Fingerprint where
    put (Fingerprint x1 x2) = do
        put x1
        put x2
    get = do
        x1 <- get
        x2 <- get
        return $! Fingerprint x1 x2

-------------------------------------------------------------------------------
-- Inspired by the concrete-typerep package, which has become out-of-date
newtype StaticTypeRep = STR { unSTR :: TypeRep }
  deriving (Eq, Ord)

instance Show StaticTypeRep where
  showsPrec p = showsPrec p . unSTR

------------- serialization: this uses Gökhan San's construction, from
---- http://www.mail-archive.com/haskell-cafe@haskell.org/msg41134.html
type TyConRep = (String, String, String)

toTyConRep :: TyCon -> TyConRep
toTyConRep (TyCon _ pack mod name) = (pack, mod, name)

fromTyConRep :: TyConRep -> TyCon
fromTyConRep (pack, mod, name) = mkTyCon3 pack mod name

newtype SerialRep = SR (TyConRep, [SerialRep]) deriving Binary

toSerial :: StaticTypeRep -> SerialRep
toSerial (STR t) =
  case splitTyConApp t of
    (con, args) -> SR (toTyConRep con, map (toSerial . STR) args)

fromSerial :: SerialRep -> StaticTypeRep
fromSerial (SR (con, args)) = STR $ mkTyConApp (fromTyConRep con)
                                               (map (unSTR . fromSerial) args)

instance Binary StaticTypeRep where
  put = put . toSerial
  get = fromSerial <$> get

-------------------------------------------------------------------------------
-- StaticRep

data StaticRep = StaticRep !StaticKey !StaticTypeRep deriving Generic

instance Binary StaticRep

instance Eq StaticRep where
  StaticRep k1 _ == StaticRep k2 _ = k1 == k2

instance Ord StaticRep where
  StaticRep k1 _ <= StaticRep k2 _ = k1 <= k2

instance Show StaticRep where
  showsPrec p (StaticRep _ tr) = showsPrec p tr

data StaticLookupException
 = WrongType
 | NotInTable
 deriving (Eq, Show)
instance Exception StaticLookupException

staticRep :: forall a. Typeable a => StaticPtr a -> StaticRep
staticRep sp = StaticRep (staticKey sp) (STR $ typeRep (Proxy :: Proxy a))

lookupStaticPtr :: forall a. Typeable a
                => StaticRep -> IO (Either StaticLookupException (StaticPtr a))
lookupStaticPtr (StaticRep sk (STR tr)) =
  if typeRep (Proxy :: Proxy a) == tr
     then do mbSP <- unsafeLookupStaticPtr sk
             return $ case mbSP of
                 Nothing -> Left NotInTable
                 Just sp -> Right sp
     else return $ Left WrongType

putStrLnSP :: StaticPtr (String -> IO ())
putStrLnSP = static putStrLn

main :: IO ()
main = do
  let rep :: StaticRep
      rep = staticRep putStrLnSP

      runRep :: Typeable a => StaticRep -> a -> IO ()
      runRep rep a = do
          eSP <- lookupStaticPtr rep
          either print (\sp -> deRefStaticPtr sp a) eSP

  -- Correct
  runRep rep "Hello, World!"

  -- Wrong type
  runRep rep 'a'

  -- Not in static pointer table
  runRep (StaticRep fingerprint0 . STR $ typeRep (Proxy :: Proxy (String -> IO ()))) "Hello, World!"

  -- Encode, decode, then lookup
  runRep (decode $ encode rep) "Hello, World!"
