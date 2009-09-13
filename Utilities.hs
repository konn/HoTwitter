{-# LANGUAGE TemplateHaskell, TypeSynonymInstances,
             FlexibleInstances, MultiParamTypeClasses,
             DeriveDataTypeable, ExistentialQuantification #-}
module Utilities where
import Control.Monad
import HOC 
import Cocoa hiding (request, toNSString)
import Foundation.NSUserDefaults

toNSString str = _NSString # stringWithHaskellString str

sharedApp = _NSApplication # sharedApplication
userDefaults :: IO (NSUserDefaults ())
userDefaults = _NSUserDefaults # standardUserDefaults
getDefault key = do k <- toNSString key
                    obj <- userDefaults >>= stringForKey k
                    if obj == nil then _NSString # stringWithHaskellString ""
                                  else return obj

setDefault key value = do
  k <- toNSString key
  userDefaults >>= setObjectForKey k value

userName = getDefault "user" >>= haskellString
passWord = getDefault "pass" >>= haskellString
interval :: IO Float
interval = do ud <- userDefaults
              key <- toNSString "interval"
              ud # floatForKey key 