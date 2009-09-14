{-# LANGUAGE TemplateHaskell, TypeSynonymInstances,
             FlexibleInstances, MultiParamTypeClasses,
             DeriveDataTypeable, ExistentialQuantification #-}
module Utilities where
import Control.Monad
import HOC 
import Cocoa hiding (request)
import Foundation.NSUserDefaults

sharedApp = _NSApplication # sharedApplication
userDefaults :: IO (NSUserDefaults ())
userDefaults = _NSUserDefaults # standardUserDefaults
getDefault key = do obj <- userDefaults >>= stringForKey (toNSString key)
                    if obj == nil then _NSString # stringWithHaskellString ""
                                  else return obj

setDefault key value = do
  userDefaults >>= setObjectForKey (toNSString key) value

userName = getDefault "user" >>= haskellString
passWord = getDefault "pass" >>= haskellString
interval :: IO Float
interval = userDefaults >>= floatForKey (toNSString "interval") 