{-# LANGUAGE TemplateHaskell, TypeSynonymInstances,
             FlexibleInstances, MultiParamTypeClasses,
             DeriveDataTypeable, ExistentialQuantification #-}
module TwitterController where
import Control.Monad
import HOC 
import Cocoa hiding (request, toNSString)
import Foreign.Ptr (nullPtr, Ptr(..))
import Codec.Binary.UTF8.String
import Prelude hiding (init)
import Control.Concurrent

import Utilities
import TwitterClient
import StatusDataSource


$(declareClass "TwitterController" "NSObject")

$(declareSelector "cancelPref:" [t| forall a. NSButton a -> IO () |])
$(declareSelector "donePref:" [t| forall a. NSButton a -> IO () |])
$(declareSelector "post:" [t| forall a. NSTextField a -> IO () |])
$(declareSelector "showPref:" [t| forall a. NSButton a -> IO () |])
$(declareSelector "sheetDidEnd:returnCode:contextInfo:" [t| forall a. NSWindow () -> Int -> Ptr () -> IO () |])
$(declareSelector "reloadTimeline:" [t| forall a. NSButton a -> IO () |])
$(exportClass "TwitterController" "tc_" [
    Outlet "passField" [t| NSTextField () |],
    Outlet "userField" [t| NSTextField () |],
    Outlet "intervalField" [t| NSTextField () |],
    Outlet "tweetField" [t| NSTextField () |],
    Outlet "prefWindow" [t| NSWindow () |],
    Outlet "mainWindow" [t| NSWindow () |],
    Outlet "statusDataSource" [t| StatusDataSource () |],
    
    InstanceVariable "user" [t| String |] [| "" |],
    InstanceVariable "pass" [t| String |] [| "" |],
    InstanceVariable "interval" [t| Float |] [| 0 |],
    InstanceVariable "tweets" [t| [TwitterStatus] |] [| [] |],
    
    InstanceMethod 'cancelPref,
    InstanceMethod 'donePref,
    InstanceMethod 'post,
    InstanceMethod 'showPref,
    InstanceMethod 'sheetDidEndReturnCodeContextInfo,
    InstanceMethod 'awakeFromNib,
    InstanceMethod 'reloadTimeline
  ])

instance Has_reloadTimeline (TwitterController ())

tc_awakeFromNib self = do
  user <- userName
  pass <- passWord
  inte <- interval
  self # setIVar _user user
  self # setIVar _pass pass
  self # setIVar _interval inte

tc_showPref sender self = do
  app <- _NSApplication # sharedApplication
  mainW <- self #. _mainWindow
  prfw <- self #. _prefWindow
  app # beginSheetModalForWindowModalDelegateDidEndSelectorContextInfo prfw mainW self (getSelectorForName "sheetDidEnd:returnCode:contextInfo:") nullPtr

closePrefWithCode code self = do
  app <- _NSApplication # sharedApplication
  prfw <- (self #. _prefWindow)
  app # endSheetReturnCode prfw code

tc_cancelPref sender self = self # closePrefWithCode 0
tc_donePref sender self = self # closePrefWithCode 1

tc_sheetDidEndReturnCodeContextInfo sheet 1 info self = do
  us <- userName
  ps <- passWord
  int <- interval
  self # setIVar _pass ps
  self # setIVar _user us
  self # setIVar _interval int
  sheet # orderOut self
  self # reloadTimeline nil

tc_sheetDidEndReturnCodeContextInfo sheet 0 info self = do
  user <- self #. _user
  pass <- self #. _pass
  int  <- self #. _interval
  us <- toNSString user
  ps <- toNSString pass
  self #. _userField >>= setStringValue us
  self #. _passField >>= setStringValue ps
  self #. _intervalField >>= setFloatValue int
  setDefault "user" us
  setDefault "pass" ps
  du <- userDefaults
  k <- toNSString "interval"
  du # setFloatForKey int k
  sheet # orderOut self

tc_post sender self = do
  pass <- passWord
  user <- userName
  msg <- sender # stringValue >>= haskellString
  str <- stringWithHaskellString "" _NSString
  unless (Prelude.null pass && Prelude.null user) $ (forkOS $ withAutoreleasePool $ do 
    sender # setStringValue str
    tweet user pass $ encodeString msg
    return ()) >> return ()

tc_reloadTimeline sender self = do
  ds <- self #. _statusDataSource
  ds # stopLoop
  ds # startLoop