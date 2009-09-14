{-# LANGUAGE TemplateHaskell, TypeSynonymInstances,
             FlexibleInstances, MultiParamTypeClasses,
             DeriveDataTypeable, ExistentialQuantification #-}
module TwitterController where
import Control.Monad
import HOC 
import Cocoa hiding (request)
import AppKit.NSUserDefaultsController
import Foreign.Ptr (nullPtr, Ptr(..))
import Codec.Binary.UTF8.String
import Prelude hiding (init)
import Control.Concurrent

import Selectors
import Utilities
import TwitterClient
import StatusDataSource


$(declareClass "TwitterController" "NSObject")
$(exportClass "TwitterController" "tc_" [
    Outlet "passField" [t| NSTextField () |],
    Outlet "userField" [t| NSTextField () |],
    Outlet "intervalField" [t| NSTextField () |],
    Outlet "tweetField" [t| NSTextField () |],
    Outlet "prefWindow" [t| NSWindow () |],
    Outlet "mainWindow" [t| NSWindow () |],
    Outlet "statusDataSource" [t| StatusDataSource () |],
    Outlet "userDefaultController" [t| NSUserDefaultsController () |],
    
    InstanceVariable "interval" [t| Float |] [| 0 |],
    InstanceVariable "tweets" [t| [TwitterStatus] |] [| [] |],
    
    InstanceMethod 'cancelPref,
    InstanceMethod 'donePref,
    InstanceMethod 'post,
    InstanceMethod 'showPref,
    InstanceMethod 'sheetDidEndReturnCodeContextInfo,
    InstanceMethod 'reloadTimeline
  ])

instance Has_reloadTimeline (TwitterController ())

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
  self #. _userDefaultController >>= save self
  sheet # orderOut self
  self # reloadTimeline nil

tc_sheetDidEndReturnCodeContextInfo sheet 0 info self = do
  self #. _userDefaultController >>= revert self
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