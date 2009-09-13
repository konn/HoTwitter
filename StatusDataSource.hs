{-# LANGUAGE TemplateHaskell, TypeSynonymInstances,
             FlexibleInstances, MultiParamTypeClasses,
             DeriveDataTypeable, ExistentialQuantification #-}
module StatusDataSource where
import HOC 
import Cocoa hiding (request, toNSString, user, status)
import AppKit.NSTableColumn(identifier)

import Control.Concurrent
import Control.Monad
import Data.Maybe

import Utilities
import TwitterClient

$(declareClass "StatusDataSource" "NSObject")

$(declareSelector "checkForUpdates" [t| IO () |])
$(declareSelector "startLoop" [t| IO () |])
$(declareSelector "stopLoop" [t| IO () |])

$(exportClass "StatusDataSource" "sds_" [
    InstanceVariable "statuses" [t| [TwitterStatus] |] [| [] |],
    InstanceVariable "timerID" [t| ThreadId |] [| undefined |],
    
    Outlet "msgView" [t| NSTableView () |],
    
    InstanceMethod 'awakeFromNib,
    InstanceMethod 'numberOfRowsInTableView,
    InstanceMethod 'tableViewObjectValueForTableColumnRow,
    InstanceMethod 'checkForUpdates,
    InstanceMethod 'startLoop,
    InstanceMethod 'stopLoop
  ])

instance Has_checkForUpdates (StatusDataSource ())
instance Has_startLoop (StatusDataSource ())
instance Has_stopLoop (StatusDataSource ())

sds_awakeFromNib self = self # startLoop
sds_startLoop self = do tid <- forkOS $ withAutoreleasePool subFunc
                        self # setIVar _timerID tid
                        return ()
  where subFunc = do
          self # checkForUpdates
          yield
          interval >>= threadDelay . fromIntegral.floor . (*60000000)
          subFunc

sds_checkForUpdates self = do
  user <- userName
  pass <- passWord
  unless (Prelude.null user && Prelude.null pass) $ do
    old <- self #. _statuses
    let since = maybeToList $ liftM ((,)"since_id".show.statusId) $ listToMaybe old
    stats <- getFriendsTimeLine user pass since
    self # setIVar _statuses (stats++old)
  self #. _msgView >>= reloadData
  
sds_numberOfRowsInTableView tv self = do
  stats <- self #. _statuses
  return $ fromIntegral $ Prelude.length stats

sds_tableViewObjectValueForTableColumnRow tv column row self = do
  stats <- self #. _statuses
  ident <- column # identifier
  label <- haskellString $ (fromID ident :: NSString ())
  let st = stats !! (fromIntegral row)
  case label of
    "user" -> liftM toID $ toNSString $ screenName $ user st
    "tweet" -> liftM toID $ toNSString $ status st

sds_stopLoop self = do
  tid <- self #. _timerID
  killThread tid