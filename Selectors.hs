{-# LANGUAGE TemplateHaskell, TypeSynonymInstances,
             FlexibleInstances, MultiParamTypeClasses,
             DeriveDataTypeable, ExistentialQuantification #-}
module Selectors where
import HOC 
import Cocoa
import Foreign.Ptr

$(declareSelector "checkForUpdates" [t| IO () |])
$(declareSelector "startLoop" [t| IO () |])
$(declareSelector "stopLoop" [t| IO () |])
$(declareSelector "cancelPref:" [t| forall a. NSButton a -> IO () |])
$(declareSelector "donePref:" [t| forall a. NSButton a -> IO () |])
$(declareSelector "post:" [t| forall a. NSTextField a -> IO () |])
$(declareSelector "showPref:" [t| forall a. NSButton a -> IO () |])
$(declareSelector "sheetDidEnd:returnCode:contextInfo:" [t| forall a. NSWindow a -> Int -> Ptr () -> IO () |])
$(declareSelector "reloadTimeline:" [t| forall a. NSButton a -> IO () |])