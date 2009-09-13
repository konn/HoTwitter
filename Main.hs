module Main where

import Cocoa hiding (main)
import TwitterController
import StatusDataSource

import AppKit.NSApplication(nsApplicationMain_)

main = do   
    initializeClass_TwitterController
    initializeClass_StatusDataSource
    nsApplicationMain_
