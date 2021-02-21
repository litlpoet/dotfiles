import           System.IO                      ( hPutStrLn )

import           XMonad

import           XMonad.Config

import           XMonad.Hooks.DynamicLog        ( PP(..)
                                                , dynamicLogWithPP
                                                , shorten
                                                , wrap
                                                , xmobarColor
                                                , xmobarPP
                                                )
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.ManageDocks       ( ToggleStruts(..)
                                                , avoidStruts
                                                , docks
                                                , docksEventHook
                                                , manageDocks
                                                )
import           XMonad.Hooks.WorkspaceHistory

import           XMonad.Util.Run                ( spawnPipe )
import           XMonad.Util.SpawnOnce          ( spawnOnce )

myModMask = mod4Mask

myTerminal = "alacritty"

myWorkspaces = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "]

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount where fadeAmount = 0.9

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "picom --experimental-backends &"
  spawnOnce "nitrogen --restore &"

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ docks def
    { modMask     = myModMask
    , layoutHook  = avoidStruts $ layoutHook def
    , logHook = workspaceHistoryHook <+> myLogHook <+> dynamicLogWithPP xmobarPP
                  { ppOutput          = \x -> hPutStrLn xmproc x
                  , ppCurrent         = xmobarColor "#98be65" "" . wrap "[" "]" -- Current workspace in xmobar
                  , ppVisible         = xmobarColor "#98be65" ""                -- Visible but not current workspace
                  , ppHidden          = xmobarColor "#82AAFF" "" . wrap "*" ""   -- Hidden workspaces in xmobar
                  , ppHiddenNoWindows = xmobarColor "#c792ea" ""        -- Hidden workspaces (no windows)
                  , ppTitle           = xmobarColor "#b3afc2" "" . shorten 60     -- Title of active window in xmobar
                  , ppSep             = "<fc=#666666> <fn=1>|</fn> </fc>"          -- Separators in xmobar
                  , ppUrgent          = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
                     -- , ppExtras = [windowCount]                           -- # of windows current workspace
                  , ppOrder = \(ws : l : t : ex) -> [ws, l] ++ ex ++ [t]
                  }
    , startupHook = myStartupHook
    , terminal    = myTerminal
    , workspaces  = myWorkspaces
    }
