import           XMonad
import           XMonad.Config
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.WorkspaceHistory
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spacing
import qualified XMonad.StackSet               as W
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run
import           XMonad.Util.SpawnOnce

main = do
  bar <- spawnPipe "xmobar"
  xmonad
    $                 docks
    $                 ewmh def { handleEventHook    = myHandleEventHook
                               , layoutHook         = myLayout
                               , logHook            = myLogHook bar
                               , manageHook         = myManageHook
                               , modMask            = mod4Mask
                               , startupHook        = myStartupHook
                               , terminal           = "alacritty"
                               , workspaces         = myWorkspaces
                               , normalBorderColor  = "#333333"
                               , focusedBorderColor = "#778899"
                               }
    `additionalKeysP` myKeys

myHandleEventHook = fullscreenEventHook

myLayout = avoidStruts tiled ||| noBorders Full
 where
  tiled  = spacingHelper 45 15 $ Tall n_main delta ratio
  n_main = 1
  delta  = 1 / 200
  ratio  = 4 / 7

myLogHook bar =
  workspaceHistoryHook
    <+> myBarPP bar
    <+> fadeInactiveCurrentWSLogHook fadeAmount
  where fadeAmount = 0.90

myManageHook = namedScratchpadManageHook myScratchPads

myStartupHook = do
  spawnOnce "picom --experimental-backends --backend glx --xrender-sync-fence &"
  spawnOnce "nitrogen --restore &"

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

myBarPP bar = dynamicLogWithPP $ namedScratchpadFilterOutWorkspacePP $ xmobarPP
  { ppCurrent         = xmobarColor "#98be65" "" . wrap "[" "]" -- Current workspace in xmobar
  , ppVisible         = xmobarColor "#98be65" "" . wrap " " " " -- Visible but not current workspace
  , ppHidden          = xmobarColor "#82AAFF" "" . wrap "*" " " -- Hidden workspaces in xmobar
  , ppHiddenNoWindows = xmobarColor "#c792ea" "" . wrap " " " " -- Hidden workspaces (no windows)
  , ppUrgent          = xmobarColor "#C45500" "" . wrap "!" "!" -- Urgent workspace
  , ppSep             = "<fc=#666666> <fn=1>|</fn> </fc>"       -- Separators in xmobar
  , ppTitle           = xmobarColor "#b3afc2" "" . shorten 60   -- Title of active window in xmobar
  , ppOrder           = \(ws : l : t : ex) -> [ws, l] ++ ex ++ [t]
                     -- , ppExtras = [windowCount]                               -- # of windows current workspace
  , ppOutput          = hPutStrLn bar
  }

myKeys =
  [ (("M-p"), spawn "rofi -show drun")
    , (("M-["), decWindowSpacing 3)
    , (("M-]"), incWindowSpacing 3)
    , (("M-S-[", decScreenSpacing 3))
    , (("M-S-]", incScreenSpacing 3))
    , (("M-s"), namedScratchpadAction myScratchPads "term")
    , (("M-o"), namedScratchpadAction myScratchPads "htop")
    , (("M-f"), namedScratchpadAction myScratchPads "lf")
    ]
    ++ [ (m ++ "M-" ++ [k], act tag)
       | (tag, k  ) <- zip myWorkspaces "123456789"
       , (m  , act) <- [("", windows . W.view), ("S-", windows . W.shift)]
       ]

myScratchPads =
  [ NS "term" spawnTerm findTerm manageTerm
  , NS "htop" spawnHtop findHtop manageHtop
  , NS "lf"   spawnLf   findLf   manageLf
  ]
 where
  spawnTerm  = "alacritty -t scratchpad"
  findTerm   = title =? "scratchpad"
  manageTerm = customFloating $ W.RationalRect l t w h
  spawnHtop  = "alacritty -t htop -e htop"
  findHtop   = title =? "htop"
  manageHtop = customFloating $ W.RationalRect l t w h
  spawnLf    = "alacritty -t lf -e lf"
  findLf     = title =? "lf"
  manageLf   = customFloating $ W.RationalRect l t w h
  h          = 0.6
  w          = 0.85
  t          = (1.0 - h) / 1.1
  l          = (1.0 - w) / 2

spacingHelper o i =
  spacingRaw False (Border o o o o) True (Border i i i i) True
