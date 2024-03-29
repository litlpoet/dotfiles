import XMonad
import XMonad.Config
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.WorkspaceHistory
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

main = do
  bar <- spawnPipe "xmobar"
  xmonad $
    docks $
      ewmh
        def
          { borderWidth = 2,
            handleEventHook = myHandleEventHook,
            layoutHook = myLayout,
            logHook = myLogHook bar,
            manageHook = myManageHook,
            modMask = mod4Mask,
            startupHook = myStartupHook,
            terminal = "alacritty",
            workspaces = myWorkspaces,
            normalBorderColor = myColBg,
            focusedBorderColor = myColH3
          }
        `additionalKeysP` myKeys

myHandleEventHook = fullscreenEventHook

myLayout = avoidStruts tiled ||| noBorders Full
  where
    tiled = spacingHelper 45 15 $ Tall n_main delta ratio
    n_main = 1
    delta = 1 / 200
    ratio = 4 / 7

myLogHook bar =
  workspaceHistoryHook <+> myBarPP bar <+> fadeInactiveLogHook fadeAmount
  where
    fadeAmount = 1.0

myManageHook = namedScratchpadManageHook myScratchPads

myStartupHook = do
  spawnOnce "lxsession &"
  spawnOnce "picom --experimental-backends &"
  spawnOnce "nitrogen --restore &"
  spawnOnce "stalonetray &"

myWorkspaces = ["1", "2", "3"]

myBarPP bar =
  dynamicLogWithPP $
    namedScratchpadFilterOutWorkspacePP $
      xmobarPP
        { ppCurrent = xmobarColor myColH1 "" . wrap "[" "]",
          ppVisible = xmobarColor myColH2 "" . wrap " " " ",
          ppHidden = xmobarColor myColFg "" . wrap "*" " ",
          ppHiddenNoWindows = xmobarColor myColDm "" . wrap " " " ",
          ppUrgent = xmobarColor myColH3 "" . wrap "!" "!",
          ppSep = xmobarColor myColDm "" " : ",
          ppWsSep = "",
          ppTitle = xmobarColor myColFg "" . wrap "<fn=1>" "</fn>" . shorten 60,
          ppOrder = \(ws : layout : t : _) -> [ws, layout, t],
          ppOutput = hPutStrLn bar
        }

myKeys =
  [ (("M-p"), spawn "rofi -show drun"),
    (("M-["), decWindowSpacing 3),
    (("M-]"), incWindowSpacing 3),
    (("M-S-[", decScreenSpacing 3)),
    (("M-S-]", incScreenSpacing 3)),
    (("M-s"), namedScratchpadAction myScratchPads "term"),
    (("M-o"), namedScratchpadAction myScratchPads "htop"),
    (("M-f"), namedScratchpadAction myScratchPads "lf")
  ]
    ++ [ (m ++ "M-" ++ [k], act tag)
         | (tag, k) <- zip myWorkspaces "123",
           (m, act) <- [("", windows . W.view), ("S-", windows . W.shift)]
       ]

myScratchPads =
  [ NS "term" spawnTerm findTerm manageTerm,
    NS "htop" spawnHtop findHtop manageHtop,
    NS "lf" spawnLf findLf manageLf
  ]
  where
    spawnTerm = "alacritty -t scratchpad"
    findTerm = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
    spawnHtop = "alacritty -t htop -e htop"
    findHtop = title =? "htop"
    manageHtop = customFloating $ W.RationalRect l t w h
    spawnLf = "alacritty -t lf -e lf"
    findLf = title =? "lf"
    manageLf = customFloating $ W.RationalRect l t w h
    h = 0.6
    w = 0.6
    t = (1.0 - h) / 1.1
    l = (1.0 - w) / 2

spacingHelper o i =
  spacingRaw False (Border o o o o) True (Border i i i i) True

myColFg = "#c5c6c7"

myColBg = "#0b0c10"

myColDm = "#1f2833"

myColH1 = "#66fcf1"

myColH2 = "#45a29e"

myColH3 = "#ee4c7c"
