import           XMonad
import           XMonad.Config
import           XMonad.Hooks.ManageDocks
import           XMonad.Util.Run                ( spawnPipe )

myModMask = mod4Mask

myTerminal = "alacritty"

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ docks def { modMask    = myModMask
                     , terminal   = myTerminal
                     , layoutHook = avoidStruts $ layoutHook def
                     }
