import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.WorkspaceCompare
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutHints
import qualified Data.Map as M
import System.IO

myBorderWidth = 1

sClrDark       = "#1b1d1e"
sClrRed        = "#f92672"
sClrGreen      = "#a6e22e"
sClrOrange     = "#fd971f"
sClrBlue       = "#66d9ef"
sClrMagenta    = "#ae81ff"
sClrCyan       = "#a1efe4"
sClrWhite      = "#f8f8f2"

mkEmptyString :: String -> String
mkEmptyString str = ""

myLayout = avoidStruts (hintedTiled ||| smartFull)
  where
    hintedTiled = layoutHintsWithPlacement (0.5, 0.5) (Tall 1 (3/100) (2/3))
    smartFull = smartBorders Full

myXmobarPP :: PP
myXmobarPP = defaultPP { ppCurrent = xmobarColor sClrOrange "" . wrap "[" "]"
                       , ppHidden  = xmobarColor sClrMagenta "" . wrap " " ""
                       , ppVisible = xmobarColor sClrMagenta "" . wrap "(" ")"
                       , ppUrgent  = xmobarColor sClrRed ""
                       , ppLayout  = (\_ -> [])
                       , ppTitle   = xmobarColor sClrGreen "" . shorten 40
                       , ppSep     = " "
                       , ppWsSep   = ""
                       , ppSort    = getSortByXineramaRule
                       }

main = do
  xmproc <- spawnPipe "/home/siler/.cabal/bin/xmobar /home/siler/.xmonad/xmobar.hs"
  xmonad $ defaultConfig
    { manageHook = manageDocks <+> manageHook defaultConfig
    , layoutHook =  myLayout
    , logHook = dynamicLogWithPP myXmobarPP { ppOutput = hPutStrLn xmproc }
    , terminal = "st -e tmux"
    , modMask = mod4Mask
    , borderWidth = myBorderWidth
    , normalBorderColor = sClrMagenta
    , focusedBorderColor = sClrOrange
    }
    `additionalKeys`
    [ ((mod4Mask, xK_p), spawn "exe=`dmenu_run` && eval \"exec $exe\"")
    , ((0, xK_Escape), spawn "slock")
    , ((mod4Mask, xK_c), spawn "chromium")
    ]
