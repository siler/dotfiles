import XMonad

-- Docks
import XMonad.Hooks.ManageDocks(manageDocks, avoidStruts, docksEventHook)
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.DynamicLog
import XMonad.Util.WorkspaceCompare(getSortByXineramaRule)

-- Layout
import XMonad.Layout.NoBorders(smartBorders, noBorders)
import XMonad.Layout.LayoutHints(layoutHintsWithPlacement)

-- Hotkeys
import XMonad.Util.EZConfig(additionalKeys)


import System.IO -- hPutStrLn

myBorderWidth = 1

sClrDark       = "#1b1d1e"
sClrRed        = "#f92672"
sClrGreen      = "#a6e22e"
sClrOrange     = "#fd971f"
sClrBlue       = "#66d9ef"
sClrMagenta    = "#ae81ff"
sClrCyan       = "#a1efe4"
sClrWhite      = "#f8f8f2"

myLayout = avoidStruts $ (smartBorders hintedTiled ||| noBorders Full)
  where
    hintedTiled = layoutHintsWithPlacement (0.5, 0.5) (Tall 1 (3/100) (2/3))

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
    , handleEventHook = docksEventHook
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
    , ((mod4Mask, xK_c), spawn "google-chrome-beta")
    ]
