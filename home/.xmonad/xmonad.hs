import XMonad

-- Screen order
import qualified Data.Map as M
import XMonad.Actions.PhysicalScreens

-- Docks
import XMonad.Hooks.ManageDocks(manageDocks, avoidStruts, docksEventHook)
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.DynamicLog
import XMonad.Util.WorkspaceCompare(getSortByXineramaRule)

-- Layout
import XMonad.Layout.NoBorders(noBorders)
import XMonad.Layout.LayoutHints(layoutHintsWithPlacement)

-- Hotkeys
import XMonad.Util.EZConfig(additionalKeys)

-- Desktop configuration
import XMonad.Config.Desktop

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

myLayout = avoidStruts $ (hintedTiled ||| noBorders Full)
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
  xmonad $ desktopConfig
    { manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
    , layoutHook =  myLayout
    , handleEventHook = docksEventHook
    , logHook = dynamicLogWithPP myXmobarPP { ppOutput = hPutStrLn xmproc }
    , terminal = "st -e tmux"
    , modMask = mod4Mask
    , borderWidth = myBorderWidth
    , normalBorderColor = sClrMagenta
    , focusedBorderColor = sClrOrange
    , keys = psKeys <+> keys defaultConfig
    }
    `additionalKeys`
    [ ((mod4Mask, xK_p), spawn "exe=`dmenu_run` && eval \"exec $exe\"")
    , ((0, xK_Escape), spawn "slock")
    , ((mod4Mask, xK_c), spawn "google-chrome-stable")
    ]

myManageHook = composeAll
  [ className =? "openarena" --> doFloat ]

--
-- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
-- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
--
psKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList
  [((modm .|. mask, key), f sc)
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
      , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]
