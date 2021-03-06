Config { font = "xft:Inconsolata:pixelsize=13:antialias=true:autohint=false"
       , bgColor = "#121212"
       , fgColor = "#ae81ff"
       , position = Top
       , lowerOnStart = True
       , commands = [ Run Memory ["-t", "<usedratio>% (<used>M)"] 100
                    , Run Wireless "wlan0" [] 100
                    , Run BatteryP ["BAT1"]
                                   [ "-t", "<acstatus>"
                                   , "--"
                                   , "-O", "Charging: <timeleft>"
                                   , "-o", "Discharging: <timeleft>"
                                   , "-i", ""
                                   , "-f", "ACAD/online"
                                   ]
                                   100
                    , Run Com "date" ["+%m/%d %H:%M"] "datetime" 600
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , allDesktops = True
       , template = " %StdinReader% }{ %wlan0wi% : %memory% : %battery% : %datetime% "
       }
