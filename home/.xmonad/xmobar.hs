Config { font = "xft:Inconsolata:pixelsize=13:antialias=true:autohint=false"
       , bgColor = "#121212"
       , fgColor = "#ae81ff"
       , position = Top
       , lowerOnStart = True
       , commands = [ Run Date "%a %b %_d %l:%M" "date" 10
                    , Run Com "conky" ["-q", "-i", "1"] "conky" 10
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , allDesktops = True
       , template = "%StdinReader% }{ %conky% "
       }
