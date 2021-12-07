-- Config file for xmobar
--
-- colorscheme:
Config { font = "xft:Ubuntu mono:pixelsize=16:antialias=true:hinting=true"
       , additionalFonts = []
       , borderColor = "black"
       , border = TopB
       , bgColor = "#344055"
       , fgColor = "#D0C6F8"
       , alpha = 255
       , position = Bottom
       , textOffset = -1
       , iconOffset = -1
       , lowerOnStart = True
       , pickBroadest = False
       , persistent = False
       , hideOnStart = False
       , iconRoot = "/home/sam/.xmonad/xpm/"
       , allDesktops = True
       , overrideRedirect = True
       , commands = [ Run Weather "KSEA" ["-t","<station>: <tempC>C",
                                          "-L","18","-H","25",
                                          "--normal","green",
                                          "--high","red",
                                          "--low","lightblue"] 36000
                    , Run Date "<fc=#FFAA46>%a</fc> <fc=#9EE493>%Y %m %d</fc> <fc=#3F88C5>%H:%M:%S</fc>" "date" 10
                    , Run Network "wlps20" ["-L","0","-H","32",
                                          "--normal","green","--high","red"] 10
                    , Run Network "eth1" ["-L","0","-H","32",
                                          "--normal","green","--high","red"] 10
                    , Run Cpu ["-L","3","-H","50",
                               "--normal","green","--high","red"] 10
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Swap [] 10
                    , Run Com "uname" ["-s","-r"] "" 36000
                    , Run Brightness ["-t", "<bar>", "-D", "/sys/class/backlight/intel_backlight"] 60
                    , Run Com "xbacklight" [] "backlight" 60
                    , Run Com "/home/sam/.xmonad/xmobar/scripts/publicIP.sh" [] "publicIP" 600
                    , Run UnsafeStdinReader -- For workspace information
                    , Run Battery [ "--template" , "<fc=#420039>Batt:</fc> <acstatus>"
                          , "--Low"      , "10"        -- units: %
                          , "--High"     , "80"        -- units: %
                          , "--low"      , "darkred"
                          , "--normal"   , "darkorange"
                          , "--high"     , "darkgreen"
                          , "--" -- battery specific options
                          , "-o"    , "<left>% (<timeleft>)" -- discharging status
                          , "-O"    , "<fc=#dAA520>Charging</fc>" -- AC "on" status
                          , "-i"    , "<fc=#006000>Charged</fc>" -- charged status
                          ] 50
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " <icon=haskell_20.xpm/> %UnsafeStdinReader% %cpu% | %memory% * %swap% | %wlps20% - %eth1% }\
                    \{ | %KSEA% \
                      \| %battery% \
                      \| %backlight% \
                      \| %date% "
       -- , template = "<icon-haskell_20.xpm/> | %UnsafeStdinReader% }{ %cpu% | %memory% %swap% | %storage% | %localIP% %globalIP% %networktraffic% | %battery% | %sound% | %date%
       }
