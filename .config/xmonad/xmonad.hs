-- Andrew Michaud's XMonad config.
-- Built from bits and pieces of other configs by other people.

import XMonad

import XMonad.Actions.SpawnOn

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
-- For dealing with Java.
import XMonad.Hooks.SetWMName

import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutHints

import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe, runProcessWithInput)

import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Xlib

import Data.Char
import Data.List

import System.IO
import System.FilePath.Posix
import System.Posix.Unistd -- to get hostname
import System.Environment(getEnv)

-- For extra workspace nonsense
import qualified XMonad.StackSet as W

------------------------------------------------------------------------------------------
-----------------------------VARIABLES AND STUFF------------------------------------------
------------------------------------------------------------------------------------------

-- Color variables.
solarizedBase03  = "#002b36"
solarizedBase02  = "#073642"
solarizedBase01  = "#586e75"
solarizedBase00  = "#657b83"
solarizedBase0   = "#839496"
solarizedBase1   = "#93a1a1"
solarizedBase2   = "#eee8d5"
solarizedBase3   = "#fdf6e3"
solarizedYellow  = "#b58900"
solarizedOrange  = "#cb4b16"
solarizedRed     = "#dc322f"
solarizedMagenta = "#d33682"
solarizedViolet  = "#6c71c4"
solarizedBlue    = "#268bd2"
solarizedCyan    = "#2aa198"
solarizedGreen   = "#859900"

font :: String
font = "-*-Inconsolata-medium-r-normal-*-14-140-75-75-p-74-iso10646-1"

home :: String
home = "/home/amichaud/.xmonad"

-- Use XFCE4 terminal without menubar
myTerminal :: String
myTerminal = "xfce4-terminal --hide-menubar"

terminalCommand :: String -> String
terminalCommand cmd = myTerminal ++ " -x " ++ cmd

namedCmd :: String -> String -> String
namedCmd cmd args = myTerminal ++ " --title=" ++ name ++ " -x " ++ command
    where name = "__" ++ map toUpper cmd
          command = cmd ++ " " ++ args

-- ModMask = windowsKey
myModMask = mod4Mask

--Borders
myFocusedBorderColor :: String
myFocusedBorderColor = solarizedOrange

myNormalBorderColor :: String
myNormalBorderColor = solarizedBase1

myBorderWidth :: Dimension
myBorderWidth = 2

-- Workspace titles
myWorkspaces :: [String]
myWorkspaces = ["` dev",  "1 term", "2 web",  "3 chat", "4 ssh",
                "5 game", "6 vid",  "7 work", "8 work", "9 work",
                "0 etc",  "- perf", "= music"]

-- Workspace keys.
myKeys = [xK_grave] ++ [xK_1 .. xK_9] ++ [xK_0, xK_minus, xK_equal]

audioKeys host

    -- Laptop doesn't have proper media keys because Lenovo are dumb.
    -- So, do it manually.
    | host == "pascal" = [  ((shiftMask, xF86XK_AudioMute),        spawn "mpc toggle")
                          , ((shiftMask, xF86XK_AudioRaiseVolume), spawn "mpc next")
                          , ((shiftMask, xF86XK_AudioLowerVolume), spawn "mpc prev")] ++ common

    | host == "raven" =  [  ((0, xF86XK_AudioPlay), spawn "mpc toggle")
                          , ((0, xF86XK_AudioNext), spawn "mpc next")
                          , ((0, xF86XK_AudioPrev), spawn "mpc prev")] ++ common

    | otherwise        = common
        where common = [ ((0, xF86XK_AudioLowerVolume),         spawn "amixer set Master 2-")
                       , ((0, xF86XK_AudioRaiseVolume),         spawn "amixer set Master 2+")
                       , ((0, xF86XK_AudioMute),                spawn "amixer set Master toggle")]

------------------------------------------------------------------------------------------
-----------------------------STATUS BAR STUFF---------------------------------------------
------------------------------------------------------------------------------------------

-- Variables governing dimensions.
-- Status bar height
statusHeight :: Int
statusHeight = 16

-- Trayer width
trayerWidth :: Int
trayerWidth = 50

-- Left Dzen - xmonad info and window title.
dzenLeft :: Int -> String
dzenLeft width = "dzen2 -x '0' -w '" ++ show width ++ "' -ta 'l'" ++ dzenStyle


-- Right Dzen - Runs conky config.
dzenRight :: Int -> Int -> String -> String
dzenRight start width config = conkyCmd ++ " | dzen2 " ++ style
    where conkyCmd = "conky --config=" ++ config ++ "/conky/dzen_config"
          style = "-x '" ++ show start ++ "' -w '" ++ show width ++ "' -ta 'r'" ++ dzenStyle

-- Background status.
conkyStatus :: Int -> Int -> String -> String
conkyStatus x y config = "conky -x " ++ show x ++ " -y " ++ show y ++ " --config=" ++ config ++ "/conky/sysdepconfig"

-- Trayer - system tray.
myTrayer :: String
myTrayer = "trayer --edge top --align right " ++
           "--widthtype pixel --width " ++ show trayerWidth ++ " " ++
           "--expand true --SetDockType true --SetPartialStrut true " ++
           "--transparent false --heighttype pixel --height " ++ show statusHeight ++ " --padding 2"

-- Bitmaps used to represent current layout.
myBitmapsPath :: String
myBitmapsPath = home ++ "/dzen/bitmaps/"

normalFG :: String
normalFG = solarizedBase3

normalBG :: String
normalBG = solarizedBase03

currentFG :: String
currentFG = solarizedBase3

currentBG :: String
currentBG = solarizedOrange

urgentBG :: String
urgentBG = solarizedRed

bgTrayer :: String
bgTrayer = "0x" ++ tail solarizedBase03

-- Stuff common to both dzen bars.
dzenStyle :: String
dzenStyle = " -fn " ++ font ++ " -h '" ++ show statusHeight ++ "' -y '0'"

-- Pretty printing.
myDzenPP :: PP
myDzenPP  = dzenPP
    { ppCurrent = dzenColor currentFG currentBG . pad
    , ppHidden  = dzenColor currentFG normalBG . pad . take 1
    , ppUrgent  = dzenColor currentFG urgentBG . pad
    , ppSep     = "|"
    , ppTitle   = shorten 500 . dzenColor normalFG normalBG . pad
    , ppLayout  = \x -> case x of
                      "Tall"        -> wrapBitmap "rob/tall.xbm"
                      "Mirror Tall" -> wrapBitmap "rob/mtall.xbm"
                      "Full"        -> wrapBitmap "rob/full.xbm"
    }
    where wrapBitmap bitmap = "^p(4)^i(" ++ myBitmapsPath ++ bitmap ++ ")^p(4)"

------------------------------------------------------------------------------------------
----------------------------------CUSTOM HOOKS--------------------------------------------
------------------------------------------------------------------------------------------

-- Custom manage hook.
myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Xfce4-notifyd"  --> doIgnore

    , title     =? "__NCMPCPP"      --> doCenterFloat
    , title     =? "__ALSAMIXER"    --> doCenterFloat
    , title     =? "__XMONADHS"     --> doCenterFloat
    , title     =? "__HTOP"         --> doShift "-:perf"

    , className =? "Gimp"           --> doFloat
    , className =? "mpv"            --> doFullFloat
    , className =? "Steam"          --> doFloat

    -- Chat windows go to workspace 3
    , className =? "Pidgin"         --> doShift "3:chat"
    , title     =? "Skype"          --> doShift "3:chat"
    ]

-- Custom log hook.
-- Forward window information to dzen bar, formatted.
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP myDzenPP { ppOutput = hPutStrLn h }

------------------------------------------------------------------------------------------
----------------------------- END CUSTOM HOOKS -------------------------------------------
------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------
--------------------------------- MAIN ---------------------------------------------------
------------------------------------------------------------------------------------------

main :: IO ()
main = do

    -- Get hostname for system-dependent stuff.
    host <- fmap nodeName getSystemID

    -- Get useful environment variables.
    editor <- getEnv "EDITOR"
    config_home <- getEnv "XDG_CONFIG_HOME"

    -- Resolution info
    xrandr <- runProcessWithInput "xrandr" [] ""
    screen <- runProcessWithInput "grep" ["Screen"] xrandr
    screencount <- runProcessWithInput "wc" ["-l"] screen

    screen0 <- runProcessWithInput "grep" ["Screen 0"] xrandr
    height0 <- runProcessWithInput "sed" ["s/^.*current\\ [0-9]\\+ x \\([0-9]\\+\\).*/\\1/"] screen0
    width0 <- runProcessWithInput "sed" ["s/^.*current \\([0-9]\\+\\).*/\\1/"] screen0

    screen1 <- runProcessWithInput "grep" ["Screen 1"] xrandr
    height1 <- runProcessWithInput "sed" ["s/^.*current\\ [0-9]\\+ x \\([0-9]\\+\\).*/\\1/"] screen1
    width1 <- runProcessWithInput "sed" ["s/^.*current \\([0-9]\\+\\).*/\\1/"] screen1

    let statuswidth  = 300
    let statusheight = 700
    let margin       = 20

    -- Write resolution info to a conky config file, then append standard conky config file
    -- to that file.  This will let us configure conky based on the current system's resolution.
    -- It's a bit sketchy, but I haven't yet found a better way to do this.
    _ <- writeFile (config_home ++ "/conky/sysdepconfig") ("minimum_size " ++
        show statuswidth ++ " " ++ show statusheight ++ "\n" ++ "maximum_width " ++
        show statuswidth ++ "\n" ++ "border_inner_margin " ++ show margin ++ "\n")
    conkyfile <- readFile $ config_home ++ "/conky/config"
    _ <- appendFile (config_home ++ "/conky/sysdepconfig") (conkyfile)

    -- TODO make magic numbers disappear.
    let x = ((read width0::Int) - statuswidth) - (margin `div` 2)
    let y = ((read height0::Int) - statusheight) `div` 2

    -- Environment variables.
    editor <- getEnv "EDITOR"
    config_home <- getEnv "XDG_CONFIG_HOME"

    -- Status bar programs.
    -- Configure statusbars based on how many monitors we have.
    -- Up to two, at least.
    let leftw = if (read screencount::Int) == 1
                    then (read width0::Int) `div` 2
                    else (read width0::Int)
    dzenL  <- spawnPipe $ dzenLeft leftw

    let rightx = leftw
    let rightw = if (read screencount::Int) == 1
                    then ((read width0::Int) `div` 2) - trayerWidth
                    else (read width1::Int)
    dzenRPID  <- spawnPID $ dzenRight rightx rightw config_home
    conkyPID  <- spawnPID $ conkyStatus x y config_home
    trayerPID <- spawnPID myTrayer

    xmonad $ withUrgencyHook NoUrgencyHook defaultConfig

        -- Hooks.
        { manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
        , layoutHook = avoidStruts $ smartBorders $ layoutHook defaultConfig
        , logHook = myLogHook dzenL

        -- Handles Java
        , startupHook = setWMName "LG3D"

        -- Stuff.
        , modMask = myModMask
        , workspaces = myWorkspaces
        , terminal = myTerminal

        -- Mice are for squares.
        , focusFollowsMouse = False

        -- Border jazz
        , focusedBorderColor = myFocusedBorderColor
        , normalBorderColor  = myNormalBorderColor
        , borderWidth        = myBorderWidth

        } `additionalKeys`

        (
        [ ((mod4Mask .|. shiftMask, xK_l),  spawn "xscreensaver-command -lock")

        -- Backlight.
        , ((0, xF86XK_MonBrightnessDown),   spawn "xbacklight -dec 7")
        , ((0, xF86XK_MonBrightnessUp),     spawn "xbacklight -inc 7")

        -- Screenshots.
        , ((0, xK_Print),                   spawn "scrot")
        , ((controlMask, xK_Print),         spawn "sleep 0.2; scrot -s")

        -- Modified to kill taskbar programs.
        , ((myModMask, xK_q),               spawn $ "killall dzen;" ++
                                                    "killall conky;" ++
                                                    "killall trayer;" ++
                                                    "xmonad --recompile;" ++
                                                    "xmonad --restart")

        -- Alsamixer, ncmpcpp, quick spawn bindings.
        , ((myModMask, xK_a),               spawn $ namedCmd "alsamixer" "")
        -- Find a better way to do this.
        , ((myModMask, xK_o),               spawn $ namedCmd "ncmpcpp" ("-c " ++
                                                                          config_home ++
                                                                          "/ncmpcpp/config"))

        -- various utility scripts
        , ((myModMask .|. shiftMask, xK_s), spawn "~/bin/setWallpaper")] ++

        -- Audio keys.
        audioKeys host ++

        -- Keys for extra workspaces
        [( (myModMask .|. shiftMask, k), windows $ W.shift i) |
         (i, k) <- zip myWorkspaces myKeys] ++

        [( (myModMask, k),          windows $ W.greedyView i) |
         (i, k) <- zip myWorkspaces myKeys]
        )

