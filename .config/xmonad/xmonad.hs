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
import XMonad.Layout.Spacing -- spacing between windows

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
    xrandr <- runProcessWithInput "xrandr" []                                            ""
    let dims = screeninfo xrandr

    -- Status bar programs.
    -- Configure statusbars based on how many monitors we have.
    -- Up to two, at least.

    -- Left bar - xmonad info.
    let leftw
            | null dims        = 300
            | length dims == 1 = head (head dims) `div` 3
            | otherwise        = head $ head dims

    dzenL  <- spawnPipe $ dzenLeft leftw

    -- Right bar - MPD and other useful status info.
    let rightx = leftw
    let rightw
            | null dims        = 300
            | length dims == 1 = ((head (head dims) `div` 3)  * 2) - trayerWidth + 1
            | otherwise        = head (dims !! 1) - trayerWidth

    dzenRPID  <- spawnPID $ dzenRight rightx rightw config_home

    -- System tray.
    trayerPID <- spawnPID myTrayer

    -- Conky background status info.
    conkyPID  <- spawnPID $ conkyStatus config_home

    xmonad $ withUrgencyHook uHook $ defaultConfig

        -- Hooks.
        { manageHook = mHook
        , layoutHook = avoidStruts $ smartBorders $ layoutHook defaultConfig
        , logHook = lHook dzenL

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

        } `additionalKeys` keybinds config_home host

---------------------------------------------
--------------- VARIABLES AND STUFF
---------------------------------------------

-- Colors and stylings.

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

-- XMonad colors
normalFG = solarizedBase3
normalBG = solarizedBase03
currentFG = normalFG
currentBG = solarizedOrange
urgentBG = solarizedRed

-- Trayer colors
bgTrayer = "0x" ++ tail normalBG

-- dmenu colors
normalBGDmenu = "'" ++ normalBG ++ "'"
normalFGDmenu = "'" ++ normalFG ++ "'"
selectedFGDmenu = "'" ++ normalFG ++ "'"
selectedBGDmenu = "'" ++ currentBG ++ "'"

--Borders
myFocusedBorderColor :: String
myFocusedBorderColor = solarizedOrange

myNormalBorderColor :: String
myNormalBorderColor = solarizedBase1

-- Fonts
fontName = "Dejavu"
font size = "-*-" ++ fontName ++ "-medium-r-normal-*-" ++ show size ++ "-140-75-75-p-74-iso10646-1"

-- Variables

keybinds config_home host =
    [
    -- Screen lock
      ((myModMask .|. shiftMask, xK_l                    ), spawn "xscreensaver-command -lock")

    -- Backlight.
    , ((0,                       xF86XK_MonBrightnessDown), spawn "xbacklight -dec 7")
    , ((0,                       xF86XK_MonBrightnessUp  ), spawn "xbacklight -inc 7")

    -- Screenshots.
    , ((0,                       xK_Print                ), spawn screenshot)
    , ((controlMask,             xK_Print                ), spawn screenshotSelect)
    , ((shiftMask,               xK_Print                ), spawn screenshotDelay)

    -- Recompile/restart XMonad. Modified to kill taskbar programs.
    , ((myModMask,               xK_q                    ), spawn $ "killall dzen;" ++
                                                                   "killall conky;killall trayer;" ++
                                                                   "xmonad --recompile;xmonad --restart")
    -- Run DMenu.
    , ((myModMask,               xK_p                    ), spawn $ "dmenu_run " ++ dmenuStyle)

    -- Alsamixer, ncmpcpp, quick spawn bindings.
    , ((myModMask,               xK_a                    ), spawn $ namedCmd "alsamixer" "")

    -- NOTE- Find a cleaner way to do this.
    , ((myModMask,               xK_o                    ), spawn $ namedCmd "ncmpcpp" ("-c " ++
                                                                              config_home ++
                                                                              "/ncmpcpp/config"))

    -- various utility scripts
    , ((myModMask .|. shiftMask, xK_s                    ), spawn "~/bin/setWallpaper")

    -- Clip password with dzen and a nifty script.
    , ((myModMask .|. shiftMask, xK_p                    ), spawn $ "~/bin/passmenu " ++ dmenuStyle)

    ] ++

    -- Audio keys.
    audioKeys host ++

    -- Keys for extra workspaces
    [((myModMask .|. shiftMask,  k                       ), windows $ W.shift i) |
        (i, k) <- zip myWorkspaces wsKeys] ++

    [( (myModMask,               k                       ), windows $ W.greedyView i) |
        (i, k) <- zip myWorkspaces wsKeys]
        where wsKeys = [xK_grave] ++ [xK_1 .. xK_9] ++ [xK_0, xK_minus, xK_equal]

------------------------------------------------------------------------------------------
-----------------------------STATUS BAR STUFF---------------------------------------------
------------------------------------------------------------------------------------------

-- Left Dzen - xmonad info and window title.
dzenLeft :: Int -> String
dzenLeft width = "dzen2 -x '0' -w '" ++ show width ++ "' -ta 'l'" ++ dzenStyle


-- Right Dzen - Runs conky config.
dzenRight :: Int -> Int -> String -> String
dzenRight start width config = conkyCmd ++ " | dzen2 " ++ style
    where conkyCmd = "conky --config=" ++ config ++ "/conky/dzen_config"
          style = "-x '" ++ show start ++ "' -w '" ++ show width ++ "' -ta 'r'" ++ dzenStyle

-- Background status.
conkyStatus :: String -> String
conkyStatus config = "conky --config=" ++ config ++ "/conky/config"

-- Trayer - system tray.
myTrayer = "trayer --edge top --align right " ++
           "--widthtype pixel --width " ++ show trayerWidth ++ " " ++
           "--expand true --SetDockType true --SetPartialStrut true " ++
           "--heighttype pixel --height " ++ show statusHeight ++ " --padding 2"

-- Bitmaps used to represent current layout.
myBitmapsPath = home ++ "/dzen/bitmaps/"

-- dmenu style
dmenuStyle = " -fn " ++ font 6 ++ " -nb " ++ normalBGDmenu ++ " -nf " ++ normalFGDmenu ++
             " -sf " ++ selectedFGDmenu ++ " -sb " ++ selectedBGDmenu

-- Dzen style
dzenStyle = " -fn " ++ font 12 ++ " -h '" ++ show statusHeight ++ "' -y '0'"

-- Pretty printing.
myDzenPP :: PP
myDzenPP  = dzenPP
    { ppCurrent = dzenColor currentFG currentBG . pad
    , ppHidden  = dzenColor currentFG normalBG . pad . take 1
    , ppUrgent  = dzenColor currentFG urgentBG . pad
    , ppSep     = " "
    , ppTitle   = dzenColor normalFG normalBG . pad
    , ppLayout  = \x -> case x of
                      "Tall"        -> wrapBitmap "rob/tall.xbm"
                      "Mirror Tall" -> wrapBitmap "rob/mtall.xbm"
                      "Full"        -> wrapBitmap "rob/full.xbm"
    }
    where wrapBitmap bitmap = "^p(4)^i(" ++ myBitmapsPath ++ bitmap ++ ")^p(4)"


-- Status bar stuff.

-- Status bar height
statusHeight :: Int
statusHeight = 16

-- Trayer width
trayerWidth :: Int
trayerWidth = 70

myBorderWidth :: Dimension
myBorderWidth = 2

-- Commands and config homes.

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

-- Workspace titles
myWorkspaces :: [String]
myWorkspaces = ["` dev",  "1 term", "2 web",  "3 chat", "4 ssh",
                "5 game", "6 vid",  "7 work", "8 work", "9 work",
                "0 etc",  "- perf", "= music"]

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
-- Screenshot commands
screenshot = "scrot '%d-%m-%Y-%s_$wx$h.png' -e 'mv $f ~/pictures/screenshots/'"
screenshotSelect = "scrot -s '%d-%m-%Y-%s_$wx$h.png' -e 'mv $f ~/pictures/screenshots/'"
screenshotDelay = "scrot -s --delay 5  '%d-%m-%Y-%s_$wx$h.png' -e 'mv $f ~/pictures/screenshots/'"

----------------------------------------------------------------
-------------------------------------UTILITY FUNCTIONS----------
----------------------------------------------------------------
-- Grab resolution given xrandr string.
screeninfo :: String -> [[Int]]
screeninfo input = intResPairs
    where separated    = map words (lines input)
          screenLines  = filter (\x -> head x == "Screen") separated
          afterCurrent = map (dropWhile (/= "current")) screenLines
          atResolution = map (dropWhile (== "current") . takeWhile (/= "maximum")) afterCurrent
          resPairs     = map (filter (/= "x")) atResolution
          intResPairs  = map (map read) resPairs

------------------------------------------------------------------------------------------
----------------------------------CUSTOM HOOKS--------------------------------------------
------------------------------------------------------------------------------------------

-- Custom urgency hook.
uHook = dzenUrgencyHook { args = ["-bg", "darkgreen", "-xs", "1"] }

-- Custom manage hook.
-- Manage docks, custom layout nonsense.
mHook :: ManageHook
mHook = manageDocks <+> composeAll
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
    ] <+> manageHook defaultConfig

-- Custom log hook.
-- Forward window information to dzen bar, formatted.
lHook :: Handle -> X ()
lHook h = dynamicLogWithPP myDzenPP { ppOutput = hPutStrLn h }

------------------------------------------------------------------------------------------
----------------------------- END CUSTOM HOOKS -------------------------------------------
------------------------------------------------------------------------------------------

