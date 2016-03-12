---------------------------------------------------------------------------------------------------
-- AUTHOR:  Andrew Michaud - https://andrewmichaud.com                                           --
-- FILE:    taffybar.hs                                                                          --
-- PURPOSE: Taffybar configuration file.                                                         --
-- UPDATED: 2016-03-11                                                                           --
-- LICENSE: ISC                                                                                  --
---------------------------------------------------------------------------------------------------
import System.Taffybar
import System.Taffybar.MPRIS2 (mpris2New)
import System.Taffybar.Pager (wrap, colorize, shorten, escape)
import System.Taffybar.SimpleClock
import System.Taffybar.Systray (systrayNew)
import System.Taffybar.TaffyPager
import System.Taffybar.Weather

import Colors

main = do
    let clock   = clockConfig
        pager   = pagerConfig
        tray    = systrayNew
        mpris2  = mpris2New
        weather = weatherConfig

    defaultTaffybar defaultTaffybarConfig { startWidgets = [pager]
                                          , endWidgets = [ tray, clock, weather, mpris2]
                                          , barHeight = 16
                                          }

---------------------------------------------------------------------------------------------------
------------------------------------   FUNCTIONS   ------------------------------------------------
---------------------------------------------------------------------------------------------------
pagerConfig = taffyPagerNew defaultPagerConfig
    { emptyWorkspace   = const ""
    , activeWorkspace  = colorize base03 base2 . wrap "[" "]"
    , visibleWorkspace = colorize base03 base2 . wrap "(" ")"
    , urgentWorkspace  = colorize base03 base2 . wrap "<" ">"
    , hiddenWorkspace  = colorize base2 base02 . wrap " " " " . take 1
    , widgetSep        = " "
    , activeLayout     = take 1
    , activeWindow     = colorize base03 base2 . shorten 70 . wrap "[" "]"
    }

weatherConfig = weatherNew ( defaultWeatherConfig "KHWD" )
    {weatherTemplate = " $tempF$ \176F|$tempC$ \176C "}
    10.0

clockConfig = textClockNew Nothing "%_d %b %Y|%A|%H:%M" 1
