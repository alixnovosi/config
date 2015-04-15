import System.Taffybar
import System.Taffybar.MPRIS2 (mpris2New)
import System.Taffybar.Pager (wrap, colorize, shorten, escape)
import System.Taffybar.SimpleClock
import System.Taffybar.Systray (systrayNew)
import System.Taffybar.TaffyPager
import System.Taffybar.Weather

-------------------
----- COLORS ------
-------------------
-- TODO put these in a common file somewhere and refer to them there.
base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"

main = do
    let clock   = clockConfig
        pager   = pagerConfig
        tray    = systrayNew
        mpris2  = mpris2New
        weather = weatherConfig

    defaultTaffybar defaultTaffybarConfig
        { startWidgets = [pager]
        , endWidgets = [ tray, clock, weather, mpris2]
        , barHeight = 16
        }

--------------------------------------------------------------------------
---------------------------   FUNCTIONS   --------------------------------
--------------------------------------------------------------------------
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

weatherConfig = weatherNew ( defaultWeatherConfig "KCQT" )
    {weatherTemplate = " $tempF$ \176F|$tempC$ \176C "}
    10.0

clockConfig = textClockNew Nothing
                           "%_d %b %Y|%A|%H:%M"
                           1
