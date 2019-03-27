import XMonad
import Data.List
import XMonad.Util.Dmenu
import Graphics.X11.ExtraTypes.XF86
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import System.IO
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace  
import XMonad.Layout.IM  
import XMonad.Layout.Grid  
import XMonad.Actions.GridSelect  
import Data.Ratio ((%))  
import XMonad.Actions.CycleWS  
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.FloatNext (floatNextHook)
import XMonad.Actions.SpawnOn
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.ResizableTile
import XMonad.Layout.ToggleLayouts
import qualified Data.Map as M

main = xmonad $ defaultConfig {
	  borderWidth 	= 2 
	, terminal   	= "urxvtc"
	, modMask	    = mod4Mask
	, manageHook	= myManageHook
	, layoutHook	= myLayoutHook
	, handleEventHook = handleEventHook defaultConfig <+> fullscreenEventHook 
	, normalBorderColor = "#000000"
	, focusedBorderColor = "#c0c0c0" } `additionalKeysP` myKeys


myLayoutHook = avoidStruts
            $ toggleLayouts (noBorders Full)
            $ smartBorders myLayout

myLayout = tiled ||| Mirror tiled ||| Full
  where
-- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio
 
-- The default number of windows in the master pane
    nmaster = 1
 
-- Default proportion of screen occupied by master pane
    ratio   = 1/2
 
-- Percent of screen to increment by when resizing panes
    delta   = 3/100

myKeys =  [ ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +1.5%")
            , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@  -1.5%")
            , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")    

            , ("<XF86AudioPlay>", spawn "playerctl play-pause")    
            , ("<XF86AudioPrev>", spawn "playerctl previous")    
            , ("<XF86AudioNext>", spawn "playerctl next")    

            , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10")    
            , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10")    
            ]


myHook :: ManageHook
myHook = composeAll $
                [ className =? "MPlayer"        --> doFloat
                , className =? "feh"            --> doFloat 
--				, className =? "Gimp" 		--> doShift "*" -- may be "Gimp" or "Gimp-2.4" instead
--   			, (className =? "Gimp" <&&> fmap ("tool" `isSuffixOf`) role) --> doFloat ]
    			]
  where role = stringProperty "WM_WINDOW_ROLE"


myManageHook = manageDocks <+>
               floatNextHook <+>
               (isFullscreen --> doFullFloat) <+> myHook <+>
               manageHook defaultConfig
