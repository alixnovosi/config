---------------------------------------------------------------------------------------------------
-- AUTHOR:  Andrew Michaud - https://andrewmichaud.com                                           --
-- FILE:    dzen_config.lua                                                                      --
-- PURPOSE: Config file for conky status in dzen.                                                --
-- UPDATED: 2017-12-17                                                                           --
-- LICENSE: ISC                                                                                  --
---------------------------------------------------------------------------------------------------
conky.config = {
	own_window_transparent = true,
	pad_percents = 3,
	update_interval = 1,
	use_xft = true,
    xftfont = 'Dejavu:size=10',
	format_human_readable = true,
	mpd_host = 'localhost',
	mpd_port = 6600,
};

conky.text = [[
###################################################################
####################         Background           #################
###################################################################
\
${if_mpd_playing}\
${if_match "${mpd_status}" == "Playing"}♪ $else $endif\
${color 0000ff}${mpd_title} ${color}\
${if_match "${mpd_status}" == "Playing"}♪ $else $endif\
${endif}\
 \
${color fdf6e3}c \
${if_match ${cpu cpu0} > 75}\
${color DC322F}${cpu cpu0}\
${endif}\
${if_match ${cpu cpu0} <= 75}\
${if_match ${cpu cpu0} > 25 }\
${color CB4B16}${cpu cpu0}\
${endif}\
${endif}\
${if_match ${cpu cpu0} <= 25}\
${color 2aa198}${cpu cpu0}\
${endif}\
 \
##############################
#####    mem status      #####
##############################
\
${color fdf6e3}m \
${if_match ${memperc} > 75}\
${color DC322F}${memperc}\
${endif}\
${if_match ${memperc} <= 75}\
${if_match ${memperc} > 25 }\
${color CB4B16}${memperc}\
${endif}\
${endif}\
${if_match ${memperc} <= 25}\
${color 2aa198}${memperc}\
${endif}\
\
#####################################
######        date/time        ######
#####################################
\
|${color fdf6e3}${time %a|%d %b. %Y|%H:%M} \
]];
