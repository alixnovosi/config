#!/bin/zsh
#-------------------------------------------------------------------------------------------------#
# AUTHOR:  Andrew Michaud                                                                         #
# FILE:    .zsenv                                                                                 #
# PURPOSE: zsh once-per-login config file                                                         #
# UPDATED: 2015-08-26                                                                             #
# LICENSE: MIT/BSD                                                                                #
#-------------------------------------------------------------------------------------------------#

# Sketchy way of detecting multiuser machine (where users is interesting).
if [[ "$(who | grep -c "$USER")" -ne "$(who | wc -l)" ]]; then

    # Display who else is logged in.
    users|tr ' ' '\n'|uniq|tr '\n' ' '|awk '{print $0} END {print ""}'
fi
