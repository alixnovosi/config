#!/bin/zsh
#-------------------------------------------------------------------------------------------------#
# AUTHOR:  Andrew Michaud                                                                         #
#          https://andrewmichaud.com                                                              #
# FILE:    .zshrc                                                                                 #
# PURPOSE: zsh config file.                                                                       #
# UPDATED: 2015-08-27                                                                             #
# LICENSE: MIT/BSD                                                                                #
#-------------------------------------------------------------------------------------------------#

# This is the only thing I can think of that needs to be done every session instead of once,
# which is why my zshenv is so full and this so empty.
which fortune &> /dev/null && fortune -s
