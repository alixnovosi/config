"-------------------------------------------------------------------------------------------------"
" AUTHOR:  Andrew Michaud - https://andrewmichaud.com                                             "
" FILE:    plugins.vim                                                                            "
" PURPOSE: Plugins used in (neo)vim.                                                              "
" UPDATED: 2016-07-26                                                                             "
" LICENSE: ISC                                                                                    "
"-------------------------------------------------------------------------------------------------"
""" Unix file endings, no backups, no modelines, yes spelling, yes persistent undo history.
set fileformats=unix nobackup nomodeline spell spelllang=en_us undofile
scriptencoding utf-8

""" Use 4-wide spaces, no tabs.
set expandtab shiftwidth=4 softtabstop=4 tabstop=4

""" Allow reasonable folding. Wrap text at 99 columns, let movement spill over into next line.
set foldenable foldlevelstart=10 foldnestmax=5 foldmethod=syntax textwidth=99 whichwrap=[,],h,l,b,s

""" Color columns past textwidth.
""" Credit http://blog.hanschen.org/2012/10/24/different-background-color-in-vim-past-80-columns.
execute "set colorcolumn=" . join(map(range(1,259,2), '"+" . v:val'), ',')

""" Highlight current line, use 2-column status (for airline) that shows commands, but not mode.
""" Ignore case in searching unless I use capital letters.
set cursorline laststatus=2 noshowmode showcmd ignorecase smartcase

""" True color!.
""" Colorscheme stuff.
let $NVIM_TUI_ENABLE_TRUE_COLOR=1
let g:base16_shell_path="$XDG_DATA_HOME/bin/base16-shell"
let base16colorspace=256
colorscheme base16-ocean
set background=dark

""" Use pipe cursor in insert mode.
let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1

let g:EclimJavascriptValidate = 0

""" JSON conceal.
let g:vim_json_syntax_conceal = 1

""" Airline preferences.
let g:airline_left_sep = ""
let g:airline_right_sep = ""
let g:airline_extensions = ["branch", "hunks", "syntastic", "tagbar", "tabline", "ycm"]
let g:airline#extensions#tabline#left_sep = " "
let g:airline#extensions#tabline#right_sep = " "
let g:airline#extensions#tabline#buffer_min_count = 2
let g:airline_inactive_collapse = 1

""" Attempt to get Eclim and Eclipse and Vim and YCM to play nicely.
let g:EclimCompletionMethod = "omnifunc"
let g:syntastic_javascript_checkers = ['jshint']

let g:syntastic_check_on_wq = 0

""" Make YCM and UltiSnips work together via supertab.
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:SuperTabDefaultCompletionType = '<C-n>'

""" Better key bindings for UltiSnipsExpandTrigger.
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"

""" Make ack ag.
let g:ackprg = 'ag --vimgrep --smart-case'
cnoreabbrev ag Ack
cnoreabbrev aG Ack
cnoreabbrev Ag Ack
cnoreabbrev AG Ack

""" Let C-s and C-q go to Vim instead of terminal. We'll later set commands using that.
silent !stty -ixon > /dev/null 2>/dev/null

""" Fun with concealing.
set conceallevel=1
let hsoptions="+"

""" 'Learn vimscript the hard way' testbed - http://learnvimscripthehardway.stevelosh.com.
let maplocalleader = "_"
