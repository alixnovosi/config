"-------------------------------------------------------------------------------------------------"
" AUTHOR:  Andrew Michaud                                                                         "
" FILE:    plugins.vim                                                                            "
" PURPOSE: Plugins used in (neo)vim                                                               "
" UPDATED: 2016-03-07                                                                             "
" LICENSE: MIT                                                                                    "
"-------------------------------------------------------------------------------------------------"
""" Most of these are hopefully self-explanatory. Try :help <thing> for more info.
""" File settings.
set fileformats=unix nobackup nomodeline spell spelllang=en undofile
scriptencoding utf-8

""" Editing/editor settings.
set expandtab lazyredraw shiftwidth=4 softtabstop=4 tabstop=4 textwidth=99 whichwrap=[,],h,l,b,s
set foldenable foldlevelstart=10 foldnestmax=10 foldmethod=syntax
set cursorline ignorecase laststatus=2 noshowmode showcmd smartcase
""" Color columns past textwidth.
""" Credit http://blog.hanschen.org/2012/10/24/different-background-color-in-vim-past-80-columns.
execute "set colorcolumn=" . join(map(range(1,259,2), '"+" . v:val'), ',')

""" One day I'll untangle the bugs this introduces.
" let $NVIM_TUI_ENABLE_TRUE_COLOR=1
let g:base16_shell_path="$XDG_DATA_HOME/bin/base16-shell"
let base16colorspace=256
colorscheme base16-ocean
set background=dark

""" Airline preferences.
let g:airline_left_sep = ""
let g:airline_right_sep = ""
let g:airline#extensions#tabline#left_sep = " "
let g:airline#extensions#tabline#right_sep = " "
let g:airline#extensions#tabline#buffer_min_count = 2
let g:airline_extensions = ["eclim", "hunks", "syntastic", "tagbar", "tabline", "ycm"]
let g:airline_inactive_collapse = 1

""" Attempt to get Eclim and Eclipse and Vim and YCM to play nicely.
let g:EclimCompletionMethod = "omnifunc"

""" Make YCM and UltiSnips work together via supertab.
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:SuperTabDefaultCompletionType = '<C-n>'

""" Better key bindings for UltiSnipsExpandTrigger.
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"

""" Let C-s and C-q go to Vim instead of terminal. We'll later set commands using that.
silent !stty -ixon > /dev/null 2>/dev/null

set conceallevel=1
let g:javascript_conceal_function   = "λ"
let g:javascript_conceal_null       = "ø"
let g:javascript_conceal_this       = "@"
let g:javascript_conceal_return     = "⇚"
let g:javascript_conceal_undefined  = "¿"
let g:javascript_conceal_NaN        = "ℕ"
let g:javascript_conceal_prototype  = "¶"
let g:javascript_conceal_static     = "•"
let g:javascript_conceal_super      = "Ω"

""" 'Learn vimscript the hard way' testbed - http://learnvimscripthehardway.stevelosh.com.
let maplocalleader = "_"
