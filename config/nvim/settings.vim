"-------------------------------------------------------------------------------------------------"
" AUTHOR:  Andrew Michaud - drew.life                                                             "
" FILE:    settings.vim                                                                           "
" PURPOSE: Settings used in (neo)vim.                                                             "
" UPDATED: 2019-08-08                                                                             "
" LICENSE: ISC                                                                                    "
"-------------------------------------------------------------------------------------------------"
""" Unix file endings, no backups, no modelines, yes spelling, yes persistent undo history.
set fileformats=unix nobackup nomodeline spell spelllang=en_us undofile
scriptencoding utf-8

set mouse=a
let base16colorspace=256

set incsearch

""" Use 4-wide spaces, no tabs.
set expandtab shiftwidth=4 softtabstop=4 tabstop=4

""" Allow reasonable folding. Wrap text at 99 columns, let movement spill over into next line.
set foldenable foldlevelstart=10 foldnestmax=5 foldmethod=syntax textwidth=99 whichwrap=[,],h,l,b,s

""" Color columns past textwidth.
""" Credit http://blog.hanschen.org/2012/10/24/different-background-color-in-vim-past-80-columns.
execute 'set colorcolumn=' . join(map(range(1,259,2), '"+" . v:val'), ',')

""" Highlight current line, no mode in status.
""" Ignore case in searching unless I use capital letters. Mouse.
set cursorline noshowmode ignorecase smartcase mouse=a

let g:vim_json_syntax_conceal = 1

""" Airline preferences.
let g:airline_extensions = ['branch', 'hunks', 'ale', 'tagbar', 'tabline', 'ycm']
let g:airline#extensions#tabline#buffer_min_count = 2
let g:airline_inactive_collapse = 1

""" Make YCM and UltiSnips work together via supertab.
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:SuperTabDefaultCompletionType = '<C-n>'

""" Better key bindings for UltiSnipsExpandTrigger.
let g:UltiSnipsExpandTrigger = '<tab>'
let g:UltiSnipsJumpForwardTrigger = '<tab>'
let g:UltiSnipsJumpBackwardTrigger = '<s-tab>'

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
let g:hsoptions='+'

" Let clangd fully control code completion
let g:ycm_clangd_uses_ycmd_caching = 0
" Use installed clangd, not YCM-bundled clangd which doesn't get updates.
let g:ycm_clangd_binary_path = exepath("clangd-9")

let g:ale_java_checkstyle_config = '/intellij-java-google-style.xml'

let g:python_host_prog = expand('$XDG_DATA_HOME/virtualenvs/neovim3/bin/python')
let g:python3_host_prog = expand('$XDG_DATA_HOME/virtualenvs/neovim3/bin/python')

""" 'Learn vimscript the hard way' testbed - http://learnvimscripthehardway.stevelosh.com.
let g:maplocalleader = '_'
