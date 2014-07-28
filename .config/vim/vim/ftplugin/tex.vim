
""" Make latex editing easy and fun and beautiful.
noremap <F5> <Esc>:!pdflatex %<Cr><Cr>
noremap <F6> <Esc>:silent !evince %<.pdf >/dev/null 2>&1 &<Cr><Cr>
setlocal tabstop=2
setlocal shiftwidth=2

