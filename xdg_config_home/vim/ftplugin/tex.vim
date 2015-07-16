" Make TeX editing more pleasant.
noremap <F5> <Esc>:!pdflatex %<Cr><Cr>
noremap <F6> <Esc>:!silent !evince %<.pdf >/dev/null 2>&1 &<Cr><Cr>
