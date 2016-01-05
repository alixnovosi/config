""" Force spellcheck and textwidth for markdown files.
set spell spelllang=en_us
set textwidth=80
execute "set colorcolumn=" . join(map(range(2,259), '"+" . v:val'), ',')
