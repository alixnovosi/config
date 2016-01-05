""" Force spellcheck and textwidth for git commit messages.
set spell spelllang=en_us
set textwidth=72
execute "set colorcolumn=" . join(map(range(2,259), '"+" . v:val'), ',')
