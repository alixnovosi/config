""" Haskell's textwidth was getting set differently for some reason, so set it again here.
setlocal textwidth=99
execute "set colorcolumn=" . join(map(range(2,259), '"+" . v:val'), ',')
