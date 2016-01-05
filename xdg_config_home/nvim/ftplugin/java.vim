""" Java is verbose.
setlocal textwidth=130
execute "set colorcolumn=" . join(map(range(2,259), '"+" . v:val'), ',')
