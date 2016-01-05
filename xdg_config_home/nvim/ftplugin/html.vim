""" Use 2-width spaced tabs for html, because there's so much indenting.
setlocal expandtab shiftwidth=2 softtabstop=2
setlocal textwidth=130
execute "set colorcolumn=" . join(map(range(2,259), '"+" . v:val'), ',')
