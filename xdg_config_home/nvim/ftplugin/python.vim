""" Google/PEP8 coding standards.
setlocal textwidth=79
execute "set colorcolumn=" . join(map(range(2,259), '"+" . v:val'), ',')

setlocal foldexpr=SimpylFold(v:lnum) foldmethod=expr
setlocal foldexpr< foldmethod<
let g:SimpylFold_docstring_preview = 1
" Indent Python in the Google way.

setlocal indentexpr=GetGooglePythonIndent(v:lnum)

let s:maxoff = 50 " maximum number of lines to look backwards.
