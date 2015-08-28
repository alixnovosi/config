setlocal foldexpr=SimpylFold(v:lnum) foldmethod=expr
setlocal foldexpr< foldmethod<
setlocal textwidth=79
let g:SimpylFold_docstring_preview = 1
" Indent Python in the Google way.

setlocal indentexpr=GetGooglePythonIndent(v:lnum)

let s:maxoff = 50 " maximum number of lines to look backwards.
