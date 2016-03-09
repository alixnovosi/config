""" Google/PEP8 coding standards.
setlocal textwidth=99

setlocal foldexpr=SimpylFold(v:lnum) foldmethod=expr
setlocal foldexpr< foldmethod<
let g:SimpylFold_docstring_preview = 1

""" Indent Python in the Google way.
setlocal indentexpr=GetGooglePythonIndent(v:lnum)

""" Maximum number of lines to look backwards.
let s:maxoff = 50 " maximum number of lines to look backwards.
