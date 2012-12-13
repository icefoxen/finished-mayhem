" Vim syntax file
"
" Language:	Mayhem
" Maintainer:	Me
" Last Change:	2007 April 6

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Don't ignore case (Modula-2 is case significant). This is the default in vim
" syntax case match
"

" All other keywords in alphabetical order:
syn keyword mayhemKeyword end : ref deref setref set get mod > >= < = /= <=
syn keyword mayhemKeyword defun struct load if else elif then

" Hrm.
syn match mayhemKeyword ":,\+,-,*,/"

syn keyword mayhemType int
syn keyword mayhemStdFunc cons car cdr fst snd 
syn keyword mayhemStdConst Null
" The underscore was originally disallowed in m2 ids, it was also added later:
syn match   mayhemIdent " [A-Z,a-z][A-Z,a-z,0-9,_]*" contained

syn region mayhemComment start=";" end="\n" contains=mayhemComment,mayhemTodo
syn keyword mayhemTodo	contained TODO FIXME XXX

syn region mayhemString start=+"+ end=+"+
syn match mayhemSymbol "`[A-Z,a-z,0-9,_]*"
syn region mayhemArray start="{" end="}"

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_mayhem_syntax_inits")
  if version < 508
    let did_mayhem_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink mayhemIdent		Identifier
  HiLink mayhemStdConst	        Boolean
  HiLink mayhemType		Identifier
  HiLink mayhemStdFunc		Identifier
  HiLink mayhemKeyword		Statement
  HiLink mayhemComment		Comment
  " The following is just a matter of taste (you want to try this instead):
  " hi mayhemComment term=bold ctermfg=DarkBlue guifg=Blue gui=bold
  HiLink mayhemTodo		Todo
  HiLink mayhemString		String
  HiLink mayhemSymbol	        PreProc
  HiLink mayhemArray		String

  delcommand HiLink
endif

let b:current_syntax = "mayhem"

" vim: ts=8
