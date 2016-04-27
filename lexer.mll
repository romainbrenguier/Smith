{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let lid = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let uid = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let abs = ['\''] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let op = ['=' '<' '>' '@' '^' '|' '&' '~' '+' '-' '*' '/' '$' '%'] ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' ';' '<' '=' '>' '?' '@' '^' '|' '~']*



rule read = parse
| white { read lexbuf }
| newline { next_line lexbuf; EOL}(*read lexbuf }*)
| "val" { VAL }
| "external" { EXTERNAL }
| "exception" { EXCEPTION }
| "type" { TYPE }
| "of" { OF }
| lid { LIDENT (Lexing.lexeme lexbuf) }
| uid { UIDENT (Lexing.lexeme lexbuf) }
| abs { ABS (Lexing.lexeme lexbuf) }
| '=' { EQUALS }
| '(' { LEFT_PAR }
| ')' { RIGHT_PAR }
| ';' { SEMI }
| ':' { COLON }
| '*' { STAR }
| '|' { PIPE }
| "->" { ARROW }
| eof { EOF }
| "(*" { comment 1 lexbuf }
| '\"' { read_string (Buffer.create 80) lexbuf }
| op { OP (Lexing.lexeme lexbuf) }
| _ { raise (SyntaxError ("Illegal character: " ^ Lexing.lexeme lexbuf))}
and comment i = parse
| "*)" { if i = 1 then read lexbuf else comment (i-1) lexbuf }
| "(*" { comment (i+1) lexbuf }
| newline { next_line lexbuf; comment i lexbuf }
| _ { comment i lexbuf }
and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }


