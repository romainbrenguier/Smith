%start <Type.declaration> prototype
%token <string> LIDENT
%token <string> UIDENT
%token <string> ABS
%token <string> STRING
%token <string> OP
%token VAL
%token TYPE
%token OF
%token COLON STAR PIPE
%token ARROW EQUALS
%token LEFT_PAR
%token RIGHT_PAR
%token EOF
%token EOL
%token SEMI
%token EXTERNAL EXCEPTION
%right ARROW 
%%

prototype:
| EOF { Nothing }
| EOL; p=prototype { p }
| TYPE; s=name; EQUALS; t=expression; EOL { NewType (s,Some t) }
| TYPE; s=name; EOL { NewType (s,None) }
| VAL; s=name; COLON; t=expression; EOL { Decl (s,t) }
| EXTERNAL; s=name; COLON; t=expression; EQUALS; STRING; EOL { Decl (s,t) }
| EXCEPTION; UIDENT; EOL; p=prototype { p }
  ;

name:
| s=LIDENT { s }
| LEFT_PAR; s=OP; RIGHT_PAR { "("^s^")" }
| LEFT_PAR; s=EQUALS; RIGHT_PAR { "(=)" }
;


expression:
| t1 = expression; t2=LIDENT { Type._of t1 (Type.atom t2) }
| t1 = expression; ARROW; t2=expression { Type.arrow t1 t2 }
| t1 = expression; STAR; tl=star_expression { Type.tuple (t1 :: tl) }
| c=UIDENT; OF; t = expression; PIPE; c2=UIDENT; OF; t2 = expression { Type.union [(c,t);(c2,t2)] }
| LEFT_PAR; t = expression; RIGHT_PAR { t } 
| s = LIDENT { Type.atom s }
| s = ABS { Type.var s }
;

constr_expr: 
(*| ce=constr_expr; PIPE; c=UIDENT; OF; t = expression { (c,t) :: ce }*)
| c=UIDENT; OF; t =expression { [c,t] }
;

star_expression:
| t1 = expression; STAR; tl=star_expression { t1 :: tl }
| t1 = expression { [t1] }
;


