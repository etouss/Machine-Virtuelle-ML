%token <int> INT
%token <bool> BOOL
%token <string> STRING
%token <string> VAR
%token PLUS AND EQ CAT LET LETF IN IF THEN_ ELSE PAIR FST SND UNIT SEMICOLON PRINT COMMA EQEQ SUBS MULT DIV LEQ
%token LPAREN RPAREN
%token EOL
%left EQEQ LEQ PRINT          /* lowest precedence */
%left PLUS SUBS AND COMMA    /* medium precedence */
%left CAT MULT DIV
%start main             /* the entry point */
%type <Ast.expr> main
%%
main:
    tyrme_exp EOL              { $1 }
;
tyrme_exp:
    | INT                        { Ast.Const(Ast.Int $1) }
    | BOOL                       { Ast.Const(Ast.Bool $1) }
    | STRING                     { Ast.Const(Ast.String $1) }
    | UNIT                       { Ast.Const(Ast.Unit) }
    | VAR                        { Ast.Var($1) }

    | LPAREN tyrme_exp COMMA tyrme_exp RPAREN { Ast.Pair($2,$4) }
    | LPAREN tyrme_exp RPAREN    { $2 }

    | IF tyrme_exp THEN_ tyrme_exp ELSE tyrme_exp { Ast.If($2,$4,$6) }

    | LET VAR EQ tyrme_exp IN tyrme_exp       { Ast.Let($2,$4,$6) }
    | LET VAR VAR EQ tyrme_exp IN tyrme_exp   { Ast.Letf($2,$3,$5,$7) }
    | PRINT tyrme_exp SEMICOLON tyrme_exp     { Ast.Print($2,$4) }

    | tyrme_exp COMMA tyrme_exp  { Ast.Pair($1,$3) }

    | FST tyrme_exp              { Ast.Fst($2) }
    | SND tyrme_exp              { Ast.Snd($2) }

    | tyrme_exp PLUS tyrme_exp   { Ast.Binop(Ast.Add,$1,$3) }
    | tyrme_exp SUBS tyrme_exp   { Ast.Binop(Ast.Sub,$1,$3) }
    | tyrme_exp MULT tyrme_exp   { Ast.Binop(Ast.Mult,$1,$3) }
    | tyrme_exp DIV tyrme_exp   { Ast.Binop(Ast.Div,$1,$3) }

    | tyrme_exp AND tyrme_exp    { Ast.Binop(Ast.And,$1,$3) }
    | tyrme_exp EQEQ tyrme_exp     { Ast.Binop(Ast.Eq,$1,$3) }
    | tyrme_exp LEQ tyrme_exp     { Ast.Binop(Ast.Leq,$1,$3) }
    | tyrme_exp CAT tyrme_exp    { Ast.Binop(Ast.Cat,$1,$3) }

    | tyrme_exp tyrme_exp        { Ast.Binop(Ast.App,$1,$2) }
