
(**************************************************************)
(* Language Tyrme: expr, lexing and parsing                   *)
(**************************************************************)

(* La definition de l'arbre syntactique des expression Tyrme se trouve
   dans ast.mli *)
open Ast


(* fonction de parsing: prends une expression de Tyrme et retourne
   l'arbre syntactique *)
let parse (s : string) : expr = Parser.main Lexer.token (Lexing.from_string s)


(**************************************************************)
(* Instructions of the MV                                     *)
(**************************************************************)
type op = Add | Sub | Mul | Div | Eqi | Cat

type tag = Tag of char

let rec string_of_tag  tag  = "Print Tag"

type instr = Halt | Push | Print | Acc of int | Const of int | Pop of int
            | Binop of op | Str of string | BranchIf of int | Branch of int
            | MakeBlock of tag * int | GetBlock of int | Closure of int * int
            | Apply | Return of int


let string_of_op : op -> string = function
  |Add -> "Add"
  |Sub -> "Sub"
  |Mul -> "Mul"
  |Div -> "Div"
  |Eqi -> "Eqi"
  |Cat -> "Cat"


let string_of_instr : instr -> string = function
  |Halt -> "Halt"
  |Push -> "Push"
  |Print -> "Print"
  |Acc n -> "Acc "^(string_of_int n)
  |Const n -> "Const "^(string_of_int n)
  |Pop n -> "Pop "^(string_of_int n)
  |BranchIf n -> "BranchIf "^(string_of_int n)
  |Branch n -> "Branch "^(string_of_int n)
  |Str s -> "Str "^s
  |Binop o -> "Binop "^(string_of_op o)
  |MakeBlock(t,n) -> "MakeBlock "^(string_of_tag t)^(string_of_int n)
  |GetBlock n -> "GetBlock "^(string_of_int n)
  |Closure(n,m) -> "Closure "^(string_of_int n)^" "^(string_of_int m)
  |Apply -> "Apply"
  |Return n -> "Return "^(string_of_int n)

(**************************************************************)
(* Asm                                                        *)
(**************************************************************)


(* Fonctions de lecture et d'ecriture d'entier 8bits et 32bits *)
let out_i8  (buf : out_channel) (i : int) : unit = output_char buf (char_of_int i)
let out_i32 (buf : out_channel) (i : int) : unit = output_binary_int buf i

let in_i8   (buf : in_channel) : int = int_of_char (input_char buf)
let in_i32  (buf : in_channel) : int = input_binary_int buf

let rec assemble_str buf s l acc =
  if(acc = l) then ()
  else out_i8 buf (int_of_char s.[acc]); assemble_str buf s l (acc+1)

let assemble_op (buf : out_channel) : op -> unit = function
  |Add -> out_i8 buf 15
  |Sub -> out_i8 buf 16
  |Mul -> out_i8 buf 17
  |Div -> out_i8 buf 18
  |Eqi -> out_i8 buf 19
  |Cat -> out_i8 buf 20

  (* Fonction d'assemblage d'instruction *)
let assemble_instr (buf : out_channel) : instr -> unit = function
  |Halt             -> out_i8 buf 0
  |Push             -> out_i8 buf 1
  |Print            -> out_i8 buf 2
  |Apply            -> out_i8 buf 3
  |Acc n            -> out_i8 buf 4; out_i32 buf n
  |Const n          -> out_i8 buf 5; out_i32 buf n
  |Return n         -> out_i8 buf 6; out_i32 buf n
  |Pop n            -> out_i8 buf 7; out_i32 buf n
  |BranchIf n       -> out_i8 buf 8; out_i32 buf n
  |Branch n         -> out_i8 buf 9; out_i32 buf n
  |GetBlock n       -> out_i8 buf 10; out_i32 buf n
  |MakeBlock(Tag(t),n)   -> out_i8 buf 11; out_i8 buf (int_of_char t); out_i32 buf n
  |Closure(o,n)     -> out_i8 buf 12; out_i32 buf o; out_i32 buf n
  |Binop o          -> out_i8 buf 13; assemble_op buf o
  |Str s            -> let l = String.length s in out_i8 buf 14;out_i32 buf l;
                            assemble_str buf s l 0

  (* Fonction d'assemblage d'une liste d'instructions *)
let rec assemble (buf : out_channel) : instr list -> unit = function
  |i::li -> assemble_instr buf i;assemble buf li
  |[] -> ()


  (* Ecrite pour vous: une fonction d'assemblage qui ecrit dans un fichier *)
let assemble_filename (name : string) (is : instr list) : unit =
  let buf = open_out_bin name in
  begin
    assemble buf is;
    close_out buf
  end





  (* fonction de desassemblage: stub *)
let rec disassemble (buf : in_channel) (is:instr list) : instr list =
  (* Get the next char, and make sure to capture the end of the file *)
  let inc = (try Some (in_i8 buf) with | End_of_file -> None) in
  (* Test if there were a char *)
  match inc with
  | None   -> List.rev is  (* Nope: end of the file *)
  | Some 0 -> disassemble buf (Halt::is)
  | Some 1 -> disassemble buf (Push::is)
  | Some 2 -> disassemble buf (Print::is)
  | Some 3 -> disassemble buf (Apply::is)
  | Some 4 -> let n = in_i32 buf in disassemble buf (Acc(n)::is)
  | Some 5 -> let n = in_i32 buf in disassemble buf (Const(n)::is)
  | Some 6 -> let n = in_i32 buf in disassemble buf (Return(n)::is)
  | Some 7 -> let n = in_i32 buf in disassemble buf (Pop(n)::is)
  | Some 14 -> let n = in_i32 buf in let s = String.make n 'z' in
               for i = 0 to (n-1) do
                  s.[i] <- char_of_int (in_i8 buf)
               done;
               disassemble buf (Str(s)::is)
  | Some 13 -> begin
                match in_i8 buf with
                |15 -> disassemble buf (Binop(Add)::is)
                |16 -> disassemble buf (Binop(Sub)::is)
                |17 -> disassemble buf (Binop(Mul)::is)
                |18 -> disassemble buf (Binop(Div)::is)
                |19 -> disassemble buf (Binop(Eqi)::is)
                |20 -> disassemble buf (Binop(Cat)::is)
                |_ -> failwith "Bad Binop"
              end
  | Some 8 -> let n = in_i32 buf in disassemble buf (BranchIf(n)::is)
  | Some 9 -> let n = in_i32 buf in disassemble buf (Branch(n)::is)
  | Some 10 -> let n = in_i32 buf in disassemble buf (GetBlock(n)::is)
  | Some 11 -> let t = in_i8 buf in let n = in_i32 buf in disassemble
              buf (MakeBlock(Tag(char_of_int t),n)::is)
  | Some 12 -> let o = in_i32 buf in let n = in_i32 buf in  disassemble
              buf (Closure(o,n)::is)
  | _ -> failwith "Bad opCode"



      (* Ecrite pour vous: une fonction de desassemblage qui lit d'un fichier *)
let disassemble_filename (name : string) : instr list =
  let buf = open_in_bin name in
  let insts = disassemble buf [] in
  let _ = close_in buf in
  insts




  (**************************************************************)
  (* Machine virtuelle                                          *)
  (**************************************************************)

type mot =
  | MotInt of int
  | PointString of string
  | PointBloc of (tag * (mot list))

let rec string_of_mot : mot -> string = function
    | MotInt n              -> "MotInt "^(string_of_int n)
    | PointString s         -> "PointString "^s
    | PointBloc (tag, lm)  -> String.concat ("PointBloc "^string_of_tag tag) (List.map string_of_mot lm)


type mv_state = {
  mutable acc: mot;
  code: instr array;
  mutable pc: int; (* indice de l’instruction courante dans code *)
  stack: mot array;
  mutable sp: int; (* indice du sommet de la pile dans stack *)
}


(* retourne l'accumulateur de l'etat donne en argument *)
let get_acc (s : mv_state) : mot =  s.acc


(* Pour construire l'etat de la machine au debut de l'execution *)
let init (c : instr list) : mv_state =
  { code = Array.of_list c;
    stack = Array.make 1000 (MotInt(5242424242)); (* pourquoi 42 ? *)
    pc = 0;
    sp = -1;   (* pourquoi -1 ? *)
    acc = MotInt(6242424242) } (* pourquoi 52 ? *)


(* Peut-etre une fonction d'impression ? *)
(*let print_state (s : mv_state) : unit = failwith "pretty-printing de l'etat de la machine"*)
let print_state s =
  print_string ("Stack:\n");
  (if (s.sp < 0)
   then print_string "<empty>\n"
   else for i = 0 to s.sp do
          print_string ("#" ^ (string_of_int (s.sp - i)) ^ " -> "
                        ^ (string_of_mot (s.stack.(i))) ^ "\n")
        done);
  print_string ("Acc = " ^ (string_of_mot s.acc) ^ "\n");
  print_string ("PC = " ^ (string_of_int s.pc) ^ "\n\n")


(* La fonction d'execution de la machine *)
let operation (o,m1,m2) = match o,m1,m2 with
   |Add,MotInt(n1),MotInt(n2) -> MotInt(n1+n2)
   |Sub,MotInt(n1),MotInt(n2) -> MotInt(n1-n2)
   |Mul,MotInt(n1),MotInt(n2) -> MotInt(n1*n2)
   |Div,MotInt(n1),MotInt(n2) -> MotInt(n1/n2)
   |Eqi,MotInt(n1),MotInt(n2) -> if (n1 = n2) then MotInt(1) else MotInt(0)
   |_->failwith "non gere"


let machine (s : mv_state) : mv_state =
  let idx = ref 0 in
  begin
    while s.pc < Array.length s.code do
      idx := !idx + 1;
      print_string ("=== Step " ^ (string_of_int !idx) ^ " ===\n");
      print_string ("With PC = " ^ (string_of_int s.pc) ^ " (" ^ (string_of_instr (s.code.(s.pc))) ^ ")\n");
      begin match s.code.(s.pc) with
            | Const n ->
               s.acc <- MotInt(n)
            | Push ->
               s.sp <- s.sp + 1;
               s.stack.(s.sp) <- s.acc
            | Binop o ->
                s.acc <- operation (o,s.stack.(s.sp),s.acc)
            | Pop n ->
               s.sp <- s.sp-n
            | Branch n ->
               s.pc <- s.pc + n - 1
            | BranchIf n ->
               s.pc <- s.pc + (if (s.acc = MotInt(0)) then n - 1 else 0);
            | Acc n ->
               s.acc <- s.stack.(s.sp-n)
            | Halt ->
                failwith "TODO : Halt Machine (~l.237)"
            | Print ->
                print_string (string_of_mot s.acc)
            | Str s ->
                failwith "TODO : Str Machine (~l.241)"
      end;
      s.pc <- s.pc + 1;
      print_string ("New state is\n");
      print_state s;
    done;
    print_string ("\n" ^ (string_of_int !idx) ^ " steps in total\n\n")
  end; s


(* La fonction d'evaluation: retourne l'accumulateur a la fin de l'evaluation *)
let eval (c : instr list) : mot =
  let s = machine (init c) in get_acc s




  (**************************************************************)
  (* Compilation                                                *)
  (**************************************************************)

  type env = (var * int) list

  let empty_env = []

  let succ (v,i) = (v,i+1)

  let repr = function
  | Int i      -> i
  | Bool true  -> 1
  | Bool false -> 0
  | _ -> failwith "non gere"


  let oper : binop -> instr = function
    | Add -> Binop(Add)
    | Sub -> Binop(Sub)
    | Mult -> Binop(Mul)
    | Div -> Binop(Div)
    | Leq -> failwith "Non sup"
    | Eq -> Binop(Eqi)
    | And -> failwith "Non sup"
    | Cat -> Binop(Cat)
    | App ->failwith "Non sup"


  (* La fonction de compilation *)
  let rec compil : env * expr -> instr list = function
  | env, Var s   -> [Acc (List.assoc s env)]
  | env, Let (s,e1,e2) ->
             let new_env = (s, 0) :: (List.map succ env) in
             compil (env, e1) @
             [Push] @
             compil (new_env, e2) @
             [Pop 1]
  | env,Const v -> [Const (repr v)]
  | env,Binop (o,e1,e2) -> compil (env,e1) @
                       [Push] @
                       compil ((List.map succ env),e2) @
                       [oper o; Pop 1]
  | env,If (e1, e2, e3) -> let i2 = compil (env,e2) in
                       let i3 = compil (env,e3) in
                       compil (env,e1) @
                       [BranchIf (2 + List.length i3)] @
                       i3 @
                       [Branch (1 + List.length i2)] @
                       i2
  |_ -> failwith "yet to be deal"





  (* Pour lire le codex *)
  let lire_codex () =
    print_string (string_of_mot (eval (disassemble_filename "codex.tm")))

    (* Exemple de compilation qui doit marcher et rendre la valeur 3 *)
  let ex_compil () =
    print_string (string_of_mot (eval (compil (empty_env, parse "let x = 1 in x
    + 2"))));;

disassemble_filename "codex.tm";;
