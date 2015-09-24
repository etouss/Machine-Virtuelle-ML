(**************************************************************)
(* Language Tyrme: expr, lexing and parsing                   *)
(**************************************************************)

(* La definition de l'arbre syntactique des expression Tyrme se trouve
   dans ast.mli *)
open Ast
open Sys


(* fonction de parsing: prends une expression de Tyrme et retourne
   l'arbre syntactique *)
let parse (s : string) : expr = Parser.main Lexer.token (Lexing.from_string s)


(**************************************************************)
(* Instructions of the MV                                     *)
(**************************************************************)
type op = Add | Sub | Mul | Div | Eqi | Cat

type instr = Halt | Push | Print | Acc of int | Const of int | Pop of int
            | Binop of op | Str of string | BranchIf of int | Branch of int
            | MakeBlock of char * int | GetBlock of int | Closure of int * int
            | Apply |ApplyTerm | Return of int

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
  |MakeBlock(t,n) -> "MakeBlock Tag: "^(Char.escaped t)^" "^(string_of_int n)
  |GetBlock n -> "GetBlock "^(string_of_int n)
  |Closure(n,m) -> "Closure "^(string_of_int n)^" "^(string_of_int m)
  |Apply -> "Apply"
  |Return n -> "Return "^(string_of_int n)
  |ApplyTerm -> "ApplyTerm"
let print_instrs inst_list =
    let rec aux l i = match l with
        | []    -> ()
        | a::b  -> print_string((string_of_int i)); print_string(" -
        ");print_string( string_of_instr a ); print_string "\n" ; aux b (i+1)
in aux (Array.to_list inst_list) 0
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
  |MakeBlock(t,n)   -> out_i8 buf 11; out_i8 buf (int_of_char t); out_i32 buf n
  |Closure(o,n)     -> out_i8 buf 12; out_i32 buf o; out_i32 buf n
  |Binop o          -> out_i8 buf 13; assemble_op buf o
  |Str s            -> let l = String.length s in out_i8 buf 14;out_i32 buf l;
                            assemble_str buf s l 0
  |_->failwith "ApplyTerm"

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
  | Some 8 -> let n = in_i32 buf in disassemble buf (BranchIf(n)::is)
  | Some 9 -> let n = in_i32 buf in disassemble buf (Branch(n)::is)
  | Some 10 -> let n = in_i32 buf in disassemble buf (GetBlock(n)::is)
  | Some 11 -> let t = in_i8 buf in let n = in_i32 buf in
               disassemble buf (MakeBlock((char_of_int t),n)::is)
  | Some 12 -> let n = in_i32 buf in let o = in_i32 buf in
               disassemble buf (Closure(n,o)::is)
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
  | Some 14 -> let n = in_i32 buf in let s = String.make n 'z' in
               for i = 0 to (n-1) do
                  s.[i] <- char_of_int (in_i8 buf)
               done;
               disassemble buf (Str(s)::is)
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

type tag = Tag of char

let string_of_tag  t  = match t with
|Tag(c) -> "Tag :"^string_of_int(int_of_char(c))


type mot =
  | MotInt of int
  | Pointeur of int

type block =
  | BlockString of string
  | BlockTableau of tag * mot array
  | Null

let string_of_mot : mot -> string = function
    | MotInt n              -> "MotInt "^(string_of_int n)
    | Pointeur n            -> "Pointeur "^(string_of_int n)

let string_of_block : block -> string = function
  |BlockString(s) ->"BlockString "^s
  |Null ->"BlockNull"
  |BlockTableau(t,ar) -> String.concat " "(("BlockTab "^string_of_tag t)::(List.map
  string_of_mot (Array.to_list ar)))

(*
    | PointString s         -> "PointString "^s
    | PointBloc (tag, lm)  -> String.concat ("PointBloc "^string_of_tag tag) (List.map string_of_mot lm)
*)

type mv_state = {
  mutable acc: mot;
  code: instr array;
  mutable pc: int; (* indice de l’instruction courante dans code *)
  stack: mot array;
  mutable sp: int; (* indice du sommet de la pile dans stack *)
  heap : block array;
  mutable sh : int;
  mutable avai : int list;
}



let rec in_lheap n lheap res =
  match lheap with
  |[] -> List.rev res
  |p :: l -> (if p = n then (List.rev res)@l
             else if p < n then (List.rev (p::res))@l
             else  in_lheap n l (p::res))

let rec already_traited n traited =
  match traited with
  |[]->false
  |p :: l -> (if p = n then true
             else if p < n then false
             else already_traited n l)

let rec ajout_sort n liste nl = match liste with
    | [] -> List.rev (n::nl)
    | a::b -> if a > n then ajout_sort n b (a::nl)
              else List.rev (n::nl) @ liste

let rec merge_lists l1 l2 l =
  match l1,l2 with
  |[],_ -> (List.rev l)@l2
  |_,[] -> (List.rev l)@l1
  |a1 :: nl1 , a2 :: nl2 -> if a1 > a2 then merge_lists nl1 l2 (a1::l)
                            else merge_lists l1 nl2 (a2::l)

(* let rec already_traited n traited = *)
(*   match traited with *)
(*   |[]->false *)
(*   |p :: l -> (if p = n then true *)
(*              else already_traited n l) *)

let rec garbage_collector lstack lheap_ok lheap mv_state traited =
  match lstack,lheap_ok with
  |[],[] -> lheap
  |cur :: new_lstack,_ -> (match cur with
                         |Pointeur(n) -> if already_traited n traited
                            then
                             garbage_collector new_lstack lheap_ok lheap mv_state traited
                            else
                             garbage_collector new_lstack (n::lheap_ok) (in_lheap n lheap []) mv_state (ajout_sort n traited [])
                         |_ -> garbage_collector new_lstack lheap_ok lheap mv_state traited)
  |[],curb :: new_lheap_ok -> (match mv_state.heap.(curb) with
                         |BlockTableau(_,ar) -> garbage_collector (Array.to_list ar) new_lheap_ok lheap mv_state traited
                         |_ -> garbage_collector lstack new_lheap_ok lheap mv_state traited)

let rec init_list n i l available =
  if n = -1 then []
  else if already_traited i available then init_list n (i+1) l available
  else if i > n then l
  else if i = n then n::l
  else init_list n (i+1) (i::l) available


let rec init_list_stack n i ar l =
  if n = -1 then []
  else if i = n then ((ar.(i))::l)
  else init_list_stack n (i+1) ar ((ar.(i))::l)

let rammasse_miette (s : mv_state) =
  s.avai <- merge_lists (garbage_collector (s.acc::(init_list_stack s.sp 0 s.stack [])) [] (init_list s.sh 0 [] s.avai) s []) s.avai []
(* Alors on va devoir avoir un pile/file qui stock les élément dispo du heap un *)
(* hashset ??  *)
(* Ensuite c'est le unionfind pour remplir le dit hashset et c'est good *)
(* On ajoutera un fuse de liste plus joli plus tard *)


(* retourne l'accumulateur de l'etat donne en argument *)
let get_acc (s : mv_state) : mot =  s.acc


(* Pour construire l'etat de la machine au debut de l'execution *)
let init (c : instr list) : mv_state =
  { code = Array.of_list c;
    stack = Array.make 5000 (MotInt(5242424242)); (* pourquoi 42 ? *)
    heap = Array.make 5000 Null;
    sh = -1;
    pc = 0;
    sp = -1;   (* pourquoi -1 ? *)
    acc = MotInt(6242424242);
    avai = []
}

let get_sh (s : mv_state) : int =
  match s.avai with
  |[] -> (s.sh<-s.sh+1;s.sh)
  |el :: new_avai -> (s.avai<-new_avai;el)


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
   print_string ("Heap:\n");
  (if (s.sh < 0)
   then print_string "<empty>\n"
   else for i = 0 to s.sh do
     if  not (already_traited i s.avai) then
          print_string ("#" ^ (string_of_int (i)) ^ " -> "
                        ^ (string_of_block (s.heap.(i))) ^ "\n")
               done);
  print_string ("Acc = " ^ (string_of_mot s.acc) ^ "\n");
  print_string ("PC = " ^ (string_of_int s.pc) ^ "\n\n")

let concat b1 b2 = match b1,b2 with
  |BlockString(s1),BlockString(s2) -> BlockString(s1^s2)
  |_-> failwith "probleme de type Concat BlockString"

(* La fonction d'execution de la machine *)
let operation (o,m1,m2) heap = match o,m1,m2 with
   |Add,MotInt(n1),MotInt(n2) -> MotInt(n1+n2)
   |Sub,MotInt(n1),MotInt(n2) -> MotInt(n1-n2)
   |Mul,MotInt(n1),MotInt(n2) -> MotInt(n1*n2)
   |Div,MotInt(n1),MotInt(n2) -> MotInt(n1/n2)
   |Eqi,MotInt(n1),MotInt(n2) -> if (n1 = n2) then MotInt(1) else MotInt(0)
   |Cat,Pointeur(n1),Pointeur(n2) -> heap.(n2) <- concat heap.(n1) heap.(n2); Pointeur(n2)
   |_->failwith "non gere2"

let creaTabBlock stack sp n =
  let ar = Array.make n (MotInt(0)) in
  for i = 0 to n-1 do
    ar.(i) <- stack.(sp-i)
  done;ar

let creaTabClos stack sp n =
  let ar = Array.make (n+1) (MotInt(0)) in
  for i = 0 to n-1 do
    ar.(i+1) <- stack.(sp-i)
  done;ar


let updateStackApp stack sp ar pc =
  let first = stack.(sp) in
  let len = Array.length ar in
  stack.(sp)<-pc;
  for i = 1 to len-1 do
     stack.(sp+i)<-ar.(len-i);
  done;stack.(sp+len)<-first;
  match ar.(0) with
  |MotInt(n) -> n
  |_-> failwith "Erreur"

let getArrFromPoint p heap = match p with
|Pointeur(n) -> (match heap.(n) with
                |BlockTableau(_,ar) -> ar
                |_->failwith "Bad pointeur")
|_-> failwith "Dereference value ?"


let machine (s : mv_state) : mv_state =
  let idx = ref 0 in
  begin
    while s.pc < Array.length s.code do
      idx := !idx + 1;
      (* print_string ("=== Step " ^ (string_of_int !idx) ^ " ===\n"); *)
      (* print_string ("With PC = " ^ (string_of_int s.pc) ^ " (" ^ *)
      (* (string_of_instr (s.code.(s.pc))) ^ ")\n"); *)
      begin match s.code.(s.pc) with
            | Const n ->
               s.acc <- MotInt(n)
            | Push ->
               s.sp <- s.sp + 1;
               s.stack.(s.sp) <- s.acc
            | Binop o ->
              s.acc <- operation (o,s.stack.(s.sp),s.acc) s.heap;
              s.sp <- s.sp - 1
            | Pop n ->
               s.sp <- s.sp - n
            | Branch n ->
               s.pc <- s.pc + (n-1)
            | BranchIf n ->
                s.pc <- (match s.acc with
                        |MotInt(0)-> s.pc
                        |_->s.pc+(n-1))
            | Acc n ->
               s.acc <- s.stack.(s.sp-n)
            | Halt ->
                failwith "TODO : Halt Machine (~l.237)"
            | Print -> begin
                        match s.acc with
                        |Pointeur(n) -> (match s.heap.(n) with
                                         |BlockString st -> print_string st
                                         |_->failwith "Bad Pointeur")
                        |_->failwith "Referecence Point ?"
                       end
            | Str st ->
                let sh = get_sh s in
                s.heap.(sh) <- BlockString(st);
                s.acc <- Pointeur(sh)
            | MakeBlock(t,n) ->
                let sh = get_sh s in
                s.heap.(sh) <- BlockTableau(Tag(t),(creaTabBlock s.stack s.sp n));
                s.sp <- s.sp-n;
                s.acc <- Pointeur(sh)
            | GetBlock n ->
                s.acc <-  (getArrFromPoint s.acc s.heap).(n)
            | Closure(n,o) ->
                let sh = get_sh s in
                let ar = creaTabClos s.stack s.sp n in
                ar.(0) <- MotInt(s.pc+o);
                s.heap.(sh) <- BlockTableau(Tag(char_of_int 88),ar);
                s.acc <- Pointeur(sh);
            | Apply ->
                let ar = getArrFromPoint s.acc s.heap in
                s.pc <- (updateStackApp s.stack s.sp ar (MotInt(s.pc+1))) -1 ;
                s.sp <- s.sp+(Array.length ar)
            | ApplyTerm ->
                let ar = getArrFromPoint s.acc s.heap in
                s.stack.(s.sp-2)<-s.stack.(s.sp);
                s.sp<-s.sp-2;
                s.pc <- (match ar.(0) with
                        |MotInt(n) -> n - 1
                        |_-> failwith "Erreur")

            | Return n ->
                s.pc <- (match s.stack.(s.sp-n) with
                        |MotInt(i)->(i-1)
                        |_->failwith "Bad input");
                s.sp <- s.sp-n-1
      end;
      s.pc <- s.pc + 1;
      if !idx mod 1000 = 0 then
        rammasse_miette s;
      (* print_string ("New state is\n"); *)
      (* print_state s; *)
    done;
    (* rammasse_miette s; *)
    print_state s;
    print_string ("\n" ^ (string_of_int !idx) ^ " steps in total\n\n")
  end; s


let machine_debog (s : mv_state) : mv_state =
  let idx = ref 0 in
  begin
    while s.pc < Array.length s.code do
      idx := !idx + 1;
      print_string ("=== Step " ^ (string_of_int !idx) ^ " ===\n");
      print_string ("With PC = " ^ (string_of_int s.pc) ^ " (" ^
      (string_of_instr (s.code.(s.pc))) ^ ")\n");
      begin match s.code.(s.pc) with
            | Const n ->
               s.acc <- MotInt(n)
            | Push ->
               s.sp <- s.sp + 1;
               s.stack.(s.sp) <- s.acc
            | Binop o ->
              s.acc <- operation (o,s.stack.(s.sp),s.acc) s.heap;
              s.sp <- s.sp - 1
            | Pop n ->
               s.sp <- s.sp - n
            | Branch n ->
               s.pc <- s.pc + (n-1)
            | BranchIf n ->
                s.pc <- (match s.acc with
                        |MotInt(0)-> s.pc
                        |_->s.pc+(n-1))
            | Acc n ->
               s.acc <- s.stack.(s.sp-n)
            | Halt ->
                failwith "TODO : Halt Machine (~l.237)"
            | Print -> begin
                        match s.acc with
                        |Pointeur(n) -> (match s.heap.(n) with
                                         |BlockString st -> print_string st
                                         |_->failwith "Bad Pointeur")
                        |_->failwith "Referecence Point ?"
                       end
            | Str st ->
                let sh = get_sh s in
                s.heap.(sh) <- BlockString(st);
                s.acc <- Pointeur(sh)
            | MakeBlock(t,n) ->
                let sh = get_sh s in
                s.heap.(sh) <- BlockTableau(Tag(t),(creaTabBlock s.stack s.sp n));
                s.sp <- s.sp-n;
                s.acc <- Pointeur(sh)
            | GetBlock n ->
                s.acc <-  (getArrFromPoint s.acc s.heap).(n)
            | Closure(n,o) ->
                let sh = get_sh s in
                let ar = creaTabClos s.stack s.sp n in
                ar.(0) <- MotInt(s.pc+o);
                s.heap.(sh) <- BlockTableau(Tag(char_of_int 88),ar);
                s.acc <- Pointeur(sh);
            | Apply ->
                let ar = getArrFromPoint s.acc s.heap in
                s.pc <- (updateStackApp s.stack s.sp ar (MotInt(s.pc+1))) -1 ;
                s.sp <- s.sp+(Array.length ar)
            | ApplyTerm ->
                let ar = getArrFromPoint s.acc s.heap in
                s.stack.(s.sp-2)<-s.stack.(s.sp);
                s.sp<-s.sp-2;
                s.pc <- (match ar.(0) with
                        |MotInt(n) -> n - 1
                        |_-> failwith "Erreur")

            | Return n ->
                s.pc <- (match s.stack.(s.sp-n) with
                        |MotInt(i)->(i-1)
                        |_->failwith "Bad input");
                s.sp <- s.sp-n-1
      end;
      s.pc <- s.pc + 1;
      print_string ("New state is\n");
      print_state s;
      if !idx mod 1000 = 0 then
        rammasse_miette s;
      let _ = read_line () in ();
    done;
    print_string ("\n" ^ (string_of_int !idx) ^ " steps in total\n\n")
  end; s




(* La fonction d'evaluation: retourne l'accumulateur a la fin de l'evaluation *)
let eval (c : instr list) : mot =
  let s = machine (init c) in get_acc s

let eval_d (c : instr list) : mot =
  let s = machine_debog (init c) in get_acc s





  (**************************************************************)
  (* Compilation                                                *)
  (**************************************************************)

  type env = (var * int) list

  let empty_env = []

  let succ (v,i) = (v,i+1)

  let oper : binop -> instr list = function
    | Add -> [Binop(Add)]
    | Sub -> [Binop(Sub)]
    | Mult -> [Binop(Mul)]
    | Div -> [Binop(Div)]
    | Leq -> [Push;Const(1);Binop(Add);Binop(Div);Push;Const(0);Binop(Eqi)]
    | Eq -> [Binop(Eqi)]
    | And -> [Binop(Mul)]
    | Cat -> [Binop(Cat)]
    | _ -> failwith "Apply"
(* "APP plus tard " *)

let rec isTerm : expr * bool * var -> bool = function
  | Var(_),term,_ -> true
  | Const(_),term,_ -> true
  | Binop(App,Var(g),e2),term,f -> if term && (g = f) then false else isTerm(e2,true,f)
  | Binop(_,e1,e2),term,f ->  print_string "here1\n";isTerm (e1,true,f) && isTerm (e2,true,f)
  | If(e1,e2,e3),term, f -> isTerm (e1,term,f) && isTerm (e2,term,f) && isTerm (e3,term,f)
  | Let(_,e1,e2),term, f -> isTerm (e1,term,f) && isTerm (e2,term,f)
  | Letf(_,_,e1,e2),term,f -> isTerm (e1,term,f) && isTerm (e2,term,f)
  | Print(e1,e2),term,f -> isTerm (e1,term,f) && isTerm (e2,term,f)
  | Pair(e1,e2),term,f ->isTerm (e1,term,f) && isTerm (e2,term,f)
  | Fst(e),term,f -> isTerm (e,term,f)
  | Snd(e),term,f -> isTerm (e,term,f)

  (* La fonction de compilation *)
  let rec compil : bool * env * expr -> instr list = function
  | term,env, Var s   -> [Acc (List.assoc s env)]
  | term,env,Const u -> (match u with
                    | ValVar(v)->failwith "no idea"
                    | Int(i) -> [Const i]
                    | Bool(true) -> [Const 1]
                    | Bool(false) -> [Const 0]
                    | String(s) -> [Str s]
                    | Unit -> [])
  | term,env,Binop (o,e1,e2) ->( match term,o with
                       |false,App ->compil (term,env,e2) @
                              [Push] @
                              compil (term,(List.map succ env),e1) @
                              [Apply]
                       |true,App ->compil (term,env,e2) @
                              [Push] @
                              compil (term,(List.map succ env),e1) @
                              [ApplyTerm]
                       | _,_ -> compil (term,env,e1) @
                              [Push] @
                              compil (term,(List.map succ env),e2) @
                              oper o)
  | term,env,If (e1, e2, e3) -> let i2 = compil (term,env,e2) in
                       let i3 = compil (term,env,e3) in
                       compil (term,env,e1) @
                       [BranchIf (2 + List.length i3)] @
                       i3 @
                       [Branch (1 + List.length i2)] @
                       i2
  | term,env, Let (s,e1,e2) ->
                       let new_env = (s, 0) :: (List.map succ env) in
                       compil (term,env, e1) @
                       [Push] @
                       compil (term,new_env, e2) @
                       [Pop 1]

  | term,env, Letf(v1,v2,e1,e2) -> let new_env2 = (v1,0) :: (v2, 1) :: (List.map succ (List.map succ env)) in
                              let new_env = (v1, 0) :: (List.map succ env) in
                              let c_e1 = compil (isTerm(e1,false,v1),new_env2, e1) in
                              [Branch ((List.length c_e1)+3);Push]@
                              c_e1@
                              [Return((List.length env) + 2);Closure((List.length env),(-(List.length c_e1)-2));Push]@
                              compil (term,new_env, e2)@
                              [Pop(1)]
  | term,env, Print(e1,e2) -> compil (term,env, e1)@[Print]@compil (term,env, e2)
  | term,env, Pair(e1,e2) -> compil (term,env, e2)@[Push]@compil (term,env, e1)@[Push;MakeBlock(char_of_int 13,2)]
  | term,env, Fst (e1) -> compil(term,env,e1)@[GetBlock(0)]
  | term,env, Snd (e1) -> compil(term,env,e1)@[GetBlock(1)]


  (* Pour lire le codex *)
  let lire_codex () =
    print_string (string_of_mot (eval (disassemble_filename "codex.tm"))^"\n")

    (* Exemple de compilation qui doit marcher et rendre la valeur 3 *)
  let ex_compil () =
    print_string (string_of_mot (eval_d (compil (false,empty_env, parse
    "let f x = if x == 0 then 8 else  f(x - 1) in f 4")))^"\n");;



(* print_instrs (Array.of_list (compil (false,empty_env, parse "let f x = if x == 0 then 8 else  f(x - 1) in f 4")));;  *)

(*  ex_compil ();;  *)
lire_codex ();;
(* print_instrs (Array.of_list (disassemble_filename "codex.tm"));; *)

