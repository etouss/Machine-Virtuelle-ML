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
            |Apply
            |ApplyTerm of int (* Fonction Apply de façon terminal *)
            |Return of int
            |GetBlockStack (* permet de recuperer un élément d'un tableau*)
            |DelFromBlock (* permet de supprimer le dernier élément d'un tableau*)

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
  |ApplyTerm n -> "ApplyTerm "^(string_of_int n)
  |GetBlockStack -> "GetBlockStack"
  |DelFromBlock -> "DelFromBlock"

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

  (* Fonction d'assemble pour pour l'instruction Binop*)
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
  |ApplyTerm n      -> out_i8 buf 21; out_i32 buf n
  |GetBlockStack    -> out_i8 buf 22
  |DelFromBlock     -> out_i8 buf 23

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
  | Some 21 -> let n = in_i32 buf in disassemble buf (ApplyTerm(n)::is)
  | Some 22 -> disassemble buf (GetBlockStack::is)
  | Some 23 -> disassemble buf (DelFromBlock::is)
  | _       -> failwith "Bad opCode disassemble"



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

(*type de donnée présente dans le tas*)
type mot =
  | MotInt of int (*represente un int*)
  | Pointeur of int (*place d'un block dans le tas*)

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


(* *)
let rec in_lheap n lheap res =
  match lheap with
  |[] -> List.rev res
  |p :: l -> (if p = n then (List.rev res)@l
             else if p < n then (List.rev (p::res))@l
             else  in_lheap n l (p::res))



(*
    Fonction qui regarde si un morceau de memoire a déjà été traité par le garbage_collector
    Donc si elle est présente dans la liste des éléments déja traité
*)
let rec already_traited n traited =
  match traited with
  |[]->false
  |p :: l -> (if p = n then true
             else already_traited n l)

(*

*)
let rec garbage_collector lstack lheap_ok lheap mv_state traited =
  match lstack,lheap_ok with
  |[],[] -> lheap
  |cur :: new_lstack,_ -> (match cur with
                         |Pointeur(n) -> if already_traited n traited
                            then
                             garbage_collector new_lstack lheap_ok lheap mv_state traited
                            else
                             garbage_collector new_lstack (n::lheap_ok) (in_lheap n lheap []) mv_state (n::traited)
                         |_ -> garbage_collector new_lstack lheap_ok lheap mv_state traited)
  |[],curb :: new_lheap_ok -> (match mv_state.heap.(curb) with
                         |BlockTableau(_,ar) -> garbage_collector (Array.to_list ar) new_lheap_ok lheap mv_state traited
                         |_ -> garbage_collector lstack new_lheap_ok lheap mv_state traited)

(*
    Cette fonction initialiste la liste des place disponible dans la memoire.
*)
let rec init_list n i l available =
  if n = -1 then []
  else if already_traited i available then init_list n (i+1) l available
  else if i > n then l
  else if i = n then n::l
  else init_list n (i+1) (i::l) available

(*

*)
let rec init_list_stack n i ar l =
  if n = -1 then []
  else if i = n then ((ar.(i))::l)
  else init_list_stack n (i+1) ar ((ar.(i))::l)


(*
    Fonction principal du garbage collector
*)
let rammasse_miette (s : mv_state) =
  s.avai <- s.avai @  garbage_collector (s.acc::(init_list_stack s.sp 0 s.stack [])) [] (init_list s.sh 0 [] s.avai) s []


(* retourne l'accumulateur de l'etat donne en argument *)
let get_acc (s : mv_state) : mot =  s.acc


(* Pour construire l'etat de la machine au debut de l'execution *)
let init (c : instr list) : mv_state =
  { code = Array.of_list c;
    stack = Array.make 5000 (MotInt(5242424242)); (* pourquoi 42 ? *)
    heap = Array.make 1000 Null;
    sh = -1;
    pc = 0;
    sp = -1;   (* pourquoi -1 ? *)
    acc = MotInt(6242424242);
    avai = [] (* tableau des places disponible en memoire placé avant notre position actuelle*)
}

(*
    Cette fonction retourne la prochaine place disponible dans la memoire
    Si par defaut elle renvoi la place actuelle +1
    Mais s'il y a une place presente dans avant s.sh donc dans avail elle donne cette place
*)
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


(*
    Concatenation de deux block String
    failwith b1,b2 non BlockString
*)
let concat_string b1 b2 = match b1,b2 with
  |BlockString(s1),BlockString(s2) -> BlockString(s1^s2)
  |_,_-> failwith "probleme de type Concat BlockString"


(*
    Concatenation d'un BlockTableau avec un élément
    failwith b1 non BlockTableau
*)
let concat_tab m2 b1 = match b1 with
  |BlockTableau(t,old_ar) -> let n = ((Array.length old_ar) + 1) in
                               let ar = Array.make (n) (MotInt(0)) in
                               for i = 0 to (n-2) do
                                  ar.(i) <- old_ar.(i)
                               done;
                               ar.(n-1) <- m2;
                               BlockTableau(t,ar)
  |_-> failwith "probleme de type concat BlockTableau"


(*
    La fonction de CAT de tyrme peut concatener des String entre elle
    mais aussi un element avec un tableau par la droite
*)
let cat_operation n1 m heap = match heap.(n1),m with
    |BlockString(s1),Pointeur(p)        -> heap.(n1) <- concat_string (
        match heap.(p) with
            | BlockString(s2) -> heap.(p)
            | _               ->  failwith "cat String with not String"
        ) heap.(n1)
    | BlockTableau(t,old_ar),_ -> heap.(n1) <- concat_tab m heap.(n1)
    | _,_              ->  failwith "cat Wrong type"

(*
    Fonction d'opération des Binop
*)
let operation (o,m1,m2) heap = match o,m1,m2 with
   |Add,MotInt(n1),MotInt(n2) -> MotInt(n1+n2)
   |Sub,MotInt(n1),MotInt(n2) -> MotInt(n1-n2)
   |Mul,MotInt(n1),MotInt(n2) -> MotInt(n1*n2)
   |Div,MotInt(n1),MotInt(n2) -> MotInt(n1/n2) (*Erreur non gérée n2 = 0 et n1,n2 <0 *)
   |Eqi,MotInt(n1),MotInt(n2) -> if (n1 = n2) then MotInt(1) else MotInt(0)
   |Cat,_,Pointeur(n2) -> cat_operation n2 m1  heap; Pointeur(n2)

   |_->failwith "non gere2"


(*
    Fonction de création d'un block
*)
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


(*

*)
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


(*
    Fonction retournant le tableau pointer par le pointeur p
*)
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
                s.pc <- Array.length s.code
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
            | GetBlockStack -> (match s.stack.(s.sp) with
                               |MotInt(n) -> s.acc <- (getArrFromPoint s.acc s.heap).(n)
                               |_ -> failwith "Erreur")
            | DelFromBlock -> (match s.acc with
                               |Pointeur(n)->(match s.heap.(n) with
                                              |BlockTableau(t,ar)-> s.heap.(n) <- BlockTableau(t,(Array.sub ar 0 (Array.length ar - 1)))
                                              |_->failwith "echec")
                               |_->failwith "echec")

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
            | ApplyTerm n ->
                let ar = getArrFromPoint s.acc s.heap in
                let n_param = s.stack.(s.sp) in
                s.sp <- s.sp-n-1;
                s.stack.(s.sp) <- n_param;
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
      if s.sh - (List.length s.avai) == 999
       then rammasse_miette s;
    done;
  end; s

(*
    Fonction machine pour le mode debug
*)
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
            | GetBlockStack -> (match s.stack.(s.sp) with
                               |MotInt(n) -> s.acc <- (getArrFromPoint s.acc s.heap).(n)
                               |_ -> failwith "Erreur")
            | DelFromBlock -> (match s.acc with
                               |Pointeur(n)->(match s.heap.(n) with
                                              |BlockTableau(t,ar)-> s.heap.(n) <- BlockTableau(t,(Array.sub ar 0 (Array.length ar - 1)))
                                              |_->failwith "echec")
                               |_->failwith "echec")

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
            | ApplyTerm n ->
                let ar = getArrFromPoint s.acc s.heap in
                let n_param = s.stack.(s.sp) in
                s.sp <- s.sp-n-1;
                s.stack.(s.sp) <- n_param;
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
      if s.sh - (List.length s.avai) == 999
       then rammasse_miette s;
      let _ = read_line () in ();
    done;
    print_string ("\n" ^ (string_of_int !idx) ^ " steps in total\n\n")
  end; s




(* La fonction d'evaluation: retourne l'accumulateur a la fin de l'evaluation *)
let eval (c : instr list) : mot =
  let s = machine (init c) in get_acc s

(*
    Fonction d'evaluation en mode debug: retourne l'accumulateur a la fin de l'evaluation
*)
let eval_d (c : instr list) : mot =
  let s = machine_debog (init c) in get_acc s



  (*
   * if(a=b) then Const(1)
   * else if(b=0) then (a+1)/a
   * else if(a/b) then (a+1)/a
   * else (b+1)/b
   *
   * (a)
   * Push;  (a::b)
   * Acc(1);Push;Acc(1);Binop(Eqi);BranchIf 27;
   * (1 :: False)
   * Acc(0);Push;Const(0);Binop(Eqi);BranchIf 14;
   * (2::False)
   * Acc(1);Push;Acc(1);Binop(Div);BranchIf 9;
   * (3::False)
   * Acc(0);Push;Const(1);Binop(Add);Push;Acc(1);Binop(Div);Branch 10;
   * (2,3::True)
   * Acc(1);Push;Acc(2);Push;Const(1);Binop(Add);Binop(Div);Branch 2;
   * (1 :: True)
   * Const(1)
   * (Clean)
   * Pop(2);
   *
   *
   * [Push; Acc(1);Push;Acc(1);Binop(Eqi);BranchIf 27;Acc(0);Push;Const(0);Binop(Eqi);BranchIf 14;
   * Acc(1);Push;Acc(1);Binop(Div);BranchIf 9; Acc(0);Push;Const(1);Binop(Add);Push;Acc(1);Binop(Div);Branch 10;
   *  Acc(1);Push;Acc(2);Push;Const(1);Binop(Add);Binop(Div);Branch 2;Const(1); Pop(2)]



  *)


  (**************************************************************)
  (* Compilation                                                *)
  (**************************************************************)

  type env = (var * int) list

  let empty_env = []

  let succ (v,i) = (v,i+1)
  let ante (v,i) = (v,i-1)


  let oper : binop -> instr list = function
    | Add -> [Binop(Add)]
    | Sub -> [Binop(Sub)]
    | Mult -> [Binop(Mul)]
    | Div -> [Binop(Div)]
    | Leq -> (* [Push;Const(1);Binop(Add);Binop(Div);Push;Const(0);Binop(Eqi)] *)
              [Push; Acc(1);Push;Acc(1);Binop(Eqi);BranchIf 27;Acc(0);Push;Const(0);Binop(Eqi);BranchIf 14;
              Acc(1);Push;Acc(1);Binop(Div);BranchIf 9; Acc(0);Push;Const(1);Binop(Add);Push;Acc(1);Binop(Div);Branch 10;
              Acc(1);Push;Acc(2);Push;Const(1);Binop(Add);Binop(Div);Branch 2;Const(1); Pop(2)]
    | Eq -> [Binop(Eqi)]
    | And -> [Binop(Mul)]
    | Cat -> [Binop(Cat)]
    | _ -> failwith "Apply"
(* "APP plus tard " *)


(*
    Permet de determiner si une expression est terminal
*)
let rec isTerm : expr * bool * var -> bool = function
  | Var(_),term,_ -> true
  | Const(_),term,_ -> true
  | Binop(App,Var(g),e2),term,f -> if term && (g = f) then false else isTerm(e2,true,f)
  | Binop(_,e1,e2),term,f ->  (* print_string "here1\n"; *)isTerm (e1,true,f) && isTerm (e2,true,f)
  | If(e1,e2,e3),term, f -> isTerm (e1,term,f) && isTerm (e2,term,f) && isTerm (e3,term,f)
  | Let(_,e1,e2),term, f -> isTerm (e1,term,f) && isTerm (e2,term,f)
  | Letf(_,_,e1,e2),term,f -> isTerm (e1,term,f) && isTerm (e2,term,f)
  | Print(e1,e2),term,f -> isTerm (e1,term,f) && isTerm (e2,term,f)
  | Tableau ([]),term,f -> true
  | Tableau(e::l),term,f -> isTerm(e,term,f) && isTerm(Tableau(l),term,f)
  | Pair(e1,e2),term,f ->isTerm (e1,term,f) && isTerm (e2,term,f)
  | Fst(e),term,f -> isTerm (e,term,f)
  | Snd(e),term,f -> isTerm (e,term,f)
  | Proj(i,e),term,f -> isTerm (e,term,f)
  | Projvar(e1,e2),term,f ->isTerm (e1,term,f) && isTerm (e2,term,f)
  | Del(e),term,f  -> isTerm (e,term,f)

  (* La fonction de compilation *)

(*

*)
let rec compil : bool * int * env * expr -> instr list = function
  | term,nb,env, Var s   -> [Acc (List.assoc s env)]
  | term,nb,env,Const u -> (match u with
                    | ValVar(v)->failwith "no idea"
                    | Int(i) -> [Const i]
                    | Bool(true) -> [Const 1]
                    | Bool(false) -> [Const 0]
                    | String(s) -> [Str s]
                    | Unit -> [])
  | term,nb,env,Binop (o,e1,e2) ->( match term,o with
                       |false,App ->compil (term,nb,env,e2) @
                              [Push] @
                              compil (term,nb,(List.map succ env),e1) @
                              [Apply]
                       |true,App ->compil (term,nb,env,e2) @
                              [Push] @
                              compil (term,nb,(List.map succ env),e1) @
                              [ApplyTerm (nb)]
                       | _,_ -> compil (term,nb,env,e1) @
                              [Push] @
                              compil (term,nb,(List.map succ env),e2) @
                              oper o)
  | term,nb,env,If (e1, e2, e3) -> let i2 = compil (term,nb,env,e2) in
                       let i3 = compil (term,nb,env,e3) in
                       compil (term,nb,env,e1) @
                       [BranchIf (2 + List.length i3)] @
                       i3 @
                       [Branch (1 + List.length i2)] @
                       i2
  | term,nb,env, Let (s,e1,e2) ->
                       let new_env = (s, 0) :: (List.map succ env) in
                       compil (term,nb,env, e1) @
                       [Push] @
                       compil (term,nb,new_env, e2) @
                       [Pop 1]

  | term,nb,env, Letf(v1,v2,e1,e2) -> let new_env2 = (v1,0) :: (v2, 1) :: (List.map succ (List.map succ env)) in
                              let new_env = (v1, 0) :: (List.map succ env) in
                              let c_e1 = compil (isTerm(e1,false,v1),((List.length env) + 4),new_env2, e1) in
                              [Branch ((List.length c_e1)+3);Push]@
                              c_e1@
                              [Return((List.length env) + 2);Closure((List.length env),(-(List.length c_e1)-2));Push]@
                              compil (term,nb,new_env, e2)@
                              [Pop(1)]
  | term,nb,env, Print(e1,e2) -> compil (term,nb,env, e1)@[Print]@compil (term,nb,env, e2)
  | term,nb,env, Pair(e1,e2) -> compil (term,nb,env, e2)@[Push]@compil (term,nb,env, e1)@[Push;MakeBlock(char_of_int 13,2)]
  | term,nb,env, Tableau(l) -> let ref_env = ref (List.map ante env) in
                            (List.concat (List.map (fun e -> ref_env := (List.map succ !ref_env);compil(term,nb,!ref_env,e)@[Push]) l)) @ [MakeBlock(char_of_int 42,List.length l)]
  | term,nb,env, Fst (e1) -> compil(term,nb,env,e1)@[GetBlock(0)]
  | term,nb,env, Snd (e1) -> compil(term,nb,env,e1)@[GetBlock(1)]
  | term,nb,env, Proj (i,e1) -> compil(term,nb,env,e1)@[GetBlock(i)]
  | term,nb,env, Projvar (e1,e2) -> compil(term,nb,env,e1)@[Push]@compil(term,nb,(List.map succ env),e2)@[GetBlockStack;Pop(1)]
  | term,nb,env, Del (e1) -> compil(term,nb,env,e1)@[DelFromBlock]


(*
    Fonction permetant d'éxecuter une liste d'instruction myrte
*)
  let mach list_inst_myrthe=
    (*Le programme myrthe*)
    Let("instrTab",Tableau((list_inst_myrthe)),
    Let("instrnb",Const(Int((List.length (list_inst_myrthe)))),
    Letf("machine_myrthe","param",
      (* La fonction *)
      Let("pc",Proj(3,Var("param")),
      Let("acc",Proj(2,Var("param")),
      Let("sh",Proj(1,Var("param")),
      Let("stack",Proj(0,Var("param")),
      If(Binop(Eq,Var("pc"),Var("instrnb"))
      (* Halt *)
      ,Var("acc"),
      Let("instr",Projvar(Var("pc"),Var("instrTab")),
      If(Binop(Eq,Fst(Var("instr")),Const(Int(1)))
      (* Const *)
      ,Binop(App,Var("machine_myrthe"),Tableau([Binop(Add,Var("pc"),Const(Int(1)));Snd(Var("instr"));Var("sh");Var("stack")])),
      If(Binop(Eq,Fst(Var("instr")),Const(Int(2)))
      (* Push *)
      ,Binop(App,Var("machine_myrthe"),Tableau([Binop(Add,Var("pc"),Const(Int(1)));Var("acc");Binop(Add,Var("sh"),Const(Int(1)));Binop(Cat,Var("acc"),Var("stack"))])),
      If(Binop(Eq,Fst(Var("instr")),Const(Int(3)))
      (* Addi *)
      ,Binop(App,Var("machine_myrthe"),Tableau([Binop(Add,Var("pc"),Const(Int(1)));Binop(Add,Var("acc"),Projvar(Var("sh"),Var("stack")));Var("sh");Var("stack")])),
      If(Binop(Eq,Fst(Var("instr")),Const(Int(4)))
      (* Andi *)
      ,Binop(App,Var("machine_myrthe"),Tableau([Binop(Mult,Var("pc"),Const(Int(1)));Binop(Mult,Var("acc"),Projvar(Var("sh"),Var("stack")));Var("sh");Var("stack")])),
      If(Binop(Eq,Fst(Var("instr")),Const(Int(5)))
      (* Eqi *)
      ,Binop(App,Var("machine_myrthe"),Tableau([Binop(Eq,Var("pc"),Const(Int(1)));Binop(Eq,Var("acc"),Projvar(Var("sh"),Var("stack")));Var("sh");Var("stack")])),
      If(Binop(Eq,Fst(Var("instr")),Const(Int(6)))
      (* Pop *)
      ,Binop(App,Var("machine_myrthe"),Tableau([Binop(Add,Var("pc"),Const(Int(1)));Var("acc");Binop(Sub,Var("sh"),Const(Int(1)));Del(Var("stack"))])),
      If(Binop(Eq,Fst(Var("instr")),Const(Int(7)))
      (* Branch n *)
      ,Binop(App,Var("machine_myrthe"),Tableau([Binop(Add,Var("pc"),Snd(Var("instr")));Var("acc");Var("sh");Var("stack")])),
      If(Binop(Eq,Fst(Var("instr")),Const(Int(8)))
      (* Branchif n *)
      ,If(Binop(Eq,Const(Int(0)),Var("acc")),Binop(App,Var("machine_myrthe"),Tableau([Binop(Add,Var("pc"),Const(Int(1)));Var("acc");Var("sh");Var("stack")])),Binop(App,Var("machine_myrthe"),Tableau([Binop(Add,Var("pc"),Snd(Var("instr")));Var("acc");Var("sh");Var("stack")]))),
      If(Binop(Eq,Fst(Var("instr")),Const(Int(9)))
      (* Acc n *)
      ,Binop(App,Var("machine_myrthe"),Tableau([Binop(Add,Var("pc"),Const(Int(1)));Projvar(Binop(Sub,Var("sh"),Snd(Var("instr"))),Var("stack"));Var("sh");Var("stack")])),
      Const(Int(2))

      )))))))))))))))
    ,
    (*Le main*)
    Binop(App,Var("machine_myrthe"),Tableau[Const(Int(0));Const(Int(-1));Const(Int(-1));Tableau([])])
    )))

(*
    Liste des instruction de Myrte
*)
  type instr_myrthe = Push | Consti of int | Addi | Eqi | Andi | Pop | BranchIf of int |
  Branch of int | Acc of int


(*
    Fonction de disassemblage de Myrte
    Sans l'instruction Andi
*)
  let rec disassemble_myrthe (buf : in_channel) (is:instr_myrthe list) :instr_myrthe list =
    (* Get the next char, and make sure to capture the end of the file *)
    let inc = (try Some (in_i8 buf) with | End_of_file -> None) in
    (* Test if there were a char *)
    match inc with
    | None   -> List.rev is  (* Nope: end of the file *)
    | Some 0 -> disassemble_myrthe buf (Push::is)
    | Some 1 -> disassemble_myrthe buf (Addi::is)
    | Some 2 -> disassemble_myrthe buf (Eqi::is)
    | Some 5 -> disassemble_myrthe buf (Pop::is)
    | Some n  when (n mod 8 = 4) -> disassemble_myrthe buf (Consti (n lsr 3)::is)
    | Some n  when (n mod 8 = 6) -> disassemble_myrthe buf (BranchIf (n lsr 3)::is)
    | Some n  when (n mod 8 = 7) -> disassemble_myrthe buf (Branch (n lsr 3)::is)
    | Some n  when (n mod 8 = 3) -> disassemble_myrthe buf (Acc (n lsr 3)::is)
    | _ -> failwith "invalid byte-code"

(*
    Fonction de disassemblage à partir d'un fichier
*)
  let disassemble_myrthe_filename (name : string) : instr_myrthe list =
    let buf = open_in_bin name in
    let insts = disassemble_myrthe buf [] in
    let _ = close_in buf in
    insts


(*
    Conversion des instruction de myrte pour l'éxécution dans la mmachine myrte de tyrme
*)
  let rec converter list_inst_myrthe conv =
    match list_inst_myrthe with
    |[] -> conv
    |i::li -> (match i with
               |Push -> converter li (Pair(Const(Int(2)),Const(Int(0)))::conv)
               |Consti n -> converter li (Pair(Const(Int(1)),Const(Int(n)))::conv)
               |Addi -> converter li (Pair(Const(Int(3)),Const(Int(0)))::conv)
               |Pop -> converter li (Pair(Const(Int(6)),Const(Int(0)))::conv)
               |Andi -> converter li (Pair(Const(Int(4)),Const(Int(0)))::conv)
               |Eqi -> converter li (Pair(Const(Int(5)),Const(Int(0)))::conv)
               |BranchIf n -> converter li (Pair(Const(Int(8)),Const(Int(n)))::conv)
               |Branch n -> converter li (Pair(Const(Int(7)),Const(Int(n)))::conv)
               |Acc n -> converter li (Pair(Const(Int(9)),Const(Int(n)))::conv)
              )

let read_file file_name =
    let lines = ref [] in
    let chan = open_in file_name in
    try
      while true; do
        lines := input_line chan :: !lines
      done; []
    with End_of_file -> close_in chan;
                        List.rev (!lines)

let eval_file_tyrme file_name =
    print_string (file_name^" ::: "^(string_of_mot (eval_d (disassemble_filename file_name))) ^ "\n")

let eval_file_myrthe file_name =
    print_string (file_name^" ::: "^(string_of_mot (eval(compil (false,0,empty_env, mach (converter
        (disassemble_myrthe_filename file_name) [])))))^"\n");;

let comp_ass_desa_exec_for_fun file_name =
    let instr = compil (false,0,empty_env, parse (String.concat "\n" (read_file file_name) )) in
      assemble_filename "tmp" instr;
      eval_file_tyrme "tmp";;




eval_file_tyrme  "codex.tm";;
comp_ass_desa_exec_for_fun "Example/ex.ty";;
comp_ass_desa_exec_for_fun "Example/leq.ty";;
eval_file_myrthe "Example/ex1.my";;
eval_file_myrthe "Example/ex2.my";;
eval_file_myrthe "Example/ex3.my";;
eval_file_myrthe "Example/ex4.my";;
eval_file_myrthe "Example/ex5.my";;
