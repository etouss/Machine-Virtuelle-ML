let mach = 
  (*Le programme myrthe*)
  Let("instrTab",Tableau([Pair(Const(Int(1)),Const(Int(3)))]),
  Let("instrnb",Const(Int(1)),
  Letf("machine_myrthe","param",
    (* La fonction *)    
    If(Binop(Eq,Proj(3,Var("param")),Var("instrnb"))
    (* Halt *)
    ,Proj(2,Var("param")), 
    If(Binop(Eq,Fst(Projvar(Proj(3,Var("param")),Var("instrTab"))),Const(Int(1)))
    (* Const *)
    ,Binop(App,Var("machine_myrthe"),Tableau([Binop(Add,Proj(3,Var("param")),Const(Int(1)));Snd(Projvar(Proj(3,Var("param")),Var("instrTab")));Proj(1,Var("param"));Proj(0,Var("param"))])),
    If(Binop(Eq,Fst(Projvar(Proj(3,Var("param")),Var("instrTab"))),Const(Int(2)))
    (* Push *)
    ,Binop(App,Var("machine_myrthe"),Tableau([Binop(Add,Proj(3,Var("param")),Const(Int(1)));Proj(2,Var("param"));Binop(Add,Proj(1,Var("param")),Const(Int(1)));Binop(Cat,Snd(Projvar(Proj(3,Var("param")),Var("instrTab"))),Proj(0,Var("param")))])),
    If(Binop(Eq,Fst(Projvar(Proj(3,Var("param")),Var("instrTab"))),Const(Int(3)))
    (* Addi *)
    ,Binop(App,Var("machine_myrthe"),Tableau([Binop(Add,Proj(3,Var("param")),Const(Int(1)));Binop(Add,Proj(2,Var("param")),Projvar(Proj(1,Var("param")),Proj(0,Var("param"))));Proj(1,Var("param"));Proj(0,Var("param"))])),
    If(Binop(Eq,Fst(Projvar(Proj(3,Var("param")),Var("instrTab"))),Const(Int(4)))
    (* Andi *)
    ,Binop(App,Var("machine_myrthe"),Tableau([Binop(Mult,Proj(3,Var("param")),Const(Int(1)));Binop(Add,Proj(2,Var("param")),Projvar(Proj(1,Var("param")),Proj(0,Var("param"))));Proj(1,Var("param"));Proj(0,Var("param"))])),
    If(Binop(Eq,Fst(Projvar(Proj(3,Var("param")),Var("instrTab"))),Const(Int(5)))
    (* Andi *)
    ,Binop(App,Var("machine_myrthe"),Tableau([Binop(Eq,Proj(3,Var("param")),Const(Int(1)));Binop(Add,Proj(2,Var("param")),Projvar(Proj(1,Var("param")),Proj(0,Var("param"))));Proj(1,Var("param"));Proj(0,Var("param"))])),
    If(Binop(Eq,Fst(Projvar(Proj(3,Var("param")),Var("instrTab"))),Const(Int(6)))
    (* Pop *)
    ,Binop(App,Var("machine_myrthe"),Tableau([Binop(Add,Proj(3,Var("param")),Const(Int(1)));Proj(2,Var("param"));Binop(Sub,Const(Int(1)),Proj(1,Var("param")));Del(Proj(0,Var("param")))])),
    If(Binop(Eq,Fst(Projvar(Proj(3,Var("param")),Var("instrTab"))),Const(Int(7)))
    (* Branch n *)
    ,Binop(App,Var("machine_myrthe"),Tableau([Binop(Add,Proj(3,Var("param")),Snd(Projvar(Proj(3,Var("param")),Var("instrTab"))));Proj(2,Var("param"));Proj(1,Var("param"));Proj(0,Var("param"))])),
    If(Binop(Eq,Fst(Projvar(Proj(3,Var("param")),Var("instrTab"))),Const(Int(8)))
    (* Branchif n *)
    ,If(Binop(Eq,Const(Int(0)),Proj(2,Var("param"))),Binop(App,Var("machine_myrthe"),Tableau([Binop(Add,Proj(3,Var("param")),Const(Int(1)));Proj(2,Var("param"));Proj(1,Var("param"));Proj(0,Var("param"))])),Binop(App,Var("machine_myrthe"),Tableau([Binop(Add,Proj(3,Var("param")),Snd(Projvar(Proj(3,Var("param")),Var("instrTab"))));Proj(2,Var("param"));Proj(1,Var("param"));Proj(0,Var("param"))]))),
    If(Binop(Eq,Fst(Projvar(Proj(3,Var("param")),Var("instrTab"))),Const(Int(1)))
    (* Acc n *)
    ,Binop(App,Var("machine_myrthe"),Tableau([Binop(Add,Proj(3,Var("param")),Const(Int(1)));Projvar(Binop(Sub,Snd(Projvar(Proj(3,Var("param")),Var("instrTab"))),Proj(1,Var("param"))),Proj(0,Var("param")));Proj(1,Var("param"));Proj(0,Var("param"))])),
    Const(Int(2))

    ))))))))))
  ,
  (*Le main*)
  Binop(App,Var("machine_myrthe"),Tableau[Const(Int(0));Const(Int(-1));Const(Int(-1));Tableau([])])
  )))

let ex_myrthe () =
    print_string (string_of_mot (eval_d (compil (false,empty_env, mach
    )))^"\n");;

(* print_instrs (Array.of_list (compil (false,empty_env,mach)));;  *)


ex_myrthe ();;
