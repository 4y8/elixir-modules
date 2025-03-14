module F1 = Formal1
module F2 = Formal2

let rec trans_behaviour = function
  | [] -> F2.{ bparam = [] ; bbody = [] }
  | F1.BParam (x, l) :: tl ->
     let b = trans_behaviour tl in
     F2.{ b with bparam = (x, l) :: b.bparam }
  | F1.BType (x, l, t) :: tl ->
     let b = trans_behaviour tl in
     F2.{ b with bbody = BType (x, l, t) :: b.bbody }
  | F1.BOpaque (x, l) :: tl ->
     let b = trans_behaviour tl in
     F2.{ b with bbody = BOpaque (x, l) :: b.bbody }
  | F1.BCallback (x, t) :: tl ->
     let b = trans_behaviour tl in
     F2.{ b with bbody = BCallback (x, t) :: b.bbody }

let trans_module env l =
  let rec trans_module b = function
    | [] -> [], Utils.SMap.empty, []
    | F1.MParamE (x, l, t) :: tl ->
       let behaviour = Utils.SMap.find x b in
       let mparam, beh, mbody = trans_module b tl in
       let bmap =
         match Utils.SMap.find_opt behaviour beh with
         | None -> Utils.SMap.empty
         | Some m -> m
       in
       assert (not Utils.SMap.(mem x bmap));
       let bmap = Utils.SMap.add x t bmap in
       mparam, Utils.SMap.add behaviour bmap beh,
       (F2.MType (x, l, t)) :: mbody
    | F1.MParam (x, l) :: tl ->
       let tl = match Utils.SMap.find_opt x b with
         | None -> tl
         | Some _ -> F1.MParamE (x, l, Core.(Expr (Var x))) :: tl
       in
       let mparam, beh, mbody = trans_module b tl in
       (x, l) :: mparam, beh, mbody
    | F1.MType (x, l, t) :: tl ->
       let mparam, beh, mbody = trans_module b tl in
       mparam, beh, F2.MType (x, l, t) :: mbody
    | F1.MOpaque (x, l, t) :: tl ->
       let mparam, beh, mbody = trans_module b tl in
       mparam, beh, F2.MOpaque (x, l, t) :: mbody
    | F1.MBehaviour behaviour :: tl ->
       let b =
         List.fold_left (fun b label ->
             assert (not Utils.SMap.(mem label b));
             Utils.SMap.add label behaviour b)
           b (Utils.SMap.find behaviour env)
       in
       trans_module b tl
    | F1.MDef (p, x, l, t, e) :: tl ->
       let mparam, beh, mbody = trans_module b tl in
       mparam, beh, F2.MDef (p, x, l, t, e) :: mbody
    | F1.MBlk l :: tl ->
       trans_module b (l @ tl)
  in
  let mparam, beh, mbody = trans_module Utils.SMap.empty l in 
  F2.{ mparam; mbody; mbehaviour = Utils.(SMap.map list_of_smap beh) }

let trans_program =
  let rec trans_program env = function
    | [] -> []
    | (x, F1.B l) :: tl ->
       let b = trans_behaviour l in
       let labels = fst @@ List.split b.bparam in
       (x, F2.B b) :: trans_program (Utils.SMap.add x labels env) tl
    | (x, F1.M l) :: tl ->
       let m = trans_module env l in
       (x, F2.M m) :: trans_program env tl
  in
  trans_program Utils.SMap.empty
