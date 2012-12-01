module type PROB_MATRIX =
sig
  exception Nonexistent
  exception SumNotOne
  exception NotMatrix
  exception CantMultiply
  exception Negative
  exception MultElt
  exception Addition
  exception DivNum
  
  type matrix
  
  val empty : matrix  
  val get_prob : matrix -> int -> int -> float
  val create_matrix : float list list -> matrix
  val mult_matrix : matrix -> matrix -> matrix
  val mult_num : matrix -> matrix -> matrix
  val num_rows : matrix -> int
  val num_cols : matrix -> int
  val add_row_a : matrix -> matrix -> matrix
  val add_row_b : matrix -> matrix -> matrix
  val get_row: matrix -> int -> matrix
  val get_col: matrix -> int -> matrix
  val transpose: matrix -> matrix
  val createnot: float list list -> matrix
  val remove_last: matrix -> matrix
  val mult_make : matrix -> matrix -> matrix
  val mult_num_one: matrix -> float -> matrix
  val mult_elt:  matrix -> matrix -> matrix
  val div_num:  matrix -> matrix -> matrix
  val sum_rows: matrix -> matrix
  val addition: matrix list -> matrix
  val length : matrix -> int
  val test_mod: unit
end

module ProbMatrix : PROB_MATRIX =
struct
  exception Nonexistent
  exception SumNotOne
  exception NotMatrix
  exception CantMultiply
  exception Negative
  exception MultElt
  exception Addition
  exception DivNum

  type matrix = float list list
  let empty = []
  let num_rows (m:matrix): int = 
    List.length m
  let num_cols (m: matrix): int = 
    List.length (List.hd m)
  let get_prob (mat: matrix) (row: int) (col: int): float  =
   if (row <= (num_rows mat) && col <= (num_cols mat)) 
   then List.nth (List.nth mat (row-1)) (col-1) 
   else (raise Nonexistent)
  let create_matrix (fll: float list list): matrix = 
    let checksum (lst: float list): bool =
      let sum =  abs_float (1. -. (List.fold_left (fun a b -> a +. b) 0. lst)) 
      in (sum < 0.01) in
    let ressum = List.fold_left ( && ) true (List.map checksum fll) in
    let base = List.length (List.hd fll) in
    let resnum = List.fold_left (fun x r -> r = base && x) true 
       (List.map List.length fll) in
    let respos = List.fold_left ( && ) true (List.map (fun x -> (x >= 0.)) 
      (List.concat fll)) in 
    if not ressum then raise SumNotOne 
    else if not resnum then raise NotMatrix 
    else if not respos then raise Negative 
    else fll 

  let createnot lst = lst  
  let help_add (row:matrix) (mat:matrix) (f:matrix->matrix->matrix) : matrix =
    if (mat = [] || (num_cols mat = num_cols row && num_rows row = 1)) then
    match mat with
      | [] -> row
      | m_rows -> f m_rows row
    else raise NotMatrix

  let add_row_a (row : matrix) (mat : matrix) : matrix =
    help_add row mat (fun a b -> List.append b a)
 
  let add_row_b (row : matrix) (mat : matrix) : matrix =
    help_add row mat (List.append)  

  let get (lst : 'a list) (i : int) = List.nth lst (i-1)

  let get_row (m : matrix) (j : int) = [(get m j)]

  let get_col (m : matrix) (k : int) = List.map (fun lst -> [(get lst k)]) m 
  
  let mult_matrix (mat1: matrix) (mat2: matrix): matrix =
    let run m1 m2 =
      (let getcol (submat: matrix) (colnum:int): float list =
      List.map (fun x -> List.nth x (colnum-1) ) submat in
      let rcmult (row: float list) (col: float list): float =
        List.fold_left2 (fun a r c -> a +. (r *. c)  ) 0. row col in
      let cols = num_cols m2 in
      let rows = num_rows m1 in
      let makerow n1 = 
        let row = List.nth m1 (n1-1) in
        let rec build n2 =
          if n2 = 0 then [] else (rcmult row (getcol m2 n2))::(build (n2-1)) in
        List.rev (build cols) in
      let rec all n =
        if n = 0 then [] else (makerow n)::(all (n-1)) in
      List.rev (all rows)) in
    if (num_rows mat2) <> (num_cols mat1) then raise CantMultiply 
    else run mat1 mat2

  let mult_num m1 m2 = 
    let col = List.concat m2 in
    if (List.length col) = (List.length (List.hd m1)) then
    List.map (List.map2 (fun x y -> x *. y) col) m1
    else raise Addition

  let remove_last mat =
    List.rev (List.tl (List.rev mat))

  let rec transpose mat = 
    (List.map (List.hd) mat)::(if ((List.length (List.hd mat)) = 1) then [] 
    else transpose (List.map (List.tl) mat))

  let mult_num_one (m: matrix) (x: float) : matrix =
    List.map (List.map (( *. ) x)) m

  let mult_make (row1 : matrix) (row2 : matrix) : matrix =
    List.map (fun x -> List.hd (mult_num_one row2 x)) (List.hd row1)

  let mult_elt (m1: matrix) (m2: matrix) : matrix =
    if (List.length m1) = (List.length m2) then
      List.map2 (List.map2 ( *. )) m1 m2
    else raise MultElt

  let div_num (m1: matrix) (m2: matrix) : matrix =
    if (List.length m1) = (List.length m2) then
      List.map2 (fun x y -> List.map (fun z -> z /. (List.hd y)) x ) m1 m2
    else raise DivNum

  let sum_rows (m: matrix) : matrix =
    List.map (fun x-> [List.fold_left (+.) 0. x]) m

  let addition mat_lst =
    if (mat_lst = []) then [] else
   (let base = mult_num_one (List.hd mat_lst) 0. in
    let add_two = 
      List.map2 (List.map2 (+.)) in
    List.fold_left (add_two) (base) mat_lst)

  let length m = List.length m
end

module type MARKOV_DATA =
sig
  exception InitFail

  type matrix
  type model

  val get_num_states: model -> int
  val get_num_obs: model -> int 
  val get_start_prob: model -> matrix
  val get_trans_prob: model -> matrix
  val get_emiss_prob: model -> matrix
  val get_prob: matrix -> int -> int -> float
  val empty: matrix
  val create_matrix : float list list -> matrix
  val init: int -> int -> matrix -> matrix -> matrix -> model
  val mult_matrix : matrix -> matrix -> matrix
  val mult_num : matrix -> matrix -> matrix
  val get_row : matrix -> int -> matrix
  val get_col : matrix -> int -> matrix
  val add_row_a : matrix -> matrix -> matrix  
  val add_row_b : matrix -> matrix -> matrix
  val transpose: matrix -> matrix
  val num_rows: matrix -> int
  val num_cols: matrix -> int
  val createnot: float list list -> matrix
  val remove_last: matrix -> matrix
  val mult_make: matrix -> matrix -> matrix
  val mult_num_one: matrix -> float -> matrix
  val mult_elt:  matrix -> matrix -> matrix
  val div_num:  matrix -> matrix -> matrix
  val sum_rows: matrix -> matrix
  val addition: matrix list -> matrix
  val length: matrix -> int  
end

module Data (M : PROB_MATRIX) : (MARKOV_DATA with type matrix = M.matrix) =
struct
  exception InitFail
  type matrix = M.matrix
  type model = {num_states: int; num_obs: int; start_prob: matrix; 
                trans_prob: matrix; emiss_prob: matrix}

  let get_num_states (m:model) = m.num_states
  let get_num_obs (m:model) = m.num_obs
  let get_start_prob (m:model) = m.start_prob
  let get_trans_prob (m:model) = m.trans_prob
  let get_emiss_prob (m:model) = m.emiss_prob
  let get_num_states (m:model) = m.num_states

  let get_prob = M.get_prob
  let empty = M.empty
  let create_matrix = M.create_matrix

  let init st obs m1 m2 m3 =
    if ((M.num_rows m1, M.num_cols m1, M.num_rows m2, M.num_cols m2,
        M.num_rows m3, M.num_cols m3) = (1,st,st,st,st,obs))
    then {num_states = st; num_obs = obs; start_prob = m1; trans_prob = m2;
          emiss_prob = m3}
    else raise InitFail

  let mult_matrix = M.mult_matrix
  let mult_num = M.mult_num
  let get_row = M.get_row
  let get_col = M.get_col
  let add_row_a = M.add_row_a
  let add_row_b = M.add_row_b
  let transpose = M.transpose
  let num_rows = M.num_rows
  let num_cols = M.num_cols
  let createnot = M.createnot
  let remove_last = M.remove_last
  let mult_make = M.mult_make 
  let mult_num_one = M.mult_num_one 
  let mult_elt = M.mult_elt 
  let div_num = M.div_num
  let sum_rows = M.sum_rows 
  let addition = M.addition  
  let length = M.length 
end

module type HMM =
sig
  exception Invalid
  type markov
  type matrix 

  val get_prob: matrix -> int -> int -> float
  val create : int -> int -> matrix -> matrix -> matrix -> markov
  val viterbi: int list -> markov -> float * int list
  val forward: int list -> markov -> matrix
  val backward: int list -> markov -> matrix
  val probability: matrix -> float
  val probability2: int list -> markov -> float
  val create_matrix : float list list -> matrix
  val createnot : float list list -> matrix
  val baumwelch : int list list -> markov -> markov
  val get_prob_bw_t: markov -> int -> int -> float 
  val get_prob_bw_e: markov -> int -> int -> float 
  val get_prob_bw_s: markov -> int -> int -> float 
end

module HiddenMarkov (D: MARKOV_DATA) : (HMM with type markov = D.model) =
struct
  exception Invalid

  type markov = D.model
  type matrix = D.matrix
  let create = D.init
  let get_prob = D.get_prob
  let viterbi (obs: int list) (m: markov) = 
    let rec rechelp z acclist  = 
       match acclist with
         |[] -> acclist
         |(a,(hd::tl))::tl2 -> ((((D.get_prob (D.get_trans_prob m) hd z) *. a), 
          ((z::(hd::tl))))::(rechelp z tl2)) 
         |_ -> raise Invalid  in 
    let max (lst) = List.fold_left 
      (fun r x -> let (a, b) = r in let (c, d) = x in if c > a then x else r) 
      (0., []) lst  in
    let rec genlst (n: int) (nlst: int list) = 
      if n < 1 then [] 
      else if n = 1 then nlst 
      else (genlst (n - 1) (((List.hd nlst) + 1)::(nlst))) in
    let rec mu (n:int) (zlst: int list) (xlst: int list) 
               (accumulatorlist: (float*int list) list) = 
      if n = 1 then
        mu 2 zlst xlst (List.map  
        (fun a -> ((D.get_prob (D.get_emiss_prob m) 
        a (List.hd xlst))*.(D.get_prob (D.get_start_prob m) 1 a), (a::[]))) 
        zlst) 
      else if n <= (List.length xlst) then 
        (mu (n + 1) zlst xlst 
        (List.map (fun a -> 
          let (pr,pa) = max (rechelp a accumulatorlist) in
          let obs = List.nth xlst (n-1) in
          (pr *. (D.get_prob (D.get_emiss_prob m) a obs),pa))
         zlst)) 
      else accumulatorlist in 
    let best = 
       max (mu 1 (genlst (D.get_num_states m) [1]) obs []) in
    let (prob, pth) = best in (prob, List.rev pth)

  let forward (obs : int list) (m : markov) = 
    let rec helper (obs2: int list) (mat: matrix) (row: matrix) : matrix =
       match obs2 with
	 | [] -> mat
	 | hd::tl -> 
           (let row2 = D.mult_num (D.mult_matrix row (D.get_trans_prob m)) 
                                 (D.get_col (D.get_emiss_prob m) hd) in 
           helper tl (D.add_row_b row2 mat) row2) in 
    match obs with
      | [] -> D.empty
      | hd::tl -> 
	(let row_one = D.mult_num (D.get_start_prob m) 
                                 (D.get_col (D.get_emiss_prob m) hd) in 
        helper tl row_one row_one)
  
  let backward (obs : int list) (m : markov) =
    let rec gen_ones (n: int) : float list =
      if n = 0 then [] else (1. :: (gen_ones (n - 1))) in
    let last_row = D.createnot [(gen_ones (D.get_num_states m))] in
    let rec gen_rows (obs2:int list) (mat:matrix) (prev_row:matrix) : matrix =
      match obs2 with
	| [] -> raise Invalid
	| _ :: [] -> mat
	| hd :: tl -> (let row = D.mult_matrix 
                        (D.mult_num prev_row (D.get_col (D.get_emiss_prob m)
                         hd)) 
                        (D.transpose (D.get_trans_prob m)) in
		      gen_rows tl (D.add_row_a row mat) row) in
    gen_rows (List.rev obs) last_row last_row

  let probability (alphas: matrix) = 
    let row = D.get_row alphas (D.num_rows alphas) in
    let nc = D.num_cols alphas in
    let rec add rw num sum =
      if num = 0 then sum 
      else add rw (num - 1) ((D.get_prob rw 1 num) +. sum) in
    add row nc 0.

  let probability2 (obs: int list) (m: markov) =
    let mf = forward obs m in
    let bf = backward obs m in
    let col = D.transpose (D.remove_last bf) in
    let row = D.get_row mf 1 in
    let prod = D.mult_num row col in
    let nc = D.num_cols prod in
    let rec add rw num sum =
      if num = 0 then sum 
      else add rw (num - 1) ((D.get_prob rw 1 num) +. sum) in
    add prod nc 0.

  let create_matrix = D.create_matrix
  let createnot = D.createnot

  let rec list_loc lst num count locations : int list =
    match lst with
      | []-> locations
      | h::t -> if (h = num) then list_loc t num (count+1) (count::locations)
		else list_loc t num (count+1) locations

  let rec list_list_loc lst num count locations : (int*int) list =
    match lst with
      | [] -> locations
      | h::t -> (let x = List.rev (List.map (fun x -> (count, x)) 
        (list_loc h num 1 [])) in list_list_loc t num (count+1) (locations @ x))

  let finder (obs: int list list) (n: int) : (int*int) list list=
    let rec helper count =
      (if count > n then []
       else (list_list_loc obs count 1 []) :: (helper (count+1))) in
  helper 1

  let o_helper (o1 : int) (o2 : int) (i :int) (a: matrix) (b: matrix)
  (hmm : markov) (prob : float) : matrix =
    let new_beta : matrix =
      D.mult_num (D.get_row b (i+1)) (D.get_col (D.get_emiss_prob hmm) o2) in
    let alpha_beta : matrix =
      D.mult_make (D.get_row a i) new_beta in
      D.mult_num_one (D.mult_elt alpha_beta (D.get_trans_prob hmm)) (1. /. prob)

  let gammas (obs: int list) (hmm : markov) : (matrix * matrix list) =
    let (alpha, beta) = (forward obs hmm, backward obs hmm) in
    let prob = probability alpha in
    let gamma_e : matrix = 
      D.mult_elt alpha (D.mult_num_one beta (1. /. prob)) in
    let gamma_t  : matrix list =
      let rec collector (os : int list) (i : int) : matrix list =
         match os with
           | o1 :: o2 :: tl -> (o_helper o1 o2 i alpha beta hmm prob) :: 
             (collector (o2 :: tl) (i+1)) 
           | _ -> []
      in collector obs 1
    in (gamma_e, gamma_t)

  let gammas_all (obs_lst: int list list) (hmm : markov) 
  : (matrix list * matrix list list) =
    List.fold_left (fun acc o -> let (e,t) = gammas o hmm in 
    let (es, ts) = acc in (es @ [e], ts @ [t])) ([], []) obs_lst

  let lengths (obs_lst : int list list) (hmm : markov) : matrix =
    let (x, y) = gammas_all obs_lst hmm in
    let (gammas_e, gammas_t) = (x, List.flatten y) in 
    (D.get_row (List.nth gammas_e 1) 3)

  let expected (obs_lst : int list list) (hmm: markov) 
  : matrix * matrix * matrix =
    let (x,y) = gammas_all obs_lst hmm in
    let (gammas_e, gammas_t) = (x, List.flatten y) in
    let obs_loc : (int * int) list list = finder obs_lst (D.get_num_obs hmm) in
    let exp_e_helper lst = 
     D.addition (List.map (fun x -> let (a,b) = x in 
     (D.get_row (List.nth gammas_e (a - 1)) b)) lst) in
    let rec gen_zeroes (n: int) : float list =
      if n = 0 then [] else (0. :: (gen_zeroes (n - 1))) in
    let helper_helper elt =
      if elt = [] then D.createnot [gen_zeroes (D.get_num_states hmm)] 
      else exp_e_helper elt in
    let expected_e : matrix = 
      D.transpose (List.fold_left (fun r x -> D.add_row_b (helper_helper x) r) 
      D.empty obs_loc) in
    let expected_t : matrix =
      D.addition gammas_t in
    let expected_s : matrix =
      D.addition (List.map (fun x -> D.get_row x 1) gammas_e)
    in (expected_s, expected_t, expected_e)

  let helper (obs_lst : int list list) (hmm: markov) 
  : matrix * matrix * matrix =
    let (exp_s, exp_t, exp_e)  = expected obs_lst hmm in
    let fract (m: matrix) : matrix  =
      let sum = D.sum_rows m in D.div_num m sum
    in (fract exp_s, fract exp_t, fract exp_e) 

  let baumwelch (obs_lst : int list list) (hmm : markov) : markov =
    let (m1, m2, m3) = helper obs_lst hmm in
    create (D.get_num_states hmm) (D.get_num_obs hmm) m1 m2 m3

  let get_prob_bw_t (hmm: markov) (r: int) (c: int) = 
    D.get_prob (D.get_trans_prob hmm) r c 

  let get_prob_bw_e (hmm: markov) (r: int) (c: int) = 
    D.get_prob (D.get_emiss_prob hmm) r c 

 let get_prob_bw_s (hmm: markov) (r: int) (c: int) = 
    D.get_prob (D.get_start_prob hmm) r c 
end

module A = Data(ProbMatrix)
module B = HiddenMarkov(A)
