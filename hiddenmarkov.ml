(* Matrix module  *)

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
  
  (* most likely will be a float list list *)
  type matrix
  
  val empty : matrix
  
  (* takes two ints (row and column) and returns the probability, checking that
   * they exist for the given matrix--throws exception otherwise *)
  val get_prob : matrix -> int -> int -> float

  (* includes helper function that checks the representation invariant of the
   * matrix intended to be created and returns exception if it fails *)
  val create_matrix : float list list -> matrix
  
  (* standard matrix multiplication...
   *
   * NOTE: checks first to see if the number of columns in the first matrix
   * matches the number of rows in the second matrix *)
  val mult_matrix : matrix -> matrix -> matrix

  (* kind of like distributive multiplication between a matrix and a column
   * vector (n x 1 matrix)...i.e.
   *                       
   *              [[7]; 
   *  [[1;2;3];    [8];     [[1*7;2*8;3*9]
   *   [4;5;6]] *  [9]]  =   [4*7;5*8;6*9]]
   *)
  val mult_num : matrix -> matrix -> matrix

  (* gives the number of rows in the matrix *)
  val num_rows : matrix -> int

  (* gives the number of columns in the matrix *)
  val num_cols : matrix -> int

  (* adds a new top row to a matrix *)
  val add_row_a : matrix -> matrix -> matrix

  (* adds a new bottom row to a matrix *)
  val add_row_b : matrix -> matrix -> matrix

  (* get a row from a matrix, where the int is the index *)
  val get_row: matrix -> int -> matrix

  (* get a column from a matrix, where the int is the index *)
  val get_col: matrix -> int -> matrix

  (* gives the transpose of a matrix *)
  val transpose: matrix -> matrix

  (* creates a matrix without checking invariants *)
  val createnot: float list list -> matrix
 
  (* removes the last row of the matrix *)
  val remove_last: matrix -> matrix
  
  (* similar to creating a times table...given two row matrices, i.e.
   *
   *  [1;2;3] and [2;3;4]
   *                
   *               [[2;3;4];
   *  should give   [4;6;8];
   *                [6;9;12]]
   *)
  val mult_make : matrix -> matrix -> matrix

  (* multiplies all elements of a matrix by the same float number *)
  val mult_num_one: matrix -> float -> matrix

  (* multiplies the elements in the same corresponding positions in two 
   * matrices, i.e. 
   *     [[3;2];          [[4;5];                  [[12;10];
   *     [1;4]]   and     [3;2]]    should give    [3;8]]
   *)
  val mult_elt:  matrix -> matrix -> matrix

  (* divides all the elements in a row by the same corresponding element of a
   * column matrix, i.e.
   *       [[2;4;6];             [[2];                  [[1;2;3];
   *        [6;9;12]]     and     [3]]   should give     [2;3;4]]
   *)
  val div_num:  matrix -> matrix -> matrix

  (* sums up all of the elements of the rows of a matrix and gives a column 
   * matrix, i.e.
   *        [[2;4;3];              [[9];
   *         [1;3;6]]    gives      [10]]
   *)
  val sum_rows: matrix -> matrix

  (* adds up a list of matrices *)
  val addition: matrix list -> matrix

  (* TESTER *)
  val length : matrix -> int

  (* runs unit tests for the above functions *)
  val test_mod: unit

end

module ProbMatrix : PROB_MATRIX =
struct
  
  (* Each exception thrown will correspond later to some impossibility
   * without needing to use options. *)
  exception Nonexistent
  exception SumNotOne
  exception NotMatrix
  exception CantMultiply
  exception Negative
  exception MultElt
  exception Addition
  exception DivNum


  (* Our "matrices" will really just be float list lists with each float list
   * inside being one row of the matrix. *)
  type matrix = float list list

  (* Empty matrix is an empty list. *)
  let empty = []

  (* The number of rows is equal to the length of the outer list because each
   * inner list is a row. *)
  let num_rows (m:matrix): int = 
    List.length m

  (* Since each row should have the same number of columns (see create matrix)
   * we can pull the first row out of the matrix and then take its length. *)
  let num_cols (m: matrix): int = 
    List.length (List.hd m)

  (* Here, we aim to return a given value of a matrix, and to do so we first
   * check to make sure the given indices are within the matrix and then 
   * extract the appropriate row then column otherwise returning
   * Nonexistent to say that it is not in the matrix. *)
  let get_prob (mat: matrix) (row: int) (col: int): float  =
   if (row <= (num_rows mat) && col <= (num_cols mat)) 
   then List.nth (List.nth mat (row-1)) (col-1) 
   else (raise Nonexistent)

  (* This function simply rebrands a float list list as a matrix. 
   * To dos so it checks various things and at the end throws any necessary
   * exceptions. *)
  let create_matrix (fll: float list list): matrix = 
    (* This section iterates over each row to check that the sum is within 0.01
     * (due to float imprecision) of 1 since that must be true if they
     * represent probabilities. *)
    let checksum (lst: float list): bool =
      let sum =  abs_float (1. -. (List.fold_left (fun a b -> a +. b) 0. lst)) 
      in (sum < 0.01) in
    let ressum = List.fold_left ( && ) true (List.map checksum fll) in
    (* This section checks to see that each row has the same number of columns
     * since this must be true for a matrix. *)
    let base = List.length (List.hd fll) in
    let resnum = List.fold_left (fun x r -> r = base && x) true 
       (List.map List.length fll) in
    (* This section checks that each entry is positive which should be true
     * if they represent probabilities. *)
    let respos = List.fold_left ( && ) true (List.map (fun x -> (x >= 0.)) 
      (List.concat fll)) in 
    if not ressum then raise SumNotOne 
    else if not resnum then raise NotMatrix 
    else if not respos then raise Negative 
    else fll 

  (* creates a matrix that doesn't necessarily follow the rep invariant *)
  let createnot lst = lst  

  (* This is a helper function meant to abstract away the process used in
   * add_row_a and add_row_b. *)
  let help_add (row:matrix) (mat:matrix) (f:matrix->matrix->matrix) : matrix =
    if (mat = [] || (num_cols mat = num_cols row && num_rows row = 1)) then
    match mat with
      | [] -> row
      | m_rows -> f m_rows row
    else raise NotMatrix

  (* This adds a row to the top of a matrix. *)
  let add_row_a (row : matrix) (mat : matrix) : matrix =
    help_add row mat (fun a b -> List.append b a)
 
  (* This adds a row to the bottom of a matrix. *)
  let add_row_b (row : matrix) (mat : matrix) : matrix =
    help_add row mat (List.append)  

  (* We're assuming here that for the user, the index of the first row/column
   * is 1. Get is just a function that corrects List.nth for its 0 indexing. *)
  let get (lst : 'a list) (i : int) = List.nth lst (i-1)

  (* get_row is essentially List.nth on our matrix without 0 indexing. *)
  let get_row (m : matrix) (j : int) = [(get m j)]

  (* get_col runs get on every row to extract a column matrix. *)
  let get_col (m : matrix) (k : int) = List.map (fun lst -> [(get lst k)]) m 
  
  (* This mildly complicated function simply multiplies two matrices. *)
  let mult_matrix (mat1: matrix) (mat2: matrix): matrix =
    (* run wraps the functionality in another function to allow for raising
     * exceptions without running the entirety of the code. *)
    let run m1 m2 =
      (* getcol gets a column as a float list by mapping over the matrix *)
      (let getcol (submat: matrix) (colnum:int): float list =
      List.map (fun x -> List.nth x (colnum-1) ) submat in
      (* rcmult multiplies a row and column and sums up the result *) 
      let rcmult (row: float list) (col: float list): float =
        List.fold_left2 (fun a r c -> a +. (r *. c)  ) 0. row col in
      (* cols and rows are self-explanatory. *)
      let cols = num_cols m2 in
      let rows = num_rows m1 in
      (* makerow takes an int and returns the corresponding row in the product
       * matrix using rcmult and getcol in the typical matrix multiplication
       * method.  *)
      let makerow n1 = 
        let row = List.nth m1 (n1-1) in
        (* For n2>0 returns the value of rcmult'ing the given row and column and
         * attaches that to the result of the same operation for the previous
         * column. *)
        let rec build n2 =
          if n2 = 0 then [] else (rcmult row (getcol m2 n2))::(build (n2-1)) in
        List.rev (build cols) in
      (* all uses makerow to make every row in the product matrix. *)
      let rec all n =
        if n = 0 then [] else (makerow n)::(all (n-1)) in
      List.rev (all rows)) in
    (* If the columns of 2 and rows of 1 are different in number then the 2
     * matrices simply cannot be multiplied, otherwise the above is run. *)
    if (num_rows mat2) <> (num_cols mat1) then raise CantMultiply 
    else run mat1 mat2

  (* kind of like distributive multiplication between a matrix and a column
   * vector (n x 1 matrix)...i.e.
   *                       
   *              [[7]; 
   *  [[1;2;3];    [8];     [[1*7;2*8;3*9]
   *   [4;5;6]] *  [9]]  =   [4*7;5*8;6*9]]
   *)
  let mult_num m1 m2 = 
    let col = List.concat m2 in
    if (List.length col) = (List.length (List.hd m1)) then
    List.map (List.map2 (fun x y -> x *. y) col) m1
    else raise Addition

  let remove_last mat =
    List.rev (List.tl (List.rev mat))

  (* This transposes the given matrix by extracting the first column, making
   * it a row, and so on with each successive column until they have all been
   * used. *)
  let rec transpose mat = 
    (List.map (List.hd) mat)::(if ((List.length (List.hd mat)) = 1) then [] 
    else transpose (List.map (List.tl) mat))


  (* multiplies each element of the matrix by the same float *)
  let mult_num_one (m: matrix) (x: float) : matrix =
    List.map (List.map (( *. ) x)) m

  (* similar to a multiplication table *)
  let mult_make (row1 : matrix) (row2 : matrix) : matrix =
    List.map (fun x -> List.hd (mult_num_one row2 x)) (List.hd row1)

  (* multiplies corresponding elements of two matrices *)
  let mult_elt (m1: matrix) (m2: matrix) : matrix =
    if (List.length m1) = (List.length m2) then
      List.map2 (List.map2 ( *. )) m1 m2
    else raise MultElt

  (* kind of like mult_num but matrix rows are all multiplied by the same 
   * number, rather than columns *)
  let div_num (m1: matrix) (m2: matrix) : matrix =
    if (List.length m1) = (List.length m2) then
      List.map2 (fun x y -> List.map (fun z -> z /. (List.hd y)) x ) m1 m2
    else raise DivNum

  (* sums all rows of a matrix *)
  let sum_rows (m: matrix) : matrix =
    List.map (fun x-> [List.fold_left (+.) 0. x]) m

  (* list of matrices addition *)
  let addition mat_lst =
    if (mat_lst = []) then [] else
   (let base = mult_num_one (List.hd mat_lst) 0. in
    let add_two = 
      List.map2 (List.map2 (+.)) in
    List.fold_left (add_two) (base) mat_lst)

  let length m = List.length m
  
  let test_mod = 
    let create_non_prob (fll: float list list): matrix = fll in
    let same m1 m2 = 
      List.fold_left2 (fun x r1 r2 -> x && (List.fold_left2 
      (fun x2 y1 y2 -> x2 && 
      ((abs_float (y2 -. y1)) < 0.001)) true r1 r2)) true m1 m2 in
    let ident = create_matrix [[1.;0.];[0.;1.]] in
    let mat23 = create_matrix [[0.8;0.1;0.1];[0.1;0.4;0.5]] in
    let mat32 = create_matrix [[0.2;0.8];[0.5;0.5];[0.4;0.6]] in
    let mat11 = create_matrix [[1.]] in
    let mat13 = create_matrix [[0.2;0.6;0.2]] in
    let mat31 = create_matrix [[1.];[1.];[1.]] in 
    let fail1 = try create_matrix [[1.];[]] with SumNotOne -> empty in
    let fail2 = try create_matrix [[1.];[1.;0.]] with NotMatrix -> empty in
    let fail3 = try create_matrix [[-1.;2.]] with Negative -> empty in
    let fail4 = try get_prob ident 3 1 with Nonexistent -> -1. in
    let fail5 = try add_row_a mat31 mat13 with NotMatrix -> empty in
    let fail6 = try add_row_b mat31 mat11 with NotMatrix -> empty in
    assert(fail1 = empty);
    assert(fail2 = empty);
    assert(fail3 = empty);
    assert(fail4 = -1.);
    assert(fail5 = empty);
    assert(fail6 = empty);
    assert(num_rows ident = 2);
    assert(num_cols ident = 2);
    assert(num_rows mat23 = 2);
    assert(num_cols mat23 = 3);
    assert(num_rows mat32 = 3);
    assert(num_cols mat32 = 2);
    assert(num_rows mat11 = 1);
    assert(num_cols mat11 = 1);
    assert(num_rows mat13 = 1);
    assert(num_cols mat13 = 3);
    assert(num_rows mat31 = 3);
    assert(num_cols mat31 = 1);
    assert(get_prob ident 1 1 = 1.);
    assert(get_prob mat31 3 1 = 1.);
    assert(get_prob mat23 2 2 = 0.4);
    assert(get_prob mat13 1 2 = 0.6);
    assert(add_row_a mat13 mat23 = create_matrix [[0.2;0.6;0.2];[0.8;0.1;0.1];
    [0.1;0.4;0.5]]);
    assert(add_row_b mat13 mat23 = create_matrix [[0.8;0.1;0.1];[0.1;0.4;0.5];
    [0.2;0.6;0.2]]);
    assert(mult_num mat23 mat31 = mat23);
    assert(get_row mat32 3 = create_matrix [[0.4;0.6]]);
    assert(get_row mat13 1 = mat13);
    assert(get_col mat31 1 = mat31);
    assert(same (mult_matrix mat23 mat32) (create_non_prob [[0.25;0.75];
    [0.42;0.58]]));
    assert(same (mult_matrix mat31 mat11) mat31);
    assert(transpose mat23 = create_non_prob [[0.8;0.1];[0.1;0.4];[0.1;0.5]]);
    assert(transpose mat32 = create_non_prob [[0.2;0.5;0.4];[0.8;0.5;0.6]]);
    () 
end


(* REPRESENTATION INVARIANT: probabilities in each row must sum to 1, and
 * the number of elements is the same in each row *)

(* A functor that takes in a PROB_MATRIX module *)
module type MARKOV_DATA =
sig
  exception InitFail

  (* a matrix *)
  type matrix

  (* a record that contains all the below values:
   *   num_states: int (j)
   *   num_obs: int (k)
   *   start_prob: matrix (1 x j)
   *   trans_prob: matrix (j x j)
   *      NOTE: the entry in row 1, col 3 should be the probability of moving
   *            to state 3 from state 1
   *   emiss_prob: matrix (j x k)
   *)
  type model
  
  (* functions for getting the matrices and data out of the model record *)
  val get_num_states: model -> int

  val get_num_obs: model -> int 

  val get_start_prob: model -> matrix
 
  val get_trans_prob: model -> matrix

  val get_emiss_prob: model -> matrix

  (* takes two ints (row and column) and returns the probability, checking that
   * they exist for the given matrix--throws exception otherwise *)
  val get_prob: matrix -> int -> int -> float

  val empty: matrix
  
  (* creates a matrix; includes helper function that checks the representation 
   * invariant of the matrix intended to be created and returns exception if 
   * it fails *)
  val create_matrix : float list list -> matrix

  (* initializes a markov model--runs the num_rows and num_cols functions from
   * PROB_MATRIX to check that they equal num_states and num_obs *)
  val init: int -> int -> matrix -> matrix -> matrix -> model
  
  (* standard matrix multiplication...
   *
   * NOTE: checks first to see if the number of columns in the first matrix
   * matches the number of rows in the second matrix *)
  val mult_matrix : matrix -> matrix -> matrix
  
  (* kind of like distributive multiplication between a matrix and a column
   * vector (n x 1 matrix)...i.e.
   *                       
   *              [[7]; 
   *  [[1;2;3];    [8];     [[1*7;2*8;3*9]
   *   [4;5;6]] *  [9]]  =   [4*7;5*8;6*9]]
   *)
  val mult_num : matrix -> matrix -> matrix

  (* get a row from a matrix, where the int is the index *)
  val get_row : matrix -> int -> matrix
 
  (* get a column from a matrix, where the int is the index *)
  val get_col : matrix -> int -> matrix

   (* adds a new top row to a matrix *)
  val add_row_a : matrix -> matrix -> matrix  

  (* adds a new bottom row to a matrix *)
  val add_row_b : matrix -> matrix -> matrix
 
  (* gives the transpose of a matrix *)
  val transpose: matrix -> matrix

  (* gives the number of rows in the matrix *)
  val num_rows: matrix -> int

  (* gives the number of columns in the matrix*)
  val num_cols: matrix -> int

  (* creates a matrix without checking invariants *)
  val createnot: float list list -> matrix

   (* removes the last row of the matrix *)
  val remove_last: matrix -> matrix

  (* makes a new matrix like a multiplication table *)
  val mult_make: matrix -> matrix -> matrix

  (* multiplies all elements of a matrix by the same number *)
  val mult_num_one: matrix -> float -> matrix

  (* multiplies corresponding elements of two matrices *)
  val mult_elt:  matrix -> matrix -> matrix

  (* divides all rows by the same respective number *)
  val div_num:  matrix -> matrix -> matrix

  (* sums all rows to give a column matrix *)
  val sum_rows: matrix -> matrix

  (* adds up all matrices in a list *)
  val addition: matrix list -> matrix

  (* TESTER *)
  val length: matrix -> int
  
end



module Data (M : PROB_MATRIX) : (MARKOV_DATA with type matrix = M.matrix) =
struct

  (* exception raised when invalid arguments are provided to create a model *)
  exception InitFail
  type matrix = M.matrix

  (* record that contains hidden Markov model information *)
  type model = {num_states: int; num_obs: int; start_prob: matrix; 
                trans_prob: matrix; emiss_prob: matrix}

  (* functions for pulling out the parts of a model *)
  let get_num_states (m:model) = m.num_states

  let get_num_obs (m:model) = m.num_obs

  let get_start_prob (m:model) = m.start_prob

  let get_trans_prob (m:model) = m.trans_prob

  let get_emiss_prob (m:model) = m.emiss_prob
  
  let get_num_states (m:model) = m.num_states


  (* function to get a probability out of a matrix of probs (inherited) *)
  let get_prob = M.get_prob
  
  (* empty matrix (inherited) *)
  let empty = M.empty

  (* function that takes in data and creates a matrix (inherited) *)
  let create_matrix = M.create_matrix

  (* checks that valid arguments are provided to the function, then creates 
   * a model to contain all the information of the hidden Markov model if so 
   * (else raises an exception) *)
  let init st obs m1 m2 m3 =
    if ((M.num_rows m1, M.num_cols m1, M.num_rows m2, M.num_cols m2,
        M.num_rows m3, M.num_cols m3) = (1,st,st,st,st,obs))
    then {num_states = st; num_obs = obs; start_prob = m1; trans_prob = m2;
          emiss_prob = m3}
    else raise InitFail


  (* matrix operations all inherited from PROB_MATRIX that is passed in *)  
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

(* a functor that takes in the MARKOV_DATA module as input *)
module type HMM =
sig
  exception Invalid
  
  (* the same as type model from MARKOV_DATA*)
  type markov

  (* matrix type will also be taken from MARKOV_DATA (which takes it 
   * from PROB_MATRIX) *)
  type matrix 

  (* gets a probability out of a matrix of probabilities when given row and 
   * column (same as earlier get_prob) *)
  val get_prob: matrix -> int -> int -> float

  (* creates a markov type *)
  val create : int -> int -> matrix -> matrix -> matrix -> markov
 
  (* runs the Viterbi algorithm, taking in an int list that represents a series
   * of observations *)
  val viterbi: int list -> markov -> float * int list
  
  (* runs the forward algorithm, taking in an int list that represents a series
   * of observations *)  
  val forward: int list -> markov -> matrix

  (* runs the backward algorithm, taking in an int list that represents a series
   * of observations *)
  val backward: int list -> markov -> matrix

  (* determines the probability of the occurence of a series of observations
   * given the alpha values *)
  val probability: matrix -> float
 
  (* creates a different method for determining probabilities *)
  val probability2: int list -> markov -> float
  
  (* creates a matrix *)
  val create_matrix : float list list -> matrix
 
  (* creates a non-representation invariant version of a matrix *)
  val createnot : float list list -> matrix

  (* runs the Baum Welch EM algorithm *)
  val baumwelch : int list list -> markov -> markov
 
  (* val helper : int list list -> markov -> (matrix * matrix * matrix) 
 
  val expected : int list list -> markov -> (matrix * matrix * matrix)

  val gammas_all : int list list -> markov -> (matrix list * matrix list list)

  val gammas: int list -> markov -> (matrix * matrix list)

  val o_helper: int -> int -> int -> matrix -> matrix -> markov -> float -> matrix

  val lengths : int list list -> markov -> matrix *)

  val get_prob_bw_t: markov -> int -> int -> float 

  val get_prob_bw_e: markov -> int -> int -> float 

  val get_prob_bw_s: markov -> int -> int -> float 

end



module HiddenMarkov (D: MARKOV_DATA) : (HMM with type markov = D.model) =
struct
  (* exception raised when an invalid operation is performed in the helper 
   * functions *)
  exception Invalid
  
  (* markov type is renamed model type from MARKOV_DATA (with matrices showing 
   * transition probabilties, emission probabilities, etc. *)
  type markov = D.model

  (* matrix type inherited from MARKOV_DATA *)
  type matrix = D.matrix

  (* create function creates a new markov data structure *)
  let create = D.init

  (* pulls a probability out of a matrix of probabilities (inherited) *)
  let get_prob = D.get_prob

  (* runs the Viterbi algorithm using several helper functions. inputs are a 
   * list of observed states (given as ints) and a markov model, which includes
   * the number of possible hidden states, the number of possible observed
   * states, the probabilities of transitioning between hidden states, the 
   * probabilities of initially starting at each hidden state, and the 
   * probabilites of emitting an observed state given a hidden state. 
   * throughout, a "path" is an int list that represents a list of possible 
   * hidden states. *)
  let viterbi (obs: int list) (m: markov) = 
    (* a helper function that calculates the probability of each previous path 
     * being extended to the new hidden state (z), when given that z and the 
     * list of (probability, path) tuples *)
    let rec rechelp z acclist  = 
       match acclist with
         |[] -> acclist
         |(a,(hd::tl))::tl2 -> ((((D.get_prob (D.get_trans_prob m) hd z) *. a), 
          ((z::(hd::tl))))::(rechelp z tl2)) 
         |_ -> raise Invalid  in 
    (* takes a list (of probabilities and paths) and returns the prob, path 
     * tuple with the highest probability *)
    let max (lst) = List.fold_left 
      (fun r x -> let (a, b) = r in let (c, d) = x in if c > a then x else r) 
      (0., []) lst  in
    (* generates list of ints from 1 to n, inclusive, when given a base  
     * nlst of [1] *)
    let rec genlst (n: int) (nlst: int list) = 
      if n < 1 then [] 
      else if n = 1 then nlst 
      else (genlst (n - 1) (((List.hd nlst) + 1)::(nlst))) in
    (* takes an int (to keep track of place along the list of observed states), 
     * the list of possible hidden states (zlst), the list of actual observed 
     * states (xlst), and the accumulator list (accumulatorlist) of tuples of 
     * probabilities associated with a given path. at the base case, the 
     * function calculates the initial probability of getting each hidden
     * state, and then multiplies that by the probability of getting the 
     * actual first observed state given the hidden state. for the other cases,
     * the max and rechelp functions are used to extend the paths one more step
     * until the end of the observed states is reached.  *) 
    let rec mu (n:int) (zlst: int list) (xlst: int list) 
               (accumulatorlist: (float*int list) list) = 
      if n = 1 then
        mu 2 zlst xlst (List.map  
        (fun a -> ((D.get_prob (D.get_emiss_prob m) 
        a (List.hd xlst))*.(D.get_prob (D.get_start_prob m) 1 a), (a::[]))) 
        zlst) 
        (* accumulator is now: List.map over list of zs: Probability of 
        x1 given za * initial probability of za; this is the base case *)
      else if n <= (List.length xlst) then 
        (mu (n + 1) zlst xlst 
        (List.map (fun a -> 
          let (pr,pa) = max (rechelp a accumulatorlist) in
          let obs = List.nth xlst (n-1) in
          (pr *. (D.get_prob (D.get_emiss_prob m) a obs),pa))
         zlst)) 
      else accumulatorlist in 
    (* uses the helper functions to find the most likely path *)
    let best = 
       max (mu 1 (genlst (D.get_num_states m) [1]) obs []) in
    (* discards the associated probability and returns only the most likely 
     * path *)
    let (prob, pth) = best in (prob, List.rev pth)

  (* Computes the so-called "alpha values" for a given sequence of observations
   * based on the Markov data structure put into the function. In and of itself,
   * it is not really useful, but it is used below to compute probabilities.
   * The alpha values are stored in rows for each successive iteration, and the
   * columns correspond to the hidden states. *) 
  let forward (obs : int list) (m : markov) = 
    (* This helper function computes the matrix that results from multiplying
     * the appropriate transmission probability and emission probability 
     * sections for a given observation, and it does this for each observation
     * in the list. *)
    let rec helper (obs2: int list) (mat: matrix) (row: matrix) : matrix =
       match obs2 with
	 | [] -> mat
	 | hd::tl -> 
           (let row2 = D.mult_num (D.mult_matrix row (D.get_trans_prob m)) 
                                 (D.get_col (D.get_emiss_prob m) hd) in 
           helper tl (D.add_row_b row2 mat) row2) in 
    (* This matches on the observation list to first compute the appropriate
     * starting point using the starting probabilities, and then it uses the
     * above helper function to compute the matrix for the rest of the list. *)
    match obs with
      | [] -> D.empty
      | hd::tl -> 
	(let row_one = D.mult_num (D.get_start_prob m) 
                                 (D.get_col (D.get_emiss_prob m) hd) in 
        helper tl row_one row_one)
  
  (* Computes the so-called "beta values" for a given sequence of observations
   * based on the Markov data structure put into the function. In and of itself,
   * it is not really useful, but it is used below to compute probabilities.
   * The beta values are stored in a single row, and the columns correspond to 
   * the hidden states. *)
  let backward (obs : int list) (m : markov) =
    (* This helper simply generates a row of ones of the given size. *)
    let rec gen_ones (n: int) : float list =
      if n = 0 then [] else (1. :: (gen_ones (n - 1))) in
    (* This creates a non-probability matrix filled with ones corresponding to
     * the number of hidden states to be used as a multiplier. *)
    let last_row = D.createnot [(gen_ones (D.get_num_states m))] in
    (* This matches on a given list of observations in order to multiply the 
     * appropriate emission probabilities by the transposition of the transition
     * probabilities. *)
    let rec gen_rows (obs2:int list) (mat:matrix) (prev_row:matrix) : matrix =
      match obs2 with
	| [] -> raise Invalid
	| _ :: [] -> mat
	| hd :: tl -> (let row = D.mult_matrix 
                        (D.mult_num prev_row (D.get_col (D.get_emiss_prob m)
                         hd)) 
                        (D.transpose (D.get_trans_prob m)) in
		      gen_rows tl (D.add_row_a row mat) row) in
    (* This runs the computation of the beta values, removing the last row which
     * is the leftover entirely ones row. *)
    gen_rows (List.rev obs) last_row last_row

 (* Probability computes using the forward algorithm the probability of 
   * observing a given sequence of observations. *)
  let probability (alphas: matrix) = 
    (* This extracts the final row of the forward result, and the next line 
     * gives the number of columns. *)
    let row = D.get_row alphas (D.num_rows alphas) in
    let nc = D.num_cols alphas in
    (* This simply adds the values of a given row. *)
    let rec add rw num sum =
      if num = 0 then sum 
      else add rw (num - 1) ((D.get_prob rw 1 num) +. sum) in
    (* This starts with a sum of 0 and adds the value in the final row of the 
     * forward result to give the probability. *)
    add row nc 0.

  (* Probability2 computes using the forward and backward algorithms the 
   * probability of observing a given sequence of observations. *)
  let probability2 (obs: int list) (m: markov) =
    (* These lines run the forward and backward algorithms. *)
    let mf = forward obs m in
    let bf = backward obs m in
    (* These lines extract the necessary row and column for the computation. *)
    let col = D.transpose (D.remove_last bf) in
    let row = D.get_row mf 1 in
    (* This multiplies the row and column. *)
    let prod = D.mult_num row col in
    (* This gets the number of columns to add. *)
    let nc = D.num_cols prod in
    (* This simply adds the values of a given row. *)
    let rec add rw num sum =
      if num = 0 then sum 
      else add rw (num - 1) ((D.get_prob rw 1 num) +. sum) in
    (* This uses the above to sum the entries in the row to get the 
     * probability. *)
    add prod nc 0.

  (* creates a matrix (inherited) *)
  let create_matrix = D.create_matrix

  let createnot = D.createnot


  (* given a list and an int, tells you which element # that int is (indexed at
   * first element position = 1) *)
  let rec list_loc lst num count locations : int list =
    match lst with
      | []-> locations
      | h::t -> if (h = num) then list_loc t num (count+1) (count::locations)
		else list_loc t num (count+1) locations

  (* given a list list, tells you where a num is in that list *)
  let rec list_list_loc lst num count locations : (int*int) list =
    match lst with
      | [] -> locations
      | h::t -> (let x = List.rev (List.map (fun x -> (count, x)) 
        (list_loc h num 1 [])) in list_list_loc t num (count+1) (locations @ x))

  (* given a list list of observations, return a list list of tuples of the 
   * locations of each observation # *)
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

  (* maybe use streams if time so we can control the number of iterations this 
   * function is run through *)
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
    (* should be a nxk matrix *)
    let expected_e : matrix = 
      D.transpose (List.fold_left (fun r x -> D.add_row_b (helper_helper x) r) 
      D.empty obs_loc) in
    (* should be a nxn matrix *)
    let expected_t : matrix =
      D.addition gammas_t in
    (* if there are n possible hidden states, this is a 1xn matrix *)
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

(* tests using CS 181 notes
 * the observations are: 1= Rainy, 2= Not Rainy
 * ths hidden states are: 1= gloomy, 2= less gloomy *)
let test =
  let tprob = B.create_matrix [[0.8;0.2];[0.1;0.9]] in
  let initprob = B.create_matrix [[0.7; 0.3]] in
  let eprob = B.create_matrix [[0.75; 0.25]; [0.4; 0.6]] in
  let mark = B.create 2 2 initprob tprob eprob in
  let (vitprob, viterbires) = B.viterbi [2;2;1] mark in
  let forwardres = B.forward [1;1] mark in
  let backwardres = B.backward [1;1] mark in
  let prob11 = B.probability forwardres in
  let prob12 = B.probability (B.forward [1;2] mark) in
  let prob13 = B.probability (B.forward [2;1] mark) in
  let prob14 = B.probability (B.forward [2;2] mark) in
  let prob21 = B.probability2 [1;1] mark in
  let prob22 = B.probability2 [1;2] mark in
  let prob23 = B.probability2 [2;1] mark in
  let prob24 = B.probability2 [2;2] mark in
  let equal a b = 
    (abs_float (a -. b)) < 0.001 in
  (* ensure both probability methods match *)
  assert(equal prob11 prob21);
  assert(equal prob12 prob22);
  assert(equal prob13 prob23);
  assert(equal prob14 prob24);
  assert(equal prob11 0.4092);
  (* ensure the four possible combinations sum to one *)
  assert(equal (prob11+.prob12+.prob13+.prob14) 1.);
  assert(equal (prob21+.prob22+.prob23+.prob24) 1.);
  (* ensure the proper sequence of hidden values *)
  assert(viterbires = [2;2;2]);
  (* ensure correct alpha and beta values *)
  assert(equal (B.get_prob forwardres 1 1) 0.525);
  assert(equal (B.get_prob forwardres 1 2) 0.12);
  assert(equal (B.get_prob forwardres 2 1) 0.324);
  assert(equal (B.get_prob forwardres 2 2) 0.0852);
  assert(equal (B.get_prob backwardres 1 1) 0.68);
  assert(equal (B.get_prob backwardres 1 2) 0.435)

(* Tests using the Viterbi page on Wikipedia as an example.
 * http://en.wikipedia.org/wiki/Viterbi_algorithm#Example *)
let test2 =
  let tprob = B.create_matrix [[0.7;0.3];[0.4;0.6]] in
  let initprob = B.create_matrix [[0.6; 0.4]] in
  let eprob = B.create_matrix [[0.5; 0.4; 0.1]; [0.1; 0.3; 0.6]] in
  let mark = B.create 2 3 initprob tprob eprob in
  let (vitprob, viterbires) = B.viterbi [3;2;1] mark in
  let prob11 = B.probability (B.forward [1;1] mark) in
  let prob12 = B.probability (B.forward [1;2] mark) in
  let prob13 = B.probability (B.forward [2;1] mark) in
  let prob14 = B.probability (B.forward [2;2] mark) in
  let prob21 = B.probability2 [1;1] mark in
  let prob22 = B.probability2 [1;2] mark in
  let prob23 = B.probability2 [2;1] mark in
  let prob24 = B.probability2 [2;2] mark in
  let equal a b = 
    (abs_float (a -. b)) < 0.001 in
  assert(equal prob11 prob21);
  assert(equal prob12 prob22);
  assert(equal prob13 prob23);
  assert(equal prob14 prob24);
  assert(equal vitprob 0.01344);
  assert(viterbires = [2;1;1])
 
(* Tests using the Viterbi example at
 * http://www.comp.leeds.ac.uk/roger/HiddenMarkovModels/
   html_dev/viterbi_algorithm/s3_pg3.html *) 
let test3 =
  let tprob = 
    B.createnot [[0.5;0.25;0.25];[0.375;0.125;0.375];[0.125;0.675;0.375]] in
  let initprob = B.create_matrix [[0.63; 0.17; 0.2]] in
  let eprob = 
    B.create_matrix [[0.6; 0.2; 0.15; 0.05]; [0.25; 0.25; 0.25; 0.25]; 
                     [0.05; 0.1; 0.35; 0.5]] in
  let mark = B.create 3 4 initprob tprob eprob in
  let (vitprob, viterbires) = B.viterbi [1;3;4] mark in
  let prob11 = B.probability (B.forward [1;1] mark) in
  let prob12 = B.probability (B.forward [1;2] mark) in
  let prob13 = B.probability (B.forward [2;1] mark) in
  let prob14 = B.probability (B.forward [2;2] mark) in
  let prob21 = B.probability2 [1;1] mark in
  let prob22 = B.probability2 [1;2] mark in
  let prob23 = B.probability2 [2;1] mark in
  let prob24 = B.probability2 [2;2] mark in
  let equal a b = 
    (abs_float (a -. b)) < 0.001 in
  assert(equal prob11 prob21);
  assert(equal prob12 prob22);
  assert(equal prob13 prob23);
  assert(equal prob14 prob24);
  assert(equal vitprob 0.0062);
  assert(viterbires = [1;3;3])


(* Test for Baum-Welch Algorithm using CS181 Notes *)
let test4 =
  let tprob = B.create_matrix [[0.8;0.2];[0.1;0.9]] in
  let initprob = B.create_matrix [[0.7; 0.3]] in
  let eprob = B.create_matrix [[0.75; 0.25]; [0.4; 0.6]] in
  let mark = B.create 2 2 initprob tprob eprob in
  let mark2 = B.baumwelch [[1;1];[2;2;1]] mark in
  let equal a b = 
    (abs_float (a -. b)) < 0.01 in
  assert (equal (B.get_prob_bw_t mark2 1 1) 0.842);
  assert (equal (B.get_prob_bw_t mark2 1 2) 0.158);
  assert (equal (B.get_prob_bw_t mark2 2 1) 0.128);
  assert (equal (B.get_prob_bw_t mark2 2 2) 0.872);
  assert (equal (B.get_prob_bw_e mark2 1 1) 0.731);
  assert (equal (B.get_prob_bw_e mark2 2 1) 0.427);
  assert (equal (B.get_prob_bw_s mark2 1 1) 0.647)
