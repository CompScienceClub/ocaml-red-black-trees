(* Types *)
type color   = R | B | BB
type 'a tree = Empty of color | Node of color * 'a tree * 'a * 'a tree


(* Membership *)
let rec member (t: 'a tree) (x: 'a) : bool =
    match t with
        | Empty _            -> false
        | Node (_, l, x', r) -> x' = x
                             || (x' < x && member r x)
                             || (x' > x && member l x) 


(* Insert *)
let bal_ins_l (t: 'a tree) : 'a tree =
    match t with
        | Node (B, Node (R, Node (R, a, x, b), y, c), z, d)
        | Node (B, Node (R, a, x, Node (R, b, y, c)), z, d)
            -> Node (R, Node (B, a, x, b), y, Node (B, c, z, d))
        | n -> n
        
let bal_ins_r (t: 'a tree) : 'a tree =
    match t with
        | Node (B, a, x, Node (R, Node (R, b, y, c), z, d))
        | Node (B, a, x, Node (R, b, y, Node (R, c, z, d))) 
            -> Node (R, Node (B, a, x, b), y, Node (B, c, z, d))
        | n -> n
   
let ins (t: 'a tree) (x: 'a) : 'a tree =
    let rec ins_int (t: 'a tree) : 'a tree = 
        match t with
            | Empty _            -> Node (R, Empty B, x, Empty B)
            | Node (c, l, x', r) -> if      x' < x 
                                    then    bal_ins_r (Node (c, l, x', ins_int r))
                                    else if x' > x 
                                    then    bal_ins_l (Node (c, ins_int l, x', r))
                                    else    Node (c, l, x', r)
    in
    ins_int t
    
let insert (t: 'a tree) (x: 'a) : 'a tree = 
    match ins t x with
        | Empty _            -> failwith "insert error"
        | Node (_, x', l, r) -> Node (B, x', l, r)


(* Delete *)
let rec min (t: 'a tree) : 'a = 
    match t with
        | Empty _                 -> failwith "min error"
        | Node (_, Empty _, x, _) -> x
        | Node (_, l, _, _)       -> min l
      
let node_val (t: 'a tree) : 'a = 
    match t with
        | Empty _           -> failwith "node_val error"
        | Node (_, _, x, _) -> x
       
let left (t: 'a tree) : 'a tree = 
    match t with
        | Empty _           -> failwith "left error"  
        | Node (_, l, _, _) -> l
        
let right (t: 'a tree) : 'a tree =
    match t with
        | Empty _           -> failwith "right error"
        | Node (_, _, _, r) -> r
        
let add_b (t: 'a tree) : 'a tree = 
    match t with
        | Empty B           -> Empty BB
        | Node (R, l, x, r) -> Node (B,  l, x, r)
        | Node (B, l, x, r) -> Node (BB, l, x, r)
        | _                 -> failwith "add_b error"

let rem_b (t: 'a tree) : 'a tree = 
    match t with
        | Empty BB            -> Empty B
        | Node (BB, l, x, r)  -> Node (B, l, x, r)
        | _                   -> failwith "rem_b error"

let is_b (t: 'a tree) : bool = 
    match t with
        | Empty B           
        | Node (B, _, _, _) -> true
        | _                 -> false

let is_r (t: 'a tree) : bool = 
    match t with
        | Node (R, _, _, _) -> true
        | _                 -> false 

let is_bb (t: 'a tree) : bool = 
    match t with
        | Empty BB          
        | Node (BB, _, _, _) -> true
        | _                  -> false

let rec bal_del_l (t: 'a tree) : 'a tree = 
    match t with
        | Node (B, d, y, Node (R, l, z, r)) -> 
            if   is_bb d 
            then Node (B, bal_del_l (Node (R, d, y, l)), z, r)
            else Node (B, d, y, Node (R, l, z, r))
        | Node (c, d, y, Node (B, l, z, r)) -> 
            if is_bb d then 
                if      is_b l && is_b r 
                then    add_b (Node (c, rem_b d, y, Node (R, l, z, r)))
                else if is_r l && is_b r 
                then    bal_del_l (Node (c, d, y, Node (B, left l, node_val l, Node (R, right l, z, r))))
                else    Node (c, Node (B, rem_b d, y, l), z, add_b r)
            else Node (c, d, y, Node (B, l, z, r))
        | n -> n

let rec bal_del_r (t: 'a tree) : 'a tree = 
    match t with
        | Node (B, Node (R, l, z, r), y, d) -> 
            if   is_bb d 
            then Node (B, l, z, bal_del_r (Node (R, r, y, d)))
            else Node (B, Node (R, l, z, r), y, d)
        | Node (c, Node (B, l, z, r), y, d) -> 
            if is_bb d then
                if      is_b l && is_b r 
                then    add_b (Node (c, Node (R, l, z, r), y, rem_b d))
                else if is_b l && is_r r 
                then    bal_del_r (Node (c, Node (B, Node (R, l, z, left r), node_val r, right r), y, d))
                else    Node (c, add_b l, z, Node (B, r, y, rem_b d))
            else Node (c, Node (B, l, z, r), y, d)
        | n -> n

let rec del (t: 'a tree) (x: 'a) : 'a tree = 
    let rec del_int (t: 'a tree) : 'a tree = 
        match t with
            | Empty _ -> t
            | Node (R, Empty _, x', Empty _) ->
                if   x' = x 
                then Empty B 
                else t
            | Node (B, Empty _, x', Empty _) -> 
                if   x' = x 
                then Empty BB 
                else t
            | Node (_, Empty _, x', Node (_, l, y', r)) 
            | Node (_, Node (_, l, y', r), x', Empty _) ->
                if      x' = x 
                then    Node (B, l, y', r)
                else if y' = x 
                then    Node (B, Empty B, x', Empty B)
                else    t
            | Node (c, l, x', r) ->
                if      x' < x 
                then    bal_del_r (Node (c, l, x', del_int r))
                else if x' > x 
                then    bal_del_l (Node (c, del_int l, x', r))
                else    let m = min r 
                        in 
                        bal_del_r (Node (c, l, m, (del r m)))
    in
    del_int t
    
let delete (t: 'a tree) (x: 'a) : 'a tree = 
    match del t x with
        | Empty _            -> Empty B
        | Node (_, l, x', r) -> Node (B, l, x', r)
      

(* Tests *)
let t = insert (Empty B) 5;;
let t = insert t 10;;
let t = insert t 2;;
let t = insert t 12;;
let t = insert t 6;;

assert ( member t 5  = true );;
assert ( member t 10 = true );;
assert ( member t 2  = true );;
assert ( member t 12 = true );;
assert ( member t 6  = true );;
assert ( member t 15 = false );;

let t = delete t 10;;
assert ( member t 5  = true );;
assert ( member t 10 = false );;
assert ( member t 2  = true );;
assert ( member t 12 = true );;
assert ( member t 6  = true );;
assert ( member t 15 = false );;

let t = delete t 6;;
assert ( member t 5  = true );;
assert ( member t 10 = false );;
assert ( member t 2  = true );;
assert ( member t 12 = true );;
assert ( member t 6  = false );;
assert ( member t 15 = false );;
