#load "pMap.cmo";;
#load "topol.cmo";;
open Topol;;

exception Wrong_Answer

let generate_graph n g = 
        Random.self_init();
        let rec vertex_helper k acc =
                if Random.int g = 0 then acc
                else if (n <= k + 1) then acc
                else vertex_helper k (((Random.int (n - k - 1)) + k + 1)::acc)
        in let rec helper k acc =
                if k = n then acc
                else helper (k + 1) ((k, (vertex_helper k []))::acc)
        in helper 0 [];;

let przetestuj test =
        let topo = topol test
        in let ids = Hashtbl.create (List.length topo)
        in List.iteri (fun i x -> Hashtbl.add ids x i) topo;
        let check a lst =
                List.iter (fun b -> if ((Hashtbl.find ids b) < a) then raise Wrong_Answer) lst
        in
        try (List.iter (fun (a, l) -> check (Hashtbl.find ids a) l) test ; true)
        with Wrong_Answer -> false

let rec testuj size_t size_n g =
        if size_t = 0 then true
        else
        let test = generate_graph size_n g
        in (przetestuj test) && (testuj (size_t - 1) size_n g)


let l = [(1, [])];;
przetestuj l;;
let l = [(1, [2])];;
przetestuj l;;
let l = [(1, [0 ; 2]) ; (2, [0])];;
przetestuj l;;
let l = [('a', ['b' ; 'c']) ; ('c', ['b']) ; ('d', ['c']) ; ('e', ['f'])];;
przetestuj l;;
let l = [(false, [true])];;
przetestuj l;;
let l = [(2, [1; 3]) ; (3, [1]) ; (0, [4;6]); (6, [4]) ; (4, [])];;
przetestuj l;;

testuj 100 100 2;;
testuj 100 100 3;;
testuj 100 100 4;;
testuj 1000 100 2;;
testuj 1000 100 3;;
testuj 1000 100 5;;
testuj 10000 10 1;;
testuj 10000 10 2;;
testuj 10000 10 3;;
testuj 100000 2 2;;
testuj 100000 2 3;;
