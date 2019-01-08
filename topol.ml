(* Autor : Michał Borowski *)
(* Code Review : Artur Matyjasek *)

open PMap

exception Cykliczne

(* Tworzy mapę, która przyporządkowuje id (liczby 0, 1, 2, ...) 
 * wierzchołkom grafu. Początkowe numery (0, 1, 2, ...)
 * są zajmowane przez kolejne pierwsze elementy
 * par na liście lst (zgodnej ze specyfikacją funkcji topol). *)
let map_init lst =
        let add_id =
                fun (map, siz) a -> 
                        if exists a map then (map, siz)
                        else (add a siz map, siz + 1)
        in let (mapa, size) = List.fold_left add_id (empty, 0) (List.map fst lst)
        in let (mapa, size) = List.fold_left add_id (mapa, size) (List.concat (List.map snd lst))
        in mapa

(* Modyfikuje listę sąsiedztwa, tak aby każdy wierzchołek grafu 
 * miał na niej swoją listę. Elementy na wynikowej liście są ułożone 
 * według kolejnych id przypisanym wierzchołkom przez mapę map. *)
let edges_init lst map =
        let len = List.length lst
        in let additional_vertices =
                foldi (fun a id acc -> if id >= len then (a, [])::acc else acc) map []
        in lst @ (List.rev additional_vertices)

let topol lst =
        let mapa = map_init lst
        in let edges = edges_init lst mapa
        in let to_id = fun a -> find a mapa
        in let list_to_id = fun l -> List.map to_id l 
        (* Tablica list sąsiadów wierzchołków. *)
        in let neighbours =
                let l = List.map snd edges
                in let l2 = List.map list_to_id l
                in Array.of_list l2
        (* Tablica etykiet wierzchołków o kolejnych id. *)
        in let vertices =
                let l = List.map fst edges
                in Array.of_list l
        (* Liczba wszystkich wierzchołków. *)
        in let n = List.length edges
        (* Tablica stopni krawędzi wchodzących. *)
        in let deg = Array.make n 0
        (* Funkcja zwiększająca stopień wierzchołków na liście l o 1.*)
        in let add_degs l = List.iter (fun a -> deg.(a) <- (deg.(a) + 1)) l
        (* Lista, która będzie przechowywać id wierzchołków, które nie mają żadnych
         * krawędzi wchodzących. Dane wierzchołki będą na bieżąco usuwane. *)
        in let cut = ref []
        (* Wynik sortowania topologicznego. *)
        in let topo = ref []
        in let sub_deg = fun id ->
                begin
                        deg.(id) <- deg.(id) - 1;
                        if deg.(id) = 0 then cut := id::(!cut);
                end;
        (* Funkcja usuwająca wierzchołek i aktualizująca przy tym deg i cut. *)
        in let sub_degs l = List.iter sub_deg l
        in begin
                Array.iter add_degs neighbours;
                (* Inicjalizowanie listy cut. *)
                Array.iteri (fun id d -> if d = 0 then cut := (id::!cut)) deg;
                while !cut <> []
                do
                      (* Usuwanie wierzchołka, do którego nic nie wchodzi, z grafu. *)
                      let x = List.hd !cut in
                      begin
                              topo := vertices.(x)::!topo;
                              cut := List.tl !cut;
                              sub_degs neighbours.(x);
                      end;
                done;
         end;
         if List.length !topo = n then List.rev !topo
         else raise Cykliczne
         
                         
