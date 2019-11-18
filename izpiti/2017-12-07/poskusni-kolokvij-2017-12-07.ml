(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

(* 1.1) Definirajte funkcijo, ki vzame dve celi števili ter vrne njuno vsoto.
   Primer: /sestej 2 3 = 5/ *)
let sestej a b = a + b

(* 1.2) Definirajte funkcijo, ki svojemu argumentu prišteje 3.
   Primer: /pristej_tri 10 = 13/ *)
let pristej_tri x = x + 3

(* 1.3) Definirajte funkcijo, ki vsem elementom seznama prišteje 5.
   Primer: /vsem_pristej_pet [1; 2] = [6; 7]/ *)

let vsem_pristej_pet list =
  let rec aux acc list =
    match list with
    | [] -> List.rev acc
    | x :: xs -> aux ( (x + 5) :: acc) xs
  in
  aux [] list


(* 1.4) Definirajte funkcijo, ki vrne zadnjo komponento nabora s tremi elementi.
   Primer: /tretji (1, "horse", [None]) = [None]/ *)
let tretji (_, _, a) = a

(* 1.5) Definirajte funkcijo, ki vzame dve funkciji ter vrne njun kompozitum.
   Primer: /kompozitum succ string_of_int 5 = "6"/ *)
let kompozitum f1 f2 = function
    x -> f1 (f2 x)


(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

(* 2.1) Rožno drevo je drevo, v katerem ima vsak koren poljubno mnogo otrok,
   ki so zopet rožna drevesa. Rožna drevesa predstavimo s parametričnim
   tipom /'a drevo/ z enim konstruktorjem, ki sprejme:
   - vrednost (koren) tipa /'a/ in
   - seznam (gozd) dreves tipa /'a drevo/. *)

type 'a drevo =
  | Drevo of 'a * 'a drevo list 

(* 2.2) Napišite funkcijo, ki vrne koren danega rožnega drevesa. *)
let koren (Drevo(koren, lst)) = koren

(* 2.3) Napišite funkcijo, ki preveri, ali drevo celih števil vsebuje kakšno negativno število. *)
let rec kaksno_negativno (Drevo(x, lst)) =
  if x < 0 then true
  else
    let rec aux flag lst =
      if flag then true
      else match lst with
        | [] -> false
        | drevo :: rest -> aux (kaksno_negativno drevo) rest
    in
    aux false lst

(* 2.4) Sestavite funkcijo, ki sprejme naravno število ter sestavi (poljubno)
   drevo, ki ima toliko otrok.
   Namig: napišite pomožno funkcijo, ki ustvari poljuben seznam dane dolžine. *)
let senzam_dreves_dolzine n =
  let rec aux acc n =
    if n == 0 then acc
    else aux (Drevo(1, []) :: acc) (n-1)
  in
  aux [] n


let drevo_z_veliko_otroci n = Drevo(1, senzam_dreves_dolzine n)

(* 2.5) Sestavite funkcijo, ki izračuna število vseh vozlišč v drevesu.
   Če želite vse točke, mora biti funkcija repno rekurzivna.

   Opomba: kot ste videli na vajah, nekatere funkcije iz modula List,
   na primer List.map, niso repno rekurzivne, zato se jim raje
   izognite. *)

let rec fold f acc lst = 
  match lst with
  | [] -> acc
  | x :: xs -> fold f (f acc x) xs 

let velikost drevo =
  let rec aux count drevo =
    match drevo with 
    | Drevo(_, []) -> count + 1
    | Drevo(_, lst) -> fold aux (count + 1) lst
  in aux 0 drevo


(* let velikost drevo =
   let rec aux count drevo lst =
    match drevo with
    | Drevo(_, []) ->
      (match lst with
       | [] -> count + 1
       | x :: xs -> aux (count + 1) x xs)
    | Drevo(_, drevo :: rest) -> aux (count + 1) drevo (lst :: rest)
   in
   aux 0 drevo [] *)


let rec insert cmp x lst = 
  match lst with
  | [] -> [x]
  | (y :: ys) ->
    if cmp x y then x :: y :: ys
    else y :: (insert cmp x ys)

let rec sort cmp lst =
  match lst with
  | [] -> []
  | x :: xs -> insert cmp x (sort cmp xs)   


let foo = Drevo(1, []);;
let bar = Drevo(1, [foo; foo; foo]);;
let baz = Drevo(1, [bar; bar; bar]);;
let lower a b = if a < b then true else false