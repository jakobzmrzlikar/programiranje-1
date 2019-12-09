(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

let is_root a b = if a > 0 then (a * a == b) else false

let pack3 a b c = (a, b, c)

let sum_if_not cmp lst =
  let rec aux acc cmp lst =
    match lst with
    | [] -> acc
    | x :: xs -> 
      if (cmp x) then aux acc cmp xs
      else aux (acc + x) cmp xs
  in
  aux 0 cmp lst

let map f lst =
  let rec aux acc f lst =
    match lst with
    | [] -> acc
    | x :: xs -> aux (f x :: acc) f xs
  in
  aux [] f lst

let apply f_list lst =
  let rec aux acc f_list lst =
    match f_list with
    | [] -> acc
    | f :: fs -> aux (map (f lst) :: acc) fs lst
  in
  aux [] f_list lst

(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

type vrsta_srecanja = 
  | Predavanja
  | Vaje

type srecanje = {predmet: string; vrsta: vrsta_srecanja; trajanje: int}

type urnik = srecanje list list

let vaje = {predmet = "Analiza 2a"; vrsta = Vaje; trajanje = 3}

let predavanje = {predmet = "Programiranje 1"; vrsta = Predavanja; trajanje = 2}

let urnik_profesor () = failwith "dopolni me"

let je_preobremenjen urnik = 
  let rec aux flag urnik =
    if flag then true else 
      match urnik with 
      | [] -> flag
      | dan :: rest ->
        let rec inner_aux pred_count vaje_count flag dan = 
          match dan with 
          | [] -> (pred_count > 4 || vaje_count > 4)
          | {predmet; vrsta; trajanje} :: xs -> 
            match vrsta with
            | Predavanja -> inner_aux (pred_count + trajanje) vaje_count flag xs
            | Vaje -> inner_aux pred_count (vaje_count + trajanje) flag xs
        in
        aux (inner_aux 0 0 flag dan) rest
  in 
  aux false urnik


let bogastvo = ()
