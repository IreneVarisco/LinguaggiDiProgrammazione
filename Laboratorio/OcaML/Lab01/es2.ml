
(* Definizione del tipo di temperatura *)
type temperatura = { valore : float; unita : string };;

(* Definizione delle scale di temperatura *)
let scale = ["Celsius"; "Fahrenheit"; "Kelvin"; "Rankine"; "Delisle"; "Newton"; "Réaumur"; "Rømer"];;

(* Funzione per convertire una temperatura da una scala all'altra *)
let any2c (t : temperatura) (unit : string) : temperatura =
  match unit with
  | "Celsius" -> t
  | "Fahrenheit" -> { valore = (t.valore -. 32.) /. 1.8; unita = "Celsius" }
  | "Kelvin" -> { valore = t.valore -. 273.15; unita = "Celsius" }
  | "Rankine" -> { valore = (t.valore -. 491.67) /. 1.8; unita = "Celsius" }
  | "Delisle" -> { valore = (100. -. t.valore) /. 3. *. 2.; unita = "Celsius" }
  | "Newton" -> { valore = t.valore /. 33. *. 100.; unita = "Celsius" }
  | "Réaumur" -> { valore = t.valore /. 4. *. 5.; unita = "Celsius" }
  | "Rømer" -> { valore = (t.valore -. 7.5) /. 21. *. 40.; unita = "Celsius" }
  | _ -> failwith "Scala non supportata";;

(* Funzione ausiliaria per creare una lista di tuple (valore, unità) *)
let rec p temp list ppf =
  match list with
  | [] -> Format.fprintf ppf "\n"
  | hd::tl -> Format.fprintf ppf "%s: %f %s\n" hd.unita hd.valore hd.unita; p temp tl ppf;;

(* Funzione principale per creare la tabella delle conversioni *)
let temp_table_format temp list ppf =
  Format.fprintf ppf "Temperatura %f %s:\n" temp.valore temp.unita;
  p temp list ppf;;

(* Lista delle scale di temperatura *)
let cons = ["Celsius"; "Fahrenheit"; "Kelvin"; "Rankine"; "Delisle"; "Newton"; "Réaumur"; "Rømer"];;

(* Funzione che converte una temperatura in tutte le scale *)
let rec any2ct (t : temperatura) (units : string list) =
  match units with
  | [] -> []
  | hd::tl -> (any2c t hd)::(any2ct t tl);;

(* Funzione principale per stampare la tabella delle conversioni *)
let main () =
  let temp = {valore = 42.0; unita = "Celsius"} in
  let cons = ["Celsius"; "Fahrenheit"; "Kelvin"; "Rankine"; "Delisle"; "Newton"; "Réaumur"; "Rømer"] in
  let temp_list = List.rev (any2ct temp cons) in
  temp_table_format temp temp_list Format.std_formatter;;

let _ = main ();;