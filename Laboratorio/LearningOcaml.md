
# **Guida Introduttiva a OCaml**

### **Cos'è OCaml?**
OCaml è un linguaggio di programmazione funzionale fortemente tipizzato. È noto per la sua combinazione di programmazione funzionale, imperativa e orientata agli oggetti. OCaml è utile in molti settori, tra cui l'analisi formale, l'intelligenza artificiale e la creazione di sistemi sicuri.

### **Sintassi di Base**

#### 1. **Tipi di Dato di Base**

OCaml ha molti tipi di dati predefiniti:

- **Interi**: numeri senza virgola mobile.
  ```ocaml
  let x = 10;;  (* x è un intero *)
  ```
  
- **Float**: numeri con virgola mobile (devono finire con un punto esclamativo `.`).
  ```ocaml
  let pi = 3.14;;
  ```
  
- **Stringhe**: sequenze di caratteri delimitate da doppi apici.
  ```ocaml
  let nome = "Mario";;
  ```

- **Booleani**: due valori possibili, `true` o `false`.
  ```ocaml
  let is_open = true;;
  ```

- **Liste**: sequenze ordinate di elementi dello stesso tipo.
  ```ocaml
  let numeri = [1; 2; 3; 4];;
  ```

#### 2. **Espressioni e Funzioni**

Una funzione è un costrutto fondamentale in OCaml.

- Definizione di una funzione:
  ```ocaml
  let add x y = x + y;;
  ```

  Qui, la funzione `add` prende due parametri `x` e `y` e restituisce la somma.

- Applicazione della funzione:
  ```ocaml
  let somma = add 3 5;;  (* Risultato: somma è 8 *)
  ```

- Funzioni ricorsive:
   è necessario usare `let rec`.
  
  ```ocaml
  let rec fattoriale n =
    if n = 0 then 1
    else n * fattoriale (n - 1);;
  ```

#### 3. **Match (Pattern Matching)**

Il pattern matching è usato per fare controlli condizionali in modo chiaro e conciso.

- Uso di `match`:
  ```ocaml
  let descrizione x =
    match x with
    | 0 -> "zero"
    | 1 -> "uno"
    | _ -> "numero diverso da zero o uno";;
  ```

Qui, `match` analizza il valore di `x` e restituisce la stringa corrispondente.

#### 4. **Tipi di Dato Personalizzati**

OCaml ti permette di definire tipi di dato personalizzati.

- Definizione di un tipo `persona`:
  ```ocaml
  type persona = { nome : string; eta : int };;
  ```

  Questo definisce un record `persona` con due campi: `nome` e `eta`.

- Creazione di un valore di tipo `persona`:
  ```ocaml
  let mario = { nome = "Mario"; eta = 30 };;
  ```

#### 5. **Liste**

Le liste sono una delle strutture dati più usate in OCaml. Una lista è una sequenza ordinata di elementi dello stesso tipo.

- Creare una lista:
  ```ocaml
  let numeri = [1; 2; 3; 4];;
  ```

- Aggiungere un elemento all'inizio di una lista:
  ```ocaml
  let nuova_lista = 0 :: numeri;;  (* nuova_lista = [0; 1; 2; 3; 4] *)
  ```

- Concatenare due liste:
  ```ocaml
  let lista_completa = [1; 2] @ [3; 4];;  (* lista_completa = [1; 2; 3; 4] *)
  ```

#### 6. **Tuple**

Le tuple sono simili alle liste, ma possono contenere valori di tipo diverso.

- Creazione di una tupla:
  ```ocaml
  let tupla = (1, "ciao", 3.14);;
  ```

- Accesso agli elementi:
  ```ocaml
  let primo = fst tupla;;  (* primo = 1 *)
  let secondo = snd tupla;;  (* secondo = "ciao" *)
  ```

#### 7. **Espressioni Condizionali**

OCaml supporta le espressioni condizionali con `if-then-else`.

- Esempio di uso:
  ```ocaml
  let is_even n =
    if n mod 2 = 0 then "pari"
    else "dispari";;
  ```

#### 8. **Cicli** IGNORA QUESTO PUNTO

Anche se OCaml è principalmente un linguaggio funzionale, è possibile usare costrutti imperativi come i cicli.

- Ciclo `for`:
  ```ocaml
  for i = 1 to 10 do
    print_int i;
    print_newline ();
  done;;
  ```

- Ciclo `while`:
  ```ocaml
  let n = ref 10 in
  while !n > 0 do
    print_int !n;
    print_newline ();
    n := !n - 1;
  done;;
  ```

#### 9. **Moduli**

OCaml organizza il codice in moduli, che sono utili per la gestione di progetti più grandi.

- Definire un modulo:
  ```ocaml
  module Matematica = struct
    let pi = 3.14159
    let quadrato x = x * x
  end;;
  ```

- Utilizzare un modulo:
  ```ocaml
  let area = Matematica.pi *. Matematica.quadrato 5.0;;
  ```

#### 10. **Lavorare con File**

OCaml permette la lettura e scrittura su file.

- Scrivere su un file:
  ```ocaml
  let out = open_out "output.txt" in
  output_string out "Ciao, mondo!";
  close_out out;;
  ```

- Leggere da un file:
  ```ocaml
  let in_channel = open_in "input.txt" in
  let linea = input_line in_channel in
  print_endline linea;
  close_in in_channel;;
  ```

---

### **Risorse Aggiuntive**

- **Documentazione Ufficiale**: https://ocaml.org/docs
- **Learn OCaml (tutorial interattivo)**: https://ocaml.org/learn/tutorials
- **TryOCaml (ambiente di sviluppo online)**: https://try.ocamlpro.com/

---

