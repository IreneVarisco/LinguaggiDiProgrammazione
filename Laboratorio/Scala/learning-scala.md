# Imparare Scala: Guida Introduttiva

## Dichiarazione di Variabili
In Scala, ci sono due modi principali per dichiarare variabili:

### Variabili Immutabili (Preferite)
```scala
val numero: Int = 10     // Tipo esplicito
val testo = "Ciao"       // Tipo inferito
```

### Variabili Mutabili
```scala
var contatore: Int = 0   // Può essere modificata
contatore = 5            // Riassegnazione consentita
```

## Tipi di Dati Base
- `Int`: Numeri interi
- `Double`: Numeri decimali
- `Boolean`: Valori vero/falso
- `String`: Testo
- `Char`: Singolo carattere

## Strutture di Controllo

### Condizionali (If-Else)
```scala
val x = 10
if (x > 5) {
  println("Maggiore di 5")
} else {
  println("Minore o uguale a 5")
}
```

### Ciclo For
```scala
// Range
for (i <- 1 to 5) {
  println(i)
}

// Iterazione su Liste
val lista = List(1, 2, 3, 4, 5)
for (elemento <- lista) {
  println(elemento)
}
```

### Ciclo While
```scala
var i = 0
while (i < 5) {
  println(i)
  i += 1
}
```

## Funzioni

### Definizione Base
```scala
def saluta(nome: String): String = {
  s"Ciao, $nome!"
}

// Chiamata alla funzione
val messaggio = saluta("Mario")
```

### Funzioni Brevi
```scala
def quadrato(x: Int) = x * x
```

## Collezioni

### Liste
```scala
val numeri = List(1, 2, 3, 4, 5)
val primoNumero = numeri.head
val restoLista = numeri.tail
```

### Mappe
```scala
val etaPersone = Map(
  "Mario" -> 30,
  "Luigi" -> 25
)
val etaMario = etaPersone("Mario")
```

## Gestione dell'Input/Output

### Stampa a Console
```scala
println("Ciao, mondo!")
```

### Lettura da Input
```scala
print("Inserisci il tuo nome: ")
val nome = scala.io.StdIn.readLine()
```

## Note Importanti
- Scala supporta sia la programmazione funzionale che quella orientata agli oggetti
- È fortemente tipizzato ma supporta l'inferenza dei tipi
- Compatibile con le librerie Java
