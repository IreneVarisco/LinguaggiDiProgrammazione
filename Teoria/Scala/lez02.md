# Lab02
## Scala Traits

**Java** &rarr; Ereditarietà Singola

**Scala** &rarr; Ereditarietà Multipla

## Interfacce in Java
### Vantaggi
* Permettono a una classe di implementare un numero illimitato di interfacce
* Consentono di:
  - Dichiarare che una classe espone molteplici astrazioni
  - Simulare una forma limitata di ereditarietà multipla

### Limitazioni
1. Rigidità nell'implementazione:
   - La stessa interfaccia richiede sempre la stessa implementazione
   - Poca flessibilità nell'adattamento del codice
2. Scarsa modularità:
   * Il codice implementato potrebbe non essere strettamente correlato alla classe principale
3. Riutilizzo del codice:
   * Mancanza di un meccanismo semplice per riutilizzare le implementazioni

***

## Traits in Scala
### Concetti Fondamentali
1. **Mixin**:
   * Parti di codice focalizzate su uno specifico aspetto
   * Componenti riutilizzabili in diverse parti del programma
2. **Concern**:
   * Aspetti o preoccupazioni specifiche del codice
   * Funzionalità modulari e indipendenti

### Vantaggi rispetto alle Interfacce Java
1. Maggiore Flessibilità:
   - Possono essere "mixed in" durante la creazione delle istanze
   - Permettono la composizione dinamica dei comportamenti
2. Migliore Modularità:
   - Preservano la separazione dei _concern_
   - Consentono la composizione dei comportamenti su richiesta
3. Implementazione Ibrida:
   - Fungono da interfacce con implementazioni opzionali
   - Offrono una forma controllata di ereditarietà multipla

### Caratteristiche Chiave
* Riutilizzabilità elevata del codice
* Composizione più efficiente dei comportamenti
* Applicabilità a diverse tipologie di dati
* Maggiore flessibilità nella progettazione delle classi

***

## Esempio Traits
Questo codice implementa il pattern Observer, dove:

- Il bottone è l'oggetto osservato (subject)
- ButtonCountObserver è l'osservatore che tiene traccia dei click
- Ogni volta che il bottone viene cliccato, tutti gli osservatori vengono notificati
- Il trait Subject fornisce la funzionalità di base per gestire gli osservatori

```scala
class Button(val label: String) {
  def click() = { /* Logic to give the appearance of clicking a button... */ }
}

trait Subject {
  type Observer = { def receiveUpdate(subject: Any) }
  private var observers = List[Observer]()

  def addObserver(observer: Observer) = observers ::= observer
  def notifyObservers = observers foreach (_.receiveUpdate(this))
}

class ButtonCountObserver {
  var count = 0
  def receiveUpdate(subject: Any) = count += 1
}

class ObservableButton(name: String) extends Button(name) with Subject {
  override def click() = {
    super.click()
    notifyObservers
  }
}

object ButtonObserverTest {
  def main(args: Array[String]) = {
    val observableButton = new ObservableButton("Okay")
    val buttonObserver = new ButtonCountObserver

    observableButton.addObserver(buttonObserver)
    for (i <- 1 to 3) observableButton.click()

    printf("The button has been clicked %d times
", buttonObserver.count)
  }
}
```
- **Button**: Definisce un semplice bottone con un'etichetta e un metodo `click()`.

- **Trait Subject**:
  - Definisce il tipo `Observer` come un tipo strutturale
  - crea una lista privata di osservatori
  - definisce un metodo per aggiungere un osservatore
  - crea un metodo per notificare tutti gli osservatori

- **ButtonCountObserver**: Mantiene il conteggio dei click e incrementa il contatore ogni volta che riceve un aggiornamento

- **ObservableButton**: Eredita da `Button` e include il trait `Subject` e sovrascrive il metodo `click()` per notificare gli osservatori dopo ogni click.

- **Object ButtonObserverTest**: Crea un bottone osservabile e un osservatore, collega l'osservatore al bottone, clicca il bottone 3 voltee stampa il numero totale di click

## Stackable Traits

```scala
trait Clickable { def click() }
class Button(val label: String) extends Clickable {
  def click() = { /* Logic to give the appearance of clicking a button... */ }
}

trait ObservableClicks extends Clickable with Subject {
  abstract override def click() = {
    super.click()
    notifyObservers
  }
}

object ButtonClickableObserverTest {
  def main(args: Array[String]) = {
    val observableButton = new Button("Okay") with ObservableClicks
    val buttonClickCountObserver = new ButtonCountObserver

    observableButton.addObserver(buttonClickCountObserver)
    for (i <- 1 to 3) observableButton.click()

    printf("The button has been clicked %d times
", buttonClickCountObserver.count)
  }
}
```
- **Clickable**: È un trait base che dichiara solo il metodo `click()`, Definisce un'interfaccia semplice per oggetti che possono essere cliccati

- **Button**: Implementa il trait Clickable e fornisce una implementazione concreta del metodo `click()`.

- **ObservableClicks**: Estende sia `Clickable` che `Subject` (il trait Subject è lo stesso dell'esempio precedente)
Usa `abstract override` che è una caratteristica importante di Scala:
  - Permette di modificare un metodo che potrebbe non essere ancora implementato
  - È necessario quando si usa un trait in una "catena di traits"
Aggiunge la funzionalità di notifica agli osservatori dopo ogni click

- **Object per il test**: Crea un bottone e lo compone con il trait ObservableClicks usando la sintassi with
Questo è un esempio di "mixin composition" in Scala
Il resto del codice è simile all'esempio precedente

super NON si riferisce a Clickable (che dichiara click() ma non lo implementa)
e NON si riferisce a Subject (che non ha proprio il metodo click())
Invece, super si riferirà alla implementazione di click() che verrà fornita quando il trait sarà effettivamente utilizzato.

Questo comportamento è chiamato "late binding" (binding ritardato):

Il riferimento a super.click() non viene risolto quando il trait viene definito
Viene risolto solo quando il trait viene effettivamente "mischiato" (mixed in) con una classe concreta.

## Costruzione dei Traits

I traits non possono avere né costruttori ausiliari né liste di argomenti per il costruttore primario.

Quando estendono classi o altri traits non possono passare argomenti MA possono estendere solo classi/traits che hanno costruttori senza argomenti.

```scala
trait T1 { println(" in T1: x = " + x); val x = 1; println(" in T1: x = " + x) }
trait T2 { println(" in T2: y = " + y); val y = "T2"; println(" in T2: y = " + y) }
class Base12 {
  println(" in Base12: b = " + b); val b = "Base12"; println(" in Base12: b = " + b)
}
class C12 extends Base12 with T1 with T2 {
  println(" in C12: c = " + c); val c = "C12"; println(" in C12: c = " + c)
}

println("Creating C12:")
new C12
println("After Creating C12")
```
Output:

```cmd
[18:24]cazzola@surtur:~/lp/scala>scala TT.scala
Creating C12:
in Base12: b = null
in Base12: b = Base12
in T1: x = 0
in T1: x = 1
in T2: y = null
in T2: y = T2
in C12: c = null
in C12: c = C12
After Creating C12
```
### Ordine di Inizializzazione
1. Classe base (`Base12`).
2. Traits dichiarati (`T1`, poi `T2`).
3. Classe concreta (`C12`).

### Valori durante l'inizializzazione
Ogni valore viene stampato due volte:
- Prima dell'inizializzazione (`null` o `0`).
- Dopo l'inizializzazione (con il valore corretto).
