# DSL: Domain Specific Languages

## Parser

Un **parser** è una funzione che accetta stringhe come input e restituisce strutture come alberi sintattici (parse tree). I parser si basano su **grammatiche** descritte da una quadrupla:

```
<Simboli Terminali, Simboli Non Terminali, Assioma, Regole di Produzione>
```

I parser seguono una strategia di **discesa ricorsiva** e sono modulari.

## Parser Combinators

I **Parser Combinators** sono funzioni di ordine superiore che accettano parser come input e restituiscono nuovi parser. Essi permettono una strategia di parsing ricorsiva e discendente e possono essere combinati per parsare grandi quantità di dati.

Sono quindi blocchi costruttori per parser che possono essere combinati insieme.

### Vantaggi dei Parser Combinators

- Moduli componibili per parsing complessi.
- Supporto per composizioni sequenziali, alternative, ripetitive e opzionali.
- Possono essere usati per analizzare strutture DSL.

Un framework combinator facilita la combinazione dei parser per gestire con casi sequenziali e alternativi, ripetizioni, termini opzionali ecc...

## Composizioni

### Composizioni Sequenziali
Mantengono o eliminano i risultati delle produzioni adiacenti.

- **~**: Utilizzato quando i risultati prodotti dalle produzioni a sinistra e a destra del simbolo devono essere mantenuti per ulteriori elaborazioni.
  def paycheck = empl ~ gross ~ deduct
- **~>**: Utilizzato quando il risultato delle produzioni a sinistra non è più necessario.
  def empl = "paycheck" ~> "for" ~> "employee" ~> employeeName
- **<~**: Utilizzato quando il risultato delle produzioni a destra non è più necessario.
  def tax = fedState <~ "income" <~ "tax"


### Composizioni Alternative
Forniscono opzioni tra parser alternativi.
- **|**: Esprime quando due parser sono in alternativa.
  def weeksDays = "weeks" | "week" | "days" | "day"
  Composizione Repetitiva

### Coimpozizioni ripetitive
Gestiscono le ripetizioni 

- **rep/repsep**: Abbinano zero o più ripetizioni.
  def deduct = "minus" ~> "deductions" ~> "for" ~> "{" ~> repsep(deductItem,",") <~ "}"
  Termini Opzionali

### Composizioni Opzionali
Per termini non sempre presenti.
- **opt**: Utilizzato per termini opzionali che non sono sempre necessari.


```scala
paycheck for employee "Buck Trends" is salary for 2 weeks minus deductions for {
  federal income tax is 25. percent of gross,
  state income tax is 5. percent of gross,
  insurance premiums are 500. in gross currency,
  retirement fund contributions are 10. percent of gross
}
```


## Esempio DSL: Payroll

Questo DSL calcola la busta paga di un dipendente, includendo deduzioni come tasse, premi assicurativi e contributi previdenziali.

```scala
package payroll.pcdsl
import scala.util.parsing.combinator._
import payroll._
import payroll.Type2Money._

class PayrollParserCombinatorsV1 extends JavaTokenParsers {
  def paycheck = empl ~ gross ~ deduct
  def empl = "paycheck" ~> "for" ~> "employee" ~> employeeName
  def gross = "is" ~> "salary" ~> "for" ~> duration
  def deduct = "minus" ~> "deductions" ~> "for" ~> "{" ~> deductItems <~ "}"
  def employeeName = stringLiteral // stringLiteral from JavaTokenParsers
  def duration = decimalNumber ~ weeksDays // decimalNumber from JavaTokenParsers
  def weeksDays = "weeks" | "week" | "days" | "day"
  def deductItems = repsep(deductItem, ",")
  def deductItem = deductKind ~> deductAmount
  def deductKind = tax | insurance | retirement
  def tax = fedState <~ "income" <~ "tax"
  def fedState = "federal" | "state"
  def insurance = "insurance" ~> "premiums"
  def retirement = "retirement" ~> "fund" ~> "contributions"
  def deductAmount = percentage | amount
  def percentage = toBe ~> doubleNumber <~ "percent" <~ "of" <~ "gross"
  def amount = toBe ~> doubleNumber <~ "in" <~ "gross" <~ "currency"
  def toBe = "is" | "are"
  def doubleNumber = floatingPointNumber // floatingPointNumber from JavaTokenParsers
}
```

### Struttura del parser

1. **Employee:** Cerca un dipendente nel sistema.
2. **Gross Salary:** Calcola lo stipendio lordo in base alla durata.
3. **Deductions:** Applica deduzioni come percentuali o importi fissi.


Per utilizzare un parser definito:

```scala
val p = new PayrollParserCombinatorsV1
p.parseAll(p.paycheck, input) match {
  case p.Success(r, _) => ...
  case x => ...
}
```

- **parseAll** è definito in una classe genitore che riceve un parser (un'invocazione alla busta paga nel nostro caso) e la stringa di input da analizzare;
  - se il processo di analisi ha esito positivo, il risultato è un'istanza di p.Success[+T] una classe case dichiarata nel tratto Parsers;
  - il prefisso p indica che p.Success è un tipo dipendente dal percorso e permette di distinguere il risultato da due diversi parser;
  - l'istanza Success ha due campi, il primo è il risultato del parse (di tipo T), il secondo è l'input rimanente da analizzare (normalmente vuoto);
  - se l'analisi fallisce, l'istanza restituita è p.Failure o p.Errore; entrambi derivano da p.NoSuccess e contengono campi per un messaggio di errore e l'input non consumato nel punto di errore.
- **Success:** Ritorna un risultato valido e l'input residuo.

- **Failure/Error:** In caso di errore, restituisce un messaggio di errore e l'input non consumato.

```cmd
scala> import scala.util.parsing.combinator._
scala> import payroll.pcdsl._
scala> val p = new PayrollParserCombinatorsV1
scala> p.empl
res0: p.Parser[String] = Parser (~>)
scala> p.weeksDays
res2: p.Parser[String] = Parser (|)
scala> p.paycheck
res3: p.Parser[p.~[p.~[String,p.~[String,String]],List[String]]] = Parser (~)
scala> p.parseAll(p.weeksDays, "weeks")
res4: p.ParseResult[String] = [1.6] parsed: weeks
scala> val input = """paycheck for employee "Buck Trends"
| is salary for 2 weeks minus deductions for {}"""
input: java.lang.String =
paycheck for employee "Buck Trends" is salary for 2 weeks minus deductions for {}
scala> p.parseAll(p.paycheck, input)
res5: p.ParseResult[p.~[p.~[String,p.~[String,String]],List[String]]] =
[2.46] parsed: (("Buck Trends"~(2~weeks))~List())
scala> val input = """paycheck for employe "Buck Trends"
| is salary for 2 weeks minus deductions for {}"""
input: java.lang.String =
paycheck for employe "Buck Trends" is salary for 2 weeks minus deductions for {}
scala> p.parseAll(p.paycheck, input)
res6: p.ParseResult[p.~[p.~[String,p.~[String,String]],List[String]]] =
[1.14] failure: ’employee’ expected but ’ ’ found
paycheck for employe "Buck Trends"
```

Durante il parsing di un DSL (Domain Specific Language) per le buste paga:
- È necessario identificare il dipendente usando il nome.
- Recuperare lo stipendio lordo.
- Calcolare le detrazioni.

Il parser restituisce una coppia con:
1. L'istanza di `Employee`.
2. La busta paga completa (`Paycheck`).

## Implementazione del Parser

### Definizione della classe principale
```scala
package payroll.pcdsl
import scala.util.parsing.combinator._
import payroll._
import payroll.Type2Money._

class UnknownEmployee(name: Name) extends RuntimeException(name.toString)

class PayrollParserCombinators(val employees: Map[Name, Employee]) extends JavaTokenParsers {
  var currentEmployee: Employee = null
  var grossAmount: Money = Money(0)

  /** @return Parser[(Employee, Paycheck)] */
  def paycheck = empl ~ gross ~ deduct ^^ { case e ~ g ~ d => (e, Paycheck(g, g - d, d)) }

  /** @return Parser[Employee] */
  def empl = "paycheck" ~> "for" ~> "employee" ~> employeeName ^^ { name =>
    val names = name.substring(1, name.length - 1).split(" ")
    val n = Name(names(0), names(1))
    if (!employees.contains(n)) throw new UnknownEmployee(n)
    currentEmployee = employees(n)
    currentEmployee
  }

  /** @return Parser[Money] */
  def gross = "is" ~> "salary" ~> "for" ~> duration ^^ { dur =>
    grossAmount = salaryForDays(dur)
    grossAmount
  }

  def deduct = "minus" ~> "deductions" ~> "for" ~> "{" ~> deductItems <~ "}"

  /** "stringLiteral" provided by JavaTokenParsers
    * @return Parser[String] */
  def employeeName = stringLiteral

  /** "decimalNumber" provided by JavaTokenParsers
    * @return Parser[Int] */
  def duration = decimalNumber ~ weeksDays ^^ { case n ~ factor => n.toInt * factor }

  def weeksDays = weeks | days
  def weeks = "weeks?".r ^^ { _ => 5 }
  def days = "days?".r ^^ { _ => 1 }

  /** @return Parser[Money] */
  def deductItems = repsep(deductItem, ",") ^^ { items => items.foldLeft(Money(0)) { _ + _ } }

  def deductItem = deductKind ~> deductAmount
  def deductKind = tax | insurance | retirement
  def tax = fedState <~ "income" <~ "tax"
  def fedState = "federal" | "state"
  def insurance = "insurance" ~> "premiums"
  def retirement = "retirement" ~> "fund" ~> "contributions"
  def deductAmount = percentage | amount
  def percentage = toBe ~> doubleNumber <~ "percent" <~ "of" <~ "gross" ^^ { percentage =>
    grossAmount * (percentage / 100.0)
  }
  def amount = toBe ~> doubleNumber <~ "in" <~ "gross" <~ "currency" ^^ { Money(_) }
  def toBe = "is" | "are"
  def doubleNumber = floatingPointNumber ^^ { _.toDouble }
  def salaryForDays(days: Int) = (currentEmployee.annualGrossSalary / 260.0) * days
}
```

- parser utilizza map(name) degli employee conosciuti per semplicità.

- `paycheck`: Combina i parser per dipendente (`empl`), stipendio lordo (`gross`) e detrazioni (`deduct`) per restituire una coppia (`Employee`, `Paycheck`).

- `currentEmployee` e `grossAmount` memorizzano rispettivamente il dipendente, il parser sta elaborando e paga lo stipendio lordo per i periodi di paga;

Questa versione di parser è un'evoluzione di quello precedente che prende in considerazione ciò che dovrevve essere il risultato finale.

```scala
def paycheck = empl ~ gross ~ deduct ^^ {case e~g~d => (e, Paycheck(g, g-d, d))}
```

restituirà una coppia con Employee e Paycheck

- il combinatore `^^`, `p1^^f1` applica f1 al risultato di p1 quando ha successo
```scala
def empl = "paycheck" ~> "for" ~> "employee" ~> employeeName ^^ { name =>
  val names = name.substring(1, name.length - 1).split(" ")
  val n = Name(names(0), names(1))
  if (!employees.contains(n)) throw new UnknownEmployee(n)
  currentEmployee = employees(n)
  currentEmployee
}
```

- `weeks` e `days` ignorano la stringa parsed e restituiscono un fattore multiplicazione utilizzato per determinare il totale dei giorni nella `duration` production rule

```scala
import payroll._
import payroll.Type2Money._
import payroll.pcdsl._

object PayRollBuilder {
  def main(args: Array[String]) = {
    val buck = Employee(Name("Buck", "Trends"), Money(80000))
    val jane = Employee(Name("Jane", "Doe"), Money(90000))
    val employees = Map(buck.name -> buck, jane.name -> jane)
    val p = new PayrollParserCombinators(employees)

    args.foreach { filename =>
      val src = scala.io.Source.fromFile(filename)
      val lines = src.mkString
      p.parseAll(p.paycheck, lines) match {
        case p.Success((employee, paycheck), _) =>
          println(f"${employee.name.first} ${employee.name.last}: $paycheck")
        case x => println(x.toString)
      }
      src.close()
    }
  }
}

```
## Esempi Input e Output
```scala
paycheck for employee "Jane Doe"
is salary for 2 weeks minus deductions for {}
```
- Questo programma calcola la busta paga di un dipendente di nome "Jane Doe" per due settimane. Poiché le detrazioni sono indicate con delle parentesi graffe vuote, significa che non ci sono detrazioni da applicare in questo caso.


```scala
paycheck for employee "Buck Trends"
is salary for 2 weeks minus deductions for {
  federal income tax is 25. percent of gross,
  state income tax is 5. percent of gross,
  insurance premiums are 500. in gross currency,
  retirement fund contributions are 10. percent of gross
}
```

- Questo programma fa lo stesso calcolo, ma per un dipendente di nome "Buck Trends". In questo caso, vengono specificate diverse tipologie di detrazioni: tasse federali e statali calcolate come percentuale dello stipendio lordo, premi assicurativi e contributi previdenziali.

Questo esempio invece è errato:

```scala
paycheck for employee "John Doe"
is salary for 2 weeks minus deductions for {}
```

- Questo programma cerca di calcolare la busta paga di un dipendente che non esiste nel sistema ("John Doe"). Per questo motivo, viene generato un errore.

Output:

```cmd
[16:29]cazzola@surtur:~/lp/scala/>scala PayRollBuilder test1.pr test2.pr test3.pr
Jane Doe: Paycheck($3461.54,$3461.54,$0.00)
Buck Trends: Paycheck($3076.92,$1346.15,$1730.77)
payroll.pcdsl.UnknownEmployee: Name(John,Doe)
at payroll.pcdsl.PayrollParserCombinators$$anonfun$empl$4.apply(payroll-pc.scala:24)
```

Questo DSL è progettato per semplificare la gestione dei calcoli delle buste paga.
- Il linguaggio è intuitivo e facile da leggere, anche per chi non ha conoscenze approfondite di programmazione.
- Il DSL è in grado di gestire diversi tipi di detrazioni e di calcolare la busta paga netta.
- Il DSL è in grado di gestire anche gli errori, come nel caso di un dipendente inesistente.
