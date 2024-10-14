# Lez02.md

08/11/2022

---


Tutto ciò che ha la prima lettera maiuscola è una variabile, altrimenti è un atomo.

> Erlang use **Actors Model** for approaching to concurrences
> It doesn't use *thread*


> Ogni oggetto è un **attore**
> > + ha una casella di posta ed un comportamento
> > + gli attori comunicano tramite uno scambio di messaggi bufferato nella mailbox

**Non c'è nessuna garanzia che i messaggi siano inoltrati ed arrivino a destinazione**


**ACTORS**

+ `spawn()        ` Costruzione di attore
+ `!              ` Invio di un messaggio
+ `<pattern-match>` Ricezione di un messaggio

> pid <nodo_macchina>.<current_pid>.<non_usato>

Esempio
  : `pid < 0.36.0 >`



Il figlio conosce il proprio `pid` ma non conosce quello del padre.
Mentre il padre conosce il `pid` del padre! 


Erlang è in grado di gestire milioni di attori senza crearea problemi alla macchina su cui
sta girando.


