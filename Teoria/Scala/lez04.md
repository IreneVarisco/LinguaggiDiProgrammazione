---
File          : lez04.md
Author        : Kevin
Date          : 10/01/2023, 14:14
output        :
  pdf_document: default
  html_notebook: default
---

# Parser Combinators

<!--toc:start-->
- [Parser Combinators](#parser-combinators)
<!--toc:end-->

---

> + Sono interni ai linguaggi stessi
> + Un aspetto importante è proprio la modularità

Sono a strategia di **Discesa Ricorsiva**

Si basano sulle Grammatiche: quadrupla
`< Insieme simboli terminali, Insieme simboli non terminali, assioma, regole di produzione >`

I **Parser Combinators** sono dei *blocchetti* che possono essere combinati insieme per poter
parsare anche grandi quantità di dati.

