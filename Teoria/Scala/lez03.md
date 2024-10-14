<!-- info:start -->
---
    title         : lez03.md
    author        : Kevin
    date          : 20/12/2022
    output        : 
      pdf_document: default
      html_notebook: default
---
<!-- info:end -->

# lez03.md

---

È possibile fare l'overriding dei `Fields` nei `Traits` 

```scala
  trait T1 { val name = "T1" }
  class Base
  class ClassWithT1 extends Base with T1 { override val name = "ClassWithT1" }

  val c = new ClassWithT1()
  println(c.name)

  class ClassExtendsT1 extends T1 { override val name = "ClassExtendsT1" }

  val c2 = new ClassExtendsT1()
  println(c2.name)
```
