
class C1 { def m = List("C1") }
trait T1 extends C1 { override def m = { "T1" :: super.m } }
trait T2 extends C1 { override def m = { "T2" :: super.m } }
trait T3 extends C1 { override def m = { "T3" :: super.m } }
class C2A extends T2 { override def m = { "C2A" :: super.m } }
class C2 extends C2A with T1 with T2 with T3 { override def m = { "C2" :: super.m } }
def linearization(obj: C1, name: String) = {
  val lin = obj.m ::: List("ScalaObject", "AnyRef", "Any") println(name + ": " + lin)
}

