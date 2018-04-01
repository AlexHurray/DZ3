val str = "Hello"
 val num: Int = 42
 val xs: Seq[AnyVal] = Seq(1, 'a', 20f)
 //не скомпилируется так как 'new Object' имеет тип AnyRef
//var xsInv: Seq[AnyVal] = Seq(1, 'a', 20f, new Object())
val xs2 = Seq(1, 'a', 20f, new Object())