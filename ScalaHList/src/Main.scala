
trait HList {
  def ::[E](e: E): HList
  def foldr[V](f: (Any, V) => V, i: V) : V
  def ++(other: HList): HList
}

case class HCons[H, T <: HList](h: H, t: T) extends HList{
  override def ::[E](nh: E): HList = HCons(nh, this)
  override def foldr[V](f: (Any, V) => V, i: V): V = f(h, t.foldr(f, i))
  override def ++(other: HList): HList = h :: (t ++ other)
}

object HNil extends HList {
  override def ::[E](h: E) = HCons(h, this)
  override def foldr[V](f: (Any, V) => V, i: V): V = i
  override def ++(other: HList): HList = other
}

object Main {
  def main(args: Array[String]) {
    val x = "str" :: true :: HNil
    val y = 1 :: 2 :: new Object :: HNil
    val xy = x ++ y

    println("length of x: " + x.foldr((x: Any, v: Int) => v + 1, 0))
    println()
    xy.foldr((e: Any, v: Int) => {println(e); 0}, 0)
  }
}
