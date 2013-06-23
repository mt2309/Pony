trait Option[K] {
  function isDefined()->(y: Bool);
  function get()->(res: K);
}

object Some[K] is Option[K] {
  var value: K;

  new of(v: K) { value = v }

  function isDefined()->(y: Bool) { y = true }

  function get()->(res: K) { res = value }
}

object None[K] is Option[K] {
  function isDefined()->(y: Bool) { y = false }

  function get()->(res: K) { throw }
}
