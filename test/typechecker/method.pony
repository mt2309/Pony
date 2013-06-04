object M {
  function method(q:Int)->() {
    //does nothing
  }
}

object M1 {

  function main()->() {
    var x: M = M
    x.method()
  }
}
