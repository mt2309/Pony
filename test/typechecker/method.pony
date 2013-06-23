object M {
  function method(q:Int)->() {
    //does nothing
  }
}

object M1 {

  function init()->() {
    var x: M = M
    x.method()
  }
}
