object M {
  function method(q: Int)->() {
    //does nothing
  }
}

actor M1 {

  function main()->() {
    var x: M = M
    x.method(1)
  }
}
