object Buffer {
  new build(x: Int, y: Int, z: Int) { }

  function~ append(x: Int) { }

  function~ empty() { }
}

actor Receiver {
  new construct() {

  }

  message receive(buff: Buffer[:imm]) {
    buff.append(4)
  }
}

actor Main {
  function main() {
    var x: Receiver = Receiver.construct()
    var buff: Buffer[:imm] = Buffer.build(1,2,3)
    x.receive(buff)
    buff.empty()
  }
}

