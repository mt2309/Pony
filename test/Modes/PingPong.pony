object Buffer {
  new build(x: Int, y: Int, z: Int) { }

  function print() { }

  function~ append(x: Int) { }

  function~ empty() { }
}

actor Receiver {
  new construct() {

  }

  message receive(buff: Buffer[:uniq]) {
    buff.append(4)
  }

  message printBuffer(buff: Buffer[:imm]) {
    buff.print()
  }
}

actor Main {
  function main() {
    var x:Receiver = Receiver.construct()
    var buff: Buffer[:uniq] = Buffer.build(1,2,3)
    x.receive(buff)

    var toPrint: Buffer[:imm] = Buffer.build(1,1,1)
    x.printBuffer(toPrint)
    buff.empty()
  }
}

