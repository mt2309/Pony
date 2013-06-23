actor Accumulator {
  var actorCount: Int;
  var actorReceive: Int = 0
  var total : Double = 0d

  new build(size: Int) {
    actorCount = size
  }

  message receive(partial: Double) {
    actorReceive = actorReceive + 1

    total = total + partial

    if (actorReceive == actorCount) {
      (IO.println(total))
    }
  }
}
