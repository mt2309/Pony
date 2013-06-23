actor Integrator {
  var iterations: Int;
  var actorCount: Int;
  var accumulator: Accumulator;

  new build(it: Int, count: Int, acc: Accumulator) {
    iterations = it
    actorCount = count
    accumulator = acc
  }

  function functionAt(x: Double)->(res: Double) {
    var square: Double = x * x + 1
    res = 4d / (square)

  }

  function trapeziumRule(h: Double, a: Double, b: Double)->(res: Double) {
    var at: Double = functionAt(a) + functionAt(b)
    res = (h * at)/ 2d
  }

  message partial_pi(rank: Int) {
    var stepSize: Double = 1d / iterations
    var localPi: Double = 0d

    var startPoint: Double = (rank) / (actorCount)
    var endPoint: Double = (rank + 1d) / (actorCount)

    while(startPoint < endPoint) {
      var nextPoint: Double = startPoint + stepSize
      localPi = localPi + trapeziumRule(stepSize,startPoint,nextPoint)
      startPoint = nextPoint
    }

    accumulator.receive(localPi)
  }
}

static object Main {
  function main(count: Int) {
    var acc: Accumulator = Accumulator.build(10)

    for i:Int in (0 to count) {
      var in: Integrator = Integrator.build(10000, count, acc)
      (in.partial_pi(i))
    }
  }
}

