package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      b() * b() - 4 * a() * c()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      if (delta() < 0) Set.empty
      else {
        val deltaRoot = math.sqrt(delta())

        def posSolve(root: Double): Double = (-b() + root) / (2 * a())
        Set(posSolve(deltaRoot), posSolve(-deltaRoot))
      }
    }
  }
}
