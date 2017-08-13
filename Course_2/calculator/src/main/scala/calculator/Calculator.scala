package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions mapValues {
      expr =>
        Signal {
          eval(expr(), namedExpressions)
        }
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {

    def evalRec(expr: Expr, deps: List[String]): Double =
      expr match {
        case Literal(v) => v
        case Ref(name) =>
          if (!deps.contains(name) && references.contains(name)) evalRec(references(name)(), name :: deps)
          else Double.NaN
        case Plus(a, b) => evalRec(a, deps) + evalRec(b, deps)
        case Minus(a, b) => evalRec(a, deps) - evalRec(b, deps)
        case Times(a, b) => evalRec(a, deps) * evalRec(b, deps)
        case Divide(a, b) => evalRec(a, deps) / evalRec(b, deps)
      }

    evalRec(expr, Nil)
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
