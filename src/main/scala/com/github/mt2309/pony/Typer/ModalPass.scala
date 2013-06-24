package com.github.mt2309.pony.Typer

import collection.mutable

import com.github.mt2309.pony.Common.ID
import com.github.mt2309.pony.CodeGen.CodeGenContext

/**
 * User: mthorpe
 * Date: 24/06/2013
 * Time: 09:25
 */

private sealed abstract class V {
  def mode: TMode
  var read: Boolean
  var written: Boolean
  var sent: Boolean
  def isPrimitive: Boolean
  def update(mode: TMode): Unit = mode match {
    case r: TReadOnly => read = true
    case i: TImmutable => { read = true; sent = true }
    case m: TMutable => written = true
    case u: TUnique => { written = sent; sent = true }
    case e: TModeExpr =>
  }
  def cp: V
}
private final case class Instance(mode: TMode, isPrimitive: Boolean, var read: Boolean = false, var written: Boolean  = false, var sent: Boolean = false) extends V {
  override def cp = this.copy()
}
private final case class Local   (mode: TMode, isPrimitive: Boolean, var read: Boolean = false, var written: Boolean  = false, var sent: Boolean = false) extends V {
  override def cp = this.copy()
}


final class ModalPass(conc: ConcreteClass)(implicit context: CodeGenContext) {
  private var map: Map[ID, V] = Map.empty

  for (v <- conc.variables)
    map += v._1 -> new Instance(TyperHelper.mode(v._2), TyperHelper.isPrimitive(v._2))

  for (meth <- conc.methods) {
    val a: Unit = meth._2 match {
      case f: TField => Unit
      case d: TDelegate => Unit
      case c: TConstructor => Unit // Constructors don't do anything (perhaps)
      case m: TMethod => passMethod(m)
    }
  }

  private def passMethod(m: TMethod): Unit = {
    implicit val localMap: mutable.Map[ID, V] = mutable.Map()

    for (i <- map)
      localMap.update(i._1, i._2.cp)

    for (arg <- m.args) {
      localMap.update(arg.name, new Local(arg.mode, TyperHelper.isPrimitive(arg.ofType) || m.isInstanceOf[TMessage]))
    }

    m.block.map(passBlock)

    if (!process(localMap, m.name)) {
      val dis: Unit = m.mode match {
        case r: TReadOnly => Unit
        case i: TImmutable => Unit
        case i: TMutable => println(s"Method ${m.name} could be made readonly")
        case u: TUnique => println(s"Method ${m.name} could be made readonly")
        case e: TModeExpr => Unit
      }
    }
  }

  private def process(m: mutable.Map[ID, V], name: ID): Boolean = {
    var instance = false
    for (elem <- m) {
      val a: Unit = elem._2 match {
        case i: Instance => {
          instance |= i.written
          i.mode match {
            case r: TReadOnly => Unit
            case m: TImmutable => if (!i.sent && !i.isPrimitive) println(s"Immutable variable ${elem._1} could be made readonly") else Unit
            case m: TMutable => if (!i.read)   println(s"Mutable variable ${elem._1} could be made readonly") else Unit
            case u: TUnique => {
              Unit
            }
            case e: TModeExpr => Unit
          }
        }
        case l: Local => l.mode match {
          case r: TReadOnly => Unit
          case i: TImmutable => if (!l.sent && !l.isPrimitive) println(s"Immutable variable ${elem._1} could be made readonly") else Unit
          case m: TMutable => if (!l.read)   println(s"Mutable variable ${elem._1} could be made readonly") else Unit
          case u: TUnique => {
            Unit
          }
          case e: TModeExpr => Unit
        }
      }
    }

    instance
  }

  private def passBlock(b: TBlock)(implicit localMap: mutable.Map[ID, V]): Unit = for (cont <- b.contents) {
    val r: Unit = cont match {
      case t: AtomicContent => Unit
      case TAssignment(lvalue, expr) => {
        lvalue.map(passLValue)
        expr.map(passExpr)
      }
      case t: TBlock => passBlock(t)
      case TConditional(cond, eBlock) => {
        cond.map(b => {passExpr(b._1); passBlock(b._2)})
        eBlock.map(passBlock)
      }
      case TDoLoop(blk, expr) => {
        passBlock(blk)
        passExpr(expr)
      }
      case TWhileLoop(expr, blk) => {
        passExpr(expr)
        passBlock(blk)
      }
      case TForLoop(vars, range, block) => {
        vars.map(passForVar)
        passExpr(range._1)
        passExpr(range._2)
        passBlock(block)
      }
      case TMatch(matches) => matches.map(f => {passExpr(f._1); passCaseBlock(f._2) })
      case TVarDec(id, of, expr) => {
        localMap.update(id, new Local(TyperHelper.mode(of), TyperHelper.isPrimitive(of)))
        expr.map(passExpr)
      }
    }
  }

  private def passForVar(f: TForVar)(implicit localMap: mutable.Map[ID, V]): Unit = {
    localMap.update(f.id, new Local(TyperHelper.mode(f.ofType), TyperHelper.isPrimitive(f.ofType)))
  }

  private def passCaseBlock(c: TCaseBlock)(implicit localMap: mutable.Map[ID, V]): Unit = {
    c.c.map(passSubBlock)

    passBlock(c.block)
  }

  private def passSubBlock(sB: TCaseSubBlock)(implicit localMap: mutable.Map[ID, V]): Unit = {

  }

  private def passLValue(lvalue: TLValue)(implicit localMap: mutable.Map[ID, V]): Unit = lvalue match {
    case TLValueVar(dec) => {
      localMap.update(dec.id, new Local(TyperHelper.mode(dec.ofType), TyperHelper.isPrimitive(dec.ofType)))
      dec.expr.map(passExpr)
    }
    case TLValueCommand(command) => {
      passCommand(command)
    }
  }

  private def passCommand(command: TCommand)(implicit localMap: mutable.Map[ID, V]): Unit = {
    command.first match {
      case TCommandExpr(expr) => command.second match {
        case Some(snd) => Unit
        case None => Unit
      }
      case TCommandArgs(args) => command.second match {
        case Some(snd) => Unit
        case None => Unit
      }
      case TPonyID(id) => command.second match {
        case Some(snd) => snd match {
          case TSecondCommandArgs(args) => args.map(passArg)
          case TCommandCall(body, formal, args) => body match {
            case m: TInstanceVariable => throw new UnsupportedOperationException
            case m: TMethod => {
              val zip = m.args.zip(args)
              zip.map(t => passZArg(t._1, t._2))
              localMap(id).update(id.mode)
            }
          }
        }
        case None => Unit
      }
      case _ => Unit
    }
  }

  private def passZArg(param: TParam, arg: TArg)(implicit localMap: mutable.Map[ID, V]): Unit = {
    for (e <- arg.expr; id <- e.id) {
      localMap(id).update(param.mode)
    }
  }

  private def passArg(arg: TArg)(implicit localMap: mutable.Map[ID, V]): Unit = {
    arg.expr.map(passExpr)
    arg.assign.map(passExpr)
  }

  private def passExpr(expr: TExpr)(implicit localMap: mutable.Map[ID, V]): Unit = {
    passUnary(expr.unary)
    expr.operator.map(o => passUnary(o._2))
  }

  private def passUnary(un: TUnary)(implicit localMap: mutable.Map[ID, V]): Unit = un match {
    case TUnaryCommand(ops, command) => passCommand(command)
    case TUnaryLambda(ops, lambda) => Unit
  }
}
