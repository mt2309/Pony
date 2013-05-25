package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.Common._
import com.github.mt2309.pony.AST._
import scala.annotation.tailrec

/**
 * User: mthorpe
 * Date: 11/05/2013
 * Time: 16:30
 */
final class LowerTypeChecker(val topTypes: Set[PreTypedModule]) {

  def typeCheck: Set[TypedModule] = {topTypes.map(checkModule(_))}

  private def checkModule(module: PreTypedModule): TypedModule = {
    implicit val scope: Scope = module.scope
    new TypedModule(module.imports, module.classes.map(c => c._1 -> checkClass(c._2)))
  }

  private def checkClass(moduleMember: ModuleMember)(implicit scope: Scope): TModuleMember = moduleMember match {
    case Primitive(name) => throw new PrimitiveFound(s"Primitive $name found where it should not be")
    case Declare(t, is, map) => new TDeclare(checkTypeClass(t), checkIs(is), checkDeclareMap(map, is))
    case Type(name, of, is) => new TType(name, checkOf(of), checkIs(is))
    case Actor(name, formal, is, typeBody) => new TActor(name, checkFormal(formal), checkIs(is), checkTypeBody(typeBody))
    case Object(name, formal, is, typeBody) => new TObject(name, checkFormal(formal), checkIs(is), checkTypeBody(typeBody))
    case Trait(name, formal, is, typeBody) => new TTrait(name, checkFormal(formal), checkIs(is), checkTypeBody(typeBody))
  }

  private def checkTypeClass(typeclass: TypeClass)(implicit scope: Scope): TTypeClass = {
    val t = scope.search(typeclass)

    new TTypeClass(typeclass.name, typeclass.module, checkMode(typeclass.mode), checkFormal(typeclass.formalArgs))
  }

  private def checkIs(is: Is)(implicit scope: Scope): TIs = new TIs(is.list.map(checkTypeClass(_)))

  private def checkDeclareMap(decMap: DeclareMap, is: Is)(implicit scope: Scope): TDeclareMap = {
    new TDeclareMap(decMap.map.map(checkPonyMap(_, is)))
  }

  private def checkPonyMap(pm: PonyMap, is: Is)(implicit scope: Scope): TPonyMap = new TPonyMap(bodyLookUp(pm.from, is), pm.to)

  private def checkOf(of: OfType)(implicit scope: Scope): TOfType = new TOfType(of.typeList.map(checkTypeElement(_)))

  private def checkTypeElement(element: TypeElement)(implicit scope: Scope): TTypeElement = element match {
    case PartialType(name) => new TPartialType(checkTypeClass(name))
    case t: TypeClass => checkTypeClass(t)
    case l: Lambda => checkLambda(l)
  }

  private def checkLambda(l: Lambda)(implicit scope: Scope): TLambda = {
    new TLambda(checkMode(l.mode), checkArgs(l.args), checkOptArgs(l.result), l.throws, checkOptBlock(l.block))
  }

  private def checkArgs(list: List[Arg])(implicit scope: Scope): List[TArg] = list.map(checkArg(_))

  private def checkArg(arg: Arg)(implicit scope: Scope): TArg = {
    val expr = arg.expr.map(checkExpression(_))
    val ofType = arg.ofType.map(checkOf(_))
    val assign = arg.assign.map(checkAssignment(_, ofType.get))

    new TArg(expr, ofType, assign)
  }

  private def checkAssignment(expr: Expr, ofType: TOfType)(implicit scope: Scope): TExpr = {
    val ex = checkExpression(expr)

    // TODO: handle sub-typing relationship.
    if (ex.extractOfType.typeList forall (ofType.typeList.contains))
      ex
    else
      throw new AssignmentException(s"Type error, type from assign does not match lvalue type")
  }

  private def checkExpression(expr: Expr)(implicit scope: Scope): TExpr = {
    new TExpr(checkUnary(expr.unary), checkOpUnary(expr.operator))
  }

  private def checkUnary(unary: Unary)(implicit scope: Scope): TUnary = unary match {
    case UnaryCommand(ops, command) => new TUnaryCommand(ops, checkCommand(command))
    case UnaryLambda(ops, lambda) => new TUnaryLambda(ops, checkLambda(lambda))
  }

  private def checkCommand(command: Command)(implicit scope: Scope): TCommand = {
    val fst: TFirstCommand = checkFirstCommand(command.first)
    val ofType: TOfType = fst.extractOfType

    new TCommand(fst, command.second.map(checkSecondCommand(_, ofType)))
  }

  private def checkFirstCommand(fst: FirstCommand)(implicit scope: Scope): TFirstCommand = fst match {
    case CommandExpr(expr) => new TCommandExpr(checkExpression(expr))
    case CommandArgs(args) => new TCommandArgs(checkArgs(args))
    case t: Atom => matchAtom(t)
  }

  private def matchAtom(a: Atom): TAtom = a match {
    case This => TThis
    case True => TTrue
    case False => TFalse
    case PonyInt(i: Int) => new TPonyInt(i)
    case PonyDouble(d: Double) => new TPonyDouble(d)
    case PonyString(s: String) => new TPonyString(s)
    case PonyID(i: ID) => new TPonyID(i)
    case PonyTypeId(t: TypeId) => new TPonyTypeId(t)
  }

  private def checkSecondCommand(snd: SecondCommand, ofType: TOfType)(implicit scope: Scope): TSecondCommand = snd match {
    case SecondCommandArgs(args) => new TSecondCommandArgs(checkArgs(args))
    case CommandCall(id, formalArgs, args) => new TCommandCall(bodyLookUp(id, ofType), checkFormal(formalArgs), checkArgs(args))
  }

  private def checkOpUnary(list: List[(Operator, Unary)])(implicit scope: Scope): List[(Operator, TUnary)] = {
    list.map(t => t._1 -> checkUnary(t._2))
  }

  private def checkOptArgs(l: Option[List[Arg]])(implicit scope: Scope): Option[List[TArg]] = {
    l.map(checkArgs(_))
  }

  private def checkOptBlock(block: Option[Block])(implicit scope: Scope): Option[TBlock] = {
    block.map(checkBlock(_))
  }

  private def checkBlock(block: Block)(implicit scope: Scope): TBlock = {
    new TBlock(checkBlockContents(block.contents), checkOptBlock(block.catchBlock), checkOptBlock(block.alwaysBlock))
  }

  private def checkBlockContents(list: List[BlockContent])(implicit scope: Scope): List[TBlockContent] = list match {
    case x::xs => {
      val ret: (TBlockContent, Scope) = checkBlockContent(x)
      ret._1 :: checkBlockContents(xs)(ret._2)
    }
    case Nil => Nil
  }

  private def checkBlockContent(b: BlockContent)(implicit scope: Scope): (TBlockContent, Scope) = b match {
    case Return => (TReturn, scope)
    case Throw => (TThrow, scope)
    case Break => (TBreak, scope) // TODO: Should we check if we're in a loop here
    case Continue => (TContinue, scope)
    case b:Block => (checkBlock(b), scope)
    case VarDec(id, ofType, expr) => {
      // TODO type inference could be done here
      assert(ofType.isDefined, s"Variable $id has no type parameter assigned to it, when inference is added this will go away")
      val of = ofType.map(checkOf(_)).get
      if (scope.varScope.contains(id))
        throw new VariableShadowingException(s"Variable $id, of type $of shadows variable with type ${scope.varScope.get(id)}")
      else {
        // TODO: check assignment type matches
        (new TVarDec(id, Some(of), expr.map(checkExpression(_))), scope.copy(varScope = scope.varScope + (id -> of)))
      }
    }
    case Match(list, cases) => ???
    case DoLoop(block, whileExpr) => ???
    case WhileLoop(whileExpr, block) => ???
    case ForLoop(forVars, inExpr, block) => ???
    case Conditional(condList, elseBlock) => ???
    case Assignment(lValues, optExpr) => ???
  }

  private def checkFormal(formal: FormalArgs)(implicit scope: Scope): TFormalArgs = formal.map(checkTypeClass(_))

  private def checkTypeBody(typeBody: TypeBody)(implicit scope: Scope): TTypeBody = new TTypeBody(typeBody.body.map(t => t._1 -> checkBodyContent(t._2)))

  private def checkBodyContent(bodyContent: BodyContent)(implicit scope: Scope): TBodyContent = bodyContent match {
    case Field(name, ofType, expr) => new TField(name, checkOf(ofType), expr.map(checkExpression(_)))
    case Delegate(name, ofType) => new TDelegate(name, checkOf(ofType))
    case Constructor(contents, throws, block) => new TConstructor(checkContents(contents), throws, block.map(checkBlock(_)))
    case Ambient(contents, throws, block) => new TAmbient(checkContents(contents), throws, block.map(checkBlock(_)))
    case Function(contents, results, throws, block) => new TFunction(checkContents(contents), checkOptArgs(results), throws, block.map(checkBlock(_)))
    case Message(contents, block) => new TMessage(checkContents(contents), block.map(checkBlock(_)))
  }

  private def checkContents(c: MethodContent)(implicit scope: Scope): TMethodContent = {
    val comb = new TCombinedArgs(checkFormal(c.combinedArgs.formalArgs), checkArgs(c.combinedArgs.args))
    new TMethodContent(checkMode(c.mode), c.id, comb)
  }

  private def checkMode(mode: Mode)(implicit scope: Scope): TMode = mode match {
    case ReadOnly => TReadOnly
    case Immutable => TImmutable
    case Mutable => TMutable
    case Unique => TUnique
    case ModeExpr(expr) => new TModeExpr(checkExpression(expr))
  }

  private def bodyLookUp(name: ID, is: Is)(implicit scope: Scope): BodyContent = {
    for (i <- is.list) {
      val typeclass = scope.search(i)
    }
    ???
  }

  private def bodyLookUp(name: ID, ofType: TOfType)(implicit scope: Scope): BodyContent = ???
}