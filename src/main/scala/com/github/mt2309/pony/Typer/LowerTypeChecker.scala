package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.Common._
import com.github.mt2309.pony.AST._
import scala.annotation.tailrec

/**
 * User: mthorpe
 * Date: 11/05/2013
 * Time: 16:30
 */
final class LowerTypeChecker(val topTypes: Set[ITypedModule]) {

  def typeCheck: Set[TypedModule] = {topTypes.map(checkModule)}

  private def checkModule(module: ITypedModule): TypedModule = {
    implicit val scope: Scope = module.scope
    new TypedModule(module.imports, module.types.map(c => c._1 -> checkClass(c._2)))
  }

  private def checkClass(moduleMember: IModuleMember)(implicit scope: Scope): TModuleMember = moduleMember match {
    case IPrimitive(name) => throw new PrimitiveFound(s"Primitive $name found where it should not be")
    case d:IDeclare => new TDeclare(d.name, checkIs(d.is), checkDeclareMap(d.declareMap, d.is)).setPos(d.pos)
    case t:IType => new TType(t.typename, checkOf(t.ofType), checkIs(t.is))
    case c:IActor => new TActor(  c.n, checkIFormal(c.f), checkIs(c.i), checkTypeBody(c.t)).setPos(c.pos)
    case c:IObject => new TObject(c.n, checkIFormal(c.f), checkIs(c.i), checkTypeBody(c.t)).setPos(c.pos)
    case c:ITrait => new TTrait(  c.n, checkIFormal(c.f), checkIs(c.i), checkTypeBody(c.t)).setPos(c.pos)
  }

  private def checkTypeClass(typeclass: ITypeClass)(implicit scope: Scope): TTypeClass = {
    new TTypeClass(typeclass.iType, checkMode(typeclass.mode), checkFormal(typeclass.formalArgs)).setPos(typeclass.pos)
  }

  private def checkIFormal(formal: IFormalArgs)(implicit scope: Scope): TFormalArgs = {
    formal.map(checkTypeClass)
  }

  private def checkIs(is: IIs)(implicit scope: Scope): TIs = new TIs(is.list.map(checkTypeClass)).setPos(is.pos)

  private def checkOf(of: IOfType)(implicit scope: Scope): TOfType = new TOfType(of.typeSet.map(checkTypeElement)).setPos(of.pos)

  private def checkTypeClass(typeclass: TypeClass)(implicit scope: Scope): TTypeClass = {
    val t = scope.search(typeclass)

    new TTypeClass(t, checkMode(typeclass.mode), checkFormal(typeclass.formalArgs)).setPos(typeclass.pos)
  }

  private def checkDeclareMap(decMap: DeclareMap, is: IIs)(implicit scope: Scope): TDeclareMap = {
    new TDeclareMap(decMap.map.map(checkPonyMap(_, is))).setPos(decMap.pos)
  }

  private def checkPonyMap(pm: PonyMap, is: IIs)(implicit scope: Scope): TPonyMap = {
    new TPonyMap(scope.findMethod(pm.from, is), pm.to).setPos(pm.pos)
  }

  private def checkOf(of: OfType)(implicit scope: Scope): TOfType = {
    new TOfType(of.typeList.map(checkTypeElement)).setPos(of.pos)
  }

  private def checkTypeElement(elem: ITypeElement)(implicit scope: Scope): TTypeElement = elem match {
    case IPartialType(clazz) => new TPartialType(checkTypeClass(clazz)).setPos(elem.pos)
    case t: ITypeClass => checkTypeClass(t)
    case l: ILambda => checkLambda(l)
  }

  private def checkTypeElement(element: TypeElement)(implicit scope: Scope): TTypeElement = element match {
    case PartialType(name) => new TPartialType(checkTypeClass(name)).setPos(element.pos)
    case t: TypeClass => checkTypeClass(t)
    case l: Lambda => checkLambda(l)
  }

  private def checkLambda(l: ILambda)(implicit scope: Scope): TLambda = {
    new TLambda(checkMode(l.mode), checkArgs(l.args), checkParams(l.result), l.throws, l.block.map(checkBlock)).setPos(l.pos)
  }

  private def checkLambda(l: Lambda)(implicit scope: Scope): TLambda = {
    new TLambda(checkMode(l.mode), checkArgs(l.args), checkParams(l.result), l.throws, l.block.map(checkBlock)).setPos(l.pos)
  }

  private def checkArgs(list: List[Arg])(implicit scope: Scope): List[TArg] = list.map(checkArg)

  private def checkArg(arg: Arg)(implicit scope: Scope): TArg = {
    val expr = arg.expr.map(checkExpression)
    val ofType = arg.ofType.map(checkOf)
    val assign = arg.assign.map(checkAssignment(_, ofType.get))

    new TArg(expr, ofType, assign).setPos(arg.pos)
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
    new TExpr(checkUnary(expr.unary), checkOpUnary(expr.operator)).setPos(expr.pos)
  }

  private def checkUnary(unary: Unary)(implicit scope: Scope): TUnary = unary match {
    case UnaryCommand(ops, command) => new TUnaryCommand(ops, checkCommand(command)).setPos(unary.pos)
    case UnaryLambda(ops, lambda) => new TUnaryLambda(ops, checkLambda(lambda)).setPos(unary.pos)
  }

  private def checkCommand(command: Command)(implicit scope: Scope): TCommand = {
    val fst: TFirstCommand = checkFirstCommand(command.first)
    val ofType: TOfType = fst.extractOfType

    new TCommand(fst, command.second.map(checkSecondCommand(_, ofType))).setPos(command.pos)
  }

  private def checkFirstCommand(fst: FirstCommand)(implicit scope: Scope): TFirstCommand = fst match {
    case CommandExpr(expr) => new TCommandExpr(checkExpression(expr)).setPos(fst.pos)
    case CommandArgs(args) => new TCommandArgs(checkArgs(args)).setPos(fst.pos)
    case t: Atom => matchAtom(t).setPos(t.pos)
  }

  private def matchAtom(a: Atom)(implicit scope: Scope): TAtom = a match {
    case This => new TThis
    case True => new TTrue
    case False => new TFalse
    case PonyInt(i: Int) => new TPonyInt(i)
    case PonyDouble(d: Double) => new TPonyDouble(d)
    case PonyString(s: String) => new TPonyString(s)
    case PonyID(i: ID) => new TPonyID(i)
    case PonyTypeId(t: TypeId) => new TPonyTypeId(t)
  }

  private def checkSecondCommand(snd: SecondCommand, ofType: TOfType)(implicit scope: Scope): TSecondCommand = snd match {
    case SecondCommandArgs(args) => new TSecondCommandArgs(checkArgs(args)).setPos(snd.pos)
    case CommandCall(id, formalArgs, args) => {
      new TCommandCall(scope.findMethod(id, ofType), checkFormal(formalArgs), checkArgs(args)).setPos(snd.pos)
    }
  }

  private def checkOpUnary(list: List[(Operator, Unary)])(implicit scope: Scope): List[(Operator, TUnary)] = {
    list.map(t => t._1 -> checkUnary(t._2))
  }

  private def checkBlock(block: Block)(implicit scope: Scope): TBlock = {
    new TBlock(checkBlockContents(block.contents), block.catchBlock.map(checkBlock), block.alwaysBlock.map(checkBlock)).setPos(block.pos)
  }

  private def checkBlockContents(list: List[BlockContent])(implicit scope: Scope): List[TBlockContent] = list match {
    case x::xs => {
      val ret: (TBlockContent, Scope) = checkBlockContent(x)
      ret._1.setPos(x.pos) :: checkBlockContents(xs)(ret._2)
    }
    case Nil => Nil
  }

  private def checkBlockContent(b: BlockContent)(implicit scope: Scope): (TBlockContent, Scope) = b match {
    case Return => (TReturn, scope)
    case Throw => (TThrow, scope)
    case Break => (TBreak, scope) // TODO: Should we check if we're in a loop here
    case Continue => (TContinue, scope)
    case b:Block => (checkBlock(b), scope)
    case v: VarDec => checkVarDec(v)
    case Match(list, cases) => new TMatch(list.map(checkExpression), cases.map(checkCaseBlock)).setPos(b.pos) -> scope
    case DoLoop(block, whileExpr) => new TDoLoop(checkBlock(block), checkBooleanExpr(whileExpr)).setPos(b.pos) -> scope
    case WhileLoop(whileExpr, block) => new TWhileLoop(checkBooleanExpr(whileExpr), checkBlock(block)).setPos(b.pos) -> scope
    case ForLoop(forVars, inExpr, block) => {
      val fV = checkForVars(forVars)
      val sc = fV.last.scope
      new TForLoop(fV, checkExpression(inExpr), checkBlock(block)(sc)).setPos(b.pos) -> scope
    }
    case Conditional(condList, elseBlock) => {
      new TConditional(condList.map(t => checkBooleanExpr(t._1) -> checkBlock(t._2)), elseBlock.map(checkBlock)).setPos(b.pos) -> scope
    }
    case Assignment(lValues, optExpr) => {
      new TAssignment(lValues.map(checkLValue), optExpr.map(checkExpression)).setPos(b.pos)-> scope
    }
  }

  def checkForVars(list: List[ForVar])(implicit scope: Scope): List[TForVar] = list match {
    case x :: xs => {
      val ret = checkForVar(x)
      ret._1 :: checkForVars(xs)(ret._2)
    }
    case Nil => Nil
  }

  def checkLValue(l: LValue)(implicit scope: Scope): TLValue = l match {
    case LValueVar(nVar) => {
      val varDec = checkVarDec(nVar)
      new TLValueVar(varDec._1)(varDec._2).setPos(l.pos)
    }
    case LValueCommand(command) => new TLValueCommand(checkCommand(command)).setPos(l.pos)
  }

  def checkVarDec(nVar: VarDec)(implicit scope: Scope): (TVarDec, Scope) = {
    val of = nVar.ofType.map(checkOf).getOrElse(throw new TyperInferenceException)
    new TVarDec(nVar.id, of, nVar.assign.map(checkExpression)) -> scope.updateScope(nVar.id, of)
  }

  def checkBooleanExpr(expr: Expr)(implicit scope: Scope): TExpr = {
    val ex = checkExpression(expr)
    if (ex.extractOfType eq boolOfType)
      ex
    else
      throw new TypeMismatch(ex.extractOfType.toString, boolOfType.toString)
  }

  private def checkCaseBlock(c: CaseBlock)(implicit scope: Scope): TCaseBlock = {
    val sub = c.subBlock.map(checkCaseSubBlock)
    val sc = sub.map(_._2).getOrElse(scope)
    new TCaseBlock(sub.map(_._1), checkBlock(c.block)(sc))
  }

  private def checkCaseSubBlock(sub: CaseSubBlock)(implicit scope: Scope): (TCaseSubBlock, Scope) = sub match {
    case CaseIf(expr) => new TCaseIf(checkBooleanExpr(expr)) -> scope
    case CaseVarList(varList) => {
      val list = checkVarList(varList)
      new TCaseVarList(list) -> list.last.scope
    }
  }

  private def checkVarList(list: List[CaseVar])(implicit scope: Scope): List[TCaseVar] = list match {
    case x :: xs => {
      val ret = checkCaseVar(x)
      ret._1 :: checkVarList(xs)(ret._2)
    }
    case Nil => Nil
  }

  private def checkCaseVar(caseVar: CaseVar)(implicit scope: Scope): (TCaseVar, Scope) = {
    val forVar = checkForVar(caseVar.forVar)
    new TCaseVar(caseVar.expr.map(checkExpression(_)(forVar._2)), forVar._1) -> forVar._2
  }

  private def checkForVar(forVar: ForVar)(implicit scope: Scope): (TForVar, Scope) = {
    val of = checkOf(forVar.ofType.getOrElse(throw new TyperInferenceException))
    new TForVar(forVar.id, of) -> scope.updateScope(forVar.id, of)
  }

  private def checkFormal(formal: FormalArgs)(implicit scope: Scope): TFormalArgs = formal.map(checkTypeClass)

  private def checkTypeBody(typeBody: TypeBody)(implicit scope: Scope): TTypeBody = new TTypeBody(checkBodyContents(typeBody.body))

  private def checkBodyContents(bd: Map[ID, BodyContent])(implicit scope: Scope): Map[ID, TBodyContent] = {
    if (bd.isEmpty) {
      Map.empty
    }
    else {
      val tup = checkBodyContent(bd.head._2)
      Map(bd.head._1 -> tup._1) ++ checkBodyContents(bd.tail)(tup._2)
    }
  }

  private def checkBodyContent(bodyContent: BodyContent)(implicit scope: Scope): (TBodyContent, Scope) = bodyContent match {
    case Field(name, ofType, expr) => {
      val of = checkOf(ofType)
      new TField(name, of, expr.map(checkExpression)) -> scope.updateScope(name, of)
    }
    case Delegate(name, ofType) => {
      val of = checkOf(ofType)
      new TDelegate(name, of) -> scope.updateScope(name, of)
    }
    case Constructor(contents, throws, block) => new TConstructor(checkContents(contents), throws, block.map(checkBlock)) -> scope
    case Ambient(contents, throws, block) => new TAmbient(checkContents(contents), throws, block.map(checkBlock)) -> scope
    case Function(contents, results, throws, block) => {
      val res = checkParams(results)
      new TFunction(checkContents(contents), res, throws, block.map(checkBlock)) -> scope
    }
    case Message(contents, block) => new TMessage(checkContents(contents), block.map(checkBlock)) -> scope
  }

  private def checkParams(list: Params)(implicit scope: Scope): TParams = list match {
    case x :: xs => {
      val res = checkParam(x)
      res._1 :: checkParams(xs)(res._2)
    }
    case Nil => Nil
  }

  private def checkParam(p: Param)(implicit scope: Scope): (TParam, Scope) = {
    val of = checkOf(p.ofType)

    new TParam(p.name, of) -> scope.updateScope(p.name, of)
  }

  private def checkContents(c: MethodContent)(implicit scope: Scope): TMethodContent = {
    val comb = new TCombinedArgs(checkFormal(c.combinedArgs.formalArgs), checkParams(c.combinedArgs.args))
    new TMethodContent(checkMode(c.mode), c.id, comb)
  }

  private def checkMode(mode: Mode)(implicit scope: Scope): TMode = mode match {
    case ReadOnly => TReadOnly
    case Immutable => TImmutable
    case Mutable => TMutable
    case Unique => TUnique
    case ModeExpr(expr) => new TModeExpr(checkExpression(expr))
  }

//  private def bodyLookUp(name: ID, is: IIs)(implicit scope: Scope): BodyContent = {
//    for (i <- is.list) {
//      val typeclass = i
//    }
//    ???
//  }
//
//  private def bodyLookUp(name: ID, ofType: TOfType)(implicit scope: Scope): BodyContent = ???
}