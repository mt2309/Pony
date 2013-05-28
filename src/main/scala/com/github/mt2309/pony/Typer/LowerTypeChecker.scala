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
    case IDeclare(t, is, map) => new TDeclare(checkTypeClass(t), checkIs(is), checkDeclareMap(map, is))
    case IType(name, of, is) => new TType(name, checkOf(of), checkIs(is))
    case IActor(name, formal, is, typeBody) => new TActor(  name, checkIFormal(formal), checkIs(is), checkTypeBody(typeBody))
    case IObject(name, formal, is, typeBody) => new TObject(name, checkIFormal(formal), checkIs(is), checkTypeBody(typeBody))
    case ITrait(name, formal, is, typeBody) => new TTrait(  name, checkIFormal(formal), checkIs(is), checkTypeBody(typeBody))
  }

  private def checkTypeClass(typeclass: ITypeClass)(implicit scope: Scope): TTypeClass = {
    new TTypeClass(typeclass.iType, checkMode(typeclass.mode), checkFormal(typeclass.formalArgs))
  }

  private def checkIFormal(formal: IFormalArgs)(implicit scope: Scope): TFormalArgs = {
    formal.map(checkTypeClass)
  }

  private def checkIs(is: IIs)(implicit scope: Scope): TIs = new TIs(is.list.map(checkTypeClass))

  private def checkOf(of: IOfType)(implicit scope: Scope): TOfType = new TOfType(of.typeSet.map(checkTypeElement))

  private def checkTypeClass(typeclass: TypeClass)(implicit scope: Scope): TTypeClass = {
    val t = scope.search(typeclass)

    new TTypeClass(t, checkMode(typeclass.mode), checkFormal(typeclass.formalArgs))
  }

  private def checkIs(is: Is)(implicit scope: Scope): TIs = new TIs(is.list.map(checkTypeClass))

  private def checkDeclareMap(decMap: DeclareMap, is: IIs)(implicit scope: Scope): TDeclareMap = {
    new TDeclareMap(decMap.map.map(checkPonyMap(_, is)))
  }

  private def checkPonyMap(pm: PonyMap, is: IIs)(implicit scope: Scope): TPonyMap = new TPonyMap(bodyLookUp(pm.from, is), pm.to)

  private def checkOf(of: OfType)(implicit scope: Scope): TOfType = new TOfType(of.typeList.map(checkTypeElement))

  private def checkTypeElement(elem: ITypeElement)(implicit scope: Scope): TTypeElement = elem match {
    case IPartialType(clazz) => new TPartialType(checkTypeClass(clazz))
    case t: ITypeClass => checkTypeClass(t)
    case l: ILambda => checkLambda(l)
  }

  private def checkTypeElement(element: TypeElement)(implicit scope: Scope): TTypeElement = element match {
    case PartialType(name) => new TPartialType(checkTypeClass(name))
    case t: TypeClass => checkTypeClass(t)
    case l: Lambda => checkLambda(l)
  }

  private def checkLambda(l: ILambda)(implicit scope: Scope): TLambda = {
    new TLambda(checkMode(l.mode), checkArgs(l.args), checkOptArgs(l.result), l.throws, checkOptBlock(l.block))
  }

  private def checkLambda(l: Lambda)(implicit scope: Scope): TLambda = {
    new TLambda(checkMode(l.mode), checkArgs(l.args), checkOptArgs(l.result), l.throws, checkOptBlock(l.block))
  }

  private def checkArgs(list: List[Arg])(implicit scope: Scope): List[TArg] = list.map(checkArg)

  private def checkArg(arg: Arg)(implicit scope: Scope): TArg = {
    val expr = arg.expr.map(checkExpression)
    val ofType = arg.ofType.map(checkOf)
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
    case SecondCommandArgs(args) => {
      new TSecondCommandArgs(checkArgs(args))
    }
    case CommandCall(id, formalArgs, args) => {
      new TCommandCall(bodyLookUp(id, ofType), checkFormal(formalArgs), checkArgs(args))
    }
  }

  private def checkOpUnary(list: List[(Operator, Unary)])(implicit scope: Scope): List[(Operator, TUnary)] = {
    list.map(t => t._1 -> checkUnary(t._2))
  }

  private def checkOptArgs(l: Option[List[Arg]])(implicit scope: Scope): Option[List[TArg]] = {
    l.map(checkArgs)
  }

  private def checkOptBlock(block: Option[Block])(implicit scope: Scope): Option[TBlock] = {
    block.map(checkBlock)
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
    case v: VarDec => checkVarDec(v)
    case Match(list, cases) => (new TMatch(list.map(checkExpression), cases.map(checkCaseBlock)), scope)
    case DoLoop(block, whileExpr) => (new TDoLoop(checkBlock(block), checkBooleanExpr(whileExpr)), scope)
    case WhileLoop(whileExpr, block) => (new TWhileLoop(checkBooleanExpr(whileExpr), checkBlock(block)), scope)
    case ForLoop(forVars, inExpr, block) => (new TForLoop(forVars.map(checkForVar), checkExpression(inExpr), checkBlock(block)), scope)
    case Conditional(condList, elseBlock) => (new TConditional(condList.map(t => checkBooleanExpr(t._1) -> checkBlock(t._2)), elseBlock.map(checkBlock)), scope)
    case Assignment(lValues, optExpr) => (new TAssignment(lValues.map(checkLValue), optExpr.map(checkExpression)), scope)
  }

  def checkLValue(l: LValue)(implicit scope: Scope): TLValue = l match {
    case LValueVar(nVar) => {
      val varDec = checkVarDec(nVar)
      new TLValueVar(varDec._1)(varDec._2)
    }
    case LValueCommand(command) => new TLValueCommand(checkCommand(command))
  }

  def checkVarDec(nVar: VarDec)(implicit scope: Scope): (TVarDec, Scope) = {
    val of = nVar.ofType.map(checkOf).getOrElse(throw new TyperInferenceException)
    if (scope.varScope.contains(nVar.id))
      throw new VariableShadowingException(s"Variable ${nVar.id}, of type ${nVar.ofType} shadows variable with type ${scope.varScope.get(nVar.id)}")
    else {
      (new TVarDec(nVar.id, of, nVar.assign.map(checkExpression)), scope.copy(varScope = scope.varScope + (nVar.id -> of)))
    }
  }

  def checkBooleanExpr(expr: Expr)(implicit scope: Scope): TExpr = {
    val ex = checkExpression(expr)
    if (ex.extractOfType eq boolOfType)
      ex
    else
      throw new TypeMismatch(ex.extractOfType.toString, boolOfType.toString)
  }

  private def checkCaseBlock(c: CaseBlock)(implicit scope: Scope): TCaseBlock = new TCaseBlock(c.subBlock.map(checkCaseSubBlock), checkBlock(c.block))

  private def checkCaseSubBlock(sub: CaseSubBlock)(implicit scope: Scope): TCaseSubBlock = sub match {
    case CaseIf(expr) => new TCaseIf(checkExpression(expr))
    case CaseVarList(varList) => new TCaseVarList(varList.map(checkCaseVar))
  }

  private def checkCaseVar(caseVar: CaseVar)(implicit scope: Scope): TCaseVar = {
    new TCaseVar(caseVar.expr.map(checkExpression), checkForVar(caseVar.forVar))
  }

  private def checkForVar(forVar: ForVar)(implicit scope: Scope): TForVar = {
    if (scope.varScope.contains(forVar.id))
      throw new VariableShadowingException(s"${forVar.id} redefined in ${scope.filename}")
    else
      new TForVar(forVar.id, checkOf(forVar.ofType.getOrElse(throw new TyperInferenceException)))
  }

  private def checkFormal(formal: FormalArgs)(implicit scope: Scope): TFormalArgs = formal.map(checkTypeClass)

  private def checkTypeBody(typeBody: TypeBody)(implicit scope: Scope): TTypeBody = new TTypeBody(typeBody.body.map(t => t._1 -> checkBodyContent(t._2)))

  private def checkBodyContent(bodyContent: BodyContent)(implicit scope: Scope): TBodyContent = bodyContent match {
    case Field(name, ofType, expr) => new TField(name, checkOf(ofType), expr.map(checkExpression))
    case Delegate(name, ofType) => new TDelegate(name, checkOf(ofType))
    case Constructor(contents, throws, block) => new TConstructor(checkContents(contents), throws, block.map(checkBlock))
    case Ambient(contents, throws, block) => new TAmbient(checkContents(contents), throws, block.map(checkBlock))
    case Function(contents, results, throws, block) => new TFunction(checkContents(contents), checkOptArgs(results), throws, block.map(checkBlock))
    case Message(contents, block) => new TMessage(checkContents(contents), block.map(checkBlock))
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

  private def bodyLookUp(name: ID, is: IIs)(implicit scope: Scope): BodyContent = {
    for (i <- is.list) {
      val typeclass = i
    }
    ???
  }

  private def bodyLookUp(name: ID, ofType: TOfType)(implicit scope: Scope): BodyContent = ???
}