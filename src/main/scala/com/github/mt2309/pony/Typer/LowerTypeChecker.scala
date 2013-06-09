package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.Common._
import com.github.mt2309.pony.AST._
import scala.util.parsing.input.Position

/**
 * User: mthorpe
 * Date: 11/05/2013
 * Time: 16:30
 */
final class LowerTypeChecker(val topTypes: Set[ITypedModule]) {

  def typeCheck: Set[TypedModule] = {topTypes.map(checkModule)}

  private def checkModule(module: ITypedModule): TypedModule = {
    new TypedModule(module.imports, module.types.map(c => c._1 -> checkClass(c._2, module.scope)))(module.scope)
  }

  private def checkClass(m: IModuleMember, scope: Scope): TModuleMember = {
    implicit val sc = scope.setClass(Some(m))

    val cached = TyperHelper.lookupModule(m)

    val res = cached.getOrElse(m match {
      case IPrimitive(name) => throw new PrimitiveFound(s"Primitive $name found where it should not be")(m.pos, scope)
      case IDeclare(name, is, declareMap) => new TDeclare(name, checkIs(is), checkDeclareMap(declareMap, is)).setPos(m.pos)
      case t:IType => new TType(t.typename, checkOf(t.ofType), checkIs(t.is))
      case c:IActor => {
        val tIs = checkIs(c.i)
        val traitVars = tIs.variables
        new TActor(  c.n, checkIFormal(c.f), tIs, checkTypeBody(c.t)(scope.updateScope(traitVars, this)(m.pos))).setPos(m.pos)
      }
      case c:IObject => {
        val tIs = checkIs(c.i)
        val traitVars = tIs.variables
        new TObject(  c.n, checkIFormal(c.f), tIs, checkTypeBody(c.t)(scope.updateScope(traitVars, this)(m.pos))).setPos(m.pos)
      }
      case c:ITrait => {
        val tIs = checkIs(c.i)
        val traitVars = tIs.variables
        new TTrait(  c.n, checkIFormal(c.f), tIs, checkTypeBody(c.t)(scope.updateScope(traitVars, this)(m.pos))).setPos(m.pos)
      }
      case EmptyType(name) => throw new EmptyTypeFound(s"Empty type $name found where it should not be")(m.pos, scope)
    })

    TyperHelper.updateModules(m, res)

    res
  }

  private def checkTypeClass(typeclass: ITypeClass)(implicit scope: Scope): TTypeClass = {

    val lookup = scope

    new TTypeClass(typeclass.iType, checkMode(typeclass.mode), checkFormal(typeclass.formalArgs)).setPos(typeclass.pos)
  }

  private def checkIFormal(formal: IFormalArgs)(implicit scope: Scope): TFormalArgs = {
    formal.map(checkTypeClass)
  }

  private def checkIs(is: IIs)(implicit scope: Scope): TIs = {
    val cached = TyperHelper.lookupIs(is)

    val res = cached.getOrElse(new TIs(is.list.map(checkTypeClass)).setPos(is.pos))
    TyperHelper.updateIs(is, res)

    res
  }

  private def checkOf(of: IOfType)(implicit scope: Scope): TOfType = {
    val cached = TyperHelper.lookupOf(of)

    val res = cached.getOrElse(new TOfType(of.typeSet.map(checkTypeElement)).setPos(of.pos))
    TyperHelper.updateOfType(of, res)

    res
  }

  private def checkTypeClass(typeclass: TypeClass)(implicit scope: Scope): TTypeClass = {
    val cached = TyperHelper.lookupTypeclass(typeclass)
    val res = cached.getOrElse{
      val t = scope.search(typeclass)
      new TTypeClass(t, checkMode(typeclass.mode), checkFormal(typeclass.formalArgs)).setPos(typeclass.pos)
    }

    TyperHelper.updateTypeclass(typeclass, res)

    res.asInstanceOf[TTypeClass]
  }

  private def checkDeclareMap(decMap: DeclareMap, is: IIs)(implicit scope: Scope): TDeclareMap = {
    new TDeclareMap(decMap.map.map(checkPonyMap(_, is))).setPos(decMap.pos)
  }

  private def checkPonyMap(pm: PonyMap, is: IIs)(implicit scope: Scope): TPonyMap = {
    new TPonyMap(scope.findMethod(pm.from, is), pm.to).setPos(pm.pos)
  }

  def checkOf(of: OfType)(implicit scope: Scope): TOfType = of match {
    case c: ConcreteOfType => new TOfType(c.typeList.map(checkTypeElement)).setPos(of.pos)
    case t: ThisOfType => {
      if (scope.currentClass.currentClass.isDefined && !scope.currentClass.isStatic)
        new TOfType(Set(new TTypeClass(scope.currentClass.currentClass.get)))
      else
        throw new ThisTypeOutsideClass()(t.pos)
    }
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
    if (ex.extractOfType.typeList forall ofType.typeList.contains)
      ex
    else
      throw new AssignmentException(s"Type error, type from assign does not match lvalue type")(expr.pos, scope)
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
      new TCommandCall(scope.findMethod(id, ofType)(snd.pos), checkFormal(formalArgs), checkArgs(args)).setPos(snd.pos)
    }
  }

  private def checkOpUnary(list: List[(Operator, Unary)])(implicit scope: Scope): List[(Operator, TUnary)] = {
    list.map(t => t._1 -> checkUnary(t._2))
  }

  private def checkBlock(block: Block)(implicit scope: Scope): TBlock = {
    new TBlock(checkBlockContents(block.contents), block.catchBlock.map(checkBlock), block.alwaysBlock.map(checkBlock)).setPos(block.pos)
  }

  private def checkBlockContents(list: List[BlockContent])(implicit scope: Scope): List[TBlockContent] = {
    list match {
      case x :: xs => {
        val ret: (TBlockContent, Scope) = checkBlockContent(x)
        ret._1.setPos(x.pos) :: checkBlockContents(xs)(ret._2)
      }
      case Nil => Nil
    }
  }

  private def checkBlockContent(b: BlockContent)(implicit scope: Scope): (TBlockContent, Scope) = b match {
    case Return => new TReturn -> scope
    case Throw => new TThrow -> scope
    case Break => new TBreak -> scope // TODO: Should we check if we're in a loop here
    case Continue => new TContinue -> scope
    case b:Block => checkBlock(b) -> scope
    case v: VarDec => checkVarDec(v)
    case Match(list, cases) => {
      new TMatch(list.map(checkExpression), cases.map(checkCaseBlock)).setPos(b.pos) -> scope
    }
    case DoLoop(block, whileExpr) => {
      new TDoLoop(checkBlock(block), checkBooleanExpr(whileExpr)).setPos(b.pos) -> scope
    }
    case WhileLoop(whileExpr, block) => new TWhileLoop(checkBooleanExpr(whileExpr), checkBlock(block)).setPos(b.pos) -> scope
    case ForLoop(forVars, inExpr, block) => {
      val fV = checkList(forVars, checkForVar)
      new TForLoop(fV._1, checkExpression(inExpr), checkBlock(block)(fV._2)).setPos(b.pos) -> scope
    }
    case Conditional(condList, elseBlock) => {
      new TConditional(condList.map(t => checkBooleanExpr(t._1) -> checkBlock(t._2)), elseBlock.map(checkBlock)).setPos(b.pos) -> scope
    }
    case Assignment(lValues, optExpr) => {
      val lV = checkList(lValues, checkLValue)
      new TAssignment(lV._1, optExpr.map(checkExpression(_)(lV._2))).setPos(b.pos) -> lV._2
    }
  }

  private def checkList[J, K <: Typer](list: List[J], f: (J, Scope) => (K, Scope))(implicit scope: Scope): (List[K], Scope) = {
    val ret = recList(list, f)
    val sc = if (ret.isEmpty) scope else ret.last.scope
    ret -> sc
  }

  private def recList[J,K <: Typer](list: List[J], f: (J, Scope) => (K, Scope))(implicit scope: Scope): List[K] = list match {
    case x :: xs => {
      val ret = f(x, scope)
      ret._1 :: recList(xs, f)(ret._2)
    }
    case Nil => Nil
  }

  def checkLValue(l: LValue, scope: Scope): (TLValue, Scope) = l match {
    case LValueVar(nVar) => {
      val varDec = checkVarDec(nVar)(scope)
      new TLValueVar(varDec._1)(varDec._2).setPos(l.pos) -> varDec._2
    }
    case LValueCommand(command) => {
      implicit val sc = scope
      new TLValueCommand(checkCommand(command)).setPos(l.pos) -> scope
    }
  }

  def checkVarDec(nVar: VarDec)(implicit scope: Scope): (TVarDec, Scope) = {
    val of = nVar.ofType.map(checkOf).getOrElse(throw new TyperInferenceException(nVar.pos, scope))
    val sc = scope.updateScope(nVar.id, of)(nVar.pos)
    new TVarDec(nVar.id, of, nVar.assign.map(checkExpression(_)(sc)))(sc) -> sc
  }

  def checkBooleanExpr(expr: Expr)(implicit scope: Scope): TExpr = {
    val ex = checkExpression(expr)
    if (ex.extractOfType == boolOfType)
      ex
    else
      throw new TypeMismatch(ex.extractOfType.toString, boolOfType.toString)(expr.pos, scope)
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
    val forVar = checkForVar(caseVar.forVar, scope)
    new TCaseVar(caseVar.expr.map(checkExpression(_)(forVar._2)), forVar._1) -> forVar._2
  }

  private def checkForVar(forVar: ForVar, scope: Scope): (TForVar, Scope) = {
    val of = checkOf(forVar.ofType.getOrElse(throw new TyperInferenceException(forVar.pos, scope)))(scope)
    val sc = scope.updateScope(forVar.id, of)(forVar.pos)
    new TForVar(forVar.id, of)(sc) -> sc
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

  private def checkBodyContent(bodyContent: BodyContent)(implicit scope: Scope): (TBodyContent, Scope) = {
    implicit val pos: Position = bodyContent.pos
    bodyContent match {
      case Field(name, ofType, expr) => {
        val of = checkOf(ofType)
        val sc = scope.updateScope(name, of)
        new TField(name, of, expr.map(checkExpression(_)(sc)))(sc) -> sc
      }
      case Delegate(name, ofType) => {
        val of = checkOf(ofType)
        val sc = scope.updateScope(name, of)
        new TDelegate(name, of)(sc) -> sc
      }
      case Constructor(contents, throws, block) => {
        val con = checkContents(contents)
        new TConstructor(con._1, throws, block.map(checkBlock(_)(con._2))) -> scope
      }
      case Ambient(contents, throws, block) => {
        val con = checkContents(contents)
        new TAmbient(con._1, throws, block.map(checkBlock(_)(con._2))) -> scope
      }
      case Function(contents, results, throws, block) => {
        val con = checkContents(contents)
        val res = checkParams(results)(con._2)
        val sc = if (res.isEmpty) con._2 else res.last.scope
        new TFunction(con._1, res, throws, block.map(checkBlock(_)(sc))) -> scope
      }
      case Message(contents, block) => {
        val con = checkContents(contents)
        new TMessage(con._1, block.map(checkBlock(_)(con._2))) -> scope
      }
    }
  }

  private def checkParams(list: Params)(implicit scope: Scope): TParams = list match {
    case x :: xs => {
      val res: (TParam, Scope) = checkParam(x)(scope)
      res._1 :: checkParams(xs)(res._2)
    }
    case Nil => Nil
  }

  private def checkParam(p: Param)(implicit scope: Scope): (TParam, Scope) = {
    val of = checkOf(p.ofType)
    val sc = scope.updateScope(p.name, of)(p.pos)

    new TParam(p.name, of)(sc) -> sc
  }

  private def checkContents(c: MethodContent)(implicit scope: Scope): (TMethodContent, Scope) = {
    val formalScope = checkFormalParams(c.combinedArgs.formalArgs, c.pos)
    val param: TParams = checkParams(c.combinedArgs.args)(formalScope)
    val sc = if (param.isEmpty) formalScope else param.last.scope
    val comb = new TCombinedArgs(c.combinedArgs.formalArgs, param)(sc)

    new TMethodContent(checkMode(c.mode)(sc), c.id, comb)(sc) -> sc
  }

  private def checkFormalParams(p: FormalParams, pos: Position)(implicit scope: Scope): Scope = {
    var sc = scope
    for (typeId <- p) {
      sc = sc.updateScope(typeId)(pos)
    }
    sc
  }

  private def checkMode(mode: Mode)(implicit scope: Scope): TMode = mode match {
    case ReadOnly => new TReadOnly
    case Immutable => new TImmutable
    case Mutable => new TMutable
    case Unique => new TUnique
    case ModeExpr(expr) => new TModeExpr(checkExpression(expr))
  }
}
