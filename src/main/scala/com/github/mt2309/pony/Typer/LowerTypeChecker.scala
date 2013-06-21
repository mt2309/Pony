package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.Common._
import com.github.mt2309.pony.AST._
import scala.util.parsing.input.Position

/**
 * User: mthorpe
 * Date: 11/05/2013
 * Time: 16:30
 */

private final class TypeCache {
  private var ofTypeCache: Map[OfType, TOfType] = Map.empty
  private var isCache: Map[Is, TIs] = Map.empty
  private var moduleCache: Map[ModuleMember, TModuleMember] = Map.empty
  private var typeclassCache: Map[TypeElement, TTypeElement] = Map.empty
  private var methodCache: Map[BodyContent, TBodyContent] = Map.empty


  def lookupMethod(is: BodyContent): Option[TBodyContent] = methodCache.get(is)
  def updateMethod(is: BodyContent, tIs: TBodyContent): Unit = methodCache += is -> tIs

  def lookupIs(is: Is): Option[TIs] = isCache.get(is)
  def updateIs(is: Is, tIs: TIs): Unit = isCache += is -> tIs

  def lookupOf(of: OfType): Option[TOfType] = ofTypeCache.get(of)
  def updateOfType(of: OfType, tOf: TOfType): Unit = ofTypeCache += of -> tOf

  def lookupModule(i: ModuleMember): Option[TModuleMember] = moduleCache.get(i)
  def updateModules(i: ModuleMember, t: TModuleMember): Unit = moduleCache += i -> t

  def lookupTypeclass(t: TypeElement): Option[TTypeElement] = typeclassCache.get(t)

  def updateTypeclass(t: TypeElement, tt: TTypeElement): Unit = typeclassCache += t -> tt
}

final class LowerTypeChecker(val topTypes: Set[PreTypedModule]) {

  private val typeCache = new TypeCache
  private val unScope: Set[Map[TypeId, (ModuleMember, CompilationUnits)]] = topTypes.map(t => t.classes -> t.imports).map(t => t._1.map(b => (b._1, b._2 -> t._2)))

  def typeCheck: Set[TypedModule] = topTypes.map(checkModule)

  private def checkModule(module: PreTypedModule): TypedModule = {

    implicit val scope = new Scope(filename = module.filename,
      imports = module.imports,
      unTypedScope = unScope.flatten.toMap)
    new TypedModule(module.imports, module.classes.map(c => c._1 -> checkClass(c._2, scope)))
  }

  def checkClass(m: ModuleMember, scope: Scope): TModuleMember = {

    implicit val sc = scope.setClass(Some(m))
    implicit val pos = m.pos

    val cached = typeCache.lookupModule(m)

    val res = cached.getOrElse(m match {
      case Primitive(name) => throw new PrimitiveFound(s"Primitive $name found where it should not be")(m.pos, scope)
      case Declare(name, is, declareMap) => {
        val i = checkIs(is)
        new TDeclare(name, i, checkDeclareMap(declareMap, i)).setPos(m.pos)
      }
      case t:Type => new TType(t.typeName, checkOf(t.ofType).get, checkIs(t.is))
      case c:Actor => {
        val formalScope: Scope = checkFormalParams(c.f)
        val tIs = checkIs(c.i)(formalScope)
        new TActor(  c.n, c.f, tIs, checkTypeBody(c.t)(formalScope)).setPos(m.pos)
      }
      case c:Object => {
        val formalScope: Scope = checkFormalParams(c.f)
        val tIs = checkIs(c.i)(formalScope)
        new TObject(  c.n, c.f, tIs, checkTypeBody(c.t)(formalScope)).setPos(m.pos)
      }
      case c:Trait => {

        val field = c.t.body.values.find(t => t.isInstanceOf[Field] || t.isInstanceOf[Delegate])
        if (field.isDefined) throw new FieldDefinedInTrait(s"Variable defined in trait ${c.n}")

        val formalScope: Scope = checkFormalParams(c.f)
        val tIs = checkIs(c.i)(formalScope)
        new TTrait(  c.n, c.f, tIs, checkTypeBody(c.t)(formalScope)).setPos(m.pos)
      }
    })

    typeCache.updateModules(m, res)

    res
  }

  private def checkFormalParams(formal: FormalParams)(implicit scope: Scope, pos: Position): Scope = {
    var sc = scope
    for (param <- formal) sc = sc.updateScope(param)

    sc
  }


  private def checkIs(is: Is)(implicit scope: Scope): TIs = {
    val cached = typeCache.lookupIs(is)

    val res = cached.getOrElse(new TIs(is.list.map(checkTypeClass(_).asInstanceOf[TTypeClass])).setPos(is.pos))
    typeCache.updateIs(is, res)

    res
  }

  private def checkTypeClass(typeclass: TypeClass)(implicit scope: Scope): TTypeElement = {

    val res = typeCache.lookupTypeclass(typeclass).getOrElse {
      scope.search(typeclass) match {
        case e: EmptyType => e
        case t: TModuleMember => new TTypeClass(t, checkMode(typeclass.mode), checkFormal(typeclass.formalArgs)).setPos(typeclass.pos)
      }
    }

    typeCache.updateTypeclass(typeclass, res)

    res
  }

  private def checkDeclareMap(decMap: DeclareMap, is: TIs)(implicit scope: Scope): TDeclareMap = {
    new TDeclareMap(decMap.map.map(checkPonyMap(_, is))).setPos(decMap.pos)
  }

  private def checkPonyMap(pm: PonyMap, is: TIs)(implicit scope: Scope): TPonyMap = {
    new TPonyMap(scope.findMethod(pm.from, is)(pm.pos), pm.to).setPos(pm.pos)
  }

  def checkOf(of: OfType)(implicit scope: Scope): Option[TOfType] = of match {
    case c: ConcreteOfType => Some(new TOfType(c.typeList map checkTypeElement ).setPos(of.pos))
    case t: ThisOfType => {
      if (scope.currentClass.currentClass.isDefined && !scope.currentClass.isStatic)
        None
      else
        throw new ThisTypeOutsideClass()(t.pos)
    }
  }

  private def checkTypeElement(elem: TypeElement)(implicit scope: Scope): TTypeElement = elem match {
    case PartialType(clazz) => new TPartialType(checkTypeClass(clazz).asInstanceOf[TTypeClass]).setPos(elem.pos)
    case t: TypeClass => checkTypeClass(t)
    case l: Lambda => checkLambda(l)
  }

  private def checkLambda(l: Lambda)(implicit scope: Scope): TLambda = {
    new TLambda(checkMode(l.mode), checkArgs(l.args), checkParams(l.result), l.throws, l.block.map(checkBlock)).setPos(l.pos)
  }

  private def checkArgs(list: List[Arg])(implicit scope: Scope): List[TArg] = list.map(checkArg)

  private def checkArg(arg: Arg)(implicit scope: Scope): TArg = {
    val expr = arg.expr.map(checkExpression)
    val ofType = arg.ofType.map(checkOf).flatten
    val assign = arg.assign.map(checkAssignment(_, ofType))

    new TArg(expr, ofType, assign).setPos(arg.pos)
  }

  private def checkAssignment(expr: Expr, ofType: Option[TOfType])(implicit scope: Scope): TExpr = {
    val ex = checkExpression(expr)

    // TODO: handle sub-typing relationship.
    if (ex.extractOfType.getOrElse(throw new AssignmentException("Assigning to this")(expr.pos, scope)).isSubType(ofType))
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
    val ofType = fst.extractOfType

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

  private def checkSecondCommand(snd: SecondCommand, ofType: Option[TOfType])(implicit scope: Scope): TSecondCommand = snd match {
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
      val zipped: List[(Expr, CaseBlock)] = list.zip(cases)
      new TMatch(zipped.map(z => checkMatchElem(z._1, z._2))).setPos(b.pos) -> scope
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
    case Assignment(lValues, expr) => {
      val lV = checkList(lValues, checkLValue)
      new TAssignment(lV._1, expr.map(checkExpression(_)(lV._2))).setPos(b.pos) -> lV._2
    }
  }

  private def checkMatchElem(expr: Expr, caseBlock: CaseBlock)(implicit scope: Scope): (TExpr, TCaseBlock) = ???

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
    val of: Option[TOfType] = nVar.ofType.map(checkOf).getOrElse(throw new TyperInferenceException(nVar.pos, scope))
    val sc = scope.updateScope(nVar.id, of)(nVar.pos)
    new TVarDec(nVar.id, of, nVar.assign.map(checkExpression(_)(sc)))(sc) -> sc
  }

  def checkBooleanExpr(expr: Expr)(implicit scope: Scope): TExpr = {
    val ex = checkExpression(expr)
    if (ex.extractOfType == Some(boolOfType))
      ex
    else
      throw new TypeMismatch(boolOfType.toString, ex.extractOfType.toString)(expr.pos, scope)
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

  private def checkTypeBody(typeBody: TypeBody)(implicit scope: Scope): TTypeBody = {
    val fields = checkBodyContents(typeBody.body.filter(t => t._2.isInstanceOf[Field] || t._2.isInstanceOf[Delegate]))

    val fieldScope: Scope = fields.values.map(_.scope).fold(scope)(_.mergeScope(_))

    val methods = checkBodyContents(typeBody.body.filterNot(t => t._2.isInstanceOf[Field] || t._2.isInstanceOf[Delegate]))(fieldScope)
    new TTypeBody(fields ++ methods)
  }

  private def checkBodyContents(bd: Map[ID, BodyContent])(implicit scope: Scope): Map[ID, TBodyContent] = {
    if (bd.isEmpty) {
      Map.empty
    }
    else {
      try {
        val tup = checkBodyContent(bd.head._2)
        Map(tup._1.name -> tup._1) ++ checkBodyContents(bd.tail)(tup._2)
      } catch {
        case v: VariableNotFoundException => {
          if (bd.tail.size == 0) throw v
          else {
            val tup = checkBodyContent(bd.tail.head._2)
            Map(tup._1.name -> tup._1) ++ checkBodyContents(bd.tail.tail + bd.head)(tup._2)
          }
        }
      }
    }
  }

  def checkBodyContent(bodyContent: BodyContent)(implicit scope: Scope): (TBodyContent, Scope) = {
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
        val fun = new TFunction(con._1, res, throws, block.map(checkBlock(_)(sc)))
        val newScope = scope.updateScope(contents.id, fun)

        fun -> newScope
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
