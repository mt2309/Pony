package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.Common._
import com.github.mt2309.pony.CompilationUnit.{UnqualifiedCompilationUnits, QualifiedCompilationUnits}
import com.github.mt2309.pony.AST.{BodyContent, ModuleMember, TypeClass}
import scala.util.parsing.input.Position

/**
 * User: mthorpe
 * Date: 28/05/2013
 * Time: 21:36
 */
final case class Scope(typeScope: ITypeScope = primScope,
                       imports: CompilationUnits = (new QualifiedCompilationUnits(Map.empty) -> new UnqualifiedCompilationUnits(Set.empty)),
                       varScope: VariableScope = Map.empty, // Could include constants here: PI, E etc?
                       currentClass: Option[IModuleMember] = None,
                       filename: Filename = "Primitive") extends NotNull
{
  def updateScope(id: ID, of: TOfType)(implicit pos: Position): Scope = {
    println(s"Updating scope with $id oftyp $of")
    if (varScope.contains(id)) {
      throw new VariableShadowingException(s"Variable $id, of type $of shadows variable with type ${this.varScope.get(id)}")
    }
    this.copy(varScope = this.varScope + (id -> of))
  }


  def search(t: TypeClass): IModuleMember = {
    val p: Option[IModuleMember] = primScope.get(t.name)
    val i: Option[IModuleMember] = if (p.isDefined) p else imports.searchType(t)

    i.getOrElse(typeScope.getOrElse(t.name, throw new TypeClassNotFoundException(s"$t not found in ${t.fileName}")(t.pos)))
  }

  def search(t: TTypeClass): IModuleMember = t.moduleMember

  def search(m: ModuleMember): IModuleMember = search(m.typeName)(m.pos)

  def checkIsList(i: IIs)(implicit pos: Position): Boolean = {
    for (mem <- i.list) {
      search(mem.iType.name)
    }

    true
  }

  def findMethod(id: ID, of: TOfType)(implicit pos: Position): BodyContent = {
    for (t <- of.typeList) {
      t match {
        case p: TPartialType => search(p.name)
        case t: TTypeClass => search(t)
        case l: TLambda => throw new LambdaInMethCallException(s"Lambda found in method lookup for for id $id")
        case TPrimitive(name) => throw new PrimitiveFound(s"Primitive $name found in method call, which have no methods defined on them")
      }
    }

    ???
  }

  def findMethod(id: ID, is: IIs): BodyContent = {
    ???
  }

  def search(t: TypeId)(implicit pos: Position): IModuleMember = typeScope.getOrElse(t, throw new TypeNotFoundException(t))

  def searchID(i: ID)(implicit pos: Position): TOfType = varScope.getOrElse(i, throw new VariableNotFoundException(s"$i not found in $filename"))
}
