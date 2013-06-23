package com.github.mt2309.pony.Typer

/**
 * User: mthorpe
 * Date: 09/06/2013
 * Time: 23:34
 */
private object ImplicitTraits {

  implicit val scope = pScope
  implicit val filename = "Implicit trait"

  val Actor: TTrait = new TTrait("Actor", List.empty, new TIs(List.empty), new TTypeBody(Map.empty))

  val Hashable: TTrait = new TTrait("Hashable", List.empty, new TIs(List.empty),
    new TTypeBody(Map("hash" -> new TFunction(
      new TMethodContent(new TReadOnly, "hash", new TCombinedArgs(List.empty, List.empty)), List(new TParam("hash", Some(intOfType))), false, Some(new TBlock(List.empty, None, None))))))

  val Partial: TTrait = new TTrait("Partial", List.empty, new TIs(List.empty),
    new TTypeBody(Map("mirror" -> new TFunction(
      new TMethodContent(new TReadOnly, "mirror", new TCombinedArgs(List.empty, List.empty)), List(new TParam("mirror", None)), false, Some(new TBlock(List.empty, None, None))))))

  val Construct: TTrait = new TTrait("Construct", List.empty, new TIs(List.empty),
    new TTypeBody(Map("construct" -> new TConstructor(
      new TMethodContent(new TReadOnly, "construct", new TCombinedArgs(List.empty, List.empty)), false, Some(new TBlock(List.empty, None, None))))))


  val allTraits: List[TTypeClass] = List(Hashable, Partial, Construct).map(new TTypeClass(_))
  val actorTraits = new TTypeClass(Actor) :: allTraits

  val PonyObject: TTrait = new TTrait("PonyObject", List.empty, new TIs(allTraits), new TTypeBody(Map.empty))

  val tPO = new TTypeClass(PonyObject)

  val implicitTraits: TypeScope = Set(PonyObject, Hashable, Partial, Construct).map(t => t.name -> t).toMap

  val tArray: TObject = new TObject("Array", List("K"), new TIs(List(tPO)), new TTypeBody(Map.empty))

  val range: TFunction = new TFunction(
    new TMethodContent(new TReadOnly, "to", new TCombinedArgs(List.empty, List(new TParam("until",Some(numericOfType))))), List(new TParam("arr",
      Some(new TOfType(Set(new TTypeClass(moduleMember = tArray, formalArgs = List(pInt))))))), false, Some(new TBlock))
}
