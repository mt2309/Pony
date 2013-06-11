package com.github.mt2309.pony.Typer

/**
 * User: mthorpe
 * Date: 09/06/2013
 * Time: 23:34
 */
object ImplicitTraits {

  implicit val scope = pScope
  implicit val filename = "Implicit trait"

  val Actor: TTrait = new TTrait("Actor", List.empty, new TIs(List.empty), new TTypeBody(Map.empty))

  val Hashable: TTrait = new TTrait("Hashable", List.empty, new TIs(List.empty),
    new TTypeBody(Map("hash" -> new TFunction(
      new TMethodContent(new TReadOnly, "hash", new TCombinedArgs(List.empty, List.empty)), List(new TParam("hash", Some(intOfType))), false, Some(new TBlock(List.empty, None, None))))))

  val Partial: TTrait = new TTrait("Partial", List.empty, new TIs(List.empty),
    new TTypeBody(Map("mirror" -> new TFunction(
      new TMethodContent(new TReadOnly, "mirror", new TCombinedArgs(List.empty, List.empty)), List(new TParam("mirror", None)), false, Some(new TBlock(List.empty, None, None))))))

  val allTraits: List[TTypeClass] = List(Hashable, Partial).map(new TTypeClass(_))
  val actorTraits = new TTypeClass(Actor) :: allTraits
}
