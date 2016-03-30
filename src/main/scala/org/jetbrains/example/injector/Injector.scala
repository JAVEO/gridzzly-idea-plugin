package org.jetbrains.example.injector

import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScClass, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.SyntheticMembersInjector
import org.jetbrains.plugins.scala.lang.psi.types.ScParameterizedType

class Injector extends SyntheticMembersInjector {
  override def injectFunctions(source: ScTypeDefinition): Seq[String] = {
    source match {
      case c: ScClass if (c.hasAnnotation("grid.Gridzzly").isDefined || c.hasAnnotation("grid.GridAnnotation").isDefined) && c.extendsBlock.superTypes.headOption.isDefined =>

        val firstType = c.extendsBlock.superTypes.collect{
          case ScParameterizedType(sctype, typeParameters) => typeParameters.map(
            _.toString().replaceAll("PostgresDriver.api.Rep", "slick.driver.PostgresDriver.api.Rep")
            )}.flatten.head
        val secondType = c.extendsBlock.superTypes.collect{case ScParameterizedType(sctype, typeParameters) => typeParameters.tail.head.toString}.head
        Seq(
          s"def run(conditions: GridConditions)(implicit dbConnection: DBConnection): (_root_.scala.concurrent.Future[Seq[$secondType]], _root_.scala.concurrent.Future[Int]) = ???",
          s"def run(conditions: GridConditions, initialFilter: (${firstType}) => Rep[Boolean])(implicit dbConnection: DBConnection): (_root_.scala.concurrent.Future[Seq[$secondType]], _root_.scala.concurrent.Future[Int]) = ???",
          s"def colsForFrontend: List[ColForFrontend] = ???"
        )
      case _ => Seq.empty
    }
  }
}
