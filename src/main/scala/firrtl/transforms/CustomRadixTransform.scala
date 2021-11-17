// SPDX-License-Identifier: Apache-2.0

package firrtl.transforms

import firrtl.annotations.TargetToken.{Field, Instance}
import firrtl.annotations.{Annotation, NoTargetAnnotation, ReferenceTarget, SingleTargetAnnotation}
import firrtl.options.{CustomFileEmission, Dependency, HasShellOptions, ShellOption}
import firrtl.stage.TransformManager.TransformDependency
import firrtl.stage.{Forms, RunFirrtlTransformAnnotation}
import firrtl.{AnnotationSeq, CircuitState, DependencyAPIMigration, Transform}

/** Contains a static map from signal value(BigInt) to signal name(String)
  * This is useful for enumeration(finite state machine, bus transaction name, etc)
  *
  * @param name    identifier for this alias
  * @param filters a sequence of translation filter
  * @param width   width of this alias
  */
case class CustomRadixDefAnnotation(name: String, filters: Seq[(BigInt, String)], width: Int) extends NoTargetAnnotation

/** A annotation making a ReferenceTarget to be a specific [[CustomRadixDefAnnotation]].
  *
  * @param target the ReferenceTarget which the alias applied to
  * @param name   the identifier for the alias
  */
case class CustomRadixApplyAnnotation(target: ReferenceTarget, name: String)
    extends SingleTargetAnnotation[ReferenceTarget] {
  override def duplicate(n: ReferenceTarget): Annotation = this.copy(n)
}

/** Dumps a JSON config file for custom radix. Users can generate script using the emitted file.
  *
  * @param signals which alias contains which signals, the signals should be converted from ReferenceTarget to String
  * @param filters sequence of [[CustomRadixDefAnnotation]], the name should match [[signals]].map(_._1)
  */
case class CustomRadixConfigFileAnnotation(
  signals: Seq[(String, Seq[String])],
  filters: Seq[CustomRadixDefAnnotation])
    extends NoTargetAnnotation
    with CustomFileEmission {
  def waveViewer = "config"

  def baseFileName(annotations: AnnotationSeq): String = "custom_radix"

  def suffix: Option[String] = Some(".json")

  def getBytes: Iterable[Byte] = {
    import org.json4s.JsonDSL.WithBigDecimal._
    import org.json4s.native.JsonMethods._
    val aliasMap = signals.toMap
    pretty(
      render(
        filters.map { a =>
          val values = a.filters.map {
            case (int, str) =>
              ("digit" -> int) ~
                ("alias" -> str)
          }
          a.name -> (
            ("width" -> a.width) ~
              ("values" -> values) ~
              ("signals" -> aliasMap(a.name))
          )
        }
      )
    )
  }.getBytes
}

/** Dumps a Tcl script for GTKWave on Custom Radix, using GTKWave's `setCurrentTranslateEnums` and `installFileFilter`
  * If you want to generate the script through [[AnnotationSeq]], use:
  * {{{
  *   Seq(
  *     GTKWaveCustomRadixScriptAnnotation(Seq(), Seq()),
  *     RunFirrtlTransformAnnotation(Dependency(CustomRadixTransform))
  *   )
  * }}_gtkwave}
  *
  * You can pass
  * {{{
  *   Array("--wave-viewer-script", "gtkwave")
  * }}}
  * to `args` for the sake of ease.
  *
  * @param signals which alias contains which signals, the signals should be converted from ReferenceTarget to String
  * @param filters sequence of [[CustomRadixDefAnnotation]], the name should match [[signals]].map(_._1)
  */
case class GTKWaveCustomRadixScriptAnnotation(
  signals: Seq[(String, Seq[String])],
  filters: Seq[CustomRadixDefAnnotation])
    extends NoTargetAnnotation
    with CustomFileEmission {
  def baseFileName(annotations: AnnotationSeq): String = "custom_radix_gtkwave"

  def suffix: Option[String] = Some(".tcl")

  private def toStr(bigInt: BigInt, width: Int, base: Int) =
    if (base == 2) String.format(s"%${width}s", bigInt.toString(2)).replace(' ', '0')
    else s"%0${math.ceil(width / 4.0).toInt}x".format(bigInt)

  def getBytes: Iterable[Byte] = {
    val aliasWidth = filters.map(a => a.name -> a.width).toMap
    val setTranslateFilters = filters.map { a =>
      val values = a.filters.map {
        case (bigInt, str) => s"${toStr(bigInt, a.width, 16)} $str ${toStr(bigInt, a.width, 2)} $str"
      }.mkString(" ")
      s"""set enum_${a.name} {$values}
         |set file_${a.name} [ gtkwave::setCurrentTranslateEnums $${enum_${a.name}} ]
         |""".stripMargin
    }.mkString("\n\n")
    val setSignalLists = signals.map {
      case (aliasName, sigNames) =>
        val width = aliasWidth(aliasName)
        val names = sigNames.map(n => n + (if (width == 1) "" else s"[${width - 1}:0]")).mkString(" ")
        s"set signals_$aliasName {$names}"
    }.mkString("\n") + "\n"
    val procSignalShowString =
      """proc showString {signals fileFilter} {
        |    gtkwave::highlightSignalsFromList $signals
        |    gtkwave::installFileFilter $fileFilter
        |    gtkwave::unhighlightSignalsFromList $signals
        |}
        |""".stripMargin
    val procUpdateFilter =
      s"""proc updateFilter {} {
         |    set signals [ gtkwave::getDisplayedSignals ]
         |    foreach signal $$signals {
         |        ${filters
        .map(a => s"if {$$signal in $${::signals_${a.name}}} {showString $$signal $${::file_${a.name}}}")
        .mkString("\n        ")}
         |    }
         |}""".stripMargin

    s"""$setTranslateFilters
       |$setSignalLists
       |$procSignalShowString
       |$procUpdateFilter
       |""".stripMargin
  }.getBytes
}

/** A Transform that generate scripts or config file for Custom Radix */
object CustomRadixTransform extends Transform with DependencyAPIMigration with HasShellOptions {

  val options = Seq(
    new ShellOption[String](
      longOption = "wave-viewer-script",
      toAnnotationSeq = str => {
        RunFirrtlTransformAnnotation(Dependency(CustomRadixTransform)) +: str.toLowerCase
          .split(',')
          .map {
            case "json" | "" => CustomRadixConfigFileAnnotation(Seq.empty, Seq.empty)
          }
          .toSeq
      },
      helpText = "<json>, you can combine them like 'json', pass empty string will generate json",
      shortOption = None
    )
  )

  // in case of any rename during transforms.
  override def optionalPrerequisites: Seq[TransformDependency] = Forms.BackendEmitters

  override def invalidates(a: Transform) = false

  protected def execute(state: CircuitState): CircuitState = {
    val annos = state.annotations

    def toVerilogName(target: ReferenceTarget, sep: String = ".") = {
      import target._
      (circuit +: tokens.collect { case Instance(i) => i } :+ ref)
        .++(tokens.collect { case Field(f) => f })
        .mkString(sep)
      (circuit +: tokens.collect { case Instance(i) => i } :+ (ref +: tokens.collect { case Field(f) => f })
        .mkString("_")).mkString(sep)
    }

    // todo if scalaVersion >= 2.13: use groupMap
    val filters = annos.collect { case a: CustomRadixDefAnnotation => a }

    val signals = annos.collect {
      case CustomRadixApplyAnnotation(target, name) =>
        assert(filters.exists(_.name == name), s"$name not found in CustomRadixDefAnnotation.")
        name -> target
    }
      .groupBy(_._1)
      .mapValues(_.map(t => toVerilogName(t._2)))
      .toSeq
      .sortBy(_._1)

    assert(filters.groupBy(_.name).forall(_._2.length == 1), "name collision in CustomRadixDefAnnotation detected.")

    state.copy(annotations = annos.map {
      case _: CustomRadixConfigFileAnnotation    => CustomRadixConfigFileAnnotation(signals, filters)
      case _: GTKWaveCustomRadixScriptAnnotation => GTKWaveCustomRadixScriptAnnotation(signals, filters)
      case a => a
    })
  }
}
