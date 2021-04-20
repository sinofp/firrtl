package firrtlTests

import firrtl.passes.{CommonSubexpressionElimination, RemoveEmpty}
import firrtl.{ChirrtlForm, ChirrtlToHighFirrtl, CircuitState, HighFirrtlToMiddleFirrtl, IRToWorkingIR, LowFirrtlEmitter, LowForm, MiddleFirrtlToLowFirrtl, RenameMap, ResolveAndCheck, SeqTransform}
import firrtl.passes.memlib.{InferReadWrite, MemConf, MemLibOutConfigFileAnnotation, ReadPort, ReplSeqMem, ReplSeqMemAnnotation, WritePort}
import firrtl.stage.{FirrtlSourceAnnotation, FirrtlStage}
import firrtl.testutils.SimpleTransformSpec
import firrtl.transforms.{BlackBoxHelperAnno, BlackBoxSourceHelper, ConstantPropagation, DeadCodeElimination}

class VlsiMemGenTests extends SimpleTransformSpec {
  override def emitter = new LowFirrtlEmitter
  override def transforms = Seq(new ReplSeqMem)

  def checkMemConf(mems: Set[MemConf]) {
    mems.foreach{ mem =>
      val file = new java.io.File(mem.name +".v")
      require(file.exists(), s"${file.getName} should be emitted!")
      // todo check content
      file.delete()
    }
  }

  "VlsiMemGen" should "generate verilog for mems of bundle type" in {
    val input =
      """
circuit Top :
  module Top :
    input clock : Clock
    input reset : UInt<1>
    input head_ptr : UInt<5>
    input tail_ptr : UInt<5>
    input wmask : {takens : UInt<2>, history : UInt<14>, info : UInt<14>}
    output io : {backend : {flip allocate : {valid : UInt<1>, bits : {info : {takens : UInt<2>, history : UInt<14>, info : UInt<14>}}}}, commit_entry : {valid : UInt<1>, bits : {info : {takens : UInt<2>, history : UInt<14>, info : UInt<14>}}}}

    io is invalid

    smem entries_info : {takens : UInt<2>, history : UInt<14>, info : UInt<14>}[24]
    when io.backend.allocate.valid :
      write mport W = entries_info[tail_ptr], clock
      W <- io.backend.allocate.bits.info

    read mport R = entries_info[head_ptr], clock
    io.commit_entry.bits.info <- R
""".stripMargin
    val mems = Set(
      MemConf("entries_info_ext", 24, 30, Map(WritePort -> 1, ReadPort -> 1), None)
    )
    val annos = Seq(FirrtlSourceAnnotation(input), ReplSeqMemAnnotation.parse("", genVerilog = true))
    //todo (new FirrtlStage).transform.execute
//    val res = compileAndEmit(CircuitState(parse(input), ChirrtlForm, annos))
    val res = (new FirrtlStage).execute(Array.empty,  annos)

    // Check correctness of firrtl
//    parse(res.getEmittedCircuit.value)
    // Check the emitted conf
    checkMemConf(mems)
  }
}
