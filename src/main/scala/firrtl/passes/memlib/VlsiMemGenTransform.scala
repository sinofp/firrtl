package firrtl.passes.memlib

import firrtl.{CircuitState, DependencyAPIMigration, Transform}
import firrtl.stage.Forms

import scala.collection.mutable.ListBuffer

class VlsiMemGenTransform(outputConfig: String) extends Transform with DependencyAPIMigration {
  override def prerequisites = Forms.MidForm
  override protected def execute(state: CircuitState): CircuitState = {

    def parseLine(line: String): (String, Int, Int, Int, Int, Array[String]) = {
      val tokens = line.split(' ')
      // MemConf#toString
      val _ :: name :: _ :: depth :: _ :: width :: _ :: ports :: tail = tokens.toList
      val maskGran = (if (tail.isEmpty) width else tail.last).toInt
      (name, width.toInt, depth.toInt, maskGran, width.toInt / maskGran, ports.split(','))
    }

    def genMem(name: String, width: Int, depth: Int, maskGran: Int, maskSeg: Int, ports: Array[String]): String = {
      val addrWidth = math.max(math.ceil(math.log(depth) / math.log(2)).toInt, 1)
      val portSpec = new ListBuffer[String]
      val readPorts = new ListBuffer[Int]
      val writePorts = new ListBuffer[Int]
      // todo use_latches is always 0
      //      val latchPorts = new ListBuffer[Int]
      val rwPorts = new ListBuffer[Int]
      val decl = new ListBuffer[String]
      val combinational = new ListBuffer[String]
      val sequential = new ListBuffer[String]
      val maskedPorts = new ListBuffer[Int]

      for (pid <- ports.indices) {
        val masked = ports(pid)(0) == 'm'
        if (masked) maskedPorts += pid
        val ptype = if (masked) ports(pid).substring(1) else ports(pid)

        ptype match {
          case "read" =>
            val prefix = s"R${readPorts.length}_"
            portSpec ++= Seq(
              s"input ${prefix}clk",
              s"input [${addrWidth - 1}:0] ${prefix}addr",
              s"input ${prefix}en",
              s"output [${width - 1}:0] ${prefix}data"
            )
            readPorts += pid
          case "write" =>
            val prefix = s"W${writePorts.length}_"
            portSpec ++= Seq(
              s"input ${prefix}clk",
              s"input [${addrWidth - 1}:0] ${prefix}addr",
              s"input ${prefix}en",
              s"input [${width - 1}:0] ${prefix}data"
            )
            if (masked) {
              portSpec += s"input [${maskSeg - 1}:0] ${prefix}mask"
            }
            writePorts += pid
          case "rw" =>
            val prefix = s"RW${rwPorts.length}_"
            portSpec ++= Seq(
              s"input ${prefix}clk",
              s"input [${addrWidth - 1}:0] ${prefix}addr",
              s"input ${prefix}en",
              s"input ${prefix}wmode"
            )
            if (masked) {
              portSpec += s"input [${maskSeg - 1}:0] ${prefix}wmask"
            }
            portSpec += s"input [${width - 1}:0] ${prefix}wdata"
            portSpec += s"output [${width - 1}:0] ${prefix}rdata"
            rwPorts += pid
          case _ => new Exception(s"VLSI_MEM_GEN: unknown port type $ptype") //todo
        }
      }

      val nr = readPorts.length
      val nw = writePorts.length
      val nrw = rwPorts.length

      def emitRead(idx: Int, rw: Boolean) = {
        val prefix = if (rw) s"RW${idx}_" else s"R${idx}_"
        val data = if (rw) s"${prefix}rdata" else s"${prefix}data"
        val en = if (rw) s"${prefix}en && !${prefix}wmode" else s"${prefix}en"
        decl += s"reg reg_${prefix}ren;"
        decl += s"reg [${addrWidth - 1}:0] reg_${prefix}addr;"
        sequential ++= Seq(
          s"always @(posedge ${prefix}clk)",
          s"  reg_${prefix}ren <= $en;",
          s"always @(posedge ${prefix}clk)",
          s"  if ($en) reg_${prefix}addr <= ${prefix}addr;"
        )
        combinational ++= Seq(
          "`ifdef RANDOMIZE_GARBAGE_ASSIGN",
          s"reg [${((width - 1) / 32 + 1) * 32 - 1}:0] ${prefix}random;",
          "`ifdef RANDOMIZE_MEM_INIT",
          "  initial begin",
          "    #`RANDOMIZE_DELAY begin end",
          s"    ${prefix}random = {${Seq.fill((width - 1) / 32 + 1)("$random").mkString(", ")}};",
          s"    reg_${prefix}ren = ${prefix}random[0];",
          "  end",
          "`endif",
          s"always @(posedge ${prefix}clk) ${prefix}random <= {${Seq.fill((width - 1) / 32 + 1)("$random").mkString(", ")}};",
          s"assign $data = reg_${prefix}ren ? ram[reg_${prefix}addr] : ${prefix}random[${width - 1}:0];",
          "`else",
          s"assign $data = ram[reg_${prefix}addr];",
          "`endif"
        )
      }

      for (idx <- 0 until nr) emitRead(idx, rw = false)
      for (idx <- 0 until nrw) emitRead(idx, rw = true)

      // latchPorts

      decl ++= Seq(
        s"reg [${width - 1}:0] ram [${depth - 1}:0];",
        "`ifdef RANDOMIZE_MEM_INIT",
        "  integer initvar;",
        "  initial begin",
        "    #`RANDOMIZE_DELAY begin end",
        s"    for (initvar = 0; initvar < $depth; initvar = initvar+1)",
        s"      ram[initvar] = {${(width - 1) / 32 + 1} {$$random}};"
      )
      for (idx <- 0 until nr) {
        val prefix = s"R${idx}_"
        decl += s"    reg_${prefix}addr = {${(addrWidth - 1) / 32 + 1} {$$random}};"
      }
      for (idx <- 0 until nrw) {
        val prefix = s"RW${idx}_"
        decl += s"    reg_${prefix}addr = {${(addrWidth - 1) / 32 + 1} {$$random}};"
      }
      decl += "  end"
      decl += "`endif"

      decl += "integer i;"
      for (idx <- 0 until nw) {
        val prefix = s"W${idx}_"
        val pid = writePorts(idx)
        sequential += s"always @(posedge ${prefix}clk)"
        sequential += s"  if (${prefix}en) begin"
        for (i <- 0 until maskSeg) {
          val mask = if (maskedPorts contains pid) s"if (${prefix}mask[$i]) " else ""
          val ram_range = s"${(i + 1) * maskGran - 1}:${i * maskGran}"
          sequential += s"    ${mask}ram[${prefix}addr][$ram_range] <= ${prefix}data[$ram_range];"
        }
        sequential += "  end"
      }
      for (idx <- 0 until nrw) {
        val prefix = s"RW${idx}_"
        val pid = rwPorts(idx)
        sequential += s"always @(posedge ${prefix}clk)"
        sequential += s"  if (${prefix}en && ${prefix}wmode) begin"
        if (maskSeg > 0) {
          sequential += s"    for(i=0;i<$maskSeg;i=i+1) begin"
          if (maskedPorts contains pid) {
            sequential ++= Seq(
              s"      if(${prefix}wmask[i]) begin",
              s"        ram[${prefix}addr][i*$maskGran +: $maskGran] <= ${prefix}wdata[i*$maskGran +: $maskGran];",
              "      end"
            )
          } else {
            sequential += s"      ram[${prefix}addr][i*$maskGran +: $maskGran] <= ${prefix}wdata[i*$maskGran +: $maskGran];"
          }
          sequential += "    end"
        }
        sequential += "  end"
      }

      s"""
         |module $name(
         |  ${portSpec.mkString(",\n  ")}
         |);
         |
         |  ${decl.mkString("\n  ")}
         |  ${sequential.mkString("\n  ")}
         |  ${combinational.mkString("\n  ")}
         |
         |endmodule""".stripMargin
    }

    def main(confFile: String): Unit = {
      val file = io.Source.fromFile(confFile)
      for (line <- file.getLines()) {
        val parsedLine = (genMem _).tupled(parseLine(line))
        println(parsedLine)
      }
      file.close()
    }

    main(outputConfig)

    //todo
    state
  }
}
