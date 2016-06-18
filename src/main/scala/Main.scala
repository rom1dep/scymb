import java.io.{File, PrintWriter}

val (defaultOutput, tmpOutput) = (new File("default"), new File("instructions.tmp"))

case class Config(inputFile: File = new File("."), outputFile: File = defaultOutput, overwrite: Boolean = false)

val parser = new scopt.OptionParser[Config]("scymb") {
  head("scymb", "0.1")

  note(
    """scymb is an assembler for the HACK computer as defined by the nand2tetris course.
       This program is a port of an earlier assembler written in python.
       Romain DEP. 2016
    """)

  opt[File]('i', "input-file") required() valueName "<file>" action { (x, c) => c.copy(inputFile = x)
  } text "input-file, file in assembly language"

  opt[File]('o', "output-file") optional() valueName "<file>" action { (x, c) => c.copy(outputFile = x)
  } text "output-file, where to save the assembled binary"

  opt[Unit]('w', "overwrite") action { (_, c) => c.copy(overwrite = true)
  } text "should any existing file be overwritten"

  checkConfig(c => {
    if (!c.inputFile.exists()) failure(s"specified input file doesn't exist:\n\t${c.inputFile}")
    else if (!c.overwrite && c.outputFile.exists()) failure(s"specified output file already exists, use --overwrite to discard\n\t${c.outputFile}")
    else success
  })

  help("help") text "prints this usage text"
}

def sanitizeOutputFile(passedInput: File, passedOutput: File): File = {
  if (passedOutput == defaultOutput) // we have to make-up an output file based on input name
    if (passedInput.getName.contains(".asm")) new File(passedInput.getParentFile, passedInput.getName.replace(".asm", ".hack"))
    else new File(passedInput.getParentFile, passedInput.getName + ".hack")
  else passedOutput
}

val symbols = scala.collection.mutable.Map(
  "R0" -> 0,
  "R1" -> 1,
  "R2" -> 2,
  "R3" -> 3,
  "R4" -> 4,
  "R5" -> 5,
  "R6" -> 6,
  "R7" -> 7,
  "R8" -> 8,
  "R9" -> 9,
  "R10" -> 10,
  "R11" -> 11,
  "R12" -> 12,
  "R13" -> 13,
  "R14" -> 14,
  "R15" -> 15,
  "SCREEN" -> 16384,
  "KBD" -> 24576,
  "SP" -> 0,
  "LCL" -> 1,
  "ARG" -> 2,
  "THIS" -> 3,
  "THAT" -> 4
)
var upperPointer = 0

val rgx_var = "((?:\\w|.|_)+)"
val rgx_label = s"^\\($rgx_var\\)$$".r
val rgx_aInstr = s"^@(?:(\\d+)|$rgx_var)$$".r

val rgx_cInstr_dst = "(?:(M|D|MD|A|AM|AD|AMD)=)?"
val rgx_cInstr_cmp = "(0|1|-1|D|A|!D|!A|-D|-A|D\\+1|A\\+1|D-1|A-1|D\\+A|D-A|A-D|D&A|D\\|A|M|!M|-M|M\\+1|M-1|D\\+M|D-M|M-D|D&M|D\\|M)"
val rgx_cInstr_jmp = "(?:;(JGT|JEQ|JGE|JLT|JNE|JLE|JMP))?"
val rgx_cInstr = s"^$rgx_cInstr_dst$rgx_cInstr_cmp$rgx_cInstr_jmp$$".r

val cInstr_dst_table = Map(
  "M" -> "001",
  "D" -> "010",
  "MD" -> "011",
  "A" -> "100",
  "AM" -> "101",
  "AD" -> "110",
  "AMD" -> "111"
)
val cInstr_cmp_table = Map(
  "0" -> "0101010",
  "1" -> "0111111",
  "-1" -> "0111010",
  "D" -> "0001100",
  "A" -> "0110000",
  "!D" -> "0001101",
  "!A" -> "0110001",
  "-D" -> "0001111",
  "D+1" -> "0011111",
  "A+1" -> "0110111",
  "D-1" -> "0001110",
  "A-1" -> "0110010",
  "D+A" -> "0000010",
  "D-A" -> "0010011",
  "A-D" -> "0000111",
  "D&A" -> "0000000",
  "D|A" -> "0010101",
  "M" -> "1110000",
  "!M" -> "1110001",
  "-M" -> "1110011",
  "M+1" -> "1110111",
  "M-1" -> "1110010",
  "D+M" -> "1000010",
  "D-M" -> "1010011",
  "M-D" -> "1000111",
  "D&M" -> "1000000",
  "D|M" -> "1010101"
)
val cInstr_jmp_table = Map(
  "JGT" -> "001",
  "JEQ" -> "010",
  "JGE" -> "011",
  "JLT" -> "100",
  "JNE" -> "101",
  "JLE" -> "110",
  "JMP" -> "111"
)

def using[T <: {def close()}](resource: T)(impl: T => Unit): Unit = {
  import scala.language.reflectiveCalls // This is an automated resource manager
  try impl(resource) finally resource.close()
}

def run(infile: File, outFile: File) {
  tmpOutput.delete()
  println(s"First pass in $infile building the lookup table and the instructions list")
  using(new PrintWriter(tmpOutput)) { instr_writer =>
    for (line <- scala.io.Source.fromFile(infile).getLines()) {
      val l = line.trim
      if (!l.isEmpty && !l.startsWith("//")) {
        val cl = l.split("//")(0).trim //cleanup comments
        print(s"Parsing $cl → ")
        cl match {
          case rgx_label(lbl) => println(s"Found label $lbl, mapped to M$upperPointer")
            symbols += lbl -> upperPointer
          case _ => println("skipped")
            instr_writer.println(cl)
            upperPointer += 1
        }
      }
    }
                                    }
  println(s"Second pass, assembling binary code into $outFile")
  using(new PrintWriter(outFile)) { binary_writer =>
    for (inst <- scala.io.Source.fromFile(tmpOutput).getLines()) {
      print(s"Parsing $inst → ")
      rgx_aInstr.findFirstMatchIn(inst) match {
        case Some(res) => res.subgroups match {
          case adr :: null :: Nil =>
            val aInstr = f"${adr.toInt.toBinaryString.toLong}%016d"
            println(s"into a-instruction $aInstr")
            binary_writer.println(aInstr)
          case null :: vbl :: Nil => symbols.get(vbl) match {
            case Some(adr) =>
              println(s"\t\t$adr")
              val aInstr = f"${adr.toInt.toBinaryString.toLong}%016d"
              println(s"into a-instruction $aInstr after lookup")
              binary_writer.println(aInstr)
            case None => sys.error("unable to find a matching symbol for given variable")
          }
          case _ => println("should never happen since both groups are mutually exclusive")
        }

        case None =>
          rgx_cInstr.findFirstMatchIn(inst) match {
            case Some(res) => res.subgroups match {
              case d :: c :: j :: Nil =>
                val (dw, cw, jw) = (cInstr_dst_table.getOrElse(d, "000"), cInstr_cmp_table(c), cInstr_jmp_table.getOrElse(j, "000"))
                val cInstr = s"111$cw$dw$jw"
                println(s"into c-instruction $cInstr")
                binary_writer.println(cInstr)
              case _ => sys.error("unrecognised instruction")
            }
            case None => sys.error("is neither a a- or c-instruction!")
          }
      }
    }
                                  }
}

parser.parse(args, Config()) match {
  case Some(config) => run(config.inputFile, sanitizeOutputFile(config.inputFile, config.outputFile))
  case None => parser.showUsage()
}
