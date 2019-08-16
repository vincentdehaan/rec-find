package nl.vindh.tools.recfind

import java.io.FileInputStream
import java.nio.file.{Files, Paths}

import scala.util.{Failure, Try}
import jdk.internal.org.objectweb.asm.tree.{ClassNode, MethodInsnNode}
import jdk.internal.org.objectweb.asm.{ClassReader, Opcodes}

// TODO: only direct recursion has been implemented
case class Method(owner: String, name: String, desc: String)
object Main {
  def main(args: Array[String]): Unit = {
    if(args.isEmpty) {
      println("No class or jar found.")
      System.exit(0)
    }

    for {
      classFiles <- args(0).split('.').last match {
        case "jar" => for {
          fis <- Try(new FileInputStream(args(0)))
          classFiles <- JarReader.getClassFiles(fis)
        } yield classFiles.toList.map (_._2)
        case "class" => Try {
          Files.readAllBytes(Paths.get(args(0)))
        }.map(List(_))
        case _ => Failure(new IllegalArgumentException("First argument must be *.jar or *.class file."))
      }

      methodInfos = for {
        classFile <- classFiles
        methodInfo <- handleClassFile(classFile)
      } yield methodInfo

      recursiveMethods = methodInfos.filter(isRecursive).map(_._1)
    } yield {
      println("Recursive methods found:\n")
      println(recursiveMethods.map(m => s"*  ${m.owner}/${m.name}${m.desc}").mkString("\n"))
    }
  }.recover {
    case ex: Throwable => println(ex)
  }

  def handleClassFile(file: Array[Byte]): Seq[(Method, List[Method])] = { // TODO: better name
    val cr = new ClassReader(file)
    val cn = new ClassNode()
    cr.accept(cn, 0)
    cn.methods.map {
      method => {
        val methodName = Method(cn.name, method.name, method.desc)
        val calls = method.instructions.toArray.filter {
          instruction =>
            instruction.getOpcode == Opcodes.INVOKEVIRTUAL ||
              instruction.getOpcode == Opcodes.INVOKEINTERFACE ||
              instruction.getOpcode == Opcodes.INVOKESPECIAL ||
              instruction.getOpcode == Opcodes.INVOKESTATIC
        }.map {
          instruction => {
            val instr = instruction.asInstanceOf[MethodInsnNode]
            Method(instr.owner, instr.name, instr.desc)
          }
        }
        (methodName, calls.toList)
      }
    }
  }

  def isRecursive(m: (Method, List[Method])): Boolean = m match {
    case (name, calls: List[Method]) => calls.contains(name)
  }

  implicit def utilListToIndexedSeq[A](xs: java.util.List[A]): IndexedSeq[A] =
    new IndexedSeq[A] {
      def apply(i: Int): A = xs.get(i)
      lazy val length = xs.size
    }
}