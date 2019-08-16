package nl.vindh.tools.recfind

import java.io.FileInputStream
import java.util.zip.{ZipEntry, ZipInputStream}

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

object JarReader {
  def getClassFiles(fis: FileInputStream): Try[Map[FileName, ClassFile]] = Try {
    new Iterator[(ZipEntry, Array[Byte])] {
      private val zis = new ZipInputStream(fis)
      private val BUFFER_SIZE = 1024
      private val buffer = new Array[Byte](BUFFER_SIZE)
      private def withUnit[A](f: () => Unit, g: () => A): A = {f(); g()}
      private var cache: ZipEntry = null
      def hasNext = // TODO: this implementation requires that hasNext is called always before next
        if(cache == null) {
          cache = zis.getNextEntry
          cache != null
        } else true

      def next: (ZipEntry, Array[Byte]) = {
        val nextEntry = cache
        cache = null
        var len: Int = 0
        val arrayBuf = new ArrayBuffer[Byte]()
        while (withUnit(() => len = zis.read(buffer),  () => len > 0)) // TODO: this withUnit trick is not nice!
          arrayBuf.appendAll(buffer.take(len))
        (nextEntry, arrayBuf.toArray)
      }
    }.foldLeft(List[(FileName, ClassFile)]()) {
      case (acc, (nwEntry, nwStr)) => {
        if (nwEntry.getName.endsWith(".class"))
          (nwEntry.getName, nwStr) :: acc
        else acc
      }
    }.toMap
  }

}
