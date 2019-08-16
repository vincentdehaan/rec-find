package nl.vindh.tools

package object recfind {
  type FileName = String
  type ClassFile = Array[Byte]
  type RecursionInfo = Map[Method, List[Method]]
}
