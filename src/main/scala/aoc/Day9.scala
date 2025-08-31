package aoc

import better.files.Resource
import scala.collection.immutable

object Day9 {
  def example = Resource.getAsString("day9/example.txt")
  def real = Resource.getAsString("day9/real.txt")

  sealed trait Disk
  final case class File(id: Int) extends Disk
  case object FreeSpace extends Disk  

  def parse(input: String): Vector[Disk] = {
    val digits = input.map(num => num.asDigit).toList
    val (_, result) = digits.zipWithIndex.foldLeft((0, Vector.empty[Disk])){
      case ((fileId, acc), (num, index)) =>
        if (index%2 == 0) // file
        {
        (fileId+1, acc ++ Vector.fill(num)(File(fileId)))
        } else {
          (fileId, acc ++ Vector.fill(num)(FreeSpace))
        }
    }
    result
  }

  def moveDisk(disk: Vector[Disk]): Vector[Disk] = {
    def go(disk: Vector[Disk], nextFree: Int, nextFile: Int): Vector[Disk]= {
      if(nextFree == -1 || nextFile == -1 || nextFree > nextFile) {
        disk
      } else {
        val v2 = disk.updated(nextFree, disk(nextFile))
        .updated(nextFile, FreeSpace)

        val nextNextFree = v2.indexWhere(_ == FreeSpace, 0)
        val nextNextFile = v2.lastIndexWhere{
          case File(_) => true
          case _ => false
        }

        go(v2, nextNextFree, nextNextFile)
    }
    }
    val nextFree = disk.indexWhere(_ == FreeSpace, 0)
    val nextFile = disk.lastIndexWhere{
      case File(_) => true
      case _ => false
    }
    go(disk, nextFree, nextFile)
  }

  def checksumSum(disks: Vector[Disk]): Long = {
    def checksum(disk: Disk, index: Int): Long = disk match {
      case File(id) => id * index
      case FreeSpace => 0
    }
    disks.zipWithIndex.map((checksum _).tupled).sum
  }


  def printDisk(disk: Vector[Disk]): Vector[String] = {
    def makeString(disk: Disk): String = disk match {
      case File(id) => id.toString
      case FreeSpace => "."
    }

    disk.map(makeString)
  }



  def solvePart1(str: String): Long = {
    checksumSum(moveDisk(parse(str)))
  }


  sealed trait DiskP2
  final case class FileP2(id: Int, size: Int) extends DiskP2
  final case class FreeSpaceP2(id: Int, size: Int) extends DiskP2

  def parseP2(input: String): Vector[DiskP2] = {
    val digits = input.map(num => num.asDigit).toList
    val (_, _, result) = digits.zipWithIndex.foldLeft((0, 0, Vector.empty[DiskP2])){
      case ((fileId, freespaceId, acc), (num, index)) =>
        if (index%2 == 0) // file
        {
        (fileId+1, freespaceId, acc ++ Vector.fill(num)(FileP2(fileId, num)))
        } else {
          (fileId, freespaceId+1, acc ++ Vector.fill(num)(FreeSpaceP2(freespaceId, num)))
        }
    }
    result
  }

  def getBlockSize(disk: DiskP2): Int = disk match {
    case FileP2(id, size) => size
    case FreeSpaceP2(id, size) => size
  }

  def getId(disk: DiskP2): Int = disk match {
    case FileP2(id, size) => id
    case FreeSpaceP2(id, size) => id
  }

  def moveByBlock(disks: Vector[DiskP2]): Vector[DiskP2] = {
    def go(dsiks: Vector[DiskP2], nextFree: Int, nextFile: Int): Vector[DiskP2] = {
      if (nextFree == -1 || nextFile == -1 || getId(disks(nextFile)) == 0) {
        disks
      } else {
        val nextFreeSize = getBlockSize(disks(nextFree))
        val nextFileSize = getBlockSize(disks(nextFile))

        if (nextFreeSize >= nextFileSize) {
          val fileBlockForMove = Vector.fill(nextFileSize)(disks(nextFile))
          val freeBlockForMove = Vector.fill(nextFileSize)(disks(nextFree))
          val v2: Vector[DiskP2] = disks
          .patch(nextFree, fileBlockForMove, nextFileSize)
          .patch(nextFile - nextFileSize + 1, freeBlockForMove, nextFileSize)

          val nextNextFree = v2.indexWhere( {
            case FreeSpaceP2(_, _) => true
            case _ => false
          },  nextFree + nextFreeSize -1)

          val nextNextFile = v2.lastIndexWhere({
            case FileP2(_, _) => true
            case _ => false
          }, nextFile - nextFileSize+1)
          print(v2, nextFree, nextFile, nextNextFree, nextNextFile)
          go(v2, nextNextFree, nextNextFile)
        } 
        go(disks, nextFree, nextFile-nextFileSize)
      }
      
      }
      val nextFree = disks.indexWhere({
            case FreeSpaceP2(_, _) => true
            case _ => false
          }, 0)
          
      val nextFile = disks.lastIndexWhere{
        case FileP2(_, _) => true
        case _ => false
    }
    go(disks, nextFree, nextFile)
    
  }

  def solvePart2(str: String) = moveByBlock(parseP2(str))
}
