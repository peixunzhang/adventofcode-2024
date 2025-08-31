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
    val (_, result) = digits.zipWithIndex.foldLeft((0, Vector.empty[Disk])) {
      case ((fileId, acc), (num, index)) =>
        if (index % 2 == 0) // file
          {
            (fileId + 1, acc ++ Vector.fill(num)(File(fileId)))
          } else {
          (fileId, acc ++ Vector.fill(num)(FreeSpace))
        }
    }
    result
  }

  def moveDisk(disk: Vector[Disk]): Vector[Disk] = {
    def go(disk: Vector[Disk], nextFree: Int, nextFile: Int): Vector[Disk] = {
      if (nextFree == -1 || nextFile == -1 || nextFree > nextFile) {
        disk
      } else {
        val v2 = disk
          .updated(nextFree, disk(nextFile))
          .updated(nextFile, FreeSpace)

        val nextNextFree = v2.indexWhere(_ == FreeSpace, 0)
        val nextNextFile = v2.lastIndexWhere {
          case File(_) => true
          case _       => false
        }

        go(v2, nextNextFree, nextNextFile)
      }
    }
    val nextFree = disk.indexWhere(_ == FreeSpace, 0)
    val nextFile = disk.lastIndexWhere {
      case File(_) => true
      case _       => false
    }
    go(disk, nextFree, nextFile)
  }

  def checksumSum(disks: Vector[Disk]): Long = {
    def checksum(disk: Disk, index: Int): Long = disk match {
      case File(id)  => id * index
      case FreeSpace => 0
    }
    disks.zipWithIndex.map((checksum _).tupled).sum
  }

  def printDisk(disk: Vector[Disk]): Vector[String] = {
    def makeString(disk: Disk): String = disk match {
      case File(id)  => id.toString
      case FreeSpace => "."
    }

    disk.map(makeString)
  }

  def solvePart1(str: String): Long = {
    checksumSum(moveDisk(parse(str)))
  }

  sealed trait DiskP2
  final case class FileP2(id: Int, size: Int) extends DiskP2
  final case class FreeSpaceP2(size: Int) extends DiskP2

  def parseP2(input: String): Vector[DiskP2] = {
    val digits = input.map(num => num.asDigit).toList
    val (_, result) = digits.zipWithIndex.foldLeft((0, Vector.empty[DiskP2])) {
      case ((fileId, acc), (num, index)) =>
        if (index % 2 == 0) // file
          {
            (fileId + 1, acc :+ FileP2(fileId, num))
          } else {
          (fileId, acc :+ FreeSpaceP2(num))
        }
    }
    result
  }

  def getFile(disks: Vector[DiskP2], index: Int): FileP2 = {
    disks(index) match {
      case file @ FileP2(_, _) => file
      case _ => throw new IllegalArgumentException("Expected file but found freespace")
    }
  }

  def getFreeSpace(disks: Vector[DiskP2], index: Int): FreeSpaceP2 = {
    disks(index) match {
      case freeSpace @ FreeSpaceP2(_) => freeSpace
      case _ => throw new IllegalArgumentException("Expected freespace but found file")
    }
  }

  def printDiskP2(disk: Vector[DiskP2]): String = {
    val builder = new StringBuilder()

    disk.foreach {
      case FileP2(id, size)  => builder.append(id.toString() * size)
      case FreeSpaceP2(size) => builder.append("." * size)
    }

    builder.toString()
  }

  def moveByBlock(disks: Vector[DiskP2]): Vector[DiskP2] = {
    def go(
        disks: Vector[DiskP2],
        nextFileIndex: Int,
    ): Vector[DiskP2] = {
      if (nextFileIndex == -1) {
        disks
      }
      else {
        val file = getFile(disks, nextFileIndex)

        val freeSpaceIndex = disks.indexWhere {
          case FreeSpaceP2(freeSpaceSize) if freeSpaceSize >= file.size => true
          case _ => false 
        }

        if (freeSpaceIndex == -1 || freeSpaceIndex >= nextFileIndex) {
          // didn't find free space big enough for file, continue
          val nextCandidate = disks.slice(0, nextFileIndex).lastIndexWhere {
            case FileP2(_, _)   => true
            case FreeSpaceP2(_) => false
          }
          go(disks, nextCandidate)
        } else {
          // found free space big enough for file, swap
          val freeSpace = getFreeSpace(disks, freeSpaceIndex)

          val remainingSpace = freeSpace.size - file.size
          assert(remainingSpace >= 0)
          if (remainingSpace == 0) {
            val updatedDisks = disks
              .updated(freeSpaceIndex, file)
              .updated(nextFileIndex, FreeSpaceP2(file.size))

            val nextCandidate = updatedDisks.slice(0, nextFileIndex - 1).lastIndexWhere {
              case FileP2(_, _)   => true
              case FreeSpaceP2(_) => false
            }
            go(updatedDisks, nextCandidate)
          } else {
            val updatedDisks = disks
              .updated(nextFileIndex, FreeSpaceP2(file.size))
              .patch(freeSpaceIndex, List(file, FreeSpaceP2(remainingSpace)), 1)

            val nextCandidate = updatedDisks.slice(0, nextFileIndex).lastIndexWhere {
              case FileP2(_, _)   => true
              case FreeSpaceP2(_) => false
            }
            go(updatedDisks, nextCandidate)
          }
        }
      }
    }

    val nextFile = disks.lastIndexWhere {
      case FileP2(_, _)   => true
      case FreeSpaceP2(_) => false
    }
    go(disks, nextFile)

  }

  def checksumSumP2(disks: Vector[DiskP2]): Long = {
    var checksum = 0L
    var logicalIndex = 0L

    disks.foreach {
      case FreeSpaceP2(size) => logicalIndex += size
      case FileP2(id, size) => {
        val sum = (logicalIndex until (logicalIndex + size)).map(_ * id).sum
        checksum += sum
        logicalIndex += size
      }
    }

    checksum
  }


  def solvePart2(str: String) = moveByBlock(parseP2(str))
}
