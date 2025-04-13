package aoc

import better.files.Resource
import scala.collection.immutable

object Day5 {
  val data = Resource.getAsString("day5/real.txt")

  type Front = Int
  type Back = Int

  def parseRule(str: String): Rule = {
    val part = str.split("\\|")
    Rule(part(0).toInt, part(1).toInt)
  }

  val parts =  data.split("\n\n")
  val section1 = parts(0).split("\n").toList.map(parseRule) //rules
  val section2 = parts(1).split("\n").toList.map(_.split(",").toList.map(_.toInt).toList) // updates
  
  // part1

  // use set for numbers that not allow to be back of current number ex: current number 61, Set(97, 47, 75) check, add more to the Set 
//then check ... again. Goal: Map of a number and a set of numbers that are not allow to be behind it
  def upgradeRules(rules: List[Rule]): Map[Int, Set[Int]] = {
   val groupd = rules.groupBy(_.back)
   groupd.view.mapValues(_.map(_.front).toSet).toMap
  }

  def getMiddleNum(update: List[Int]): Int = update(update.length/2)

  def check(rule: Map[Int, Set[Int]], updates: List[Int]): Boolean = {
    def step(updates: List[Int], notAllow: Set[Int]): Boolean = {
      updates match {
        case Nil => true
        case x :: _ if notAllow.contains(x) => false
        case x :: xs => {
          val additionalNotAllowed = rule.getOrElse(x, Set.empty)
          step(xs, notAllow ++ additionalNotAllowed)
        }
      }
    }
    step(updates, Set.empty)
  }

  def computeAll(rule: Map[Int, Set[Int]], allUpdates: List[List[Int]]): Int = {
    allUpdates.foldLeft(0){ (score, nextUpdate) => 
      if (check(rule, nextUpdate)) ((getMiddleNum(nextUpdate))+ score)
      else score
    }
  }

  final case class Rule(front: Int, back: Int)

// part2 

def correctOrder(update: List[Int], rules: Map[Int, Set[Int]]): List[Int] = {
  val releventRules = rules.view.filterKeys(update.toSet).mapValues(_.intersect(update.toSet)).toMap
  val mustComeBefore = releventRules.foldLeft(Map.empty[Int, Set[Int]].withDefaultValue(Set.empty)) {
    case (acc, (current, blockedBy)) => 
      blockedBy.foldLeft(acc) { case (map, other) =>
          map.updated(current, map(current) + other)
        }
  }
  val allNums = update.toSet ++ releventRules.keySet ++ releventRules.values.flatten
  val countBefore = {
    val intialCount = allNums.map(_ -> 0).toMap.withDefaultValue(0)
    mustComeBefore.values.flatten.foldLeft(intialCount){ (counts, number) => 
      counts.updated(number, counts(number) + 1)  
    }
  }
  countBefore.toList.sortBy(_._2).map(_._1) 
}

def computerReorder(rule: Map[Int, Set[Int]], allUpdates: List[List[Int]]): Int = {
  allUpdates.foldLeft(0){ (score, nextUpdate) => 
      if (!check(rule, nextUpdate)) (getMiddleNum(correctOrder(nextUpdate, rule))+ score)
      else score
    }
}
  


  def  main(arg: Array[String]): Unit = {
    val rules = upgradeRules(section1)
    val part1 = computeAll(rules, section2)
    val part2 = computerReorder(rules, section2)

    println(part2)
  }
}
