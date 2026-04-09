package it.unibo.pps.ex

import it.unibo.pps.util.Sequences.Sequence
import it.unibo.pps.util.Sequences.Sequence.*
import it.unibo.pps.util.Optionals.Optional.*

object sameCategory:
  def unapply(courses: Sequence[Course]): Option[String] = courses match
    case Sequence.Cons(first, rest) => rest.find(_.category != first.category) match
      case Empty() => Some(first.category)
      case _ => None
    case _ => None

@main def testExtractor(): Unit =
  val course1 = Course("1", "Scala", "Odersky", "Programming")
  val course2 = Course("2", "Java", "Gosling", "Programming")
  val course3 = Course("3", "C++", "Stroustrup", "Programming")
  val course4 = Course("4", "Design", "Norman", "UX")
  val sameCatCourses = Sequence(course1, course2, course3)
  val diffCatCourses = Sequence(course1, course2, course4)
  val emptyCourses = Sequence.empty[Course]

  println(testSameCategory(sameCatCourses))
  println(testSameCategory(diffCatCourses))
  println(testSameCategory(emptyCourses))

  def testSameCategory(courses: Sequence[Course]): String = courses match
    case sameCategory(cat) => s"Same category: $cat"
    case _                 => "Different categories or empty sequence"