import java.io.{BufferedWriter, FileWriter}

import grader.{GradingScheme, Student}

import scala.io.Source._

/*The breakdown of the final course grade is as follows:
  Final exam: 30 points
  Mid-term exams: 40 points (20 each)
  Homework: 25 points
  Quizzes: 5 points
*/
object GradingTest {

  println("constructing grading scheme")

  val gs = new GradingScheme {
    override val replace_midterm: Boolean = true
    override val max_final: Double = 100
    override val max_homework: List[Double] = List(10, 10, 12, 10, 8, 6, 12, 8, 8, 8, 8, 8)
    override val num2drop: Int = 2
    override val max_midterm: List[Double] = List(47, 50)
    override val max_quiz: List[Double] = List(10, 10, 10)
  }





  val fields = List("OrgDefinedId",
    "Username",
    "Last Name",
    "First Name",
    "HW 1 Points Grade <Numeric MaxPoints:10 Weight:8.333333332 Category:Homework CategoryWeight:25>",
    "HW 2 Points Grade <Numeric MaxPoints:10 Weight:8.333333332 Category:Homework CategoryWeight:25>",
    "HW 3 Points Grade <Numeric MaxPoints:12 Weight:8.333333332 Category:Homework CategoryWeight:25>",
    "HW 4 Points Grade <Numeric MaxPoints:10 Weight:8.333333332 Category:Homework CategoryWeight:25>",
    "HW 5 Points Grade <Numeric MaxPoints:10 Weight:8.333333332 Category:Homework CategoryWeight:25>",
    "HW 6 Points Grade <Numeric MaxPoints:6 Weight:8.333333332 Category:Homework CategoryWeight:25>",
    "HW 7 Points Grade <Numeric MaxPoints:12 Weight:8.333333332 Category:Homework CategoryWeight:25>",
    "HW 8 Points Grade <Numeric MaxPoints:8 Weight:8.333333332 Category:Homework CategoryWeight:25>",
    "HW 9 Points Grade <Numeric MaxPoints:8 Weight:8.333333332 Category:Homework CategoryWeight:25>",
    "HW 11 Points Grade <Numeric MaxPoints:8 Weight:8.333333332 Category:Homework CategoryWeight:25>",
    "HW 12 Points Grade <Numeric MaxPoints:8 Weight:8.333333332 Category:Homework CategoryWeight:25>",
    "HW 10 Points Grade <Numeric MaxPoints:8 Weight:8.333333332 Category:Homework CategoryWeight:25>",
    "Homework Subtotal Numerator",
    "Homework Subtotal Denominator",
    "Exam 1 Points Grade <Numeric MaxPoints:47 Weight:50 Category:Midterm Exams CategoryWeight:40>",
    "Exam 2 Points Grade <Numeric MaxPoints:50 Weight:50 Category:Midterm Exams CategoryWeight:40>",
    "Midterm Exams Subtotal Numerator",
    "Midterm Exams Subtotal Denominator",
    "Final Exam Points Grade <Numeric MaxPoints:100 Weight:100 Category:Final Exam CategoryWeight:30>",
    "Final Exam Subtotal Numerator",
    "Final Exam Subtotal Denominator",
    "Quiz 1 Points Grade <Numeric MaxPoints:10 Weight:50 Category:Quizzes CategoryWeight:5>",
    "Quiz 2 Points Grade <Numeric MaxPoints:10 Weight:50 Category:Quizzes CategoryWeight:5>",
    "Quizzes Subtotal Numerator",
    "Quizzes Subtotal Denominator",
    "Calculated Final Grade Numerator",
    "Calculated Final Grade Denominator",
    "Adjusted Final Grade Numerator",
    "Adjusted Final Grade Denominator",
    "End-of-Line Indicator")

  fields.take(16).drop(4)
  val content = fromFile("/home/williamdemeo/git/PROGRAMMING/SCALA/Grader/Grading/resources/MATH2130_LATEST.csv").getLines.map(_.split(","))
  val header = content.next //.split(",").map(_.trim)
  val allgrades = content.map(header.zip(_).toMap).toList
  //  val firstentry: Map[String, String] = allgrades.next // ignore the first (bogus) record
  //  val secondentry: Map[String, String] = allgrades.next
  //  println("second's OrgDefinedId: " + secondentry(fields(0)))
  //  println("second's Username: " + secondentry(fields(1)))
  //  println("second's First Name = " + secondentry(fields(3)))
  //  println("second's Last Name = " + secondentry(fields(2)))

  println("making list of students")
  def constructGrades(allgrades: List[Map[String,String]]): List[Student] = {

    def constructGrades_aux(ag: List[Map[String,String]], acc: List[Student]): List[Student] =
      ag match {
        case Nil => acc
        case x :: xs => {
          val record = x
          val student = new Student {
            override val studentID: String = record(fields(0))
            override val scheme: GradingScheme = gs
            override val homework_raw: List[Double] =
              fields.take(16).drop(4).map(s => record(s).toDouble)
            override val final_raw: Double =
              record(fields(22)).toDouble
            override val midterm_raw: List[Double] =
              fields.take(20).drop(18).map(s => record(s).toDouble)
            override val quiz_raw: List[Double] =
              10.0 :: fields.take(27).drop(25).map(s => record(s).toDouble)
          }
          constructGrades_aux(xs, student :: acc)
        }
      }
    constructGrades_aux(allgrades, Nil)
  }

  println("printing student records")
  allgrades.foreach(println)
  constructGrades(allgrades.drop(1)).foreach(println)

  val file = "/home/williamdemeo/git/PROGRAMMING/SCALA/Grader/Grading/resources/MATH2130_FINAL_COURSE_GRADES.csv"
  println("Writing student records to file:  " + file)

  val writer = new BufferedWriter(new FileWriter(file))
  writer.write("OrgDefinedId, Course Grade Points Grade, End-of-Line Indicator\n")
  constructGrades(allgrades).foreach(s => writer.write(s.studentID + ", " + s.course_grade.toString + ",#\n"))
  writer.close()


  //  student.field_names
  //  student.toString



}
  //res40: Iterator[scala.collection.immutable.Map[java.lang.String,java.lang.String]] = non-empty iterator

//  val bufferedSource: io.Source = io.Source.fromFile("MATH2130.csv")
//  def students(fb: io.Source): List[Student] =
//    bufferedSource.getLines.map {
//      line => {
//        val cols = line.split(",").map(_.trim)
//        new Student {
//          override val studentID: Int = cols(0).toInt
//          override val scheme: GradingScheme = gs
//          override val homework_raw: List[Double] = cols.take(16).drop(4).map(x => x.toDouble).toList //List(10, 10, 12, 9.6, 9.6, 5, 11.6, 7.6, 8, 8, 7, 8)
//          override val final_raw: Double = 59
//          override val midterm_raw: List[Double] = List(27.5, 18)
//          override val quiz_raw: List[Double] = List(10, 6, 10)
//        }
//      }
//    }.toList
//  bufferedSource.close





//
//  val fields: Enum[String] = (
//    "OrgDefinedId",
//    "Username",
//    "Last Name",
//    "First Name",
//    "HW 1 Points Grade",
//    "HW 2 Points Grade",
//    "HW 3 Points Grade",
//    "HW 4 Points Grade",
//    "HW 5 Points Grade",
//    "HW 6 Points Grade",
//    "HW 7 Points Grade",
//    "HW 8 Points Grade",
//    "HW 9 Points Grade",
//    "HW 11 Points Grade",
//    "HW 12 Points Grade",
//    "HW 10 Points Grade",
//    "Homework Subtotal Numerator",
//    "Homework Subtotal Denominator",
//    "Exam 1 Points Grade",
//    "Exam 2 Points Grade",
//    "Midterm Exams Subtotal Numerator",
//    "Midterm Exams Subtotal Denominator",
//    "Final Exam Points Grade",
//    "Final Exam Subtotal Numerator",
//    "Final Exam Subtotal Denominator",
//    "Quiz 1 Points Grade",
//    "Quiz 2 Points Grade",
//    "Quizzes Subtotal Numerator",
//    "Quizzes Subtotal Denominator",
//    "Calculated Final Grade Numerator",
//    "Calculated Final Grade Denominator",
//    "Adjusted Final Grade Numerator",
//    "Adjusted Final Grade Denominator")

