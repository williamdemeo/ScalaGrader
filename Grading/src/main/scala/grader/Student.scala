package grader

//val scheme = Map(("Final", 30), ("MT", 40), ("HW", 25), ("Q", 5))
//val max_homework: List[Double]= List(10, 10, 12, 10, 8, 6, 12, 8, 8, 8, 8, 8)
//val num2drop: Int = 2
//val max_midterm: List[Double]= List(47, 50)
//val max_quiz: List[Double]= List(10,10,10)

trait GradingScheme {
  val rubrik: Map[String,Int] = Map(("Final", 30), ("Midterm", 40), ("Homework", 25), ("Quiz", 5))
  val max_homework: List[Double]
  val num2drop: Int
  val max_midterm: List[Double]
  val max_quiz: List[Double]
  val max_final: Double
  val replace_midterm: Boolean
  val replacement_percentage = 0.8
  override def toString = rubrik.toString + "\n" +
    max_homework.toString  + "\n" +
    num2drop.toString + "\n" +
    max_midterm.toString  + "\n" +
    max_quiz.toString + "\n" +
    max_final.toString + "\n" +
    replace_midterm.toString + "\n" +
    replacement_percentage.toString + "\n"

}

trait Student {
  val studentID: String
  val scheme: GradingScheme
  val homework_raw: List[Double]
  val midterm_raw: List[Double]
  val quiz_raw: List[Double]
  val final_raw: Double

  def sorted_homework: List[Double] = {
    val hw = homework_raw.zip(scheme.max_homework).map { case (grade, max) => grade / max }
    hw.sorted
  }

  def homework: Double = sorted_homework.drop(scheme.num2drop).sum/(homework_raw.length - scheme.num2drop)

  def quiz = quiz_raw.sum/scheme.max_quiz.sum

  def final_exam = final_raw/scheme.max_final


  def mt_percents: List[Double] = midterm_raw.zip(scheme.max_midterm).map { case (grade, max) => grade / max }

  def midterm_standard: Double = mt_percents.sum / mt_percents.length
  def midterm_alternative = (final_exam * scheme.replacement_percentage :: mt_percents.sorted.drop(1)).sum / midterm_raw.length

  def midterm =
    if (scheme.replace_midterm) Math.max(midterm_standard, midterm_alternative)
    else midterm_alternative

  def course_grade: Double =
    scheme.rubrik("Final")*final_exam +
    scheme.rubrik("Midterm")*midterm +
    scheme.rubrik("Homework")*homework +
    scheme.rubrik("Quiz")*quiz

  val field_names = "studentID, homework, quiz, midterm_std, midterm_alt, midterm, final, COURSE_GRADE"

  override def toString =
    studentID + ", " +
    homework + ", " +
    quiz + ", " +
    midterm_standard + ", " +
    midterm_alternative + ", " +
    final_exam + ", " +
    course_grade

}


