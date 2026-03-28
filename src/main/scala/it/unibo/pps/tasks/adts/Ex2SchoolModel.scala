package it.unibo.pps.tasks.adts

import it.unibo.pps.u03.extensionmethods.Sequences.Sequence
import Sequence.*
import it.unibo.pps.u03.Optionals.Optional

import scala.annotation.tailrec

/*  Exercise 2: 
 *  Implement the below trait, and write a meaningful test.
 *  Suggestions:
 *  - reuse Sequences and Optionals as imported above
 *  - For other suggestions look directly to the methods and their description
 */
object SchoolModel:

  trait SchoolModule:
    type School
    type Teacher
    type Course

    /**
     * This a factory method for create a teacher from a name
     * e.g.,
     * teacher("John") // => Teacher("John")
     * Note!! The internal representation of a teacher may vary, decide what is the best for you
     * @param name the name of the teacher
     * @return the teacher created
     */
    def teacher(name: String): Teacher
    /**
     * This a factory method for create a course from a name
     * e.g.,
     * course("Math") // => Course("Math")
     * Note!! The internal representation of a course may vary, decide what is the best for you
     * @param name the name of the course
     * @return the course created
     *  */
    def course(name: String): Course

    /**
     * This method should return an empty school, namely a school without any teacher and course
     * e.g.,
     * emptySchool // => School(courses = Nil(), teachers = Nil(), teacherToCourses = Nil())
     * NOTE!! The above is just an example, the internal representation may vary, decide what is the best for you
     * You can store just the teacherToCourses, or having a case class for the school, or whatever you think is the best
     * @return the empty school
     */
    def emptySchool: School
    
    extension (school: School)
      /**
       * This method should return the list of courses
       * e.g.,
       * emptySchool.courses // => Nil()
       * emptySchool.setTeacherToCourse(teacher("John"), course("Math")).courses // => Cons("Math", Nil())
       * emptySchool
       *  .setTeacherToCourse(teacher("John"), course("Math"))
       *  .setTeacherToCourse(teacher("John"), course("Italian")).courses // => Cons("Math", Cons("Italian", Nil()))
       * Note!! If there are duplicates, just return them once
       * @return the list of courses
       */
      def courses: Sequence[String]
      /**
       * This method should return the list of teachers
       * e.g.,
       * emptySchool.teachers // => Nil()
       * emptySchool.setTeacherToCourse(teacher("John"), course("Math")).teachers // => Cons("John", Nil())
       * val john = teacher("John")
       * emptySchool
       *  .setTeacherToCourse(john, course("Math"))
       *  .setTeacherToCourse(john, course("Italian")).teachers // => Cons("John", Nil())
       * Note!! If there are duplicates, just return them once
       * @return the list of teachers
       */
      def teachers: Sequence[String]
      /**
       * This method should return a new school with the teacher assigned to the course
       * e.g.,
       * emptySchool
       *   .setTeacherToCourse(teacher("John"), course("Math")) // => School(courses = Cons("Math", Nil()), teachers = Cons("John", Nil()), teacherToCourses = Cons(("John", "Math"), Nil()))
       *  */
      def setTeacherToCourse(teacher: Teacher, course: Course): School
      /**
       * This method should return the list of courses assigned to a teacher
       * e.g.,
       * emptySchool.coursesOfATeacher(teacher("John")) // => Nil()
       * emptySchool
       *   .setTeacherToCourse(teacher("John"), course("Math"))
       *   .coursesOfATeacher(teacher("John")) // => Cons("Math", Nil())
       * emptySchool
       *   .setTeacherToCourse(teacher("John"), course("Math"))
       *   .setTeacherToCourse(teacher("John"), course("Italian"))
       *   .coursesOfATeacher(teacher("John")) // => Cons("Math", Cons("Italian", Nil()))
       * @return the list of courses assigned to a teacher
       */
      def coursesOfATeacher(teacher: Teacher): Sequence[String]
      /**
       * This method should return true if the teacher is present in the school
       * e.g.,
       * emptySchool.hasTeacher("John") // => false
       * emptySchool
       *  .setTeacherToCourse(teacher("John"), course("Math"))
       *  .hasTeacher("John") // => true
       *
       */
      def hasTeacher(name: String): Boolean
      /**
       * This method should return true if the course is present in the school
       * e.g.,
       * emptySchool.hasCourse("Math") // => false
       * emptySchool
       *  .setTeacherToCourse(teacher("John"), course("Math"))
       *  .hasCourse("Math") // => true
       *
       */
      def hasCourse(name: String): Boolean

  object BasicSchoolModule extends SchoolModule:

    case class SchoolType(teachersList: Sequence[Teacher], coursesList: Sequence[Course])
    case class TeacherType(name: String, courses: Sequence[Course])
    case class CourseType(name: String)

    override type School = SchoolType
    override type Teacher = TeacherType
    override type Course = CourseType

    def teacher(name: String): Teacher = TeacherType(name, Nil())
    def course(name: String): Course = CourseType(name)
    def emptySchool: School = SchoolType(Nil(), Nil())
    private def _teacher(name: String, courses: Sequence[Course]) = TeacherType(name, courses)

    extension (school: School)

      def courses: Sequence[String] = school match
        case School(_, courses) => courses.map {
          case Course(name) => name
        }

      def teachers: Sequence[String] = school match
        case School(teachers, _) => teachers.map {
          case Teacher(name, _) => name
        }

      def setTeacherToCourse(teacher: Teacher, course: Course): School =
        val updatedTeachers = _updateTeachersIfNecessary(teacher, course)
        val updatedCourses = _updateCoursesIfNecessary(course)
        SchoolType(updatedTeachers, updatedCourses)

      def coursesOfATeacher(teacher: Teacher): Sequence[String] =
        val foundTeacher = (school, teacher) match
          case (School(teachers, _), Teacher(name, courses)) => teachers.filter {
            case Teacher(n, _) => n == name
          }
        foundTeacher match
          case Nil() => Nil()
          case Cons(Teacher(_, courses), _) => courses.map {
            case Course(name) => name
          }
      
      def hasTeacher(name: String): Boolean =
        !school.teachers.filter(teacher => teacher == name).equals(Nil())

      def hasCourse(name: String): Boolean =
        !school.courses.filter(course => course == name).equals(Nil())
      
      private def _teacherHasCourse(teacher: Teacher, course: Course): Boolean = teacher match
        case Teacher(_, courses) => !courses.filter(c => c.equals(course)).equals(Nil())
      
      private def _updateTeachersIfNecessary(teacher: Teacher, course: Course): Sequence[Teacher] =
        (school, teacher) match
          case (School(teachers, _), Teacher(name, courses)) if school.hasTeacher(name) && _teacherHasCourse(teacher, course) =>
            teachers
          case (School(teachers, _), Teacher(name, courses)) if school.hasTeacher(name) && !_teacherHasCourse(teacher, course) =>
            Cons(
              _teacher(name, Cons(course, courses)),
              teachers.filter {
                case Teacher(n, _) => n == name
              }
            )
          case (School(teachers, _), Teacher(name, courses)) => Cons(_teacher(name, Cons(course, courses)), teachers)
      
      private def _updateCoursesIfNecessary(course: Course): Sequence[Course] = (school, course) match
        case (School(_, courses), Course(name)) if school.hasCourse(name) => courses
        case (School(_, courses), newCourse) => Cons(newCourse, courses)

@main def examples(): Unit =
  import SchoolModel.BasicSchoolModule.*
  val school = emptySchool
  println(school.teachers) // Nil()
  println(school.courses) // Nil()
  println(school.hasTeacher("John")) // false
  println(school.hasCourse("Math")) // false
  val john = teacher("John")
  val math = course("Math")
  val italian = course("Italian")
  val school2 = school.setTeacherToCourse(john, math)
  println(school2.teachers) // Cons("John", Nil())
  println(school2.courses) // Cons("Math", Nil())
  println(school2.hasTeacher("John")) // true
  println(school2.hasCourse("Math")) // true
  println(school2.hasCourse("Italian")) // false
  val school3 = school2.setTeacherToCourse(john, italian)
  println(school3.teachers) // Cons("John", Nil())
  println(school3.courses) // Cons("Math", Cons("Italian", Nil()))
  println(school3.hasTeacher("John")) // true
  println(school3.hasCourse("Math")) // true
  println(school3.hasCourse("Italian")) // true
  println(school3.coursesOfATeacher(john)) // Cons("Math", Cons("Italian", Nil()))


