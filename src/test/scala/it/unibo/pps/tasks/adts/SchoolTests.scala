package it.unibo.pps.tasks.adts

import it.unibo.pps.u03.extensionmethods.Sequences.Sequence
import Sequence.*
import SchoolModel.BasicSchoolModule.*
import org.junit.Test
import org.junit.Assert.{assertEquals, assertFalse, assertTrue}

class SchoolTests:

  @Test def testEmptySchoolHasNoTeachers(): Unit =
    val teachers = emptySchool.teachers
    val expectedTeachers = Nil()
    assertEquals(expectedTeachers, teachers)

  @Test def testEmptySchoolHasNoCourses(): Unit =
    val courses = emptySchool.courses
    val expectedCourses = Nil()
    assertEquals(expectedCourses, courses)

  @Test def testMissingTeacherIsNotFound(): Unit =
    val hasTeacher = emptySchool.hasTeacher("John")
    assertFalse(hasTeacher)

  @Test def testMissingCourseIsNotFound(): Unit =
    val hasCourse = emptySchool.hasCourse("Math")
    assertFalse(hasCourse)

  @Test def testTeacherCreation(): Unit =
    val newTeacher = teacher("John")
    val name = newTeacher match
      case Teacher(name, _) => name
    val expectedName = "John"
    assertEquals(expectedName, name)

  @Test def testCourseCreation(): Unit =
    val newCourse = course("Math")
    val name = newCourse match
      case Course(name) => name
    val expectedName = "Math"
    assertEquals(expectedName, name)

  @Test def setTeacherIsAssigned(): Unit =
    val john = teacher("John")
    val math = course("Math")
    val teachers = emptySchool
      .setTeacherToCourse(john, math)
      .teachers
    val expectedTeachers = Cons("John", Nil())
    assertEquals(expectedTeachers, teachers)

  @Test def setCourseIsAssigned(): Unit =
    val john = teacher("John")
    val math = course("Math")
    val courses = emptySchool
      .setTeacherToCourse(john, math)
      .courses
    val expectedCourses = Cons("Math", Nil())
    assertEquals(expectedCourses, courses)

  @Test def testAssignedTeacherIsFound(): Unit =
    val john = teacher("John")
    val math = course("Math")
    val hasTeacher = emptySchool
      .setTeacherToCourse(john, math)
      .hasTeacher("John")
    assertTrue(hasTeacher)

  @Test def testAssignedCourseIsFound(): Unit =
    val john = teacher("John")
    val math = course("Math")
    val hasCourse = emptySchool
      .setTeacherToCourse(john, math)
      .hasCourse("Math")
    assertTrue(hasCourse)

  @Test def testNotAssignedCourseIsNotFound(): Unit =
    val john = teacher("John")
    val math = course("Math")
    val hasCourse = emptySchool
      .setTeacherToCourse(john, math)
      .hasCourse("Italian")
    assertFalse(hasCourse)

  @Test def testSameTeacherIsNotAddedTwice(): Unit =
    val john = teacher("John")
    val math = course("Math")
    val italian = course("Italian")
    val teachers = emptySchool
      .setTeacherToCourse(john, math)
      .setTeacherToCourse(john, italian)
      .teachers
    val expectedTeachers = Cons("John", Nil())
    assertEquals(expectedTeachers, teachers)

  @Test def testSameCourseIsNotAddedTwice(): Unit =
    val john = teacher("John")
    val math = course("Math")
    val courses = emptySchool
      .setTeacherToCourse(john, math)
      .setTeacherToCourse(john, math)
      .courses
    val expectedCourses = Cons("Math", Nil())
    assertEquals(expectedCourses, courses)

  @Test def testCoursesOfTeacherAreListedCorrectly(): Unit =
    val john = teacher("John")
    val mark = teacher("Mark")
    val math = course("Math")
    val science = course("Science")
    val italian = course("Italian")
    val courses = emptySchool
      .setTeacherToCourse(john, math)
      .setTeacherToCourse(mark, italian)
      .setTeacherToCourse(john, science)
      .coursesOfATeacher(john)
    val expectedCourses = Cons("Science", Cons("Math", Nil()))
    assertEquals(expectedCourses, courses)
  
  @Test def testCoursesListOfMissingTeacherIsEmpty(): Unit =
    val john = teacher("John")
    val courses = emptySchool.coursesOfATeacher(john)
    val expectedCourses = Nil()
    assertEquals(expectedCourses, courses)
    