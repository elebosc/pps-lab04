package it.unibo.pps.tasks.adts

import it.unibo.pps.u03.extensionmethods.Sequences.Sequence
import Sequence.*
import SchoolModel.BasicSchoolModule.*
import org.junit.Test
import org.junit.Assert.{assertEquals, assertFalse, assertTrue}

class SchoolTests:

  @Test def testEmptySchoolHasNoTeachers(): Unit =
    val school = emptySchool
    val teachers = school.teachersList
    val expectedTeachers = Nil()
    assertEquals(expectedTeachers, teachers)

  @Test def testEmptySchoolHasNoCourses(): Unit =
    val school = emptySchool
    val courses = school.coursesList
    val expectedCourses = Nil()
    assertEquals(expectedCourses, courses)

  @Test def testMissingTeacherIsNotFound(): Unit =
    val school = emptySchool
    assertFalse(school.hasTeacher("John"))

  @Test def testMissingCourseIsNotFound(): Unit =
    val school = emptySchool
    assertFalse(school.hasCourse("Math"))

  @Test def setTeacherIsAssigned(): Unit =
    val school = emptySchool
    val john = teacher("John")
    val math = course("Math")
    val school2 = school.setTeacherToCourse(john, math)
    val expectedTeachers = Cons("John", Nil())
    assertEquals(expectedTeachers, school2.teachers)

  @Test def setCourseIsAssigned(): Unit =
    val school = emptySchool
    val john = teacher("John")
    val math = course("Math")
    val school2 = school.setTeacherToCourse(john, math)
    val expectedCourses = Cons("Math", Nil())
    assertEquals(expectedCourses, school2.courses)

  @Test def testAssignedTeacherIsFound(): Unit =
    val school = emptySchool
    val john = teacher("John")
    val math = course("Math")
    val school2 = school.setTeacherToCourse(john, math)
    assertTrue(school2.hasTeacher("John"))

  @Test def testAssignedCourseIsFound(): Unit =
    val school = emptySchool
    val john = teacher("John")
    val math = course("Math")
    val school2 = school.setTeacherToCourse(john, math)
    assertTrue(school2.hasCourse("Math"))

  @Test def testNotAssignedCourseIsNotFound(): Unit =
    val school = emptySchool
    val john = teacher("John")
    val math = course("Math")
    val school2 = school.setTeacherToCourse(john, math)
    assertFalse(school2.hasCourse("Italian"))

  @Test def testSameTeacherIsNotAddedTwice(): Unit =
    val school = emptySchool
    val john = teacher("John")
    val math = course("Math")
    val italian = course("Italian")
    val school2 = school.setTeacherToCourse(john, math)
    val school3 = school2.setTeacherToCourse(john, italian)
    val expectedTeachers = Cons("John", Nil())
    assertEquals(expectedTeachers, school3.teachers)
