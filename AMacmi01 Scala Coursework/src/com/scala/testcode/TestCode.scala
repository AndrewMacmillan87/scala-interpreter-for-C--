package com.scala.testcode

/**
 * Only has one function that returns the program
 * to be interpreted as a String
 */
class TestCode {
	
	def withProgram(): String = {
		"""
		  |val = 104
		  |while (val >= 2) {
		  |    if (val % 2 == 0) {
		  |        next = val / 2
		  |    } else {
		  |		   next = 3 * val + 1
		  |    }
		  |	   print val, next
		  |	   val = next
		  |}
		  |""".stripMargin
	}
}

object TestCode {
	def apply(): TestCode = new TestCode()
}
