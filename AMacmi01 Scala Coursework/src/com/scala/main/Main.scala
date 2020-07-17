package com.scala.main

import com.scala.phases.{Evaluator, Lexer, Parser}
import com.scala.symbols.SymbolTable
import com.scala.testcode.TestCode

object Main {
	
	def main(args: Array[String]): Unit = {
		
		val code = new TestCode().withProgram()
		
		val lexer = Lexer(code)
		val parser = Parser(lexer)
		val program = parser.parseProgram()
		
		if (parser.withErrors().isEmpty) {
			val symbolTable = new SymbolTable()
			val evaluator = Evaluator()
			val result = evaluator.Evaluate(program, symbolTable)
			if (result.isDefined) print(result.get.withValue())
		} else {
			for (error <- parser.withErrors())
				println(error)
		}
	}
}
