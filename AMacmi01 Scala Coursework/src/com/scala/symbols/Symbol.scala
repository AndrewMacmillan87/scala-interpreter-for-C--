package com.scala.symbols

/**
 * Symbol class represent symbols that will be used to
 * return values when the program is evaluated. C-- only
 * has one type , integer, so it only accepts one argument
 * of type Int
 *
 */
class Symbol(v: Int) {
	
	private val intType: String = "INTEGER"
	private val value: Int = v
	
	def withType(): String = { intType }
	def withValue(): Int = { value }
	
}

object Symbol {
	def apply(v: Int): Symbol = new Symbol(v)
}


