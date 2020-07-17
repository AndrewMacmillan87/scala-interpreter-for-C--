package com.scala.traits

/**
 * Symbol trait that outlines the basic
 * methods on a Symbol that will be returned
 * when the program statements are evaluated
 */
trait tSymbol {
	def withType(): String
	def withValue(): Any
}
