package com.scala.traits

/**
 * Token trait that defines the basic methods
 * available to a Token that is created during
 * lexical analysis
 */
trait tToken {
	def withType(): String
	def withValue(): String
}
