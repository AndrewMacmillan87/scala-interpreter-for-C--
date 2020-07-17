package com.scala.symbols

import scala.collection.immutable.HashMap

/**
 * SymbolTable class is where identifiers and their values
 * are stored at evaluation time. It exposes <setValue> and
 * <getValue> methods for updating and querying the <table>
 * Hash Map
 *
 */
class SymbolTable {
	
	private var table: HashMap[String, Option[Symbol]] = HashMap[String, Option[Symbol]]()
	
	def setValue(identifier: String, value: Option[Symbol]): Option[Symbol] = {
		table += (identifier -> value)
		value
	}
	
	def withValue(identifier: String): Option[Symbol] = {
		if (table.contains(identifier)) {
			return table(identifier)
		}
		None
	}
	
	def withTable(): HashMap[String, Option[Symbol]] = { table }
	
}

object SymbolTable {
	def apply(): SymbolTable = new SymbolTable()
}
