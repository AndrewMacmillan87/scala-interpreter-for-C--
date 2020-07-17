package com.scala.traits

import scala.collection.immutable.HashMap

/**
 * The <tFunctionStore> trait outlines the functions that must
 * be implemented in order to parse different types of tokens.
 * This program uses Vaughan Pratt parse function
 * association to link a specific parsing function to
 * a specific token. Any class that implements this will
 * have access to the <prefixExpFuncs> and <infixExpFuncs>
 * hash maps that define the token -> function association.
 * The type params <E> and <S> are generic placeholders for
 * types of AST node
 */
trait tFunctionStore[E, S] {
	
	type prefixExpFunc = () => E
	type prefixStatFunc = () => S
	type infixExpFunc = E => E
	
	def parseIdentifier(): E
	def parseInteger(): E
	def parseBoundExpression(): E
	
	val prefixExpFuncs: HashMap[String, prefixExpFunc] = HashMap(
		("IDENTIFIER", parseIdentifier),
		("INTEGER", parseInteger),
		("LEFT_PARENTHESES", parseBoundExpression)
	)
	
	def withPrefixExpFuncs(): HashMap[String, prefixExpFunc] = prefixExpFuncs
	
	def parseInfix(exp: E): E
	
	val infixExpFuncs: HashMap[String, infixExpFunc] = HashMap(
		("MULTIPLY", parseInfix),
		("DIVIDE", parseInfix),
		("MODULO", parseInfix),
		("PLUS", parseInfix),
		("MINUS", parseInfix),
		("LESS_THAN", parseInfix),
		("GREATER_THAN", parseInfix),
		("LESS_THAN_EQUAL_TO", parseInfix),
		("GREATER_THAN_EQUAL_TO", parseInfix),
		("EQUAL", parseInfix),
		("NOT_EQUAL", parseInfix),
		("AND", parseInfix),
		("OR", parseInfix)
	)
	
	def withInfixExpFuncs(): HashMap[String, infixExpFunc] = infixExpFuncs
}
