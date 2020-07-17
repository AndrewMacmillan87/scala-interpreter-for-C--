package com.scala.phases

import com.scala.traits.tToken

import scala.collection.immutable.HashMap
import scala.util.control.Breaks.breakable

/**
 * A class implementing the functionality required to carry
 * out lexical analysis on an input string that represents
 * the C-- program being interpreted
 *
 * @param input A string denoting the program being interpreted
 */
final class Lexer(input: String) {
	
	/**
	 * Case class implementing the <tToken> trait for
	 * creating new instances of <Token> that will later
	 * be processed by the <Parser>
	 *
	 * @param tokenType  A string denoting the token type
	 * @param tokenValue A string denoting the token value
	 */
	case class Token(tokenType: String, tokenValue: String) extends tToken {
		override def withType(): String = tokenType
		override def withValue(): String = tokenValue
	}
	
	// C-- keywords
	private val KEYWORDS: HashMap[String, Token] = HashMap(
		("while", Token("WHILE", "while")),
		("if", Token("IF", "if")),
		("else", Token("ELSE", "else")),
		("print", Token("PRINT", "print"))
	)
	
	private val filteredInput = input.replaceAll("[\r\n\t]", " ")
	private var inputIndex: Int = 0
	private var currentChar: Option[Char] = Option(filteredInput(inputIndex))
	
	/**
	 * Inspects <currentChar> and creates an appropriate <Token>
	 * based on the value of <currentChar>
	 *
	 * @return An instance of Token representing the <currentChar>
	 */
	def fetchNextToken(): Token = {
		
		val IdentifierPattern = "([a-zA-Z]+)".r
		val NumberPattern = "([0-9]+)".r
		val ArithmeticOperators = "([+-/*%])".r
		val LogicalOperators = "([<>!])".r
		val whitespace = "([\\s])".r
		
		while (currentChar.isDefined) {
			
			val thisChar = currentChar.get
			
			breakable {
				thisChar match {
					case ',' =>
						advance()
						return Token("COMMA", ",")
					case IdentifierPattern(thisChar) => return createIdentifier()
					case NumberPattern(thisChar) => return createNumber()
					case ArithmeticOperators(thisChar) =>
						advance()
						if (thisChar == '+')
							return Token("PLUS", thisChar.toString)
						if (thisChar == '-')
							return Token("MINUS", thisChar.toString)
						if (thisChar == '*')
							return Token("MULTIPLY", thisChar.toString)
						if (thisChar == '/')
							return Token("DIVIDE", thisChar.toString)
						if (thisChar == '%')
							return Token("MODULO", thisChar.toString)
					case LogicalOperators(thisChar) =>
						if (thisChar == '!') {
							if (checkNext().get.equals('=')) {
								advance()
								advance()
								return Token("NOT_EQUAL", thisChar.toString + "=")
							}
							error(currentChar)
						}
						if (thisChar == '<') {
							if (checkNext().get.equals('=')) {
								advance()
								advance()
								return Token("LESS_THAN_EQUAL_TO", thisChar.toString + "=")
							}
							advance()
							return Token("LESS_THAN", thisChar.toString)
						}
						if (thisChar == '>') {
							if (checkNext().get.equals('=')) {
								advance()
								advance()
								return Token("GREATER_THAN_EQUAL_TO", thisChar.toString + "=")
							}
							advance()
							return Token("GREATER_THAN", thisChar.toString)
						}
					case whitespace(thisChar) =>
						while (currentChar.isDefined && currentChar.get == ' ')
							advance()
					case '=' =>
						if (checkNext().get.equals('=')) {
							advance()
							advance()
							return Token("EQUAL", "==")
						}
						advance()
						return Token("ASSIGNMENT", "=")
					case '(' =>
						advance()
						return Token("LEFT_PARENTHESES", "(")
					case ')' =>
						advance()
						return Token("RIGHT_PARENTHESES", ")")
					case '{' =>
						advance()
						return Token("LEFT_CURLY_BRACE", "{")
					case '}' =>
						advance()
						return Token("RIGHT_CURLY_BRACE", "}")
					case '&' =>
						if (checkNext().get.equals('&')) {
							advance()
							advance()
							return Token("AND", thisChar.toString + "&")
						}
						error(currentChar)
					case '|' =>
						if (checkNext().get.equals('|')) {
							advance()
							advance()
							return Token("OR", thisChar.toString + "|")
						}
						error(currentChar)
					case _ =>
						// If no token was matched, throw an Exception
						error(currentChar)
				}
			}
		}
		Token("(end)", null)
	}
	
	/**
	 * Throws an error in the event that an invalid
	 * character is found in the input
	 *
	 * @param value The current character
	 */
	private def error(value: Option[Char]): Unit = {
		throw new Exception(s"Invalid character found: ${value.get}")
	}
	
	/**
	 * Increments the current <inputIndex> by 1 and sets the
	 * <currentChar> to None: Option[Char] if the <inputIndex>
	 * is greater than the length of the <filteredInput>, or sets the
	 * <currentChar> to <filteredInput(inputIndex)> if <inputIndex> is
	 * less than its length
	 */
	private def advance(): Unit = {
		inputIndex += 1
		if (inputIndex > filteredInput.length() - 1)
			currentChar = None: Option[Char]
		else
			currentChar = Option(filteredInput(inputIndex))
	}
	
	/**
	 * Checks to see what the next character in <filteredInput> is
	 * and returns it if is it is not <None>
	 *
	 * @return Option[Char]
	 */
	private def checkNext(): Option[Char] = {
		val nextIndex = inputIndex + 1
		if (inputIndex > filteredInput.length() - 1)
			None
		else
			Option(filteredInput(nextIndex))
	}
	
	/**
	 * Creates a number string from the <filteredInput> and returns
	 * a <Token> of type INTEGER that represents the number
	 *
	 * @return Token(String, String)
	 */
	private def createNumber(): Token = {
		var number = ""
		
		while (currentChar.isDefined && currentChar.get.isDigit) {
			number += currentChar.get
			advance()
		}
		
		val token: Token = Token("INTEGER", number)
		token
	}
	
	/**
	 * Creates a, identifier string from the <filteredInput> and returns
	 * a <Token> of type IDENTIFIER that represents the identifier
	 *
	 * @return Token(String, String)
	 */
	private def createIdentifier(): Token = {
		var identifier = ""

		while (currentChar.isDefined && currentChar.get.isLetter) {
			identifier += currentChar.get
			advance()
		}

		if (KEYWORDS.contains(identifier)) {
			return KEYWORDS(identifier)
		}
		Token("IDENTIFIER", identifier)
	}
}

object Lexer {
	def apply(input: String): Lexer = new Lexer(input)
}
