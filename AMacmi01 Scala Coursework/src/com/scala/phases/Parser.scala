package com.scala.phases

import com.scala.traits.{tFunctionStore, tToken}
import com.scala.ast.AST

import scala.collection.immutable.HashMap
import scala.collection.mutable

/**
 * A class implementing the functionality required to parse the
 * tokens produced by the lexer into statements and expressions
 * that will form the Abstract Syntax Tree that will be evaluated
 * after parsing
 *
 * @param lexer An instance of the Lexer class
 */
final class Parser(lexer: Lexer) extends tFunctionStore[AST.tree.Expression, AST.tree.Statement] {
	
	// Class attributes
	private var currentToken: tToken = _
	private var nextToken: tToken = _
	private final val NIL_PRECEDENCE = 0
	private var leftBraceCount = 0
	private val errors: mutable.ListBuffer[String] = mutable.ListBuffer[String]()
	private val precedences: HashMap[String, Int] = HashMap(
		("*", 6),
		("/", 6),
		("%", 6),
		("+", 5),
		("-", 5),
		("<", 4),
		(">", 4),
		("<=", 4),
		(">=", 4),
		("==", 3),
		("!=", 3),
		("&&", 2),
		("||", 1)
	)
	
	/**
	 * Main parse function for looping over each token value
	 * and starting the process of defining the statement/s that
	 * are appropriate for each token. The function builds up a
	 * list of <Statements> that form the individual components of
	 * the program
	 *
	 * @return AST.tree.Program - a collection of program statements
	 */
	def parseProgram(): AST.tree.Program = {
		val program = AST.createProgram()
		
		setTokens()
		setTokens()
		
		while (!currentToken.withType().equals("(end)")) {
			val statement = parseStatement(fromBlock=false)
			if (statement != null) {
				program.addStatement(statement)
				leftBraceCount = 0
			}
			setTokens()
		}
		program
	}
	
	/**
	 * Returns the <errors> ListBuffer
	 *
	 * @return mutable.listBuffer[String] - collection of errors
	 */
	def withErrors(): mutable.ListBuffer[String] = errors
	
	/**
	 * Logs a new error and appends it to the <errors> ListBuffer
	 *
	 * @param char - the value of the current token
	 */
	private def logError(char: String): Unit = {
		val errorMsg = s"Syntax error, didn't expect $char"
		errors += errorMsg
	}
	
	/**
	 * Check the <precedences> Hash Map to see what the precedence
	 * of the current token is. If it is not present in the Hash Map,
	 * return <NIL_PRECEDENCE>
	 *
	 * @return Int - an integer representing the precedence of
	 *         the current token
	 */
	private def checkCurrentPrecedence(): Int = {
		if (precedences.contains(currentToken.withValue())) {
			return precedences(currentToken.withValue())
		}
		NIL_PRECEDENCE
	}
	
	/**
	 * Check the <precedences> Hash Map to see what the precedence
	 * of the next token is. If it is not present in the Hash Map,
	 * return <NIL_PRECEDENCE>
	 *
	 * @return Int - an integer representing the precedence of
	 *         the next token
	 */
	private def checkNextPrecedence(): Int = {
		if (precedences.contains(nextToken.withValue())) {
			return precedences(nextToken.withValue())
		}
		NIL_PRECEDENCE
	}
	
	/**
	 * Set the <currentToken> and the <nextToken> values so that
	 * the parser can process each token in the lexer
	 */
	private def setTokens(): Unit = {
		currentToken = nextToken
		nextToken = lexer.fetchNextToken()
	}
	
	/**
	 * Checks the type of the <nextToken> and returns true if
	 * is equal to the <tokenType> parameter
	 *
	 * @param tokenType - string of the type of the <nextToken>
	 * @return Boolean - true if the type of <nextToken> is what
	 *         is expected, false otherwise
	 */
	private def expectNext(tokenType: String): Boolean = {
		if (nextToken.withType().equals(tokenType)) {
			setTokens()
			return true
		}
		logError(nextToken.withValue())
		false
	}
	
	/**
	 * Core helper function that checks the type of the
	 * <currentToken> and calls the appropriate function to
	 * start recursively building statements from the tokens
	 *
	 * @param fromBlock - Boolean to check if the calling function
	 *                  was called from inside a BlockStatement
	 * @return AST.tree.Statement - an AST Statement node
	 */
	private def parseStatement(fromBlock: Boolean): AST.tree.Statement = {
		currentToken.withType() match {
			case "IDENTIFIER" =>
				if (fromBlock) {
					return parseExpressionStatement()
				}
				parseVariableDeclaration()
			case "IF" =>
				parseIfStatement()
			case "WHILE" =>
				parseWhileStatement()
			case "PRINT" =>
				parsePrintStatement()
			case _ =>
				parseExpressionStatement()
		}
	}
	
	/**
	 * Begins constructing an expression by adding a AST node that represents
	 * the whole expression
	 *
	 * @return AST.tree.ExpressionStatement - Associates a node with the
	 *         expression to be constructed
	 */
	private def parseExpressionStatement(): AST.tree.ExpressionStatement = {
		val statement = AST.createExpressionStatement(currentToken)
		statement.setExpression(parseExpression(NIL_PRECEDENCE, fromPrint=false))
		statement
	}
	
	/**
	 * All expressions are constructed from this function. The <tFunctionStore>
 	 * Hash Maps containing the relevant functions for each token type are
	 * queried so that the correct function is called in order to build the
	 * expression correctly. From there, a recursive process begins to create
	 * an infix expression if necessary, or just returns a single value if that
	 * is all there is.
	 *
	 * @param precedence - Int value of the current operator precedence
	 * @param fromPrint - Boolean checking whether the calling function came
	 *                  from a PrintStatement
	 * @return AST.tree.Expression - AST node representing the left side of an expression
	 */
	private def parseExpression(precedence: Int, fromPrint: Boolean): AST.tree.Expression = {
		// Get the correct prefix function from tFunctionStore
		val prefix = this.prefixExpFuncs(currentToken.withType())
		
		if (prefix == null) {
			logError(currentToken.withValue())
			return null
		}
		
		// Use the returned prefix function to make the left side
		var leftExpression = prefix()
		
		if (!fromPrint) {
			// Check if an operator is missing. If so, log an error
			if (nextToken.withType().equals("INTEGER") || nextToken.withType().equals("IDENTIFIER")) {
				logError(nextToken.withValue())
				return null
			}
		}
		
		while (precedence < checkNextPrecedence()) {
			// Get the correct infix function from tFunctionStore
			val infix = this.infixExpFuncs(nextToken.withType())
			if (infix == null) {
				return leftExpression
			}
			setTokens()
			// Recursively build up the expression by calling the infix function
			leftExpression = infix(leftExpression)
		}
		leftExpression
	}
	
	/**
	 * Parses a block of code that is contained within a while statement
	 * block or an if/else statement block
	 *
	 * @return AST.tree.BlockStatement - code block
	 */
	def parseBlockStatement(): AST.tree.BlockStatement = {
		if (currentToken.withType().equals("LEFT_CURLY_BRACE")) {
			setTokens()
			// This variable is for keeping track of nested BlockStatements
			// Without it, blocks are returned early when the first RIGHT_CURLY_BRACE is found
			leftBraceCount += 1
		}
		
		val block = AST.createBlockStatement(currentToken)
		
		var returnBlock = false
		
		while (!currentToken.withType().equals("RIGHT_CURLY_BRACE") && !currentToken.withType().equals("(end)")) {
			if (returnBlock) return block
			
			var statement: AST.tree.Statement = null
			
			if (currentToken.withType().equals("ELSE")) return block
			
			if (currentToken.withType().equals("IDENTIFIER") && nextToken.withType().equals("ASSIGNMENT")) {
				statement = parseStatement(fromBlock=false)
				setTokens()
			} else {
				statement = parseStatement(fromBlock=true)
			}
			
			if (statement != null) {
				block.addStatement(statement)
				if (currentToken.withType().equals("RIGHT_CURLY_BRACE") && leftBraceCount > 1) {
					setTokens()
					// Reduce the number of leftBraceCount as inner blocks are returned
					leftBraceCount -= 1
					returnBlock = true
				}
			} else {
				return block
			}
		}
		block
	}
	
	/**
	 * Construct a variable declaration node
	 *
	 * @return AST.tree.VariableStatement - AST node representing a variable
	 *         declaration statement
	 */
	private def parseVariableDeclaration(): AST.tree.VariableStatement = {
		val varStatement = AST.createVariableStatement(currentToken)
		val identifier = AST.createIdentifier(currentToken)
		identifier.setValue(currentToken.withValue())
		varStatement.setName(identifier)
		
		if (!expectNext("ASSIGNMENT")) {
			logError(nextToken.withValue())
			return null
		}
		
		setTokens()
		varStatement.setValue(parseExpression(NIL_PRECEDENCE, fromPrint=false))
		varStatement
	}
	
	/**
	 * Parse expressions that are nested inside parentheses
	 *
	 * @return - an AST node of type <Expression>
	 */
	override def parseBoundExpression(): AST.tree.Expression = {
		setTokens()
		val expression = parseExpression(NIL_PRECEDENCE, fromPrint=false)
		
		if (!expectNext("RIGHT_PARENTHESES")) {
			logError(nextToken.withValue())
			return null
		}
		expression
	}
	
	/**
	 * Parse if/else statements
	 *
	 * @return AST.tree.Statement - an AST node of type <Statement>
	 */
	private def parseIfStatement(): AST.tree.Statement = {
		val statement = AST.createIfStatement(currentToken)
		
		if (!expectNext("LEFT_PARENTHESES")) {
			logError(nextToken.withValue())
			return null
		}
		
		setTokens()
		statement.setCondition(parseExpression(NIL_PRECEDENCE, fromPrint=false))
		
		if (statement.withCondition() == null) {
			logError(currentToken.withValue())
			return null
		}
		
		if (!expectNext("RIGHT_PARENTHESES")) {
			logError(nextToken.withValue())
			return null
		}
		
		setTokens()
		statement.setFirstBranch(parseBlockStatement())
		
		if (currentToken.withType().equals("ELSE")) {
			setTokens()
			statement.setSecondBranch(parseBlockStatement())
		}
		statement
	}
	
	/**
	 * Parse while statements
	 *
	 * @return AST.tree.Statement - an AST node of type <Statement>
	 */
	private def parseWhileStatement(): AST.tree.Statement = {
		val statement = AST.createWhileStatement(currentToken)
		
		if (!expectNext("LEFT_PARENTHESES")) {
			logError(nextToken.withValue())
			return null
		}
		
		setTokens()
		statement.setCondition(parseExpression(NIL_PRECEDENCE, fromPrint=false))
		
		if (statement.withCondition() == null) {
			logError(currentToken.withValue())
			return null
		}
		
		if (!expectNext("RIGHT_PARENTHESES")) {
			logError(nextToken.withValue())
			return null
		}
		
		setTokens()
		statement.setLoop(parseBlockStatement())
		statement
	}
	
	/**
	 * Parse print statements
	 *
	 * @return AST.tree.Statement - an AST node of type <Statement>
	 */
	private def parsePrintStatement(): AST.tree.Statement = {
		val statement = AST.createPrintStatement(currentToken)
		
		@scala.annotation.tailrec
		def setValues(ps: AST.tree.PrintStatement): Unit = {
			setTokens()
			ps.addValue(parseExpression(NIL_PRECEDENCE, fromPrint=true))
			
			// Keep checking for values to print by checking for a COMMA
			if (nextToken.withType().equals("COMMA")) {
				setTokens()
				setValues(ps)
			}
		}
		setValues(statement)
		setTokens()
		statement
	}
	
	/**
	 * Parse an infix expression
	 *
	 * @param left - the left side of an infix expression to parse
	 * @return AST.tree.Expression - an AST node of type <Expression>
	 */
	override def parseInfix(left: AST.tree.Expression): AST.tree.Expression = {
		val expression = AST.createInfixExpression(currentToken)
		expression.setLeft(left)
		expression.setOperator(currentToken.withValue())
		
		// Setting the precedence sets the order that infix expressions are evaluated in
		val precedence = checkCurrentPrecedence()
		setTokens()
		expression.setRight(parseExpression(precedence, fromPrint=false))
		expression
	}
	
	/**
	 * Parse an identifier from the <currentToken>
	 *
	 * @return AST.tree.Expression - an AST node of type <Expression>
	 */
	override def parseIdentifier(): AST.tree.Expression = {
		val identifier = AST.createIdentifier(currentToken)
		identifier.setValue(currentToken.withValue())
		identifier
	}
	
	/**
	 * Parse an integer from the <currentToken>
	 *
	 * @return - an AST node of type <Expression>
	 */
	override def parseInteger(): AST.tree.Expression = {
		val integer = AST.createInteger(currentToken)
		try {
			val value = currentToken.withValue().toInt
			integer.setValue(value)
			integer
		} catch {
			case e: NumberFormatException =>
				logError(s"${currentToken.withValue()}, expected Int")
				null
		}
	}
}

object Parser {
	def apply(lexer: Lexer): Parser = new Parser(lexer)
}
