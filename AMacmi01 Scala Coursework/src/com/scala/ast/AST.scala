package com.scala.ast

import com.scala.traits.{tNode, tToken}

import scala.collection.mutable

/**
 * AST class provides case classes for all the expression and
 * statement types the interpreter will use to parse and
 * evaluate the program. Each <Statement> or <Expression> is
 * a node in the Abstract Syntax Tree
 *
 */
final class AST {
	
	/**
	 * The Statement case class extends the Node interface
	 * and denotes statements in the program
	 */
	abstract class Statement() extends tNode {
		var token: tToken
	}
	
	/**
	 * The Expression case class extends the Node interface
	 * and denotes expressions in the program
	 */
	abstract class Expression() extends tNode {
		var token: tToken
	}
	
	/**
	 * The Program case class is a collection of program <statements> that
	 * are stored in a mutable list
	 */
	case class Program() {
		private val statements: mutable.ListBuffer[Statement] = mutable.ListBuffer[Statement]()
		
		def withStatements(): mutable.ListBuffer[Statement] = { statements }
		
		def addStatement(statement: Statement): Unit = { statements += statement }
	}
	
	/*
	==================================
				Statements
	==================================
	*/
	
	/**
	 * The ExpressionStatement case class denotes an <expression> to be
	 * evaluated
	 */
	case class ExpressionStatement(tok: tToken) extends Statement {
		override var token: tToken = tok
		private var expression: Expression = _
		
		def setExpression(exp: Expression): Unit = { expression = exp }
		
		def withToken(): tToken = { token }
		def withExpression(): Expression = { expression }
	}
	
	/**
	 * The VariableStatement case class denotes a variable that has a
	 * <name> to identify it and a <value> to associate with it
	 */
	case class VariableStatement(tok: tToken) extends Statement {
		override var token: tToken = tok
		private var name: Identifier = _
		private var value: Expression = _
		
		def setName(n: Identifier): Unit = { name = n }
		def setValue(exp: Expression): Unit = { value = exp }
		
		def withToken(): tToken = { token }
		def withName(): Identifier = { name }
		def withValue(): Expression = { value }
	}
	
	/**
	 * The BlockStatement case class denotes a series of <statements>
	 * that form a block of code
	 */
	case class BlockStatement(tok: tToken) extends Statement {
		override var token: tToken = tok
		private val statements: mutable.ListBuffer[Statement] = mutable.ListBuffer[Statement]()
		
		def withToken(): tToken = { token }
		
		def withStatements(): mutable.ListBuffer[Statement] = { statements }
		
		def addStatement(statement: Statement): Unit = { statements += statement }
	}
	
	/**
	 * The IfStatement case class denotes an if statement that can
	 * be comprised of a <condition> and up to 2 branches
	 */
	case class IfStatement(tok: tToken) extends Statement {
		override var token: tToken = tok
		private var condition: Expression = _
		private var firstBranch: BlockStatement = _
		private var secondBranch: BlockStatement = _
		
		def setCondition(cond: Expression): Unit = { condition = cond }
		def setFirstBranch(fb: BlockStatement): Unit = { firstBranch = fb }
		def setSecondBranch(sb: BlockStatement): Unit = { secondBranch = sb }
		
		def withToken(): tToken = { token }
		def withCondition(): Expression = { condition }
		def withFirstBranch(): BlockStatement = { firstBranch }
		def withSecondBranch(): BlockStatement = { secondBranch }
	}
	
	/**
	 * The WhileStatement case class denotes a while statement that
	 * is comprised of a <condition> and a <loop> of type <BlockStatement>
	 */
	case class WhileStatement(tok: tToken) extends Statement {
		override var token: tToken = tok
		private var condition: Expression = _
		private var loop: BlockStatement = _
		
		def setCondition(cond: Expression): Unit = { condition = cond }
		def setLoop(lp: BlockStatement): Unit = { loop = lp }
		
		def withToken(): tToken = { token }
		def withCondition(): Expression = { condition }
		def withLoop(): BlockStatement = { loop }
	}
	
	/**
	 * The PrintStatement case class denotes a print statement that
	 * is comprised of a set of <Values>
	 */
	case class PrintStatement(tok: tToken) extends Statement {
		override var token: tToken = tok
		private var values: mutable.ListBuffer[Expression] = mutable.ListBuffer[Expression]()
		
		def withToken(): tToken = { token }
		
		def withValues(): mutable.ListBuffer[Expression] = { values }
		
		def addValue(value: Expression): Unit = { values += value }
	}
	
	/*
	==================================
			   Expressions
	==================================
	*/
	
	/**
	 * The Identifier case class denotes an identifier token that
	 * represents the <value> of a variable in the program
	 */
	case class Identifier(tok: tToken) extends Expression {
		override var token: tToken = tok
		private var value: String = _
		
		def setValue(v: String): Unit = { value = v }
		
		def withToken(): tToken = { token }
		def withValue(): String = { value }
	}
	
	/**
	 * The Integer case class denotes an integer token with
	 * an Int <value>
	 */
	case class Integer(tok: tToken) extends Expression {
		override var token: tToken = tok
		private var value: Int = _
		
		def setValue(i: Int): Unit = { value = i }
		
		def withToken(): tToken = { token }
		def withValue(): Int = { value }
	}
	
	/**
	 * The InfixExpression case class represents an infix
	 * expression that is comprised of the <left> expression,
	 * an <operator> and the <right> expression
	 */
	case class InfixExpression(tok: tToken) extends Expression {
		override var token: tToken = tok
		private var left: Expression = _
		private var operator: String = _
		private var right: Expression = _
		
		def setLeft(l: Expression): Unit = { left = l }
		def setOperator(o: String): Unit = { operator = o }
		def setRight(r: Expression): Unit = { right = r }
		
		def withToken(): tToken = { token }
		def withLeft(): Expression = { left }
		def withOperator(): String = { operator }
		def withRight(): Expression = { right }
	}
}

/**
 * Constructor functions for the various case classes in the <AST> class
 */
object AST {
	val tree = new AST()
	def createProgram(): tree.Program = tree.Program()
	def createExpressionStatement(tok: tToken): tree.ExpressionStatement = tree.ExpressionStatement(tok: tToken)
	def createVariableStatement(tok: tToken): tree.VariableStatement = tree.VariableStatement(tok: tToken)
	def createBlockStatement(tok: tToken): tree.BlockStatement = tree.BlockStatement(tok: tToken)
	def createIfStatement(tok: tToken): tree.IfStatement = tree.IfStatement(tok: tToken)
	def createWhileStatement(tok: tToken): tree.WhileStatement = tree.WhileStatement(tok: tToken)
	def createPrintStatement(tok: tToken): tree.PrintStatement = tree.PrintStatement(tok: tToken)
	def createInteger(tok: tToken): tree.Integer = tree.Integer(tok: tToken)
	def createIdentifier(tok: tToken): tree.Identifier = tree.Identifier(tok: tToken)
	def createInfixExpression(tok: tToken): tree.InfixExpression = tree.InfixExpression(tok: tToken)
}
