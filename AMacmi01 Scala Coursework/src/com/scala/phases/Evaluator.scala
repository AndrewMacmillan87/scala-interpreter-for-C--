package com.scala.phases

import com.scala.ast.AST
import com.scala.symbols.{Symbol, SymbolTable}

import scala.collection.mutable

/**
 * Evaluator class evaluates the <AST> and creates a
 * <Symbol> from each statement/expression in the tree.
 * This is done recursively by breaking down each statement
 * until it is evaluated into either a boolean integer
 * (1 or 0) or a <Symbol> of type <Integer>
 */
final class Evaluator() {
	
	def Evaluate(program: AnyRef, symbolTable: SymbolTable): Option[Symbol] = {
		// Match the different Statement/Expression types in the program
		program match {
			case nodeType: AST.tree.Program =>
				evaluateStatements(nodeType.withStatements(), symbolTable)
			case nodeType: AST.tree.VariableStatement =>
				val variableValue = Evaluate(nodeType.withValue(), symbolTable)
				symbolTable.setValue(nodeType.withName().withValue(), variableValue)
				variableValue
			case nodeType: AST.tree.ExpressionStatement =>
				Evaluate(nodeType.withExpression(), symbolTable)
			case nodeType: AST.tree.BlockStatement =>
				evaluateStatements(nodeType.withStatements(), symbolTable)
			case nodeType: AST.tree.IfStatement =>
				evaluateIfStatement(nodeType, symbolTable)
			case nodeType: AST.tree.WhileStatement =>
				evaluateWhileStatement(nodeType, symbolTable)
			case nodeType: AST.tree.PrintStatement =>
				evaluatePrintStatement(nodeType, symbolTable)
			case nodeType: AST.tree.InfixExpression =>
				val left = Evaluate(nodeType.withLeft(), symbolTable)
				val right = Evaluate(nodeType.withRight(), symbolTable)
				evaluateInfix(nodeType.withOperator(), left, right, symbolTable)
			case nodeType: AST.tree.Identifier =>
				evaluateIdentifier(nodeType.withValue(), symbolTable)
			case nodeType: AST.tree.Integer =>
				Option(new Symbol(nodeType.withValue()))
			case _ =>
				None
		}
	}
	
	/**
	 * Evaluates all the program statements by repeatedly calling the
	 * recursive <Evaluate> function inside a loop.
	 *
	 * @param statements - ALl the statements that comprise the program
	 * @param symbolTable - Table of symbols present in the program
	 * @return Symbol - The evaluated result
	 */
	private def evaluateStatements(statements: mutable.ListBuffer[AST.tree.Statement], symbolTable: SymbolTable): Option[Symbol] = {
		var result: Option[Symbol] = None
		for (statement <- statements) {
			result = Evaluate(statement, symbolTable)
		}
		result
	}
	
	/**
	 * Evaluates an if/else statement by evaluating the statement <condition>
	 * into a boolean value (1 or 0) and then evaluating the correct branch
	 * based on the result
	 *
	 * @param ifStatement - The if statement to evaluate
	 * @param symbolTable - Table of symbols present in the program
	 * @return Symbol - The evaluated result
	 */
	private def evaluateIfStatement(ifStatement: AST.tree.IfStatement, symbolTable: SymbolTable): Option[Symbol] = {
		val condition = Evaluate(ifStatement.withCondition(), symbolTable)
		 if (condition.get.withValue() == 1) {
			return Evaluate(ifStatement.withFirstBranch(), symbolTable)
		} else {
			if (condition.get.withValue() == 0 && ifStatement.withSecondBranch() != null) {
				return Evaluate(ifStatement.withSecondBranch(), symbolTable)
			}
		}
		None
	}
	
	/**
	 * Evaluates a while statement by evaluating the statement <condition>
	 * into a boolean value (1 or 0) and then evaluating the loop based on
	 * the result
	 *
	 * @param whileStatement - The if statement to evaluate
	 * @param symbolTable - Table of symbols present in the program
	 * @return Symbol - The evaluated result
	 */
	private def evaluateWhileStatement(whileStatement: AST.tree.WhileStatement, symbolTable: SymbolTable): Option[Symbol] = {
		while (Evaluate(whileStatement.withCondition(), symbolTable).get.withValue() == 1) {
			Evaluate(whileStatement.withLoop(), symbolTable)
		}
		None
	}
	
	/**
	 * Evaluates a print statement by evaluating each statement in the
	 * <AST.tree.PrintStatement.Values> ListBuffer
	 *
	 * @param printStatement - The print statement to evaluate
	 * @param symbolTable - Table of symbols present in the program
	 * @return Symbol - The evaluated result
	 */
	private def evaluatePrintStatement(printStatement: AST.tree.PrintStatement, symbolTable: SymbolTable): Option[Symbol] = {
		for (expression <- printStatement.withValues()) {
			print(Evaluate(expression, symbolTable).get.withValue() + " ")
		}
		None
	}
	
	/**
	 * Evaluates an infix expression by looking at the <left> and <right>
	 * sides and then matching the operator to return the correct
	 * calculation
	 *
	 * @param operator - The operator for the infix expression
	 * @param left - The left side of the infix expression
	 * @param right - The right side of the infix expression
	 * @param symbolTable - Table of symbols present in the program
	 * @return Symbol - The evaluated result
	 */
	private def evaluateInfix(operator: String, left: Option[Symbol], right: Option[Symbol], symbolTable: SymbolTable): Option[Symbol] = {
		val leftValue = left.get.withValue()
		val rightValue = right.get.withValue()
		
		operator match {
			case "+" =>
				Option(new Symbol(leftValue + rightValue))
			case "-" =>
				Option(new Symbol(leftValue - rightValue))
			case "*" =>
				Option(new Symbol(leftValue * rightValue))
			case "/" =>
				Option(new Symbol(leftValue / rightValue))
			case "%" =>
				Option(new Symbol(leftValue % rightValue))
			case "<" =>
				val result = evaluateToBooleanInteger(leftValue < rightValue)
				Option(new Symbol(result))
			case ">" =>
				val result = evaluateToBooleanInteger(leftValue > rightValue)
				Option(new Symbol(result))
			case "<=" =>
				val result = evaluateToBooleanInteger(leftValue <= rightValue)
				Option(new Symbol(result))
			case ">=" =>
				val result = evaluateToBooleanInteger(leftValue >= rightValue)
				Option(new Symbol(result))
			case "==" =>
				val result = evaluateToBooleanInteger(leftValue == rightValue)
				Option(new Symbol(result))
			case "!=" =>
				val result = evaluateToBooleanInteger(leftValue != rightValue)
				Option(new Symbol(result))
			case "&&" =>
				evaluateBooleanInfix(operator, leftValue, rightValue)
			case "||" =>
				evaluateBooleanInfix(operator, leftValue, rightValue)
		}
	}
	
	/**
	 * Evaluates an <identifier> to its corresponding value. Throws an
	 * exception if the <identifier> can be found in the <symbolTable>
	 *
	 * @param identifier - The identifier to evaluate
	 * @param symbolTable - Table of symbols present in the program
	 * @return Symbol - The evaluated result
	 */
	private def evaluateIdentifier(identifier: String, symbolTable: SymbolTable): Option[Symbol] = {
		val result = symbolTable.withValue(identifier)
		if (result == null) {
			throw new Exception(s"Unknown identifier: $identifier")
		}
		result
	}
	
	/**
	 * Evaluates the result of an infix expression containing the
	 * boolean comparators && or ||
	 *
	 * @param operator - The operator for the infix expression
	 * @param left - The left side of the infix expression
	 * @param right - The right side of the infix expression
	 * @return Symbol - The evaluated result
	 */
	private def evaluateBooleanInfix(operator: String, left: Int, right: Int): Option[Symbol] = {
		val leftBoolValue = setBooleanValue(left)
		val rightBoolValue = setBooleanValue(right)
		
		operator match {
			case "&&" =>
				val result = leftBoolValue && rightBoolValue
				if (result)
					return Option(new Symbol(1))
				Option(new Symbol(0))
			case "||" =>
				val result = leftBoolValue || rightBoolValue
				if (result)
					return Option(new Symbol(1))
				Option(new Symbol(0))
			case _ =>
				None
		}
	}
	
	/**
	 * Inspects a boolean expression and returns either 1 or
	 * 0 based on whether or not it is true or false
	 *
	 * @param expression - The boolean expression to inspect
	 * @return
	 */
	private def evaluateToBooleanInteger(expression: Boolean): Int = {
		if (expression)
			return 1
		0
	}
	
	/**
	 * Inspects the <value> and returns true if it is 1 or
	 * false if it is 0. C-- boolean values are represented
	 * by integers so in order to carry out boolean logic
	 * such as AND or OR, the value must be converted to a
	 * boolean literal for the Scala compiler to understand
	 * it
	 *
	 * @param value - The value to convert to Boolean
	 * @return Boolean
	 */
	private def setBooleanValue(value: Int): Boolean = {
		if (value == 1)
			return true
		false
	}
}

object Evaluator {
	def apply(): Evaluator = new Evaluator()
}
