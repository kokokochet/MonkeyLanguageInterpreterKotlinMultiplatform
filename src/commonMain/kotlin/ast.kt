interface Node {
    fun tokenLiteral(): String
}

interface Statement: Node {
    val token: Token
    override fun tokenLiteral(): String = token.literal
}

typealias Expression = Statement

class Program(val statements: ArrayList<Statement>): Node {
    override fun tokenLiteral() = if (statements.isNotEmpty()) statements[0].tokenLiteral() else ""

    override fun toString(): String {
        return statements.joinToString(separator = "") { it.toString() }
    }
}

class Identifier(override val token: Token, val value: String): Expression {
    override fun toString(): String {
        return value
    }
}

class LetStatement(override val token: Token, val name: Identifier, val value: Expression?): Statement {
    override fun toString(): String {
        return "${token.literal} $name = $value;"
    }
}

class ReturnStatement(override val token: Token, val returnValue: Expression?): Statement {
    override fun toString(): String {
        return "${token.literal} ${returnValue?.toString()?:""};"
    }
}

class ExpressionStatement(override val token: Token, val expression: Expression?): Statement {
    override fun toString(): String {
        return expression?.toString() ?: ""
    }
}

class IntegerLiteral(override val token: Token, val value: Long): Expression {
    override fun toString(): String {
        return "$value"
    }
}
class PrefixExpression(override val token: Token, val operator: String, val right: Expression?): Expression {
    override fun toString(): String {
        return "($operator$right)"
    }
}

class InfixExpression(
    override val token: Token,
    val left: Expression?,
    val operator: String,
    val right: Expression?
): Expression {
    override fun toString(): String {
        return "($left $operator $right)"
    }
}

class BooleanLiteral(override val token: Token, val value: Boolean): Expression {
    override fun toString(): String {
        return value.toString()
    }
}

class BlockStatement(override val token: Token, val statements: List<Statement>): Statement {
    override fun toString(): String {
        return "{\n ${statements.joinToString()} \n}"
    }
}

class IfExpression(
    val condition: Expression,
    val consequence: BlockStatement,
    val alternative: BlockStatement?
): Expression {
    override val token: Token = Token(TokenType.IF, "if")
    override fun toString(): String {
        return "if $condition $consequence ${if (alternative == null) "" else " else $alternative"}"
    }
}

class FunctionLiteral(
    val parameters: List<Identifier>,
    val body: BlockStatement
): Expression {
    override val token: Token = Token(TokenType.FUNCTION, "fn")
    override fun toString(): String {
        return "${tokenLiteral()}(${parameters.joinToString("")}) $body"
    }
}

class CallExpression(override val token: Token, val func: Expression, val args: List<Expression>): Expression {
    override fun toString(): String {
        return "$func(${args.joinToString(", ")})"
    }
}