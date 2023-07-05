interface Node {
    fun tokenLiteral(): String
}
interface Statement: Node {
    val token: Token
    override fun tokenLiteral(): String = token.literal
}
interface Expression: Node {
    val token: Token
    override fun tokenLiteral(): String = token.literal
}

class Program(val statements: ArrayList<Statement>): Node {
    override fun tokenLiteral() = if (statements.isNotEmpty()) statements[0].tokenLiteral() else ""
}

class Identifier(override val token: Token, val value: String): Expression
class LetStatement(override val token: Token, val value: Expression?, val name: Identifier?): Statement

class ReturnStatement(override val token: Token, returnValue: Expression?): Statement
