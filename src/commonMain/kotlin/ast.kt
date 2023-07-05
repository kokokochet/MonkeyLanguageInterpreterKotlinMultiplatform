interface Node {
    fun tokenLiteral(): String
}

interface Statement: Node

interface Expression: Node
class Program(val statements: ArrayList<Statement>): Node {
    override fun tokenLiteral() = if (statements.isNotEmpty()) statements[0].tokenLiteral() else ""
}

class Identifier(val token: Token, val value: String): Expression {
    override fun tokenLiteral() = token.literal
}
class LetStatement(val token: Token, val value: Expression?, val name: Identifier?): Statement {
    override fun tokenLiteral() = token.literal

}