object Evaluator {

    val TRUE = BooleanObj(true)
    val FALSE = BooleanObj(false)

    fun eval(node: Node?): Object {
        return when (node) {
            null -> Null
            is Program -> evalStatements(node.statements)
            is ExpressionStatement -> eval(node.expression)
            is IntegerLiteral -> Integer(node.value)
            is BooleanLiteral -> nativeBoolToBooleanObject(node.value)
            else -> Null
        }
    }

    private fun nativeBoolToBooleanObject(input: Boolean): BooleanObj {
        return if(input) TRUE else FALSE
    }

    private fun evalStatements(statements: List<Statement>): Object {
        var result: Object? = null
        for (stmt in statements) {
            result = eval(stmt)
        }
        return result!!
    }
}

