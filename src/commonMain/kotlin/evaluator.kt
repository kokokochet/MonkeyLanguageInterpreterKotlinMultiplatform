fun eval(node: Node?): Object {
    return when (node) {
        null -> Null()
        is Program -> evalStatements(node.statements)
        is ExpressionStatement -> eval(node.expression)
        is IntegerLiteral -> Integer(node.value)
        else -> Null()
    }
}

fun evalStatements(statements: List<Statement>): Object {
    var result: Object? = null
    for (stmt in statements) {
        result = eval(stmt)
    }
    return result!!
}