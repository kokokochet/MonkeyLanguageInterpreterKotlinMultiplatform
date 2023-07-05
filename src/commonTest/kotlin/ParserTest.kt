import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue
import kotlin.test.fail

class ParserTest {

    @Test
    fun testOperatorPrecedenceParsing() {
        val tests = listOf(
            "-a * b" to "((-a) * b)",
            "!-a" to "(!(-a))",
            "a + b + c" to "((a + b) + c)",
            "a + b - c" to "((a + b) - c)",
            "a * b * c" to "((a * b) * c)",
            "a * b / c" to "((a * b) / c)",
            "a + b / c" to "(a + (b / c))",
            "a + b * c + d / e - f" to "(((a + (b * c)) + (d / e)) - f)",
            "3 + 4; -5 * 5" to "(3 + 4)((-5) * 5)",
            "5 > 4 == 3 < 4" to "((5 > 4) == (3 < 4))",
            "5 < 4 != 3 > 4" to "((5 < 4) != (3 > 4))",
            "3 + 4 * 5 == 3 * 1 + 4 * 5" to "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            "3 + 4 * 5 == 3 * 1 + 4 * 5" to "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"
        )

        for ((input, expected) in tests) {
            val program = Parser(Lexer(input)).parseProgram()
            assertEquals(expected, program.toString())
        }
    }

    @Test
    fun testParsingInfixExpressions() {
        data class InfixTest(
            val input: String,
            val leftValue: Long,
            val operator: String,
            val rightValue: Long
        )
        val infixTests = listOf(
            InfixTest("5 + 5", 5, "+", 5),
            InfixTest("5 - 5;", 5, "-", 5),
            InfixTest("5 * 5;", 5, "*", 5),
            InfixTest("5 / 5;", 5, "/", 5),
            InfixTest("5 > 5;", 5, ">", 5),
            InfixTest("5 < 5;", 5, "<", 5),
            InfixTest("5 == 5;", 5, "==", 5),
            InfixTest("5 != 5;", 5, "!=", 5)
        )

        for (test in infixTests) {
            val program = Parser(Lexer(test.input)).parseProgram()
            assertEquals(1, program.statements.size)
            val statement = program.statements[0]
            assertTrue(statement is ExpressionStatement, "statement is not ExpressionStatement. Got $statement")
            val exp = statement.expression
            assertTrue(exp is InfixExpression, "exp is not InfixExpression. Got $exp")
            testIntegerLiteral(exp.left, test.leftValue)
            assertEquals(test.operator, exp.operator)
            testIntegerLiteral(exp.right, test.rightValue)
        }
    }

    @Test
    fun testIdentifierExpression() {
        val input = "foobar;"
        val parser = Parser(Lexer(input))
        val program = parser.parseProgram()
        assertEquals(1, program.statements.size)

        val statement = program.statements[0]
        if (statement !is ExpressionStatement) {
            fail("program.statements[0] is not ExpressionStatement. Got $statement")
        }

        val ident = statement.expression
        if (ident !is Identifier) {
            fail("exp not Identifier. Got $ident")
        }
        assertEquals("foobar", ident.value)
        assertEquals("foobar", ident.tokenLiteral())
    }

    @Test
    fun testIntegerLiteralExpression() {
        val input = "5;"
        val program = Parser(Lexer(input)).parseProgram()
        assertEquals(1, program.statements.size)

        val statement = program.statements[0]
        assertTrue(statement is ExpressionStatement)

        val literal = statement.expression
        assertTrue(literal is IntegerLiteral)
        assertEquals(5, literal.value)
        assertEquals("5", literal.tokenLiteral())
    }

    @Test
    fun testParsingPrefixExpressions() {
        val prefixTest = listOf(
            Triple("!5", "!", 5L),
            Triple("-15", "-", 15L)
        )
        for ((input, operator, intValue) in prefixTest) {
            val program = Parser(Lexer(input)).parseProgram()
            assertEquals(1, program.statements.size)
            val statement = program.statements[0]
            assertTrue(
                statement is ExpressionStatement,
                "program.statements[0] is not ExpressionStatement. got=$statement"
            )
            val expression = statement.expression
            assertTrue(
                expression is PrefixExpression,
                "expression is not PrefixExpression. got=$expression"
            )
            assertEquals(operator, expression.operator)
            testIntegerLiteral(expression.right!!, intValue)
        }
    }

    fun testIntegerLiteral(expression: Expression?, value: Long): Boolean {
        assertTrue(expression is IntegerLiteral, "expression not IntegerLiteral, got = $expression")
        assertEquals(value, expression.value)
        assertEquals(expression.tokenLiteral(), value.toString())
        return true
    }

//    @Test
//    fun testLetStatements() {
//        val input = """
//            let =;
//            let =;
//            let =;
//        """.trimIndent()
//
//        val parser = Parser(Lexer(input))
//        val program = parser.parseProgram()
//
//        if (program.statements.size != 3) {
//            fail("program.Statements does not contain 3 statements. got=${program.statements.size}")
//        }
//
//        val tests = listOf("x", "y", "foobar")
//
//        for ((ind, identTest) in tests.withIndex()) {
//            val stmt = program.statements[ind]
//            testLetStatement(stmt, identTest)
//        }
//
//    }

    fun testLetStatement(s: Statement, name: String): Boolean {
        if (s.tokenLiteral() != "let") {
            fail("s.tokenLiteral() not 'let'. Got ${s.tokenLiteral()}")
        }
        if (s !is LetStatement) fail("s not LetStatement. got=${s.token.type}")
        assertEquals(s.name.value, name)
        assertEquals(s.name.tokenLiteral(), name)
        return true
    }

    @Test
    fun testReturnStatements() {
        val input = """
        return 5;
        return 10;
        return 993322;
        """.trimIndent()
        val parser = Parser(Lexer(input))
        val program = parser.parseProgram()
        assertEquals(3, program.statements.size)
        for (stmt in program.statements) {
            if (stmt !is ReturnStatement) fail("stmt not ReturnStatement. got=${stmt.token}")
            assertEquals("return", stmt.tokenLiteral())
        }
    }
}