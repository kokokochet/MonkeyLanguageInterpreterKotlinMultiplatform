import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue
import kotlin.test.fail

private class ParserTest {

    @Test
    fun testOperatorPrecedenceParsing() {
        val tests = listOf(
            "1 + 2 + 3" to "((1 + 2) + 3)",
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

    inline fun <reified T>assertLiteralExpression(expression: Expression?, expected: T) {
        when (expected) {
            is Long -> testIntegerLiteral(expression, expected)
            is Int -> testIntegerLiteral(expression, expected.toLong())
            is String -> testIdentifier(expression, expected)
            else -> fail("type of value not handled. got=$expected")
        }
    }

    inline fun <reified LT, reified RT>testInfixExpression(
        value: String,
        left: LT,
        operator: String,
        right: RT
    ) {
        val program = Parser(Lexer(value)).parseProgram()
        assertEquals(1, program.statements.size)
        val exp = program.statements[0]
        assertTrue(
            exp is ExpressionStatement,
            "exp is not ExpressionStatement. got=$exp"
        )
        assertInfixExpression(exp.expression, left, operator, right)
    }

    inline fun <reified LT, reified RT>assertInfixExpression(
        expression: Expression?,
        left: LT,
        operator: String,
        right: RT
    ) {
        assertTrue(
            expression is InfixExpression,
            "expression is not InfixExpression. got=$expression"
        )
        assertLiteralExpression(expression.left, left)
        assertEquals(expression.operator, operator)
        assertLiteralExpression(expression.right, right)
    }

    fun testIdentifier(exp: Expression?, value: String) {
        assertTrue(exp is Identifier, "exp is not Identifier. Got = $exp")
        assertEquals(value, exp.value)
        assertEquals(value, exp.tokenLiteral())
    }

    @Test
    fun testParsingInfixExpressions() {
        testInfixExpression("5 + 5", 5, "+", 5)
        testInfixExpression("5 - 5;", 5, "-", 5)
        testInfixExpression("5 * 5;", 5, "*", 5)
        testInfixExpression("5 / 5;", 5, "/", 5)
        testInfixExpression("5 > 5;", 5, ">", 5)
        testInfixExpression("5 < 5;", 5, "<", 5)
        testInfixExpression("5 == 5;", 5, "==", 5)
        testInfixExpression("5 != 5;", 5, "!=", 5)
        testInfixExpression("foobar + barfoo;", "foobar", "+", "barfoo")
        testInfixExpression("foobar - barfoo;", "foobar", "-", "barfoo")
        testInfixExpression("foobar * barfoo;", "foobar", "*", "barfoo")
        testInfixExpression("foobar / barfoo;", "foobar", "/", "barfoo")
        testInfixExpression("foobar > barfoo;", "foobar", ">", "barfoo")
        testInfixExpression("foobar < barfoo;", "foobar", "<", "barfoo")
        testInfixExpression("foobar == barfoo;", "foobar", "==", "barfoo")
        testInfixExpression("foobar != barfoo;", "foobar", "!=", "barfoo")
//        testInfixExpression("true == true", true, "==", true)
//        testInfixExpression("true != false", true, "!=", false)
//        testInfixExpression("false == false", false, "==", false)
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

    fun testLetStatement(s: Statement, name: String) {
        if (s.tokenLiteral() != "let") {
            fail("s.tokenLiteral() not 'let'. Got ${s.tokenLiteral()}")
        }
        if (s !is LetStatement) fail("s not LetStatement. got=${s.token.type}")
        assertEquals(s.name.value, name)
        assertEquals(s.name.tokenLiteral(), name)
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