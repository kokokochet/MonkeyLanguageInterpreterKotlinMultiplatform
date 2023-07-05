import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.fail

class ParserTest{

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
    fun testLetStatements() {
        val input = """
            let x = 5;
            let y = 10;
            let foobar = 838383;
        """.trimIndent()

        val parser = Parser(Lexer(input))
        val program = parser.parseProgram()

        if (program.statements.size != 3) {
            fail("program.Statements does not contain 3 statements. got=${program.statements.size}")
        }

        val tests = listOf("x", "y", "foobar")

        for ((ind, identTest) in tests.withIndex()) {
            val stmt = program.statements[ind]
            testLetStatement(stmt, identTest)
        }

    }

    fun testLetStatement(s: Statement, name: String): Boolean {
        if (s.tokenLiteral() != "let") {
            fail("s.tokenLiteral() not 'let'. Got ${s.tokenLiteral()}")
        }
        if (s !is LetStatement) fail("s not LetStatement. got=${s.token.type}")
        assertEquals(s.name!!.value, name)
        assertEquals(s.name!!.tokenLiteral(), name)
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
            assertEquals("return" , stmt.tokenLiteral())
        }
    }
}