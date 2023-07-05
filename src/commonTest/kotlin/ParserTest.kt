import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.fail

class ParserTest{
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
        if (s !is LetStatement) fail("s not LetStatement. got=${s::class.qualifiedName}")
        assertEquals(s.name!!.value, name)
        assertEquals(s.name!!.tokenLiteral(), name)
        return true
    }
}