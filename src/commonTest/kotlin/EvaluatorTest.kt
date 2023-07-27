import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class EvaluatorTest {
    @Test
    fun testEvalIntegerExpression() {
        val tests = listOf(
            "5" to 5L,
            "10" to 10L
        )

        for ((inp, value) in tests) {
            testIntegerObject(
                testEval(inp),
                value
            )
        }
    }

    private fun testEval(inp: String): Object {
        val p = Parser(Lexer(inp)).parseProgram()
        return eval(p)
    }

    private fun testIntegerObject(obj: Object, exp: Long) {
        assertTrue(obj is Integer)
        assertEquals(exp, obj.value)
    }
}