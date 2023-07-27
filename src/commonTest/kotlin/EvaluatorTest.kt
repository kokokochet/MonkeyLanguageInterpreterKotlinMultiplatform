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

    @Test
    fun testEvalBooleanExpression() {
        val tests = listOf(
            "true" to true,
            "false" to false
        )
        for ((inp, value) in tests) {
            testBoooleanObject(
                testEval(inp),
                value
            )
        }
    }

    private fun testBoooleanObject(obj: Object, expected: Boolean) {
        assertTrue(obj is BooleanObj)
        assertEquals(expected, obj.value)
    }

    private fun testEval(inp: String): Object {
        val p = Parser(Lexer(inp)).parseProgram()
        return Evaluator.eval(p)
    }

    private fun testIntegerObject(obj: Object, exp: Long) {
        assertTrue(obj is Integer)
        assertEquals(exp, obj.value)
    }


}