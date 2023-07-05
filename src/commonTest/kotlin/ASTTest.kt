import kotlin.test.Test
import kotlin.test.assertEquals

class ASTTest {
    @Test
    fun testString() {
        val program = Program(
            statements = arrayListOf(
                LetStatement(
                    token = Token(TokenType.LET, literal = "let"),
                    name = Identifier(
                        Token(TokenType.IDENT, "myVar"),
                        "myVar"
                    ),
                    value = Identifier(
                        Token(TokenType.IDENT,"anotherVar"),
                        "anotherVar",
                    )
                )
            )
        )

        assertEquals("let myVar = anotherVar;", program.toString())
    }
}