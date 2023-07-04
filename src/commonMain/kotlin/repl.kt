
const val PROMPT = ">>"
fun repl() {
    println("""
        Hello mrnugget! This is the Monkey programming language!
        Feel free to type in commands
    """.trimIndent())
    while (true) {
        print(PROMPT)
        val inp = readlnOrNull() ?: return
        val lexer = Lexer(inp)

        for (tok in lexer) {
            println(tok)
        }
    }
}