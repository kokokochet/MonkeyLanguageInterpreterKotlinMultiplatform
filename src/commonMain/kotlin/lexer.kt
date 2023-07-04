class Lexer(private val input: String): Iterator<Token> {
    private var position: Int = 0
    private var readPosition: Int = 0
    private var isFinished = false

    override fun hasNext() = !isFinished

    override fun next(): Token {
        return when (val ch = skipWhitespace(readChar())) {
            '=' -> if (peekChar() == '=') {
                readChar()
                Token(TokenType.EQ, "==")
            } else {
                Token(TokenType.ASSIGN, "$ch")
            }
            '!' -> if (peekChar() == '=') {
                readChar()
                Token(TokenType.NOT_EQ, "!=")
            } else {
                Token(TokenType.BANG, "$ch")
            }
            '+' -> Token(TokenType.PLUS, "$ch")
            '-' -> Token(TokenType.MINUS, "$ch")
            '/' -> Token(TokenType.SLASH, "$ch")
            '*' -> Token(TokenType.ASTERISK, "$ch")
            '<' -> Token(TokenType.LT, "$ch")
            '>' -> Token(TokenType.GT, "$ch")
            ';' -> Token(TokenType.SEMICOLON, "$ch")
            ',' -> Token(TokenType.COMMA, "$ch")
            '(' -> Token(TokenType.LPAREN, "$ch")
            ')' -> Token(TokenType.RPAREN, "$ch")
            '{' -> Token(TokenType.LBRACE, "$ch")
            '}' -> Token(TokenType.RBRACE, "$ch")
            in 'a'..'z', in 'A'..'Z', '_' -> {
                Token(TokenType.IDENT, readIdentifier(ch))
            }
            in '0'..'9' -> Token(TokenType.INT, readNumber(ch))
            0.toChar() -> {
                isFinished = true
                Token(TokenType.EOF, "")
            }
            else -> {
                isFinished = true
                Token(TokenType.ILLEGAL, "$ch")
            }
        }
    }

    private fun peekChar() = if (readPosition >= input.length) 0.toChar() else input[readPosition]

    private fun readChar(): Char {
        val ch = if (readPosition >= input.length) {
            0.toChar()
        } else {
            input[readPosition]
        }
        position = readPosition
        readPosition++
        return ch
    }

    private fun stepBack() {
        position--
        readPosition--
    }

    private fun readNumber(startChar: Char): String {
        val startPos = position
        var ch = startChar
        while (ch.isDigit()) {
            ch = readChar()
        }
        stepBack()
        return input.substring(startPos..position)
    }

    private tailrec fun skipWhitespace(ch: Char): Char {
        return if (!(ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r')) ch else skipWhitespace(readChar())
    }

    private fun readIdentifier(startChar: Char): String {
        val startPos = position
        var ch = startChar
        while (ch in 'a'..'z' || ch in 'A'..'Z' || ch == '_') {
            ch = readChar()
        }
        stepBack()
        return input.substring(startPos..position)
    }

}