class Parser(private val lexer: Lexer) {
    private var curToken: Token = lexer.next()
    private var peekToken: Token = lexer.next()

    private fun nextToken() {
        curToken = peekToken
        peekToken = lexer.next()
    }

    fun parseProgram(): Program {
        val program = Program(ArrayList())

        while (curToken.type != TokenType.EOF) {
            parseStatement()?.let {
                program.statements.add(it)
            }
            nextToken()
        }

        return program
    }

    private fun parseStatement(): Statement? {
        return when (curToken.type) {
            TokenType.LET -> parseLetStatement()
            TokenType.RETURN -> parseReturnStatement()
            else -> null
        }
    }

    private fun parseReturnStatement(): Statement {
        val statement = ReturnStatement(curToken, null)
        nextToken()
        while (!curTokenIs(TokenType.SEMICOLON)) nextToken()
        return statement
    }

    private fun parseLetStatement(): LetStatement {
        val letToken = curToken
        if (!expectPeek(TokenType.IDENT)) peekError(TokenType.ASSIGN)

        val statement = LetStatement(
            letToken,
            null,
            Identifier(curToken, curToken.literal)
        )

        if (!expectPeek(TokenType.ASSIGN)) peekError(TokenType.ASSIGN)

        // TODO: We're skipping the expressions until we encounter a semicolon
        while (curTokenIs(TokenType.ASSIGN)) nextToken()

        return statement
    }

    private fun curTokenIs(t: TokenType) = curToken.type == t
    private fun peekTokenIs(t: TokenType) = peekToken.type == t

    private fun peekError(t: TokenType): Nothing {
        throw Exception("expected next token to be ${t.name}, got ${peekToken.type.name} instead")
    }

    private fun expectPeek(t: TokenType): Boolean {
        return if (peekTokenIs(t)) {
            nextToken()
            true
        } else {
            false
        }
    }

}