typealias PrefixParse = () -> Expression?
typealias InfixParse = (Expression?) -> Expression?

class Parser(private val lexer: Lexer) {
    private var curToken: Token = lexer.next()
    private var peekToken: Token = lexer.next()

    enum class Precedence {
        LOWEST, EQUALS, LESS_GREATER, SUM, PRODUCT, PREFIX, CALL
        //      ==      < or >        +    *        -X|+X   myFunction(x)
    }

    private fun prefixParsers(t: TokenType): PrefixParse = when (t) {
        TokenType.IDENT -> {
            { Identifier(curToken, curToken.literal) }
        }
        TokenType.INT -> {
            {
                val literal = curToken
                val value = literal.literal.toLong()
                IntegerLiteral(literal, value)
            }
        }
        TokenType.BANG -> ::parsePrefixExpression
        TokenType.MINUS -> ::parsePrefixExpression
        else -> throw Exception("no prefix parse function for $t found")
    }

    private fun parsePrefixExpression(): Expression {
        val tok = curToken
        nextToken()
        val right = parseExpression(Precedence.PREFIX)
        return PrefixExpression(tok, tok.literal, right)
    }

    private fun nextToken() {
        curToken = peekToken
        peekToken = lexer.next()
    }

    fun parseProgram(): Program {
        val program = Program(ArrayList())

        while (curToken.type != TokenType.EOF) {
            parseStatement().let {
                program.statements.add(it)
            }
            nextToken()
        }

        return program
    }

    private fun parseStatement(): Statement {
        return when (curToken.type) {
            TokenType.LET -> parseLetStatement()
            TokenType.RETURN -> parseReturnStatement()
            else -> parseExpressionStatement()
        }
    }

    private fun parseExpressionStatement(): Statement {
        val tok = curToken
        val statement = ExpressionStatement(
            tok,
            parseExpression(Precedence.LOWEST)
        )
        if (peekTokenIs(TokenType.SEMICOLON)) {
            nextToken()
        }
        return statement
    }

    private fun parseExpression(p: Precedence): Expression? {
        val prefix = prefixParsers(curToken.type)
        return prefix()
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
            Identifier(curToken, curToken.literal),
            null
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