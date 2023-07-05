typealias PrefixParse = () -> Expression?
typealias InfixParse = (Expression) -> Expression?

class Parser(private val lexer: Lexer) {
    private var curToken: Token = lexer.next()
    private var peekToken: Token = lexer.next()

    enum class Precedence {
        LOWEST, EQUALS, LESS_GREATER, SUM, PRODUCT, PREFIX, CALL
        //      ==      < or >        +    *        -X|+X   myFunction(x)
    }

    private fun getPrecedence(tokType: TokenType): Precedence {
        return when (tokType) {
            TokenType.EQ -> Precedence.EQUALS
            TokenType.NOT_EQ -> Precedence.EQUALS
            TokenType.LT -> Precedence.LESS_GREATER
            TokenType.GT -> Precedence.LESS_GREATER
            TokenType.PLUS -> Precedence.SUM
            TokenType.MINUS -> Precedence.SUM
            TokenType.SLASH -> Precedence.PRODUCT
            TokenType.ASTERISK -> Precedence.PRODUCT
            else -> Precedence.LOWEST
        }
    }

    fun curPrecedence() = getPrecedence(curToken.type)
    fun peekPrecedence() = getPrecedence(peekToken.type)

    private fun infixParsers(t: TokenType): InfixParse? = when (t) {
        TokenType.PLUS, TokenType.MINUS,
        TokenType.SLASH, TokenType.ASTERISK,
        TokenType.EQ, TokenType.NOT_EQ,
        TokenType.LT, TokenType.GT -> ::parseInfixExpression

        else -> null
    }

    private fun parseInfixExpression(left: Expression): Expression {
        val token = curToken
        val operator = curToken.literal
        val precedence = curPrecedence()
        nextToken()
        return InfixExpression(
            token,
            left,
            operator,
            right = parseExpression(precedence)
        )
    }

    private fun prefixParsers(t: Token): PrefixParse = when (t.type) {
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
        TokenType.TRUE -> ::parseBoolean
        TokenType.FALSE -> ::parseBoolean
        TokenType.LPAREN -> ::parseGroupedExpression
        else -> throw Exception("no prefix parse function for '${t.literal}' found")
    }

    private fun parseGroupedExpression(): Expression? {
        nextToken()
        val expression = parseExpression(Precedence.LOWEST)
        if (!expectPeek(TokenType.RPAREN)) {
            return null
        }
        return expression
    }

    private fun parseBoolean(): Expression {
        return BooleanLiteral(curToken, curToken.type == TokenType.TRUE)
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
        val prefix = prefixParsers(curToken)
        var leftExp = prefix()
        while (!peekTokenIs(TokenType.SEMICOLON) && p < peekPrecedence()) {
            val infix = infixParsers(peekToken.type) ?: return leftExp
            nextToken()
            leftExp = infix(leftExp!!)
        }
        return leftExp
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