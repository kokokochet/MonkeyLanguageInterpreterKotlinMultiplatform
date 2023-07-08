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
            TokenType.LPAREN -> Precedence.CALL
            else -> Precedence.LOWEST
        }
    }

    private fun curPrecedence() = getPrecedence(curToken.type)
    private fun peekPrecedence() = getPrecedence(peekToken.type)

    private fun infixParsers(t: TokenType): InfixParse? = when (t) {
        TokenType.PLUS, TokenType.MINUS,
        TokenType.SLASH, TokenType.ASTERISK,
        TokenType.EQ, TokenType.NOT_EQ,
        TokenType.LT, TokenType.GT -> ::parseInfixExpression

        TokenType.LPAREN -> ::parseCallExpression
        else -> null
    }

    private fun parseCallExpression(function: Expression): Expression {
        val tok = curToken
        val args = parseCallArguments()
        return CallExpression(tok, function, args)
    }

    private fun parseCallArguments(): List<Expression> {
        val args = ArrayList<Expression>()
        if (peekTokenIs(TokenType.RPAREN)) {
            nextToken()
            return args
        }
        nextToken()
        args.add(
            parseExpression(Precedence.LOWEST) ?: throw Exception("Function call parsing error")
        )
        while (peekTokenIs(TokenType.COMMA)) {
            nextToken()
            nextToken()
            args.add(
                parseExpression(Precedence.LOWEST) ?: throw Exception("Function call parsing error")
            )
        }

        if (!expectPeek(TokenType.RPAREN)) throw Exception("The call expected a closing brace")

        return args
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
        TokenType.IF -> ::parseIfExpression
        TokenType.FUNCTION -> ::parseFunctionLiteral
        else -> throw Exception("no prefix parse function for '${t.literal}' found")
    }

    private fun parseFunctionLiteral(): Expression {
        if (!expectPeek(TokenType.LPAREN)) throw Exception("open brace expected after fn")

        val params = parseFunctionParameters()

        if (!expectPeek(TokenType.LBRACE)) throw Exception("expected closing brace after fn parameters")

        val body = parseBlockStatement()

        return FunctionLiteral(params, body)
    }

    private fun parseFunctionParameters(): List<Identifier> {
        val identifiers = ArrayList<Identifier>()
        if (peekTokenIs(TokenType.RPAREN)) {
            nextToken()
            return identifiers
        }
        nextToken()

        identifiers.add(
            Identifier(curToken, curToken.literal)
        )

        while (peekTokenIs(TokenType.COMMA)) {
            nextToken()
            nextToken()
            identifiers.add(Identifier(curToken, curToken.literal))
        }

        if (!expectPeek(TokenType.RPAREN)) peekError(TokenType.RPAREN)

        return identifiers
    }

    private fun parseIfExpression(): Expression {
        if (!expectPeek(TokenType.LPAREN)) peekError(TokenType.LPAREN)

        nextToken()
        val condition = parseExpression(Precedence.LOWEST) ?: (throw Exception("failed to parse if condition"))

        if (!expectPeek(TokenType.RPAREN)) peekError(TokenType.RPAREN)
        if (!expectPeek(TokenType.LBRACE)) peekError(TokenType.LBRACE)

        val consequence = parseBlockStatement()
        var alternative: BlockStatement? = null

        if (peekTokenIs(TokenType.ELSE)) {
            nextToken()
            if (!expectPeek(TokenType.LBRACE)) peekError(TokenType.LBRACE)

            alternative = parseBlockStatement()
        }

        return IfExpression(condition, consequence, alternative)
    }

    private fun parseBlockStatement(): BlockStatement {
        val tok = curToken
        val statements = ArrayList<Statement>()
        nextToken()
        while (!curTokenIs(TokenType.RBRACE) && !curTokenIs(TokenType.EOF)) {
            statements.add(parseStatement())
            nextToken()
        }
        return BlockStatement(tok, statements)
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
            program.statements.add(parseStatement())
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
        val tok = curToken
        nextToken()
        val returnStatement = parseExpression(Precedence.LOWEST)
        if (peekTokenIs(TokenType.SEMICOLON)) nextToken()
        return ReturnStatement(tok, returnStatement)
    }

    private fun parseLetStatement(): LetStatement {
        val letToken = curToken

        if (!expectPeek(TokenType.IDENT)) peekError(TokenType.ASSIGN)

        val ident = Identifier(curToken, curToken.literal)

        if (!expectPeek(TokenType.ASSIGN)) peekError(TokenType.ASSIGN)
        nextToken()

        val value = parseExpression(Precedence.LOWEST)

        if (peekTokenIs(TokenType.SEMICOLON)) nextToken()

        return LetStatement(letToken, ident, value)
    }

    private fun curTokenIs(t: TokenType) = curToken.type == t
    private fun peekTokenIs(t: TokenType) = peekToken.type == t

    private fun peekError(t: TokenType): Nothing {
        throw Exception("expected next token to be ${t.name}, got ${peekToken.literal} instead")
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