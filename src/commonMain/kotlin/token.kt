enum class TokenType {
    // first test from book
    ASSIGN,
    PLUS,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    COMMA,
    SEMICOLON,
    EOF,
    // second test from book
    LET,
    INT,
    FUNCTION,
    IDENT,
    ILLEGAL,
    // 1.4 - Extending our Token Set and Lexer
    BANG,
    MINUS,
    SLASH,
    ASTERISK,
    LT,
    GT,
    IF,
    RETURN,
    TRUE,
    ELSE,
    FALSE,
    EQ,
    NOT_EQ
}

class Token(type: TokenType, val literal: String) {
    val type: TokenType

    init {
        if (type == TokenType.IDENT) {
            this.type = when (literal) {
                "fn" -> TokenType.FUNCTION
                "let" -> TokenType.LET
                "true" -> TokenType.TRUE
                "false" -> TokenType.FALSE
                "if" -> TokenType.IF
                "else" -> TokenType.ELSE
                "return" -> TokenType.RETURN
                else -> type
            }
        } else {
            this.type = type
        }
    }

    override fun toString(): String {
        return "Token(literal='$literal', type=$type)"
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other == null || this::class != other::class) return false

        other as Token

        if (literal != other.literal) return false
        return type == other.type
    }

    override fun hashCode(): Int {
        var result = literal.hashCode()
        result = 31 * result + type.hashCode()
        return result
    }
}