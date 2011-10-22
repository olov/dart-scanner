// Copyright (c) 2011, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#library("token.dart");

/**
 * Dart tokens and associated data.
 *
 * Note: Token ordinals matter for some accessors, so don't change the order of these without
 * knowing what you're doing.
 */

class Token {
  static HashMap<String, Token> tokens;

  static Token EOS;

  /* Punctuators. */
  static Token LPAREN;
  static Token RPAREN;
  static Token LBRACK;
  static Token RBRACK;
  static Token LBRACE;
  static Token RBRACE;
  static Token COLON;
  static Token SEMICOLON;
  static Token PERIOD;
  static Token ELLIPSIS;
  static Token COMMA;
  static Token CONDITIONAL;
  static Token ARROW;

  /* Assignment operators. */
  static Token ASSIGN;
  static Token ASSIGN_BIT_OR;
  static Token ASSIGN_BIT_XOR;
  static Token ASSIGN_BIT_AND;
  static Token ASSIGN_SHL;
  static Token ASSIGN_SAR;
  static Token ASSIGN_SHR;
  static Token ASSIGN_ADD;
  static Token ASSIGN_SUB;
  static Token ASSIGN_MUL;
  static Token ASSIGN_DIV;
  static Token ASSIGN_MOD;
  static Token ASSIGN_TRUNC;

  /* Binary operators sorted by precedence. */
  static Token OR;
  static Token AND;
  static Token BIT_OR;
  static Token BIT_XOR;
  static Token BIT_AND;
  static Token SHL;
  static Token SAR;
  static Token SHR;
  static Token ADD;
  static Token SUB;
  static Token MUL;
  static Token DIV;
  static Token TRUNC;
  static Token MOD;

  /* Compare operators sorted by precedence. */
  static Token EQ;
  static Token NE;
  static Token EQ_STRICT;
  static Token NE_STRICT;
  static Token LT;
  static Token GT;
  static Token LTE;
  static Token GTE;
  static Token IS;

  /* Unary operators. */
  static Token NOT;
  static Token BIT_NOT;

  /* Count operators (also unary). */
  static Token INC;
  static Token DEC;

  /* [] operator overloading. */
  static Token INDEX;
  static Token ASSIGN_INDEX;

  /* Keywords. */
  static Token BREAK;
  static Token CASE;
  static Token CATCH;
  static Token CONST;
  static Token CONTINUE;
  static Token DEFAULT;
  static Token DO;
  static Token ELSE;
  static Token FINAL;
  static Token FINALLY;
  static Token FOR;
  static Token IF;
  static Token IN;
  static Token NEW;
  static Token RETURN;
  static Token SUPER;
  static Token SWITCH;
  static Token THIS;
  static Token THROW;
  static Token TRY;
  static Token VAR;
  static Token VOID;
  static Token WHILE;

  /* Literals. */
  static Token NULL_LITERAL;
  static Token TRUE_LITERAL;
  static Token FALSE_LITERAL;
  static Token HEX_LITERAL;
  static Token INTEGER_LITERAL;
  static Token DOUBLE_LITERAL;
  static Token STRING;

  /** String interpolation and string templates. */
  static Token STRING_SEGMENT;
  static Token STRING_LAST_SEGMENT;
  // STRING_EMBED_EXP_START does not have a unique string representation in the code:
  //   "$id" yields the token STRING_EMBED_EXP_START after the '$', and similarly
  //   "${id}" yield the same token for '${'.
  static Token STRING_EMBED_EXP_START;
  static Token STRING_EMBED_EXP_END;

  // Note: STRING_EMBED_EXP_END uses the same symbol as RBRACE, but it is
  // recognized by the scanner when closing embedded expressions in string
  // interpolation and string templates.
  
  /* Directives */
  static Token LIBRARY;
  static Token IMPORT;
  static Token SOURCE;
  static Token RESOURCE;
  static Token NATIVE;

  /* Identifiers (not keywords). */
  static Token IDENTIFIER;
  static Token WHITESPACE;

  /* Pseudo tokens. */
  // If you add another pseudo token, don't forget to update the predicate below.
  static Token ILLEGAL;
  static Token COMMENT;

  /**
   * Non-token to be used by tools where a value outside the range of anything
   * returned by the scanner is needed. This is the equivalent of -1 in a C
   * tokenizer.
   *
   * This token is never returned by the scanner. It must have an ordinal
   * value outside the range of all tokens returned by the scanner.
   */
  static Token NON_TOKEN;




  /**
   * Given a string finds the corresponding token. Pseudo tokens (EOS, ILLEGAL and COMMENT) are
   * ignored.
   */
  static Token lookup(String syntax) {
    Token token = tokens[syntax];
    if (null == token) {
      return IDENTIFIER;
    }
    return token;
  }

  final String syntax_;
  final int precedence_;
  final int ordinal_;
  final String name_;

  static initialize() {
    tokens = new HashMap<String, Token>();
    int ordinal = 0;
    Token T(syntax, precedence, [name]) {
      return new Token(syntax, precedence, ordinal++, name);
    }

    /* End-of-stream. */
    EOS = T(null, 0, "EOS");

    /* Punctuators. */
    LPAREN = T("(", 0);
    RPAREN = T(")", 0);
    LBRACK = T("[", 0);
    RBRACK = T("]", 0);
    LBRACE = T("{", 0);
    RBRACE = T("}", 0);
    COLON = T(":", 0);
    SEMICOLON = T(";", 0);
    PERIOD = T(".", 0);
    ELLIPSIS = T("...", 0);
    COMMA = T(",", 0);
    CONDITIONAL = T("?", 3);
    ARROW = T("=>", 0);

    /* Assignment operators. */
    ASSIGN = T("=", 2);
    ASSIGN_BIT_OR = T("|=", 2);
    ASSIGN_BIT_XOR = T("^=", 2);
    ASSIGN_BIT_AND = T("&=", 2);
    ASSIGN_SHL = T("<<=", 2);
    ASSIGN_SAR = T(">>=", 2);
    ASSIGN_SHR = T(">>>=", 2);
    ASSIGN_ADD = T("+=", 2);
    ASSIGN_SUB = T("-=", 2);
    ASSIGN_MUL = T("*=", 2);
    ASSIGN_DIV = T("/=", 2);
    ASSIGN_MOD = T("%=", 2);
    ASSIGN_TRUNC = T("~/=", 2);

    /* Binary operators sorted by precedence. */
    OR = T("||", 4);
    AND = T("&&", 5);
    BIT_OR = T("|", 6);
    BIT_XOR = T("^", 7);
    BIT_AND = T("&", 8);
    SHL = T("<<", 11);
    SAR = T(">>", 11);
    SHR = T(">>>", 11);
    ADD = T("+", 12);
    SUB = T("-", 12);
    MUL = T("*", 13);
    DIV = T("/", 13);
    TRUNC = T("~/", 13);
    MOD = T("%", 13);

    /* Compare operators sorted by precedence. */
    EQ = T("==", 9);
    NE = T("!=", 9);
    EQ_STRICT = T("===", 9);
    NE_STRICT = T("!==", 9);
    LT = T("<", 10);
    GT = T(">", 10);
    LTE = T("<=", 10);
    GTE = T(">=", 10);
    IS = T("is", 10);

    /* Unary operators. */
    NOT = T("!", 0);
    BIT_NOT = T("~", 0);

    /* Count operators  = T(also unary). */
    INC = T("++", 0);
    DEC = T("--", 0);

    /* [] operator overloading. */
    INDEX = T("[]", 0);
    ASSIGN_INDEX = T("[]=", 0);

    /* Keywords. */
    BREAK = T("break", 0);
    CASE = T("case", 0);
    CATCH = T("catch", 0);
    CONST = T("const", 0);
    CONTINUE = T("continue", 0);
    DEFAULT = T("default", 0);
    DO = T("do", 0);
    ELSE = T("else", 0);
    FINAL = T("final", 0);
    FINALLY = T("finally", 0);
    FOR = T("for", 0);
    IF = T("if", 0);
    IN = T("in", 0);
    NEW = T("new", 0);
    RETURN = T("return", 0);
    SUPER = T("super", 0);
    SWITCH = T("switch", 0);
    THIS = T("this", 0);
    THROW = T("throw", 0);
    TRY = T("try", 0);
    VAR = T("var", 0);
    VOID = T("void", 0);
    WHILE = T("while", 0);

    /* Literals. */
    NULL_LITERAL = T("null", 0);
    TRUE_LITERAL = T("true", 0);
    FALSE_LITERAL = T("false", 0);
    HEX_LITERAL = T(null, 0, "HEX_LITERAL");
    INTEGER_LITERAL = T(null, 0, "INTEGER_LITERAL");
    DOUBLE_LITERAL = T(null, 0, "DOUBLE_LITERAL");
    STRING = T(null, 0, "STRING");

    /** String interpolation and string templates. */
    STRING_SEGMENT = T(null, 0, "STRING_SEGMENT");
    STRING_LAST_SEGMENT = T(null, 0, "STRING_LAST_SEGMENT");
    // STRING_EMBED_EXP_START does not have a unique string representation in the code:
    //   "$id" yields the token STRING_EMBED_EXP_START after the '$', and similarly
    //   "${id}" yield the same token for '${'.
    STRING_EMBED_EXP_START = T(null, 0, "STRING_EMBED_EXP_START");
    STRING_EMBED_EXP_END = T(null, 0, "STRING_EMBED_EXP_END");

    // Note: STRING_EMBED_EXP_END uses the same symbol as RBRACE, but it is
    // recognized by the scanner when closing embedded expressions in string
    // interpolation and string templates.
  
    /* Directives */
    LIBRARY = T("#library", 0);
    IMPORT = T("#import", 0);
    SOURCE = T("#source", 0);
    RESOURCE = T("#resource", 0);
    NATIVE = T("#native", 0);

    /* Identifiers  = T(not keywords). */
    IDENTIFIER = T(null, 0, "IDENTIFIER");
    WHITESPACE = T(null, 0, "WHITESPACE");

    /* Pseudo tokens. */
    // If you add another pseudo token, don't forget to update the predicate below.
    ILLEGAL = T(null, 0, "ILLEGAL");
    COMMENT = T(null, 0, "COMMENT");

    /**
     * Non-token to be used by tools where a value outside the range of anything
     * returned by the scanner is needed. This is the equivalent of -1 in a C
     * tokenizer.
     *
     * This token is never returned by the scanner. It must have an ordinal
     * value outside the range of all tokens returned by the scanner.
     */
    NON_TOKEN = T(null, 0, "NON_TOKEN");
  }

  /**
   * The <CODE>syntax</CODE> parameter serves two purposes: 1. map tokens that
   * look like identifiers ("null", "true", etc.) to their correct token.
   * 2. Find the string-representation of operators.</BR>
   * When it is <CODE>null</CODE> then the token either doesn't have a unique
   * representation, or it is a pseudo token (which doesn't physically appear
   * in the source).
   */
  Token(syntax, precedence, ordinal, name) :
  syntax_ = syntax, precedence_ = precedence, ordinal_ = ordinal, name_ = name {
    if (null != syntax) {
      tokens[syntax] = this;
    }
  }

  Token asBinaryOperator() {
    int ordinal = ordinal() - ASSIGN_BIT_OR.ordinal() + BIT_OR.ordinal();
    return values()[ordinal];
  }

  int ordinal() => ordinal_; // TODO remove

  int getPrecedence() => precedence_; // TODO remove

  String getSyntax() => syntax_; // TODO remove

  bool isEqualityOperator() {
    int ordinal = ordinal();
    return EQ.ordinal() <= ordinal && ordinal <= NE_STRICT.ordinal();
  }

  bool isRelationalOperator() {
    int ordinal = ordinal();
    return LT.ordinal() <= ordinal && ordinal <= GTE.ordinal();
  }

  bool isAssignmentOperator() {
    int ordinal = ordinal();
    return ASSIGN.ordinal() <= ordinal && ordinal <= ASSIGN_TRUNC.ordinal();
  }

  bool isBinaryOperator() {
    int ordinal = ordinal();
    return (ASSIGN.ordinal() <= ordinal && ordinal <= IS.ordinal())
        || (ordinal == COMMA.ordinal());
  }

  bool isCountOperator() {
    int ordinal = ordinal();
    return INC.ordinal() <= ordinal && ordinal <= DEC.ordinal();
  }

  bool isUnaryOperator() {
    int ordinal = ordinal();
    return NOT.ordinal() <= ordinal && ordinal <= DEC.ordinal();
  }

  bool isUserDefinableOperator() {
    int ordinal = ordinal();
    return ((BIT_OR.ordinal() <= ordinal && ordinal <= GTE.ordinal())
        || this == BIT_NOT || this == INDEX || this == ASSIGN_INDEX)
        && this != NE && this != EQ_STRICT && this != NE_STRICT;
  }

  String toString() {
    assert(syntax_ != null || name_ != null);
    return (syntax_ != null) ? syntax_ : name_;
  }
}
