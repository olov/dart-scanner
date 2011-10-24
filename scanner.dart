// Copyright (c) 2011, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#library("scanner.dart");
#import("token.dart");

/**
 * Represents a position in a source file, including absolute character position,
 * line, and column.
 */
class Position {
  int pos;
  int line;
  int col;

  Position(int this.pos, int this.line, int this.col);

  Position clone() => new Position(pos, line, col);

  int getPos() => pos;
  int getLine() => line;
  int getCol() => col;

  void advance(bool isNewline) {
    ++pos;
    if (isNewline) {
      col = 1;
      ++line;
    } else {
      ++col;
    }
  }

  String toString() => "$line,$col@$pos";
}


/**
 * Represents a span of characters in a source file.
 */
class Location {
  static final Location NONE = null;
  final Position _begin, _end;

  Location(Position begin, [Position end])
      : _begin = begin, _end = (null != end ? end : begin) {
  }

  Location clone() => new Location(_begin.clone(),
                                   (_begin == _end) ? null : _end.clone());

  Position getBegin() => _begin;
  Position getEnd() => _end;

  String toString() => "$_begin::$_end";
}

class RollbackToken {
  final int absoluteOffset;
  final Token replacedToken;

  RollbackToken(int tokenOffset, Token token)
      : absoluteOffset = tokenOffset, replacedToken = token {
  }
}


class State {
  State(int this.baseOffset);

  /* Stack of tokens present before setPeek() */
  List<RollbackToken> rollbackTokens = null;
  final int baseOffset;

  String toString() => "ofs=$baseOffset";
}



class Mode {
  static final int DEFAULT = 0;
  static final int IN_STRING = 1;

  /**
   * Inside a string, scanning a string-interpolation expression.
   * Ex: "${foo}".
   */
  static final int IN_STRING_EMBEDDED_EXPRESSION = 2;

  /**
   * Inside a string, scanning a string-interpolation identifier.
   * <pre>
   * Ex: "$foo bc".
   *        ^
   * </pre>
   */
  static final int IN_STRING_EMBEDDED_EXPRESSION_IDENTIFIER = 3;

  /**
   * Inside a string, just after having scanned a string-interpolation identifier.
   * <pre>
   * Ex: "$foo bc".
   *          ^
   * </pre>
   */
  static final int IN_STRING_EMBEDDED_EXPRESSION_END = 4;
}


/**
 * Maintains the state of scanning strings, including interpolated
 * expressions/identifiers, nested braces for terminating an interpolated
 * expression, the quote character used to start/end the string, and whether
 * it is a multiline string.
 */
class StringState {
  int bracesCount;
  int mode;
  final bool multiLine;
  final String quote;

  /**
   * Push a new mode on state stack.  If the new mode is
   * {@link Mode#IN_STRING_EMBEDDED_EXPRESSION}, mark that we have seen an
   * opening brace.
   *
   * @param mode
   * @param quote
   * @param multiLine
   */
  StringState(int this.mode, String this.quote, bool this.multiLine) {
    bracesCount = mode == Mode.IN_STRING_EMBEDDED_EXPRESSION ? 1 : 0;
  }

  /**
   * Mark that we have seen an opening brace.
   */
  void openBrace() {
    if (mode == Mode.IN_STRING_EMBEDDED_EXPRESSION) {
      bracesCount++;
    }
  }

  /**
   * Mark that we have seen a closing brace.
   *
   * @return true if the current mode is now complete and should be popped
   * off the stack
   */
  bool closeBrace() {
    if (mode == Mode.IN_STRING_EMBEDDED_EXPRESSION) {
      return --bracesCount == 0;
    }
    return false;
  }

  /**
   * @return the string scanning mode.
   */
  int getMode() => mode;

  /**
   * @return the quote character used to bound the current string.
   */
  String getQuote() => quote;

  /**
   * @return true if the current string is a multi-line string.
   */
  bool isMultiLine() => multiLine;

  /**
   * @param mode the string scanning mode.
   */
  void setMode(int mode) {
    this.mode = mode;
  }

  String toString() {
    StringBuffer buf = new StringBuffer();
    buf.add(mode).add("/quote=").add(quote);
    if (multiLine) {
      buf.add("/multiline");
    }
    return buf.toString();
  }
}


/**
 * Stores the entire state for the scanner.
 */
class InternalState {
  List<String> lookahead;
  List<Position> lookaheadPos;
  Position nextLookaheadPos;
  List<TokenData> tokens;
  TokenData lastToken;

  // Current offset in the token list
  int currentOffset;

  // The following fields store data used for parsing string interpolation.
  // The scanner splits the interpolated string in segments, alternating
  // strings and expressions so that the parser can construct the embedded
  // expressions as it goes. The following information is used to ensure that
  // the string is closed with matching quotes, and to deal with parsing
  // ambiguity of "}" (which closes both embedded expressions and braces
  // within embedded expressions).

  /** The string scanning state stack. */
  List<StringState> stringStateStack;

  InternalState() {
    lookahead = new List<String>(NUM_LOOKAHEAD);
    lookaheadPos = new List<Position>(NUM_LOOKAHEAD);
    stringStateStack = <StringState>[];
    currentOffset = 0;
  }

  String toString() {
    StringBuffer ret = new StringBuffer();

    ret.add("currentOffset(");
    ret.add(currentOffset);
    ret.add(")");
    if ( currentOffset > -1 ) {
      TokenData tok = tokens[currentOffset];
      ret.add(" = [");
      ret.add(tok.token);
      if (tok.value != null) {
        ret.add(" (${tok.value})");
      }
      ret.add("], ");
    }

    ret.add("[");
    for (int i = 0; i < tokens.length; i++) {
      TokenData tok = tokens[i];
      ret.add(tok.token);
      if (tok.value != null) {
        ret.add(" (${tok.value})");
      }
      if (i < tokens.length - 1) {
        ret.add(", ");
      }
    }
    ret.add("]");
    if (getMode() != Mode.DEFAULT) {
      ret.add("(within string starting with ");
      ret.add(getQuote());
      if (isMultiLine()) {
        ret.add(getQuote());
        ret.add(getQuote());
      }
      ret.add(')');
    }
    return ret.toString();
  }

  /**
   * @return the current scanning mode
   */
  int getMode() {
    return stringStateStack.length == 0 ? Mode.DEFAULT : getCurrentState().getMode();
  }

  /**
   * Mark that we have seen an open brace.
   */
  void openBrace() {
    if (stringStateStack.length > 0) {
      getCurrentState().openBrace();
    }
  }

  /**
   * Mark that we have seen a close brace.
   *
   * @return true if the current mode is now complete and should be popped
   */
  bool closeBrace() {
    if (stringStateStack.length > 0) {
      return getCurrentState().closeBrace();
    }
    return false;
  }

  /**
   * Pop the current mode.
   */
  void popMode() {
    if (stringStateStack.length > 0) {
      stringStateStack.removeLast();
    }
  }

  /**
   * @param mode the mode to push
   */
  void pushMode(int mode, String quote, bool multiLine) {
    stringStateStack.add(new StringState(mode, quote, multiLine));
  }

  /**
   * @param mode the mode to push
   */
  void replaceMode(int mode) {
    getCurrentState().setMode(mode);
  }

  /**
   * Remove all modes, returning to the default state.
   */
  void resetModes() {
    stringStateStack.clear();
  }

  /**
   * @return the quote
   */
  String getQuote() {
    return getCurrentState().getQuote();
  }

  /**
   * @return the current string scanning state
   */
  StringState getCurrentState() {
    assert(stringStateStack.length > 0); //"called with empty state stack"
    return stringStateStack.last();
  }

  /**
   * @return the multiLine
   */
  bool isMultiLine() {
    return getCurrentState().isMultiLine();
  }
}

class TokenData {
  Token token;
  Location location;
  String value;

  TokenData clone() {
    TokenData clone = new TokenData();
    // token and value are immutable
    clone.token = token;
    clone.location = (location == null) ? null : location.clone();
    clone.value = value;
    return clone;
  }

  String toString() {
    String str = token.toString();
    return (value != null) ? "$str($value)" : str;
  }
}





final int NUM_LOOKAHEAD = 2;

/**
 * The Dart scanner. Should normally be used only by {@link DartParser}.
 */
class DartScanner {
  static bool isDecimalDigit(String c) {
    return c.compareTo('0') >= 0 && c.compareTo('9') <= 0;
  }

  static bool isHexDigit(String c) {
    return isDecimalDigit(c) || (c.compareTo('a') >= 0 && c.compareTo('f') <= 0) || (c.compareTo('A') >= 0 && c.compareTo('F') <= 0);
  }

  static bool isIdentifierPart(String c) {
    return isIdentifierStart(c) || isDecimalDigit(c);
  }

  static bool isIdentifierStart(String c) {
    return (c.compareTo('a') >= 0 && c.compareTo('z') <= 0) || (c.compareTo('A') >= 0 && c.compareTo('Z') <= 0) || (c == '_') || (c == '\$');
  }

  static bool isLineTerminator(String c) {
    return c == '\r' || c == '\n';
  }

  static bool isWhiteSpace(String c) {
    return c == ' ' || c == '\t';
  }

  int commentLineCount = 0;
  int commentCharCount = 0;
  int lastCommentStart = 0;
  int lastCommentStop = 0;
  String source;
  InternalState internalState;

  DartScanner(String source, [int start = 0]) {
    // TODO fix traceevent
    //final TraceEvent logEvent = Tracer.canTrace() ? Tracer.start(DartEventType.SCANNER) : null; 
    try {
      this.source = source;
      internalState = new InternalState();
      internalState.tokens = <TokenData>[]; // TODO check not needed (source.length ~/ 2);

      // Initialize lookahead positions.
      // TODO Determine if line & column should be relative to 0 or 'start'
      internalState.nextLookaheadPos = new Position(start, 1, 1);
      for (int i = 0; i < internalState.lookaheadPos.length; ++i) {
        internalState.lookaheadPos[i] = new Position(start, 1, 1);
      }

      // Fill all the characters in the look-ahead and all the peek
      // elements in the tokens buffer.
      for (int i = 0; i < NUM_LOOKAHEAD; i++) {
        advance();
      }

      // Scan all the tokens up front
      scanFile();
    } finally {
      //TODO
      //Tracer.end(logEvent);
    } 
  }

  /**
   * Returns the number of lines of source that were scanned, excluding the number of lines
   * consumed by comments.
   */
  int getNonCommentLineCount() {
    return getLineCount() - commentLineCount;
  }

  /**
   * Returns the number of lines of source that were scanned.
   */
  int getLineCount() {
    int lineCount = internalState.nextLookaheadPos.line;
    if (isEos()) {
      // At the end of the file the next line has advanced one past the end
      lineCount -= 1;
    }
    return lineCount;
  }

  /**
   * Returns the number of characters of source code that were scanned.
   */
  int getCharCount() {
    return internalState.nextLookaheadPos.pos;
  }

  /**
   * Returns the number of characters of source code that were scanned excluding the number of
   * characters consumed by comments.
   */
  int getNonCommentCharCount() {
    return getCharCount() - commentCharCount;
  }

  /**
   * Get the token value for one of the look-ahead tokens.
   */
  String getPeekTokenValue(int n) {
    assert (0 <= n && (internalState.currentOffset + n + 1) < internalState.tokens.length);
    return internalState.tokens[internalState.currentOffset + n + 1].value;
  }

  /**
   * Gets a copy of the current scanner state. This state can be passed to {@link
   * #restoreState(State)}.
   */
  State getState() {
    return new State(internalState.currentOffset);
  }

  /**
   * Gets the current offset of the scanner.
   */
  int getOffset() {
    return internalState.currentOffset;
  }

  /**
   * Gets the current token.
   */
  Token getToken() {
    return internalState.tokens[internalState.currentOffset].token;
  }

  /**
   * Gets the location of the current token.
   */
  Location getTokenLocation() {
    return internalState.tokens[internalState.currentOffset].location;
  }

  Location peekTokenLocation(int n) {
    if ((internalState.currentOffset + n + 1) < internalState.tokens.length) {
      return internalState.tokens[internalState.currentOffset + n + 1].location;
    } else {
      // It is not valid to read beyond the end of the token stream, so we
      // return the Location of the EOS token.
      return internalState.tokens[internalState.tokens.length - 1].location;
    }

  }

  /**
   * Get the token value or location for the current token previously returned
   * by a call to next().
   */
  String getTokenValue() {
    return internalState.tokens[internalState.currentOffset].value;
  }

  String peekTokenValue(int n) {
    if ((internalState.currentOffset + n + 1) < internalState.tokens.length) {
      return internalState.tokens[internalState.currentOffset + n + 1].value;
    } else {
      // It is not valid to read beyond the end of the token stream, so we
      // return the null, the default value of an EOS token.
      return null;
    }
  }

  /**
   * Returns the next token.
   */
  Token next() {
    // Do not advance the current offset beyond the end of the stoken stream
    if (internalState.currentOffset + 1 < internalState.tokens.length) {
      internalState.currentOffset++;
    }
    return getToken();
  }

  /**
   * Token look-ahead - past the token returned by next().
   */
  Token peek(int n) {
    if ((internalState.currentOffset + n + 1) < internalState.tokens.length) {
      return internalState.tokens[internalState.currentOffset + n + 1].token;
    } else {
      // It is not valid to read beyond the end of the token stream, so we
      // return the EOS token
      return Token.EOS;
    }
  }

  /**
   * Sets the scanner's state, using a state object returned from {@link #getState()}.
   */
  void restoreState(State oldState) {
    // reset offset
    internalState.currentOffset = oldState.baseOffset;
  }

  /**
   * Sets the token at the specified slot in the lookahead buffer.
   */
  void setPeek(int n, Token token) {
    assert (0 <= n && (internalState.currentOffset + n + 1) < internalState.tokens.length);
    internalState.tokens[internalState.currentOffset + n + 1].token = token;
  }

  /**
   * Sets the token at the specified slot in the lookahead buffer.
   */
  void setAbsolutePeek(int n, Token token) {
    assert (0 <= n && n < internalState.tokens.length);
    internalState.tokens[n].token = token;
  }

  String toString() {
    if (internalState == null) {
      return super.toString();
    }
    return internalState.toString();
  }

  /**
   * A hook into low-level scanning machinery. Use with care and only as directed.<p>
   * Record the location of a comment. Given a source string <code>source,</code>
   * the actual comment string is <code>source.substring(start - 1, stop)</code>
   * because the comment cannot be recognized until its second character is
   * scanned.<p>
   * Note: A single comment may be scanned multiple times. If the scanner has
   * to backtrack it will re-scan comments until it no longer has to backtrack.
   * Clients are responsible for filtering duplicate comment locations.<p>
   * Warning: This method may be called during initialization of the scanner in
   * the <code>DartScanner</code> constructor. Fields defined in the subclass
   * that implements this method may not have been initialized before the first
   * invocation.
   * @param start the character position of the second character in the comment
   * @param stop the character position of the final character in the comment
   * @param line the line number at <code>start</code>
   * @param col the column number at <code>start</code>
   */
  void recordCommentLocation(int start, int stop, int line, int col) {
  }

  void advance() {
    for (int i = 0; i < NUM_LOOKAHEAD - 1; ++i) {
      internalState.lookahead[i] = internalState.lookahead[i + 1];
      internalState.lookaheadPos[i] = internalState.lookaheadPos[i + 1].clone();
    }
    if (internalState.nextLookaheadPos.pos < source.length) {
      String ch = source[internalState.nextLookaheadPos.pos];
      internalState.lookahead[NUM_LOOKAHEAD - 1] = ch;
      internalState.lookaheadPos[NUM_LOOKAHEAD - 1] = internalState.nextLookaheadPos.clone();
      internalState.nextLookaheadPos.advance(ch == '\n');
    } else {
      // Let the last look-ahead position be past the source. This makes
      // the position information for the last token correct.
      internalState.lookahead[NUM_LOOKAHEAD - 1] = '';
      internalState.lookaheadPos[NUM_LOOKAHEAD - 1] = new Position(source.length,
        internalState.nextLookaheadPos.line, internalState.nextLookaheadPos.col);

      // Leave the nextLookahead position pointing to the line after the last line
      internalState.nextLookaheadPos = new Position(source.length,
          internalState.nextLookaheadPos.line + 1, 1);
    }
  }

  /**
   * Called when comments are identified to aggregate the total number of comment lines and comment
   * characters then delegate to {@link #recordCommentLocation(int, int, int, int)}.  This provides
   * a light weight way to track how much of the code is made up of comments without having to keep
   * all comments.
   *
   * @param start the character position of the second character in the comment
   * @param stop the character position of the final character in the comment
   * @param startLine the line number at <code>start</code>
   * @param endLine the line number of the last line of the comment
   * @param col the column number at <code>start</code>
   */
  void commentLocation(int start, int stop, int startLine, int endLine, int col) {
    if (start <= lastCommentStart && stop <= lastCommentStop) {
      return;
    }

    lastCommentStart = start;
    lastCommentStop = stop;
    commentLineCount += endLine - startLine + 1;
    commentCharCount += stop - start + 1;

    recordCommentLocation(start, stop, startLine, col);
  }

  bool isChar(String c) {
    return internalState.lookahead[0] == c;
  }

  bool isEos() {
    return internalState.lookahead[0] == '';
  }

  String lookahead(int n) {
    assert (0 <= n && n < NUM_LOOKAHEAD);
    return internalState.lookahead[n];
  }

  // Get the current source code position.
  Position position() {
    return internalState.lookaheadPos[0];
  }

  void scanFile() {
    // First node inserted as a dummy.
    internalState.lastToken = new TokenData();
    internalState.tokens.add(internalState.lastToken);

    while (true) {
      internalState.lastToken = new TokenData();
      Token token;
      Position begin, end;
      do {
        skipWhiteSpace();
        begin = position();
        token = scanToken();
      } while (token == Token.COMMENT);
      end = position();

      internalState.lastToken.token = token;
      internalState.lastToken.location = new Location(begin, end);
      internalState.tokens.add(internalState.lastToken);
      if (token == Token.EOS) {
        return;
      }
    }
  }

  Token scanIdentifier(bool allowDollars) {
    assert (isIdentifierStart(lookahead(0)));
    Position begin = position();
    while (true) {
      String nextChar = lookahead(0);
      if (!isIdentifierPart(nextChar) || (!allowDollars && nextChar == '\$')) {
        break;
      }
      advance();
    }
    int size = position().pos - begin.pos;

    // Use a substring of the source string instead of copying all the
    // characters to the token value buffer.
    String result = source.substring(begin.pos, begin.pos + size);
    internalState.lastToken.value = result;
    return Token.lookup(result);
  }

  Token scanNumber() {
    bool isDouble = false;
    assert (isDecimalDigit(lookahead(0)) || isChar('.'));
    Position begin = position();
    while (isDecimalDigit(lookahead(0)))
      advance();
    if (isChar('.') && isDecimalDigit(lookahead(1))) {
      isDouble = true;
      advance();  // Consume .
      while (isDecimalDigit(lookahead(0)))
        advance();
    }
    if (isE()) {
      isDouble = true;
      advance();
      if (isChar('+') || isChar('-')) {
        advance();
      }
      if (!isDecimalDigit(lookahead(0))) {
        return Token.ILLEGAL;
      }
      while (isDecimalDigit(lookahead(0)))
        advance();
    } else if (isIdentifierStart(lookahead(0))) {
      // Number literals must not be followed directly by an identifier.
      return Token.ILLEGAL;
    }
    int size = position().pos - begin.pos;
    internalState.lastToken.value = source.substring(begin.pos, begin.pos + size);
    return isDouble ? Token.DOUBLE_LITERAL : Token.INTEGER_LITERAL;
  }

  bool isE() {
    return isChar('e') || isChar('E');
  }

  Token scanHexNumber() {
    assert (isDecimalDigit(lookahead(0)) && (lookahead(1) == 'x' || lookahead(1) == 'X'));
    // Skip 0x/0X.
    advance();
    advance();

    Position begin = position();
    if (!isHexDigit(lookahead(0))) {
      return Token.ILLEGAL;
    }
    advance();
    while (isHexDigit(lookahead(0))) {
      advance();
    }
    if (isIdentifierStart(lookahead(0))) {
      return Token.ILLEGAL;
    }
    internalState.lastToken.value = source.substring(begin.pos, position().pos);
    return Token.HEX_LITERAL;
  }

  Token scanString(bool isRaw) {
    String quote = lookahead(0);
    assert (isChar('\'') || isChar('"'));
    bool multiLine = false;
    advance();

    // detect whether this is a multi-line string:
    if (lookahead(0) == quote && lookahead(1) == quote) {
      multiLine = true;
      advance();
      advance();
      // according to the dart guide, when multi-line strings start immediatelly
      // with a \n, the \n is not part of the string:
      if (isChar('\n')) {
        advance();
      }
    }
    internalState.pushMode(Mode.IN_STRING, quote, multiLine);
    if (isRaw) {
      return scanRawString();
    } else {
      return scanWithinString(true);
    }
  }

  Token scanRawString() {
    assert (internalState.getMode() == Mode.IN_STRING);
    String quote = internalState.getQuote();
    bool multiLine = internalState.isMultiLine();
    // TODO(floitsch): Do we really need a StringBuffer to accumulate the characters?
    assert(false);
    StringBuffer tokenValueBuffer = new StringBuffer();
    while (true) {
      if (isEos()) {
        // Unterminated string (either multi-line or not).
        internalState.popMode();
        return Token.ILLEGAL;
      }
      String c = lookahead(0);
      advance();
      if (c == quote) {
        if (!multiLine) {
          // Done parsing the string literal.
          break;
        } else if (lookahead(0) == quote && lookahead(1) == quote) {
          // Done parsing the multi-line string literal.
          advance();
          advance();
          break;
        }
      }
      tokenValueBuffer.add(c);
    }
    internalState.lastToken.value = tokenValueBuffer.toString();
    internalState.popMode();
    return Token.STRING;
  }

  /**
   * Scan within a string watching for embedded expressions (string
   * interpolation). This function returns 4 kinds of tokens:
   * <ul>
   *   <li> {@link Token#STRING} when {@code start} is true and no embedded
   *   expressions are found (default to string literals when no interpolation
   *   was used).
   *   <li> {@link Token#STRING_SEGMENT} when the string is interrupted with an
   *   embedded expression.
   *   <li> {@link Token#STRING_EMBED_EXP_START} when an embedded expression is
   *   found right away (the lookahead is "${").
   *   <li> {@link Token#STRING_LAST_SEGMENT} when {@code start} is false and no
   *   more embedded expressions are found.
   * </ul>
   */
  Token scanWithinString(bool start) {
    assert (internalState.getMode() == Mode.IN_STRING);
    String quote = internalState.getQuote();
    bool multiLine = internalState.isMultiLine();

    StringBuffer tokenValueBuffer = new StringBuffer();
    while (true) {
      if (isEos()) {
        // Unterminated string (either multi-line or not).
        internalState.resetModes();
        return Token.EOS;
      }
      String c = lookahead(0);
      if (c == quote) {
        advance();
        if (!multiLine) {
          // Done parsing string constant.
          break;
        } else if (lookahead(0) == quote && lookahead(1) == quote) {
          // Done parsing multi-line string constant.
          advance();
          advance();
          break;
        }
      } else if (c == '\n' && !multiLine) {
        advance();
        internalState.popMode();
        // unterminated (non multi-line) string
        return Token.ILLEGAL;
      } else if (c == '\\') {
        advance();
        if (isEos()) {
          // Unterminated string (either multi-line or not).
          internalState.resetModes();
          return Token.EOS;
        }
        c = lookahead(0);
        advance();
        switch (c) {
          case 'b':
            c = 0x08;
            break;
          case 'f':
            c = 0x0C;
            break;
          case 'n':
            c = '\n';
            break;
          case 'r':
            c = '\r';
            break;
          case 't':
            c = '\t';
            break;
          case 'v':
            c = 0x0B;
            break;
          case 'x':
          case 'u':
            // Parse Unicode escape sequences, which are of the form (backslash) xXX, (backslash)
            // uXXXX or (backslash) u{X*} where X is a hexadecimal digit - the delimited form must
            // be between 1 and 6 digits.
            int len = (c == 'u') ? 4 : 2;
            if (isEos()) {
              // Unterminated string (either multi-line or not).
              internalState.resetModes();
              return Token.EOS;
            }
            c = lookahead(0);
            int unicodeCodePoint = 0;
            // count of characters remaining or negative if delimited
            if (c == '{') {
              len = -1;
              advance();
              if (isEos()) {
                // Unterminated string (either multi-line or not).
                internalState.resetModes();
                return Token.EOS;
              }
              c = lookahead(0);
            }
            while (len != 0) {
              advance();
              int digit = c.charCodeAt(0);
              if (digit < 0 || digit > 15) {
                // TODO(jat): how to handle an error?  We would prefer to give a better error
                // message about an invalid Unicode escape sequence
                return Token.ILLEGAL;
              }
              unicodeCodePoint = unicodeCodePoint * 16 + digit;
              c = lookahead(0);
              if (len-- < 0 && c == '}') {
                advance();
                break;
              }
              if (isEos()) {
                // Unterminated string (either multi-line or not).
                internalState.resetModes();
                return Token.EOS;
              }
              if (len < -6) {
                // TODO(jat): better way to indicate error
                // too many characters for a delimited character
                return Token.ILLEGAL;
              }
            }
            c = new String.fromCharCodes([unicodeCodePoint]);
            // TODO implement valid codepoint checking in Dart?
            // Unicode escapes must specify a valid Unicode scalar value, and may not specify
            // UTF16 surrogates.
            /*
            if (!Character.isValidCodePoint(c) || (c < 0x10000
                && (Character.isHighSurrogate((char) c) || Character.isLowSurrogate((char) c)))) {
              // TODO(jat): better way to indicate error
              return Token.ILLEGAL;
            }
            */
            // TODO(jat): any other checks?  We could use Character.isDefined, but then we risk
            // version skew with the JRE's Unicode data.  For now, assume anything in the Unicode
            // range besides surrogates are fine.
            break;

          default:
            // any other character following a backslash is just itself
            // see Dart guide 3.3
            break;
        }
      } else if (c == '\$') {
        // TODO(sigmund): add support for named embedded expressions and
        // function embedded expressions for string templates.
        if (tokenValueBuffer.length == 0) {
          advance();
          String nextChar = lookahead(0);
          if (nextChar == '{') {
            advance();
            internalState.pushMode(Mode.IN_STRING_EMBEDDED_EXPRESSION, quote,
                multiLine);
          } else {
            internalState.pushMode(Mode.IN_STRING_EMBEDDED_EXPRESSION_IDENTIFIER,
                quote, multiLine);
          }
          return Token.STRING_EMBED_EXP_START;
        } else {
          // Encountered the beginning of an embedded expression (string
          // interpolation), return the current segment, and keep the "$" for
          // the next token.
          internalState.lastToken.value = tokenValueBuffer.toString();
          return Token.STRING_SEGMENT;
        }
      } else {
        advance();
      }
      tokenValueBuffer.add(c);
    }

    internalState.lastToken.value = tokenValueBuffer.toString();
    internalState.popMode();
    if (start) {
      return Token.STRING;
    } else {
      return Token.STRING_LAST_SEGMENT;
    }
  }

  Token scanToken() {
    switch (internalState.getMode()) {
      case Mode.IN_STRING:
        return scanWithinString(false);
      case Mode.IN_STRING_EMBEDDED_EXPRESSION_IDENTIFIER:
        // We are inside a string looking for an identifier. Ex: "$foo".
        internalState.replaceMode(Mode.IN_STRING_EMBEDDED_EXPRESSION_END);
        String c = lookahead(0);
        if (isIdentifierStart(c) && c != '\$') {
          bool allowDollars = false;
          return scanIdentifier(allowDollars);
        } else {
          internalState.popMode();
          if (!isEos()) {
            internalState.lastToken.value = c;
          }
          return Token.ILLEGAL;
        }
      case Mode.IN_STRING_EMBEDDED_EXPRESSION_END:
        // We scanned the identifier of a string-interpolation. New we return the
        // end-of-embedded-expression token.
        internalState.popMode();
        return Token.STRING_EMBED_EXP_END;
      default:
        // fall through
    }

    switch (lookahead(0)) {
      case '"':
      case '\'': {
        bool isRaw = false;
        return scanString(isRaw);
      }

      case '<':
        // < <= << <<=
        advance();
        if (isChar('='))
          return select(Token.LTE);
        if (isChar('<'))
          return selectNext('=', Token.ASSIGN_SHL, Token.SHL);
        return Token.LT;

      case '>':
        // > >= >> >>= >>> >>>=
        advance();
        if (isChar('='))
          return select(Token.GTE);
        if (isChar('>')) {
          // >> >>= >>> >>>=
          advance();
          if (isChar('='))
            return select(Token.ASSIGN_SAR);
          if (isChar('>'))
            return selectNext('=', Token.ASSIGN_SHR, Token.SHR);
          return Token.SAR;
        }
        return Token.GT;

      case '=':
        // = == === =>
        advance();
        if (isChar('>')) {
          return select(Token.ARROW);
        }
        if (isChar('='))
          return selectNext('=', Token.EQ_STRICT, Token.EQ);
        return Token.ASSIGN;

      case '!':
        // ! != !==
        advance();
        if (isChar('='))
          return selectNext('=', Token.NE_STRICT, Token.NE);
        return Token.NOT;

      case '+':
        // + ++ +=
        advance();
        if (isChar('+'))
          return select(Token.INC);
        if (isChar('='))
          return select(Token.ASSIGN_ADD);
        return Token.ADD;

      case '-':
        // - -- -=
        advance();
        if (isChar('-'))
          return select(Token.DEC);
        if (isChar('='))
          return select(Token.ASSIGN_SUB);
        return Token.SUB;

      case '*':
        // * *=
        return selectNext('=', Token.ASSIGN_MUL, Token.MUL);

      case '%':
        // % %=
        return selectNext('=', Token.ASSIGN_MOD, Token.MOD);

      case '/':
        // / // /* /=
        advance();
        if (isChar('/'))
          return skipSingleLineComment();
        if (isChar('*'))
          return skipMultiLineComment();
        if (isChar('='))
          return select(Token.ASSIGN_DIV);
        return Token.DIV;

      case '&':
        // & && &=
        advance();
        if (isChar('&'))
          return select(Token.AND);
        if (isChar('='))
          return select(Token.ASSIGN_BIT_AND);
        return Token.BIT_AND;

      case '|':
        // | || |=
        advance();
        if (isChar('|'))
          return select(Token.OR);
        if (isChar('='))
          return select(Token.ASSIGN_BIT_OR);
        return Token.BIT_OR;

      case '^':
        // ^ ^=
        return selectNext('=', Token.ASSIGN_BIT_XOR, Token.BIT_XOR);

      case '.':
        // . <number>
        if (isDecimalDigit(lookahead(1))) {
          return scanNumber();
        } else {
          advance();
          if (lookahead(0) == '.' && lookahead(1) == '.') {
            advance();
            advance();
            return Token.ELLIPSIS;
          }
          return Token.PERIOD;
        }

      case ':':
        return select(Token.COLON);

      case ';':
        return select(Token.SEMICOLON);

      case ',':
        return select(Token.COMMA);

      case '(':
        return select(Token.LPAREN);

      case ')':
        return select(Token.RPAREN);

      case '[':
        advance();
        if (isChar(']')) {
          return selectNext('=', Token.ASSIGN_INDEX, Token.INDEX);
        }
        return Token.LBRACK;

      case ']':
        return select(Token.RBRACK);

      case '{':
        internalState.openBrace();
        return select(Token.LBRACE);

      case '}':
        if (internalState.closeBrace()) {
          internalState.popMode();
          return select(Token.STRING_EMBED_EXP_END);
        }
        return select(Token.RBRACE);

      case '?':
        return select(Token.CONDITIONAL);

      case '~':
        // ~ ~/ ~/=
        advance();
        if (isChar('/')) {
          if (lookahead(1) == '=') {
            advance();
            return select(Token.ASSIGN_TRUNC);
          } else {
            return select(Token.TRUNC);
          }
        } else {
          return Token.BIT_NOT;
        }

      case '@':
        // Raw strings.
        advance();
        if (isChar('\'') || isChar('"')) {
          bool isRaw = true;
          return scanString(isRaw);
        } else {
          return select(Token.ILLEGAL);
        }

      case '#':
        return scanDirective();

      default:
        if (isIdentifierStart(lookahead(0))) {
          bool allowDollars = true;
          return scanIdentifier(allowDollars);
        }
        if (isDecimalDigit(lookahead(0))) {
          if (lookahead(0) == '0' && (lookahead(1) == 'x' || lookahead(1) == 'X')) {
            return scanHexNumber();
          } else {
            return scanNumber();
          }
        }
        if (isEos())
          return Token.EOS;
        return select(Token.ILLEGAL);
    }
  }

  /**
   * Scan for #library, #import, #source, and #resource directives
   */
  Token scanDirective() {
    assert (isChar('#'));
    Position currPos = position();
    int start = currPos.pos;
    int line = currPos.line;
    int col = currPos.col;

    // Skip over the #! if it exists and consider it a comment
    if (start == 0) {
      if (lookahead(1) == '!') {
        while (!isEos() && !isLineTerminator(lookahead(0)))
          advance();
        int stop = internalState.lookaheadPos[0].pos;
        commentLocation(start, stop, line, internalState.lookaheadPos[0].line, col);
        return Token.COMMENT;
      }
    }

    // Directives must start at the beginning of a line
    // TODO test off-by-one
    if (start > 0 && !isLineTerminator(source[start - 1]));
      return select(Token.ILLEGAL);

    // Determine which directive is being specified
    advance();
    while (true) {
      String ch = lookahead(0);
      if (ch < 'a' || ch > 'z') {
        break;
      }
      advance();
    }
    String syntax = source.substring(start, position().pos);
    Token token = Token.lookup(syntax);
    return token == Token.IDENTIFIER ? Token.ILLEGAL : token;
  }

  Token selectNext(String next, Token yes, Token no) {
    advance();
    if (lookahead(0) != next)
      return no;
    advance();
    return yes;
  }

  Token select(Token token) {
    advance();
    return token;
  }

  Token skipMultiLineComment() {
    assert (isChar('*'));
    Position currPos = internalState.lookaheadPos[0];
    int start = currPos.pos - 1;
    int line = currPos.line;
    int col = currPos.col;
    advance();
    while (!isEos()) {
      String first = lookahead(0);
      advance();
      if (first == '*' && isChar('/')) {
        Token result = select(Token.COMMENT);
        int stop = internalState.lookaheadPos[0].pos;
        commentLocation(start, stop, line, internalState.lookaheadPos[0].line, col);
        return result;
      }
    }
    int stop = internalState.lookaheadPos[0].pos;
    commentLocation(start, stop, line, internalState.lookaheadPos[0].line, col);
    // Unterminated multi-line comment.
    return Token.ILLEGAL;
  }

  Token skipSingleLineComment() {
    assert (isChar('/'));
    Position currPos = internalState.lookaheadPos[0];
    int start = currPos.pos - 1;
    int line = currPos.line;
    int col = currPos.col;
    advance();
    while (!isEos() && !isLineTerminator(lookahead(0)))
      advance();
    int stop = internalState.lookaheadPos[0].pos;
    commentLocation(start, stop, line, internalState.lookaheadPos[0].line, col);
    return Token.COMMENT;
  }

  void skipWhiteSpace() {
    if ((internalState.getMode() != Mode.DEFAULT)
        && (internalState.getMode() != Mode.IN_STRING_EMBEDDED_EXPRESSION)) {
      return;
    }
    while (true) {
      if (isLineTerminator(lookahead(0))) {
      } else if (!isWhiteSpace(lookahead(0))) {
        break;
      }
      advance();
    }
  }
}
