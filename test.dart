#import("scanner.dart");
#import("token.dart");

main() {
  print("hello from test.dart");
  Token.initialize();

  //print(Token.EQ.isEqualityOperator());
  //print(Token.lookup("=="));
  //print(Token.tokens["=="]);

  var scanner = new DartScanner("int hello = 42;");
  //var scanner = new DartScanner("main() { print('hello') }");
  print(scanner.internalState);
}
