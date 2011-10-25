#import("scanner.dart");
#import("token.dart");

String readFile(String filename) {
  var file = new File(filename, false);
  var buffer = new List<int>(file.length);
  file.readList(buffer, 0, file.length);
  file.close();
  return new String.fromCharCodes(buffer);
}

main() {
  Token.initialize();

  var src = readFile("token.dart");
  var scanner = new DartScanner(src);
  //var scanner = new DartScanner("int hello = 42;");
  //var scanner = new DartScanner("main() { var x = 0x15; }");
  print(scanner);
}
