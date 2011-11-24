#import("scanner.dart");
#import("token.dart");

String readFile(String filename) {
  File file = new File(filename);
  file.openSync();
  int len = file.lengthSync();
  List buffer = new List<int>(len);
  file.readListSync(buffer, 0, len);
  file.closeSync();
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
