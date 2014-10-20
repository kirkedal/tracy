void minmax(int input1, int input2, int input3) {
  int most;
  int least;
  most = input2;
  least = input1;
  if ( most < input2 ) {
    most = input2;
  }
  if ( most < input3 ) {
    most = input3;
  }
  if ( input2 < least ) {
    least = input2;
  }
  if ( input3 < least ) {
    least = input3;
  }
  assert(
    most >= input1 && most >= input2 && most >= input3 &&
    (most == input1 || most == input2 || most == input3)
  );
}
