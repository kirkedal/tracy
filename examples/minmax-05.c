unsigned BIT(unsigned x, unsigned n) { return (x & (0x01 << n)) >> n; }

void minmax(int input1, int input2, int input3) {
  // SPEC_BEGIN PRECONDITION
  // SPEC_END
  int most;
  int least;
  most = input1;
  least = input1; 
  if (most < input2) {
    most = input2;
  }
  if (/*ERROR_BEGIN*/most == input3/*ERROR_END*/) {
    most = input3;
  }
  if (input2 < least) {
    least = input2;
  }
  if (input3 < least) {
    least = input3;
  }
  // SPEC_BEGIN POSTCONDITION
  // SPEC_END
}
