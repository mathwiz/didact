#include "ex22.h"
#include "dbg.h"

const char *MY_NAME = "Wile E. Coyote";

void scope_demo(int count) {
  log_info("count is: %d", count);
  if (count > 10) {
    int count = 100; //BAD! BUGGY!
    log_info("count in inner scope is: %d", count);
  }
  log_info("count after leaving inner scope is: %d", count);
  count = 3000;
  log_info("count after assgining in outer scope is: %d", count);
}

int main(int argc, char *argv[]) {
  log_info("My name: %s, age: %d", MY_NAME, get_age());
  set_age(100);

  // test THE_SIZE extern
  log_info("THE_SIZE is: %d", THE_SIZE);
  print_size();

  THE_SIZE = 9;
  log_info("THE_SIZE after assignment is: %d", THE_SIZE);
  print_size();

  // test the ratio function static
  log_info("Ratio at first: %f", update_ratio(1.0));
  log_info("Ratio again: %f", update_ratio(20.0));
  log_info("Ratio once more: %f", update_ratio(300.0));

  // test the scope demo
  int count = 4;
  scope_demo(count);
  scope_demo(count * 20);
  log_info("count after calling scope_demo: %d", count);

  debug("End of main");
  return 0;
}



