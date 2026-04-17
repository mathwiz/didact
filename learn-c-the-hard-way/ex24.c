#include <stdio.h>
#include "dbg.h"

#define SUCCESS_CODE 0
#define ERROR_CODE -1

int main(int argc, char *argv[])
{


  printf("Done.\n");
  return SUCCESS_CODE;
error:
  return ERROR_CODE;
}
