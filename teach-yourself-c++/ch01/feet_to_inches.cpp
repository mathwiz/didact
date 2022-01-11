#include <iostream>
using namespace std;

bool zerop(float num) 
{
  return num == 0;
}

int main(int argc, const char * argv[])
{
  float feet;
  int rate = 12;

  do {
    cout << "Enter feet (0 to quit): ";
    cin >> feet;
    if (zerop(feet)) break;
    cout << feet << " feet equals " << feet * rate << " inches." <<  endl;
  } while (1);
  
  return 0;
}
