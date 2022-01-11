#include <iostream>
using namespace std;

int main(int argc, const char * argv[])
{
  float hours, rate;

  cout << "Enter hours: ";
  cin >> hours;
  cout << "Enter rate: ";
  cin >> rate;

  cout << "Employee wage is " << hours * rate << endl;

  return 0;
}
