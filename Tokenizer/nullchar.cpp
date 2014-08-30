#include <fstream>

using namespace std;

int main() {
  ofstream myfile;
  myfile.open("carriage.cl");
  myfile << '\r';
  myfile.close();
  return 0;
}
