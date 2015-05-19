#include <iostream>
#include <cmath>
#include <sstream>

using namespace std;

string representation(int number, int base){
    int ndigits = 1;
    stringstream repr;
    for(auto n = number; abs(n) > abs(base); n/=base) ++ndigits;
    cout << "ndigits: " << ndigits << endl; //debug
    for(auto d = ndigits; d>0; --d){
        cout << "d: " << d << endl; //debug
        cout << "\tnumber: " << number << endl; //debug
        int b = pow(base, d-1);
        cout << "\tb: " << b << endl; //debug
        auto digit = number/b;
        cout << "\tdigit: " << digit << endl; //debug
        repr << digit;
        number-=digit*b;
    }
    return repr.str();
}

int main(){
    cout << representation(929, -4) << endl;
}
