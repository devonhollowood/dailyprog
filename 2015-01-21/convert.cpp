#include <iostream>
#include <complex>
#include <cmath>

using namespace std;

int numeric(string repr, int base){
    cout << "entering numeric" << endl;
    int res = 0;
    size_t ndigits = repr.size();
    for(size_t ndigit = 0; ndigit<ndigits; ++ndigit){
        int place = static_cast<int>(pow(base, ndigit));
        res += (repr[ndigits-ndigit-1]-'0')*place;
    }
    return res;
}

//returns the representation of _number_ in _base_
string representation(int number, int base){
    cout << "entering repr" << endl;
    string result;
    size_t digit_num = 1;
    while(number!=0){
        //get digit
        auto place = static_cast<int>(pow(base, digit_num-1));
        cout << "place = " << place << endl;
        auto digit = number%(place*base)/place;
        cout << "digit = " << digit << endl;
        if (digit < 0) digit = abs(base)+digit;
        cout << "digit = " << digit << endl;
        //add digit to result
        result = to_string(digit) + result;
        cout << "result = " << result << endl;
        //set up for next iteration
        number-= digit*place;
        ++digit_num;
    }
    return result;
}

int main(int argc, char** argv){
    if(argc!=3){
        cout << "usage: convert [base] [number]" << endl;
        exit(1);
    }
    int base = atoi(argv[1]);
    string repr = argv[2];
    string new_repr = representation(numeric(repr, base), -base);
    cout << new_repr << endl;
}
