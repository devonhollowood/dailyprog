#include <iostream>
#include <array>

int main(int argc, char** argv){
    using ulint = unsigned long int;
    std::array<ulint, 8> primes {2,3,5,7,11,13,17,19}; 
    ulint count = 0;
    ulint candidate = 1;
    ulint target_count = 1000000;
    for(; count < target_count; ++candidate){
        auto n = candidate;
        for(auto prime : primes){
            while (!(n%prime)){
                n/=prime;
            }
        }
        if(n==1) ++count;
        if(!(count%1000)) std::cout << "\rgot " << count;
    }
    std::cout << std::endl;
    std::cout << candidate << std::endl;
}
