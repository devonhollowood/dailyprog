#include <set>
#include <array>
#include <iostream>

using bigint = unsigned long long int;

int main(){
    size_t target_n = 1000000;
    std::array<int, 8> primes {{2,3,5,7,11,13,17,19}};
    std::set<bigint> outer_shell = {1};
    for(size_t n=1; n<target_n; ++n){
        auto min = *outer_shell.begin();
        outer_shell.erase(min);
        for(auto prime : primes) outer_shell.insert(prime*min);
    }
    std::cout << *outer_shell.begin() << std::endl;
}
