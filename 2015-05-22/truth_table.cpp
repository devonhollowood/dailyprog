#include <iostream>
#include <iomanip>
#include <string>
#include <type_traits>

template <typename T>
typename std::enable_if<std::is_convertible<T, bool>::value, const char*>::type
truthiness(T t) {
    return t ? "True" : "False";
}

template <typename T>
typename std::enable_if<!std::is_convertible<T, bool>::value, const char*>::type
truthiness(T) {
    return "Not convertible";
}

template <typename T>
void test_truthiness(std::string repr, T t) {
    std::cout << std::setw(15) << repr << ": " << truthiness(t) << std::endl;
}

struct my_struct{};

int main(){
    test_truthiness("\"Hello World\"", "Hello World");
    test_truthiness("\"\"", "");
    test_truthiness("\'0\'", '0');
    test_truthiness("1", 1);
    test_truthiness("0", 0);
    test_truthiness("0.0", 0.0);
    test_truthiness("nullptr", nullptr);
    test_truthiness("&main", &main);
    test_truthiness("true", true);
    test_truthiness("false", false);
    test_truthiness("Empty Struct", my_struct{});
}
