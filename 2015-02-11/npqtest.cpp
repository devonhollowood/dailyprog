#include "npriorityqueue.hpp"
#include <iostream>
#include <array>

using namespace std;

void fill(NPriorityQueue<char, 3, float> npq){
    npq.enqueue('a', {1,2,3});
    npq.enqueue('b', {2,3,1});
    npq.enqueue('b', {3,1,2});
}

template<size_t N>
void empty(NPriorityQueue<char, 3, float> npq){
    while(npq.count()!=0){
        cout << npq.dequeue<N>() << endl;
    }
}

int main(){
    NPriorityQueue<char, 3, float> npq;
    fill(npq);
    empty<1>(npq);
    fill(npq);
    empty<2>(npq);
    fill(npq);
    empty<3>(npq);
}
