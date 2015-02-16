#include "npriorityqueue.hpp"
#include <iostream>
#include <array>

using namespace std;

void fill(NPriorityQueue<char, 3, float>& npq){
    npq.enqueue('A', {{20, 2,19}});
    npq.enqueue('A', {{26,10,28}});
    npq.enqueue('C', {{15,30,11}});
    npq.enqueue('D', {{19, 1,18}});
    npq.enqueue('E', {{14,27,16}});
    npq.enqueue('E', {{18,23,31}});
    npq.enqueue('F', {{12,17, 8}});
    npq.enqueue('G', {{ 9, 9,26}});
    npq.enqueue('G', {{29,16, 6}});
    npq.enqueue('I', {{31,14, 2}});
    npq.enqueue('I', {{ 7,25, 4}});
    npq.enqueue('I', {{27,29,20}});
    npq.enqueue('K', {{ 6, 4, 3}});
    npq.enqueue('K', {{16,31,12}});
    npq.enqueue('L', {{10,11,15}});
    npq.enqueue('L', {{17,24,21}});
    npq.enqueue('M', {{21,26,29}});
    npq.enqueue('M', {{32, 8,30}});
    npq.enqueue('N', {{ 8,15, 5}});
    npq.enqueue('O', {{22,18, 7}});
    npq.enqueue('O', {{ 5, 6,10}});
    npq.enqueue('O', {{ 3, 7,25}});
    npq.enqueue('P', {{ 1,20,23}});
    npq.enqueue('R', {{ 2, 3,24}});
    npq.enqueue('R', {{13, 5, 9}});
    npq.enqueue('R', {{24,19,27}});
    npq.enqueue('R', {{28,22,32}});
    npq.enqueue('R', {{30,28,13}});
    npq.enqueue('S', {{25,32,17}});
    npq.enqueue('U', {{23,21,14}});
    npq.enqueue('V', {{ 4,12, 1}});
    npq.enqueue('Y', {{11,13,22}});
}

template<size_t N>
void empty(NPriorityQueue<char, 3, float>& npq){
    cout << "count = " << npq.count() << endl;
    while(npq.count()!=0){
        cout << npq.dequeue<N>() << flush;
    }
    cout << endl;
    cout << "count = " << endl;
}

int main(){
    NPriorityQueue<char, 3, float> npq;
    fill(npq);
    empty<0>(npq);
    fill(npq);
    empty<1>(npq);
    fill(npq);
    empty<2>(npq);
}
