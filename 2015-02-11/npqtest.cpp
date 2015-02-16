#include "npriorityqueue.hpp"
#include <iostream>
#include <array>

using namespace std;

void fill(NPriorityQueue<char, 3, float>& npq){
    cout << "filling." << endl;
    npq.enqueue('A', {{20, 2,11}});
    npq.enqueue('A', {{26,10,15}});
    npq.enqueue('C', {{15,30, 7}});
    npq.enqueue('D', {{19, 1,10}});
    npq.enqueue('E', {{14,27, 9}});
    npq.enqueue('E', {{18,23,16}});
    npq.enqueue('F', {{12,17, 5}});
    npq.enqueue('G', {{ 9, 9, 4}});
    npq.enqueue('G', {{29,16,14}});
    npq.enqueue('I', {{31,14, 2}});
    npq.enqueue('I', {{ 7,25, 3}});
    npq.enqueue('I', {{27,29,11}});
    npq.enqueue('K', {{ 6, 4, 2}});
    npq.enqueue('K', {{16,31, 7}});
    npq.enqueue('L', {{10,11, 8}});
    npq.enqueue('L', {{17,24,11}});
    npq.enqueue('M', {{21,26,15}});
    npq.enqueue('M', {{32, 8,15}});
    npq.enqueue('N', {{ 8,15, 3}});
    npq.enqueue('O', {{22,18, 4}});
    npq.enqueue('O', {{ 5, 6,13}});
    npq.enqueue('O', {{ 3, 7, 6}});
    npq.enqueue('P', {{ 1,20,12}});
    npq.enqueue('R', {{ 2, 3, 5}});
    npq.enqueue('R', {{13, 5, 7}});
    npq.enqueue('R', {{24,19,12}});
    npq.enqueue('R', {{28,22,14}});
    npq.enqueue('R', {{30,28,16}});
    npq.enqueue('S', {{25,32, 9}});
    npq.enqueue('U', {{23,21, 7}});
    npq.enqueue('V', {{ 4,12, 1}});
    npq.enqueue('Y', {{11,13,11}});
}

void test_clear(NPriorityQueue<char, 3, float>& npq){
    cout << "count = " << npq.count() << endl;
    cout << "clearing." << endl;
    npq.clear();
    cout << "count = " << npq.count() << endl;
}

template<size_t N>
void dequeue_all(NPriorityQueue<char, 3, float>& npq){
    cout << "count = " << npq.count() << endl;
    cout << "dequeueing list " << N << '.' << endl;
    while(npq.count()!=0){
        cout << npq.dequeue<N>() << flush;
    }
    cout << endl;
    cout << "count = " << npq.count() << endl;
}

int main(){
    NPriorityQueue<char, 3, float> npq;
    fill(npq);
    test_clear(npq);
    fill(npq);
    dequeue_all<0>(npq);
    fill(npq);
    dequeue_all<1>(npq);
    fill(npq);
    dequeue_all<2>(npq);
}
