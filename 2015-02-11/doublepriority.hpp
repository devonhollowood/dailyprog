#include <utility>
#include <forward_list>

template<typename T>
class DPQNode{
public:
    using pDPQNode = typename std::forward_list<DPQNode<T>>::iterator;
    DPQNode(T value, pDPQNode leftA, pDPQNode leftB, pDPQNode rightA, 
            pDPQNode rightB) : 
        value_(std::move(value)),
        leftA_(std::move(leftA)),
        rightA_(std::move(rightA)),
        leftB_(std::move(leftB)),
        rightB_(std::move(rightB)) {}
    T value() const {return value;}
private:
    T value_;
    pDPQNode leftA_;
    pDPQNode rightA_;
    pDPQNode leftB_;
    pDPQNode rightB_;
};

template<typename T>
class DoublePriorityQueue{
public:
    DoublePriorityQueue() : count_(0) {}
    DoublePriorityQueue& enqueue(T value, float priorityA, float priorityB);
    T&& dequeueA();
    T&& dequeueB();
    size_t count() {return count_;}
    void clear() {nodes_.clear(); count_=0;}
private:
    using pDPQNODE = typename DPQNode<T>::pDPQNode;
    std::forward_list<DPQNode<T>> nodes_;
    size_t count_;
};
