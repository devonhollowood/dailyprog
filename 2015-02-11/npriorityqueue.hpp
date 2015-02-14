#include <array>
#include <list>
#include <deque>
#include <utility>
#include <algorithm>
#include <functional>

template <typename ValType, size_t NQueues, typename PriorityType>
class NPriorityQueue{
public:
    //types
    using PriorityList = std::array<PriorityType, NQueues>;

    //functions
    NPriorityQueue& enqueue(ValType value, PriorityList priorities){
        elements_.push_front(value, priorities);
        for(size_t N=0; N<priorities.size(); ++N){
            insert_sorted_(N, elements_.begin());
        }
        return *this;
    }
    template<size_t N> ValType&& dequeue(){
        static_assert(N<NQueues, "dequeue()'s N out of bounds.");
        auto element = *priorities_[N].front();
        for(size_t listN = 0; listN<priorities_.size(); ++listN){
            auto to_erase = find_sorted_(N, element);
            priorities_[N].erase(to_erase);
        }
    }
    size_t count(){return elements_.size();}
    void clear(){elements_.clear(); priorities_.clear();}

private:
    //types
    using Element = std::pair<ValType, PriorityList>;
    using pElement = typename std::list<Element>::iterator;
    using PriorityListItr = typename std::deque<pElement>::iterator;

    //functions
    std::function<bool(const pElement&, const pElement&)> compare_(size_t N) const{
        return [N](const pElement& a, const pElement& b){
            return a->second[N] < b->second[N];
        };
    }

    void insert_sorted_(size_t N, pElement ptr){
        auto list = priorities_[N];
        auto position = std::lower_bound(list.cbegin(), list.cend(), 
                compare_(N));
        priorities_.at(N).insert(position, ptr);
    }

    PriorityListItr find_sorted_(size_t N, Element el) const{
        auto list = priorities_[N];
        auto range = std::equal_range(list.begin(), list.end(), 
                compare_(N));
        for(auto itr=range.first; itr!=range.second; ++itr){
            if(**itr==el) return itr;
        }
        return list.end(); //if not found
    }

    //members
    std::list<Element> elements_;
    std::array<std::deque<pElement>, NQueues> priorities_;
};

