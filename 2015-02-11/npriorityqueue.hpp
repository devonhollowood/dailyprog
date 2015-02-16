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
    
    /* Adds _value_ with priorities listed in _priorities_ */
    NPriorityQueue& enqueue(ValType value, PriorityList priorities){
        elements_.push_front({value, priorities});
        for(size_t N=0; N<priorities_.size(); ++N){
            insert_sorted_(N, elements_.begin());
        }
        return *this;
    }
    /* Removes the highest-priority element from list N */
    template<size_t N> ValType&& dequeue(){
        static_assert(N<NQueues, "dequeue()'s N out of bounds.");
        auto pel = priorities_[N].front(); //pointer to element
        for(size_t listN = 0; listN<priorities_.size(); ++listN){
            auto to_erase = find_sorted_(listN, pel);
            priorities_[listN].erase(to_erase);
        }
        elements_.erase(pel);
        return std::move(pel->first);
    }

    /* Returns number of elements held */
    size_t count(){return elements_.size();}

    /* Empties the queue */
    void clear(){
        for(auto& list : priorities_){
            list.clear();
        }
        elements_.clear(); 
    }

private:
    //types
    using Element = std::pair<ValType, PriorityList>;
    using pElement = typename std::list<Element>::iterator;
    using PriorityListItr = typename std::deque<pElement>::iterator;

    //functions
    /* Returns function for comparing elements of list _N_ */
    std::function<bool(const pElement&, const pElement&)> compare_(size_t N) 
            const{
        return [N](const pElement& a, const pElement& b){
            return a->second[N] < b->second[N];
        };
    }

    /* Inserts _pel_ into list _N_ in sorted order */
    void insert_sorted_(size_t N, pElement pel){//pel = pointer to element
        auto& list = priorities_[N];
        auto position = std::upper_bound(list.begin(), list.end(), pel,
                compare_(N));
        priorities_[N].insert(position, pel);
    }

    /* Finds _pel_ in list _N_. Returns iterator-to-last if not found. */
    PriorityListItr find_sorted_(size_t N, pElement pel){
        auto& list = priorities_[N];
        auto range = std::equal_range(list.begin(), list.end(), pel, 
                compare_(N));
        for(auto itr=range.first; itr!=range.second; ++itr){
            if(*itr==pel) return itr;
        }
        return list.end(); //if not found
    }

    //members
    std::list<Element> elements_;
    std::array<std::deque<pElement>, NQueues> priorities_;
};

