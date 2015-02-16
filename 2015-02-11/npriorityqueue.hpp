#include <array>
#include <list>
#include <deque>
#include <utility>
#include <algorithm>
#include <functional>
//#include <iostream> //debug

template <typename ValType, size_t NQueues, typename PriorityType>
class NPriorityQueue{
public:
    //types
    using PriorityList = std::array<PriorityType, NQueues>;

    //functions
    NPriorityQueue& enqueue(ValType value, PriorityList priorities){
        //std::cout << "Enqueueing " << value << std::endl; //debug
        elements_.push_front({value, priorities});
        for(size_t N=0; N<priorities_.size(); ++N){
            insert_sorted_(N, elements_.begin());
        }
        return *this;
    }
    template<size_t N> ValType&& dequeue(){
        static_assert(N<NQueues, "dequeue()'s N out of bounds.");
        //std::cout << "dequeueing " << N << std::endl; //debug
        auto pelement = priorities_[N].front();
        //std::cout << "...element is " << pelement->first << std::endl; //debug
        for(size_t listN = 0; listN<priorities_.size(); ++listN){
            auto to_erase = find_sorted_(listN, pelement);
            //std::cout << "...erasing" << std::endl; //debug
            priorities_[listN].erase(to_erase);
        }
        elements_.erase(pelement);
        return std::move(pelement->first);
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
            PriorityType ap = a->second[N];
            PriorityType bp = b->second[N];
            return ap<bp;
        };
    }

    void insert_sorted_(size_t N, pElement ptr){
        //std::cout << "...inserting " << ptr->first << " into " << N //debug
            //<< " with priority " << ptr->second[N] << std::endl; //debug
        auto& list = priorities_[N];
        auto position = std::upper_bound(list.begin(), list.end(), ptr,
                compare_(N));
        priorities_[N].insert(position, ptr);
    }

    PriorityListItr find_sorted_(size_t N, pElement pel){
        //std::cout << "...finding in " << N << std::endl; //debug
        auto& list = priorities_[N];
        auto range = std::equal_range(list.begin(), list.end(), pel, 
                compare_(N));
        for(auto itr=range.first; itr!=range.second; ++itr){
            //if(*itr==pel) std::cout << "...found." << std::endl; //debug
            if(*itr==pel) return itr;
        }
        //std::cout << "...not found" << std::endl; //debug
        return list.end(); //if not found
    }

    //members
    std::list<Element> elements_;
    std::array<std::deque<pElement>, NQueues> priorities_;
};

