#include "AsyncLoader.h"

#include <thread>
#include <memory>

constexpr std::size_t MAX_THREAD_POOL_SIZE = 8;

void LoadModelGeometry(std::vector<std::shared_ptr<Model>> models, nlohmann::json layout, nlohmann::json geometry_info)
{
	//in a new thread:
	// create task queue, one item for each piece of geometry
	// create results queue
	// create appropriately sized thread pool
	//  each thread in the pool takes the next task and works on it
	//  when it is done, it will check for more tasks. if none, exit
	// main thread reads off results and adds them to models until all threads terminate
	
	//need: synchronisation primitives for both queues
}
