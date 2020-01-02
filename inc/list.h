#pragma once

#include <stdarg.h>
#include <stdlib.h>

// TODO - Separate this lib out properly
// this is shameful but it works for now
// should be able to reuse lists in other
// tree structures 


typedef struct list_item_t list_item_t;

struct list_item_t {
	void * current;
	list_item_t * next;
};

void append_item(list_item_t * list, void * newthing); 

list_item_t * create_list(void * newthing);
list_item_t * create_list_v(int count, ...);
