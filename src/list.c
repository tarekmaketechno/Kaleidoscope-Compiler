#include "list.h"

void append_item(list_item_t * list, void * newthing) {

	list_item_t * new_list_item = 
		(list_item_t *) malloc(sizeof(list_item_t));

	new_list_item->current = newthing;
	new_list_item->next = NULL;

	while(list->next != NULL) {
		list = list->next;
	}

	list->next = new_list_item;
}


list_item_t * create_list(void * newthing) {

	list_item_t * first_item = 
		(list_item_t *) malloc(sizeof(list_item_t));


	first_item->current = newthing;
	first_item->next = NULL;

	return first_item;
}
