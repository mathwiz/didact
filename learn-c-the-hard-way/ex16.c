#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

struct Person {
  char *name;
  int age;
  int height;
  int weight;
};

struct Person *Person_create(char *name, int age, int height, int weight) {
  struct Person *who = malloc(sizeof(struct Person));
  assert(who != NULL);

  who->name = strdup(name);
  who->age = age;
  who->height = height;
  who->weight = weight;

  return who;
}

void Person_update(struct Person *who, int age, int height, int weight) {
  who->age += age;
  who->height += height;
  who->weight += weight;
}

void Person_destroy(struct Person *who) {
  //assert(who != NULL);
  printf("Destroying %s\n", who->name);
  free(who->name);
  free(who);
}

void Person_print(struct Person *who) {
  printf("Name: %s\n", who->name);
  printf("Age: %d\n", who->age);
  printf("Height: %d\n", who->height);
  printf("Weight: %d\n", who->weight);
}

void Person_print_location(struct Person *who) {
  printf("%s is at memory location %p\n", who->name, who);
}

int main(int argc, char *argv[]) {
  printf("\nCreating people\n");

  struct Person *joe = Person_create("Joe Alex", 32, 64, 140);
  struct Person *frank = Person_create("Frank Blank", 20, 72, 180);

  Person_print_location(joe);
  Person_print(joe);

  Person_print_location(frank);
  Person_print(frank);

  printf("\nUpdating people\n");

  Person_print_location(joe);
  Person_update(joe, 20, -2, 40);
  Person_print(joe);

  Person_print_location(frank);
  Person_update(frank, 20, 0, 0);
  Person_print(frank);

  printf("\nDestroying people\n");
  Person_destroy(joe);
  Person_destroy(frank);

  return 0;
}
