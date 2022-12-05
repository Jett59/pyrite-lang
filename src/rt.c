#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>

struct String {
  uint64_t size;
  const char *data;
};

void printS(struct String *str) { printf("%.*s", (int)str->size, str->data); }
void printI(int64_t value) { printf("%" PRId64, value); }
