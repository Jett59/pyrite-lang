#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>

struct String {
  uint64_t size;
  const char *data;
};

void printSLn(struct String *str) { printf("%.*s\n", str->size, str->data); }
void printILn(int64_t value) {
  printf("%" PRId64 "\n", value);
}
