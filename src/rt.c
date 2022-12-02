#include <stdio.h>
#include <stdint.h>

struct String {
  uint64_t size;
  const char *data;
};

void println(struct String *str) {
  printf("%.*s\n", str->size, str->data);
}

