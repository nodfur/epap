#include <unistd.h>

int
main (int argc, char **argv)
{
  return execvp (argv[1], argv + 1);
}
