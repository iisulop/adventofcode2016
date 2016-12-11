#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#ifdef PART2
#define NUMROWS 3
#define MAXBUF 128
#endif // PART2

char valid_triangle (char* line)
{
  char ret = 0;
  unsigned int sides[3] = {0};
  unsigned char i = 0;
  sscanf (line, "%u %u %u", &(sides[0]), &(sides[1]), &(sides[2]));

  for (i = 0; i < 3; ++i)
    {
      if ((sides[0] + sides[1]) > sides[2] &&
          (sides[0] + sides[2]) > sides[1] &&
          (sides[1] + sides[2]) > sides[0])
        {
          ret = 1;
          break;
        }
    }

  return ret;
}

int main (int argc, char* argv[])
{
  char* line = NULL;
  unsigned int count = 0;
  size_t n = 0;
  FILE* f = NULL;
#ifdef PART2
  unsigned char rows = 0, i = 0, j = 0;
  unsigned char* lines[NUMROWS] = {0};
  unsigned int sides[3][3] = {0};
#endif // PART2

  if (argc != 2)
    {
      printf ("Usage: %s filename\n", argv[0]);
      return 1;
    }

  f = fopen(argv[1], "r");

  while (getline (&line, &n, f) > 0)
    {
#ifdef PART2
      lines[rows] = strdup (line);
      rows++;

      if (rows == NUMROWS)
        {
          for (i = 0; i < NUMROWS; ++i)
            {
              sscanf (lines[i], "%d %d %d",
                      &(sides[i][0]), &(sides[i][1]), &(sides[i][2]));
            }
          for (j = 0; j < NUMROWS; ++j)
            {
              free (line);
              line = malloc(MAXBUF);
              sprintf (line, "%d %d %d",
                       sides[0][j], sides[1][j], sides[2][j]);
#endif // PART2
              if (valid_triangle (line))
                {
                  count++;
                }
#ifdef PART2
            }
          free (lines[0]);
          lines[0] = NULL;
          free (lines[1]);
          lines[1] = NULL;
          free (lines[2]);
          lines[2] = NULL;
          rows = 0;
        }
#endif // PART2

      free (line);
      line = NULL;
    }

  printf ("Number of triangles: %d\n", count);

  return 0;
}
