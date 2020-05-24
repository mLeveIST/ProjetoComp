
int power(int base, int exp)
{
  int i = exp;
  int res = 1;

  while (i > 0)
  {
    res *= base;
    i--;
  }

  return res;
}

int strcompr(const char *a, const char *b)
{
  unsigned char c1, c2;

  while (1)
  {
    c1 = *a++;
    c2 = *b++;

    if (c1 != c2)
      return c1 < c2 ? -1 : 1;
    
    if (!c1)
      break;
  }

  return 0;
}

