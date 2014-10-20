void shell_sort( int a[], int size )
{
  int h, i, j, v;
  h = 1;
  do
  {
    h = h*3;
  }
  while (h <= size);
  do
  {
    h /= 3;
    for ( i = h; i < size; ++i )
    {
      v = a[i];
      for ( j = i; j >= h && a[j-h] > v; j-=h ) {
        if ( i != j )
          a[j] = v;
      }
    }
  }
  while (h != 1);
}
