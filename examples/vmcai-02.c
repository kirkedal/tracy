int absValue( int input )
{
  int sign, abs;
  if ( input == 0 )
  {
    return 0;
  }

  if ( input < 0 )
  {
    sign = -1;
    printf( "negative" );
  }
  else
  {
    sign = 1;
    printf( "positive" );
  }

  if ( sign == -1 )
  {
    sign = input*-1;
  }
  else
  {
    abs = input;
  }

  assert( abs >= 0 );
  return abs;
}
