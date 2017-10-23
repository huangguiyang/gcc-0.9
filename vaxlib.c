/* Subroutines needed by Vax code produced by the GNU compiler.  */
/* Compile this file with the Unix C compiler!  */

umulsi3 (a, b)
     unsigned a, b;
{
  return a * b;
}

udivsi3 (a, b)
     unsigned a, b;
{
  return a / b;
}

umodsi3 (a, b)
     unsigned a, b;
{
  return a % b;
}

lshrsi3 (a, b)
     unsigned a, b;
{
  return a >> b;
}
