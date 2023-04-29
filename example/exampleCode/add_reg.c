#include<stdio.h>

unsigned fletcher32(unsigned x, unsigned y, unsigned z)
{
    unsigned sum0, sum1, sum2, sum3, sum4, sum5, sum;
    //unsigned sum0, sum;

    sum0 = x + y;
    sum = sum0 + z;
    sum2 = sum0 + sum1;
    sum3 = sum1 + sum2;
    sum4 = sum2 + sum3;
    sum5 = sum3 + sum4;
    sum  = sum5 + sum0;
    return sum;
}
