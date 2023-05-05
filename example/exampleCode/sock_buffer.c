#include<stdio.h>
#include <stdint.h>

#define ARRAY_LENGTH 40
/*
struct test_md
{
    uint8_t data_start;
    uint8_t data_end;
    uint8_t array[ARRAY_LENGTH];
}; */

uint32_t foo(uint8_t data_start, uint8_t data_end, uint8_t* array, uint32_t len)
{
    uint32_t index;
    uint32_t cumul = 0;


    for (index = 0U; index < len; index++) {
        if ((data_start + index) >= data_end)
            break;

        array[index] = 1;
    }

    for (index = 0U; index < len; index++) {
        cumul += array[index];
    }
    return cumul;
}
