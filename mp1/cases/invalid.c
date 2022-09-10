#include<stdio.h>
int main(){
    putchar('"');
    for(int i=0;i<32;i++)
        putchar(i);
    putchar('"');
}