#!/bin/sh
tail -n +4 $0 | gcc -Wall -o /tmp/cscript.$$ -x c - && /tmp/cscript.$$ $*
ret=$? ; rm -f /tmp/cscript.$$ ; exit $ret
//
// Code C
//
#include <stdio.h>

int main(int argc, char** argv)
{
  printf("hello, world");
  return 0;
}
