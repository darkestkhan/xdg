#!/usr/bin/env bash

# Run the tests and count failed tests.
function run_tests ()
{
  x=0

  for file in tests/bin/*
  do
    if test -x ${file}
    then
      ./${file}
    fi
    if test 0 -ne $?
    then
      x=$((x+=1))
    fi
  done

  echo "Number of failed tests: $x"
}

# Build the library
function make ()
{
  mkdir lib obj
  gprbuild <+Main_Project+>.gpr
  echo ""
}

# Build the tests
function make_tests
{
  mkdir tests/obj tests/bin
  gprbuild <+Main_Project+>-tests.gpr
  echo ""
}

if test $1 = "<+Main_Project+>"
then
  make
elif test $1 = "check"
then
  make_tests
  run_tests
elif test $1 = "all"
then
  make
  make_tests
  run_tests
fi

if test $# -eq 0
then
  echo "No valid argument specified"
  echo ""
  echo "./make.sh <+Main_Project+> to build library only"
  echo "./make.sh check     to build tests and run them"
  echo "./make.sh all       to build library, tests and run the tests"
fi
