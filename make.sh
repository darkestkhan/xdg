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
  gprbuild -p xdg.gpr
  echo ""
}

# Build the tests
function make_tests
{
  gprbuild -p xdg-tests.gpr
  echo ""
}

# Clean up files generated by compilation process
function clean
{
  gprclean xdg.gpr
  gprclean xdg-tests.gpr
}

if test $# -eq 0
then
  echo "No valid argument specified"
  echo ""
  echo "./make.sh xdg     to build library only"
  echo "./make.sh check   to build tests and run them"
  echo "./make.sh all     to build library, tests and run the tests"
  echo "./make.sh clean   to clean up all files generated by build process"
  exit 0
fi

if test $1 = "xdg"
then
  clean
  make
elif test $1 = "check"
then
  clean
  make_tests
  run_tests
elif test $1 = "all"
then
  clean
  make
  make_tests
  run_tests
elif test $1 = "clean"
then
  clean
fi

