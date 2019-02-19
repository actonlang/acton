# How to create a test

* Copy the `test_example` directory, naming it to something that relates to what you want to test.
  The prefix `test_` must be retained.
  Rename the `test_example.c` file inside the same way (the names must be the same)
  For this example let's call them `test_foo` and `test_foo.c`.
* Edit `test_foo/Makefile` to include the necessary source files (the SRC variable) and/or
  static libraries (LIBS).
  Also set CFLAGS and LDFLAGS as needed.
* Write the testing code in `test_foo.c`.
  See https://github.com/ThrowTheSwitch/Unity for overview of the various assert macros that exist.
* Run `make`.
