all : copying-callbacks.dylib

copying-callbacks.dylib : copying-callbacks.o
	raco ctool --ld copying-callbacks.dylib copying-callbacks.o

copying-callbacks.o : copying-callbacks.c
	raco ctool --cc copying-callbacks.c
