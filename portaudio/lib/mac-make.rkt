#lang racket

;; tired of makefile, but actually, this is just the very beginning of a make script....

(define min-version-flag "-mmacosx-version-min=10.5")

(define flags
  (list #;"-arch" #;"x86_84" min-version-flag))

(define gcc
  (build-path "/usr/bin/gcc"))

(when (file-exists? "callbacks.o")
  (delete-file "callbacks.o"))
(apply system* gcc (append (list "-o" "callbacks.o") flags (list "-c" "callbacks.c")))
(apply system* gcc `("-o" "librsoundcallbacks.dylib" "-dynamiclib" ,@flags "callbacks.o"))

;all : x86_64;
;
;BCTGT = x86_64-macosx/3m
;CSTGT = x86_64-macosx/cs
;FLAGS =  -arch x86_64 -mmacosx-version-min=10.5
;
;x86_64 : $(BCTGT)/callbacks.dylib $(CSTGT)/callbacks.dylib
;
;$(CSTGT)/callbacks.dylib : $(CSTGT)/callbacks.o
;	gcc -o $(CSTGT)/callbacks.dylib -dynamiclib $(FLAGS) $(CSTGT)/callbacks.o
;
;$(CSTGT)/callbacks.o : callbacks.c
;	gcc -c -o $(CSTGT)/callbacks.o $(FLAGS) callbacks.c
;
;# they're just the same, copy the file across:
;$(BCTGT)/callbacks.dylib : $(CSTGT)/callbacks.dylib
;	cp $(CSTGT)/callbacks.dylib $(BCTGT)/callbacks.dylib
