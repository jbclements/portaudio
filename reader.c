#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <scheme.h>


unsigned long findElements(Scheme_Object *arg, Scheme_Object *immobile){
  fprintf(stderr,"arg ptr: %lX\n",(unsigned long)arg);
  fprintf(stderr,"immobile ptr: %lX\n",(unsigned long)immobile);
  if (SCHEME_CPTRP(immobile)) {
    fprintf(stderr,"yes it's a cpointer\n");
    fprintf(stderr,"CPTR_VAL: %lX\n",SCHEME_CPTR_VAL(immobile));
    fprintf(stderr,"CPTR_TYPE: %lX\n",SCHEME_CPTR_TYPE(immobile));
    // try dereferencing the CPTR_Val?
    fprintf(stderr,"word at deref of CPTR_VAL: %lX\n",*((unsigned long *)SCHEME_CPTR_VAL(immobile)));
  }
  if (SCHEME_STRUCTP(arg)){
  void *data = scheme_struct_ref(arg,0);
  Scheme_Object *curPosnField = scheme_struct_ref(arg,1);
  if (SCHEME_INTP(scheme_struct_ref(arg,1))) {
    intptr_t curPosition = SCHEME_INT_VAL(curPosnField);
    return curPosition;
  } else {
    fprintf(stderr,"not a fixnum.\n");
    return 0;
  }
  } else {
    fprintf(stderr,"not a struct!\n");
    return 0;
  }
}
