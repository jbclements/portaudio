#lang scribble/doc

@(require scribble/manual
          planet/scribble)

@title{@bold{Portaudio}: Bindings for the Portaudio portable sound library}

@author[(author+email "John Clements" "clements@racket-lang.org")]

@(require (for-label racket))

@defmodule[(planet clements/portaudio)]{This collection provides 
 bindings to the cross-platform ``Portaudio'' library, capable of playing 
 sound on Windows, OS X, and Linux. 
 
 This planet package provides more or less raw bindings, and it's definitely 
 possible to crash racket using these functions. See the 
 @racket[clements/rsound] planet library for a higher-level, believed-safe 
 interface.
 
 This package includes the portaudio dynamic libraries for Windows and Mac, 
 where I believe that users will have trouble compiling and installing 
 such a package.  On Linux, users must install it themselves.
 
 In addition, there is a small C library that provides interface code.
 This is provided in compiled form for all platforms, though come to 
 think of it, I may have omitted 32-bit linux. Hmm...
 
 Cheers!}

