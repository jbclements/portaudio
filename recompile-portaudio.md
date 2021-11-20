# Recompiling portaudio

As of 2021-11-19, here's what I had to do to recompile portaudio

## MacOSX

Download http://files.portaudio.com/archives/pa_stable_v190700_20210406.tgz
Unpack
cd portaudio
mkdir jbcbuild
cd jbcbuild
../configure
make

the generated binary is in lib/.libs/, hidden to prevent ... mischief?

## Common

diff ~/portaudio/portaudio/portaudio.rkt ../include/portaudio.h 

... any lines starting with > are lines that changed in the header file,
repair them and address any changes that need to be made in the racket
code as a result of changes in the header file.
