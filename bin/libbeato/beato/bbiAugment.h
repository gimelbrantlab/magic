/* This is a collection of routines that are basically copy-paste-then-altered */
/* routines from bbi*.h/c in Jim Kent's source.  Sometimes that library doesn't */
/* do excactly what we want, but for the sake of code duplication, it's best to */
/* keep what's here to a minimum. */

#ifndef BBIAUGMENT_H
#define BBIAUGMENT_H

struct bbiFile *bbiFileOpenWithDir(char *fileName, bits32 sig, char *typeName, char *udcDir);
/* same (mostly) as bbiFileOpen in bbiFile.c, but allows setting the temporary dir */

struct bbiFile *bigWigFileOpenWithDir(char *fileName, char *udcDir);
/* Open up big wig file with a specified temporary UDC dir. */

#endif
