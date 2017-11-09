#include <jkweb/common.h>
#include <jkweb/linefile.h>
#include <jkweb/hash.h>
#include <jkweb/obscure.h>
#include <jkweb/zlibFace.h>
#include <jkweb/bPlusTree.h>
#include <jkweb/hmmstats.h>
#include <jkweb/cirTree.h>
#include <jkweb/udc.h>
#include <jkweb/bbiFile.h>
#include <jkweb/sig.h>
#include <beato/bbiAugment.h>

struct bbiFile *bbiFileOpenWithDir(char *fileName, bits32 sig, char *typeName, char *udcDir)
/* same (mostly) as bbiFileOpen in bbiFile.c, but allows setting the temporary dir */
{
    struct bbiFile *bbi;
    AllocVar(bbi);
    bbi->fileName = cloneString(fileName);
    struct udcFile *udc = bbi->udc = udcFileOpen(fileName, udcDir);
    /* Read magic number at head of file and use it to see if we are proper file type, and
     * see if we are byte-swapped. */
    bits32 magic;
    boolean isSwapped = FALSE;
    udcMustRead(udc, &magic, sizeof(magic));
    if (magic != sig)
    {
	magic = byteSwap32(magic);
	isSwapped = TRUE;
	if (magic != sig)
	    errAbort("%s is not a %s file", fileName, typeName);
    }
    bbi->typeSig = sig;
    bbi->isSwapped = isSwapped;

/* Read rest of defined bits of header, byte swapping as needed. */
    bbi->version = udcReadBits16(udc, isSwapped);
    bbi->zoomLevels = udcReadBits16(udc, isSwapped);
    bbi->chromTreeOffset = udcReadBits64(udc, isSwapped);
    bbi->unzoomedDataOffset = udcReadBits64(udc, isSwapped);
    bbi->unzoomedIndexOffset = udcReadBits64(udc, isSwapped);
    bbi->fieldCount = udcReadBits16(udc, isSwapped);
    bbi->definedFieldCount = udcReadBits16(udc, isSwapped);
    bbi->asOffset = udcReadBits64(udc, isSwapped);
    bbi->totalSummaryOffset = udcReadBits64(udc, isSwapped);
    bbi->uncompressBufSize = udcReadBits32(udc, isSwapped);
    bbi->extensionOffset = udcReadBits64(udc, isSwapped);

/* Read zoom headers. */
    int i;
    struct bbiZoomLevel *level, *levelList = NULL;
    for (i=0; i<bbi->zoomLevels; ++i)
    {
	AllocVar(level);
	level->reductionLevel = udcReadBits32(udc, isSwapped);
	level->reserved = udcReadBits32(udc, isSwapped);
	level->dataOffset = udcReadBits64(udc, isSwapped);
	level->indexOffset = udcReadBits64(udc, isSwapped);
	slAddHead(&levelList, level);
    }
    slReverse(&levelList);
    bbi->levelList = levelList;

/* Deal with header extension if any. */
    if (bbi->extensionOffset != 0)
    {
	udcSeek(udc, bbi->extensionOffset);
	bbi->extensionSize = udcReadBits16(udc, isSwapped);
	bbi->extraIndexCount = udcReadBits16(udc, isSwapped);
	bbi->extraIndexListOffset = udcReadBits64(udc, isSwapped);
    }

/* Attach B+ tree of chromosome names and ids. */
    udcSeek(udc, bbi->chromTreeOffset);
    bbi->chromBpt =  bptFileAttach(fileName, udc);

    return bbi;
}

struct bbiFile *bigWigFileOpenWithDir(char *fileName, char *udcDir)
/* Open up big wig file with a specified temporary UDC dir. */
{
    return bbiFileOpenWithDir(fileName, bigWigSig, "big wig", udcDir);
}
