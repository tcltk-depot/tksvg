/*
 * tkImgSVGnano.c
 *
 *	A photo file handler for SVG files.
 *
 * Copyright (c) 2013-14 Mikko Mononen memon@inside.org
 * Copyright (c) 2018 Christian Gollwitzer auriocus@gmx.de
 * Copyright (c) 2018 Christian Werner https://www.androwish.org/
 * Copyright (c) 2018 Rene Zaumseil r.zaumseil@freenet.de
 *
 * See the file "license.terms" for information on usage and redistribution of
 * this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * This handler is build using the original nanosvg library files from
 * https://github.com/memononen/nanosvg
 *
 */

/* vim: set ts=8 sts=4 sw=4 : */

#include <tcl.h>
#ifndef MODULE_SCOPE
#define MODULE_SCOPE extern
#endif
#include <stdio.h>
#include <string.h>
#include <limits.h>
#ifdef _MSC_VER
#define strcasecmp _stricmp
#if _MSC_VER < 1300
#define roundf(x) floorf((x) + 0.5)
#endif
#endif
#include <math.h>
#include <float.h>
#define NANOSVG_malloc	ckalloc
#define NANOSVG_realloc	ckrealloc
#define NANOSVG_free	ckfree
#define NANOSVG_SCOPE MODULE_SCOPE
#define NANOSVG_ALL_COLOR_KEYWORDS
#define NANOSVG_IMPLEMENTATION
#include "nanosvg.h"
#define NANOSVGRAST_IMPLEMENTATION
#include "nanosvgrast.h"
#include <tk.h>

/* Adoption to use the original tk core file */
#ifndef TkSizeT
#define TkSizeT int
#endif
#ifndef TCL_IO_FAILURE
#define TCL_IO_FAILURE (-1)
#endif

/* Additional parameters to nsvgRasterize() */

typedef struct {
    double scale;
    int scaleToHeight;
    int scaleToWidth;
} RastOpts;

/*
 * Per interp cache of last NSVGimage which was matched to
 * be immediately rasterized after the match. This helps to
 * eliminate double parsing of the SVG file/string.
 */

typedef struct {
    /* A poiner to remember if it is the same svn image (data)
     * It is a Tcl_Channel if image created by -file option
     * or a Tcl_Obj, if image is created with the -data option
     */
    ClientData dataOrChan;
    Tcl_DString formatString;
    NSVGimage *nsvgImage;
    RastOpts ropts;
} NSVGcache;

/*
 * Per thread information to track registration of the
 * photo image format.
 */

typedef struct {
    int registered;
} ThreadSpecificData;
static Tcl_ThreadDataKey dataKey;

static const void *	MemMem(const void *haystack, size_t haysize,
			       const void *needle, size_t needlen);
static int		FileMatchSVG(Tcl_Channel chan, const char *fileName,
			    Tcl_Obj *format, int *widthPtr, int *heightPtr,
			    Tcl_Interp *interp);
static int		FileReadSVG(Tcl_Interp *interp, Tcl_Channel chan,
			    const char *fileName, Tcl_Obj *format,
			    Tk_PhotoHandle imageHandle, int destX, int destY,
			    int width, int height, int srcX, int srcY);
static int		StringMatchSVG(Tcl_Obj *dataObj, Tcl_Obj *format,
			    int *widthPtr, int *heightPtr, Tcl_Interp *interp);
static int		StringReadSVG(Tcl_Interp *interp, Tcl_Obj *dataObj,
			    Tcl_Obj *format, Tk_PhotoHandle imageHandle,
			    int destX, int destY, int width, int height,
			    int srcX, int srcY);
static NSVGimage *	ParseSVGWithOptions(Tcl_Interp *interp,
			    const char *input, TkSizeT length, Tcl_Obj *format,
			    RastOpts *ropts);
static int		RasterizeSVG(Tcl_Interp *interp,
			    Tk_PhotoHandle imageHandle, NSVGimage *nsvgImage,
			    int destX, int destY, int width, int height,
			    int srcX, int srcY, RastOpts *ropts);
static double		GetScaleFromParameters(NSVGimage *nsvgImage,
			    RastOpts *ropts, int *widthPtr, int *heightPtr);
static NSVGcache *	GetCachePtr(Tcl_Interp *interp);
static int		CacheSVG(Tcl_Interp *interp, ClientData dataOrChan,
			    Tcl_Obj *formatObj, NSVGimage *nsvgImage,
			    RastOpts *ropts);
static NSVGimage *	GetCachedSVG(Tcl_Interp *interp, ClientData dataOrChan,
			    Tcl_Obj *formatObj, RastOpts *ropts);
static void		CleanCache(Tcl_Interp *interp);
static void		FreeCache(ClientData clientData, Tcl_Interp *interp);
static int		RegisterObjCmd(ClientData clientData,
			    Tcl_Interp *interp, int objc, Tcl_Obj *const *objv);
static int		RenderObjCmd(ClientData clientData,
			    Tcl_Interp *interp, int objc, Tcl_Obj *const *objv);

/*
 * The format record for the SVG nano file format:
 */

Tk_PhotoImageFormat tkImgFmtSVGnano = {
    "svg",			/* name */
    FileMatchSVG,		/* fileMatchProc */
    StringMatchSVG,		/* stringMatchProc */
    FileReadSVG,		/* fileReadProc */
    StringReadSVG,		/* stringReadProc */
    NULL,			/* fileWriteProc */
    NULL,			/* stringWriteProc */
    NULL
};

/*
 *----------------------------------------------------------------------
 *
 * MemMem --
 *
 *	Like strstr() but operating on memory buffers with sizes.
 *
 *----------------------------------------------------------------------
 */

static const void *
MemMem(const void *haystack, size_t haylen,
       const void *needle, size_t needlen)
{
    const void *hayend, *second, *p;
    unsigned char first;

    if ((needlen <= 0) || (haylen < needlen)) {
	return NULL;
    }
    hayend = (const void *) ((char *) haystack + haylen - needlen);
    first = ((char *) needle)[0];
    second = (const void *) ((char *) needle + 1);
    needlen -= 1;
    while (haystack < hayend) {
	p = memchr(haystack, first, (char *) hayend - (char *) haystack);
	if (p == NULL) {
	    break;
	}
	if (needlen == 0) {
	    return p;
	}
	haystack = (const void *) ((char *) p + 1);
	if (memcmp(second, haystack, needlen) == 0) {
	    return p;
	}
    }
    return NULL;
}

/*
 *----------------------------------------------------------------------
 *
 * FileMatchSVG --
 *
 *	This function is invoked by the photo image type to see if a file
 *	contains image data in SVG format.
 *
 * Results:
 *	The return value is >0 if the file can be successfully parsed,
 *	and 0 otherwise.
 *
 * Side effects:
 *	The file is saved in the internal cache for further use.
 *
 *----------------------------------------------------------------------
 */

static int
FileMatchSVG(
    Tcl_Channel chan,
    const char *fileName,
    Tcl_Obj *formatObj,
    int *widthPtr, int *heightPtr,
    Tcl_Interp *interp)
{
    TkSizeT length;
    Tcl_Obj *dataObj = Tcl_NewObj();
    const char *data;
    RastOpts ropts;
    NSVGimage *nsvgImage;
    (void)fileName;

    CleanCache(interp);
    if (Tcl_ReadChars(chan, dataObj, 4096, 0) == TCL_IO_FAILURE) {
	/* in case of an error reading the file */
	Tcl_DecrRefCount(dataObj);
	return 0;
    }
    data = Tcl_GetStringFromObj(dataObj, &length);
    /* should have a '<svg' and a '>' in the first 4k */
    if ((memchr(data, '>', length) == NULL) ||
	(MemMem(data, length, "<svg", 4) == NULL)) {
	Tcl_DecrRefCount(dataObj);
	return 0;
    }
    if (!Tcl_Eof(chan) && (Tcl_ReadChars(chan, dataObj, -1, 1) == TCL_IO_FAILURE)) {
	/* in case of an error reading the file */
	Tcl_DecrRefCount(dataObj);
	return 0;
    }
    data = Tcl_GetStringFromObj(dataObj, &length);
    nsvgImage = ParseSVGWithOptions(interp, data, length, formatObj, &ropts);
    Tcl_DecrRefCount(dataObj);
    if (nsvgImage != NULL) {
        GetScaleFromParameters(nsvgImage, &ropts, widthPtr, heightPtr);
        if ((*widthPtr <= 0.0) || (*heightPtr <= 0.0)) {
	    nsvgDelete(nsvgImage);
	    return 0;
        }
        if (!CacheSVG(interp, chan, formatObj, nsvgImage, &ropts)) {
	    nsvgDelete(nsvgImage);
        }
        return 1;
    }
    return 0;
}

/*
 *----------------------------------------------------------------------
 *
 * FileReadSVG --
 *
 *	This function is called by the photo image type to read SVG format
 *	data from a file and write it into a given photo image.
 *
 * Results:
 *	A standard TCL completion code. If TCL_ERROR is returned then an error
 *	message is left in the interp's result.
 *
 * Side effects:
 *	The access position in file f is changed, and new data is added to the
 *	image given by imageHandle.
 *
 *----------------------------------------------------------------------
 */

static int
FileReadSVG(
    Tcl_Interp *interp,
    Tcl_Channel chan,
    const char *fileName,
    Tcl_Obj *formatObj,
    Tk_PhotoHandle imageHandle,
    int destX, int destY,
    int width, int height,
    int srcX, int srcY)
{
    TkSizeT length;
    const char *data;
    RastOpts ropts;
    NSVGimage *nsvgImage = GetCachedSVG(interp, chan, formatObj, &ropts);
    (void)fileName;

    if (nsvgImage == NULL) {
        Tcl_Obj *dataObj = Tcl_NewObj();

	if (Tcl_ReadChars(chan, dataObj, -1, 0) == TCL_IO_FAILURE) {
	    /* in case of an error reading the file */
	    Tcl_DecrRefCount(dataObj);
	    Tcl_SetObjResult(interp, Tcl_NewStringObj("read error", -1));
	    Tcl_SetErrorCode(interp, "TK", "IMAGE", "SVG", "READ_ERROR", NULL);
	    return TCL_ERROR;
	}
	data = Tcl_GetStringFromObj(dataObj, &length);
	nsvgImage = ParseSVGWithOptions(interp, data, length, formatObj,
			    &ropts);
	Tcl_DecrRefCount(dataObj);
	if (nsvgImage == NULL) {
	    return TCL_ERROR;
	}
    }
    return RasterizeSVG(interp, imageHandle, nsvgImage, destX, destY,
		width, height, srcX, srcY, &ropts);
}

/*
 *----------------------------------------------------------------------
 *
 * StringMatchSVG --
 *
 *	This function is invoked by the photo image type to see if a string
 *	contains image data in SVG format.
 *
 * Results:
 *	The return value is >0 if the file can be successfully parsed,
 *	and 0 otherwise.
 *
 * Side effects:
 *	The file is saved in the internal cache for further use.
 *
 *----------------------------------------------------------------------
 */

static int
StringMatchSVG(
    Tcl_Obj *dataObj,
    Tcl_Obj *formatObj,
    int *widthPtr, int *heightPtr,
    Tcl_Interp *interp)
{
    TkSizeT length, testLength;
    const char *data;
    RastOpts ropts;
    NSVGimage *nsvgImage;

    CleanCache(interp);
    data = Tcl_GetStringFromObj(dataObj, &length);
    /* should have a '<svg' and a '>' in the first 4k */
    testLength = (length > 4096) ? 4096 : length;
    if ((memchr(data, '>', testLength) == NULL) ||
	(MemMem(data, testLength, "<svg", 4) == NULL)) {
	return 0;
    }
    nsvgImage = ParseSVGWithOptions(interp, data, length, formatObj, &ropts);
    if (nsvgImage != NULL) {
        GetScaleFromParameters(nsvgImage, &ropts, widthPtr, heightPtr);
        if ((*widthPtr <= 0.0) || (*heightPtr <= 0.0)) {
	    nsvgDelete(nsvgImage);
	    return 0;
        }
        if (!CacheSVG(interp, dataObj, formatObj, nsvgImage, &ropts)) {
	    nsvgDelete(nsvgImage);
        }
        return 1;
    }
    return 0;
}

/*
 *----------------------------------------------------------------------
 *
 * StringReadSVG --
 *
 *	This function is called by the photo image type to read SVG format
 *	data from a string and write it into a given photo image.
 *
 * Results:
 *	A standard TCL completion code. If TCL_ERROR is returned then an error
 *	message is left in the interp's result.
 *
 * Side effects:
 *	New data is added to the image given by imageHandle.
 *
 *----------------------------------------------------------------------
 */

static int
StringReadSVG(
    Tcl_Interp *interp,
    Tcl_Obj *dataObj,
    Tcl_Obj *formatObj,
    Tk_PhotoHandle imageHandle,
    int destX, int destY,
    int width, int height,
    int srcX, int srcY)
{
    TkSizeT length;
    const char *data;
    RastOpts ropts;
    NSVGimage *nsvgImage = GetCachedSVG(interp, dataObj, formatObj, &ropts);

    if (nsvgImage == NULL) {
	data = Tcl_GetStringFromObj(dataObj, &length);
	nsvgImage = ParseSVGWithOptions(interp, data, length, formatObj,
			    &ropts);
    }
    if (nsvgImage == NULL) {
	return TCL_ERROR;
    }
    return RasterizeSVG(interp, imageHandle, nsvgImage, destX, destY,
		width, height, srcX, srcY, &ropts);
}

/*
 *----------------------------------------------------------------------
 *
 * ParseSVGWithOptions --
 *
 *	This function is called to parse the given input string as SVG.
 *
 * Results:
 *	Return a newly create NSVGimage on success, and NULL otherwise.
 *
 * Side effects:
 *
 *----------------------------------------------------------------------
 */

static NSVGimage *
ParseSVGWithOptions(
    Tcl_Interp *interp,
    const char *input,
    TkSizeT length,
    Tcl_Obj *formatObj,
    RastOpts *ropts)
{
    Tcl_Obj **objv = NULL;
    int objc = 0;
    double dpi = 96.0;
    char *inputCopy = NULL;
    NSVGimage *nsvgImage;
    int parameterScaleSeen = 0;
    static const char *const fmtOptions[] = {
        "-dpi", "-scale", "-scaletoheight", "-scaletowidth", NULL
    };
    enum fmtOptionsEnum {
	OPT_DPI, OPT_SCALE, OPT_SCALE_TO_HEIGHT, OPT_SCALE_TO_WIDTH
    };

    /*
     * The parser destroys the original input string,
     * therefore first duplicate.
     */

    inputCopy = (char *)attemptckalloc(length+1);
    if (inputCopy == NULL) {
	Tcl_SetObjResult(interp, Tcl_NewStringObj("cannot alloc data buffer", -1));
	Tcl_SetErrorCode(interp, "TK", "IMAGE", "SVG", "OUT_OF_MEMORY", NULL);
	goto error;
    }
    memcpy(inputCopy, input, length);
    inputCopy[length] = '\0';

    /*
     * Process elements of format specification as a list.
     */

    ropts->scale = 1.0;
    ropts->scaleToHeight = 0;
    ropts->scaleToWidth = 0;
    if ((formatObj != NULL) &&
	    Tcl_ListObjGetElements(interp, formatObj, &objc, &objv) != TCL_OK) {
        goto error;
    }
    for (; objc > 0 ; objc--, objv++) {
	int optIndex;

	/*
	 * Ignore the "svg" part of the format specification.
	 */

	if (!strcasecmp(Tcl_GetString(objv[0]), "svg")) {
	    continue;
	}

	if (Tcl_GetIndexFromObjStruct(interp, objv[0], fmtOptions,
		sizeof(char *), "option", 0, &optIndex) == TCL_ERROR) {
	    goto error;
	}

	if (objc < 2) {
	    ckfree(inputCopy);
	    inputCopy = NULL;
	    Tcl_WrongNumArgs(interp, 1, objv, "value");
	    goto error;
	}

	objc--;
	objv++;

	/*
	 * check that only one scale option is given
	 */
	switch ((enum fmtOptionsEnum)optIndex) {
	case OPT_SCALE:
	case OPT_SCALE_TO_HEIGHT:
	case OPT_SCALE_TO_WIDTH:
	    if ( parameterScaleSeen ) {
		Tcl_SetObjResult(interp, Tcl_NewStringObj(
			"only one of -scale, -scaletoheight, -scaletowidth may be given", -1));
		Tcl_SetErrorCode(interp, "TK", "IMAGE", "SVG", "BAD_SCALE",
			NULL);
		goto error;
	    }
	    parameterScaleSeen = 1;
	    break;
	default:
	    break;
	}

	/*
	 * Decode parameters
	 */
	switch ((enum fmtOptionsEnum) optIndex) {
	case OPT_DPI:
	    if (Tcl_GetDoubleFromObj(interp, objv[0], &dpi) == TCL_ERROR) {
	        goto error;
	    }
	    if (dpi < 0.0) {
		Tcl_SetObjResult(interp, Tcl_NewStringObj(
			"-dpi value must be positive", -1));
		Tcl_SetErrorCode(interp, "TK", "IMAGE", "SVG", "BAD_DPI",
			NULL);
		goto error;
	    }
	    break;
	case OPT_SCALE:
	    if (Tcl_GetDoubleFromObj(interp, objv[0], &ropts->scale) ==
		TCL_ERROR) {
	        goto error;
	    }
	    if (ropts->scale <= 0.0) {
		Tcl_SetObjResult(interp, Tcl_NewStringObj(
			"-scale value must be positive", -1));
		Tcl_SetErrorCode(interp, "TK", "IMAGE", "SVG", "BAD_SCALE",
			NULL);
		goto error;
	    }
	    break;
	case OPT_SCALE_TO_HEIGHT:
	    if (Tcl_GetIntFromObj(interp, objv[0], &ropts->scaleToHeight) ==
		TCL_ERROR) {
	        goto error;
	    }
	    if (ropts->scaleToHeight <= 0) {
		Tcl_SetObjResult(interp, Tcl_NewStringObj(
			"-scaletoheight value must be positive", -1));
		Tcl_SetErrorCode(interp, "TK", "IMAGE", "SVG", "BAD_SCALE",
			NULL);
		goto error;
	    }
	    break;
	case OPT_SCALE_TO_WIDTH:
	    if (Tcl_GetIntFromObj(interp, objv[0], &ropts->scaleToWidth) ==
		TCL_ERROR) {
	        goto error;
	    }
	    if (ropts->scaleToWidth <= 0) {
		Tcl_SetObjResult(interp, Tcl_NewStringObj(
			"-scaletowidth value must be positive", -1));
		Tcl_SetErrorCode(interp, "TK", "IMAGE", "SVG", "BAD_SCALE",
			NULL);
		goto error;
	    }
	    break;
	}
    }

    nsvgImage = nsvgParse(inputCopy, "px", (float) dpi);
    if (nsvgImage == NULL) {
	Tcl_SetObjResult(interp, Tcl_NewStringObj("cannot parse SVG image", -1));
	Tcl_SetErrorCode(interp, "TK", "IMAGE", "SVG", "PARSE_ERROR", NULL);
	goto error;
    }
    ckfree(inputCopy);
    return nsvgImage;

error:
    if (inputCopy != NULL) {
        ckfree(inputCopy);
    }
    return NULL;
}

/*
 *----------------------------------------------------------------------
 *
 * RasterizeSVG --
 *
 *	This function is called to rasterize the given nsvgImage and
 *	fill the imageHandle with data.
 *
 * Results:
 *	A standard TCL completion code. If TCL_ERROR is returned then an error
 *	message is left in the interp's result.
 *
 *
 * Side effects:
 *	On error the given nsvgImage will be deleted.
 *
 *----------------------------------------------------------------------
 */

static int
RasterizeSVG(
    Tcl_Interp *interp,
    Tk_PhotoHandle imageHandle,
    NSVGimage *nsvgImage,
    int destX, int destY,
    int width, int height,
    int srcX, int srcY,
    RastOpts *ropts)
{
    int w, h, c;
    NSVGrasterizer *rast;
    unsigned char *imgData;
    Tk_PhotoImageBlock svgblock;
    double scale;
    Tcl_WideUInt wh;
    (void)srcX;
    (void)srcY;

    scale = GetScaleFromParameters(nsvgImage, ropts, &w, &h);

    rast = nsvgCreateRasterizer();
    if (rast == NULL) {
	Tcl_SetObjResult(interp, Tcl_NewStringObj("cannot initialize rasterizer", -1));
	Tcl_SetErrorCode(interp, "TK", "IMAGE", "SVG", "RASTERIZER_ERROR",
		NULL);
	goto cleanAST;
    }

    /* Tk Ticket [822330269b] Check potential int overflow in following ckalloc */
    wh = (Tcl_WideUInt)w * (Tcl_WideUInt)h;
    if ( w < 0 || h < 0 || wh > INT_MAX / 4) {
	Tcl_SetObjResult(interp, Tcl_NewStringObj("image size overflow", -1));
	Tcl_SetErrorCode(interp, "TK", "IMAGE", "SVG", "IMAGE_SIZE_OVERFLOW", NULL);
	goto cleanRAST;
    }

    imgData = (unsigned char *)attemptckalloc(wh * 4);
    if (imgData == NULL) {
	Tcl_SetObjResult(interp, Tcl_NewStringObj("cannot alloc image buffer", -1));
	Tcl_SetErrorCode(interp, "TK", "IMAGE", "SVG", "OUT_OF_MEMORY", NULL);
	goto cleanRAST;
    }
    nsvgRasterize(rast, nsvgImage, 0, 0,
	    (float) scale, imgData, w, h, w * 4);
    /* transfer the data to a photo block */
    svgblock.pixelPtr = imgData;
    svgblock.width = w;
    svgblock.height = h;
    svgblock.pitch = w * 4;
    svgblock.pixelSize = 4;
    for (c = 0; c <= 3; c++) {
	svgblock.offset[c] = c;
    }
    if (Tk_PhotoExpand(interp, imageHandle,
		destX + width, destY + height) != TCL_OK) {
	goto cleanRAST;
    }
    if (Tk_PhotoPutBlock(interp, imageHandle, &svgblock, destX, destY,
		width, height, TK_PHOTO_COMPOSITE_SET) != TCL_OK) {
	goto cleanimg;
    }
    ckfree(imgData);
    nsvgDeleteRasterizer(rast);
    nsvgDelete(nsvgImage);
    return TCL_OK;

cleanimg:
    ckfree(imgData);

cleanRAST:
    nsvgDeleteRasterizer(rast);

cleanAST:
    nsvgDelete(nsvgImage);
    return TCL_ERROR;
}

/*
 *----------------------------------------------------------------------
 *
 * GetScaleFromParameters --
 *
 *	Get the scale value from the already parsed parameters -scale,
 *	-scaletoheight and -scaletowidth.
 *
 *	The image width and height is also returned.
 *
 * Results:
 *	The evaluated or configured scale value, or 0.0 on failure
 *
 * Side effects:
 *	heightPtr and widthPtr are set to height and width of the image.
 *
 *----------------------------------------------------------------------
 */

static double
GetScaleFromParameters(
    NSVGimage *nsvgImage,
    RastOpts *ropts,
    int *widthPtr,
    int *heightPtr)
{
    double scale;
    int width, height;

    if ((nsvgImage->width == 0.0) || (nsvgImage->height == 0.0)) {
        width = height = 0;
        scale = 1.0;
    } else if (ropts->scaleToHeight > 0) {
	/*
	 * Fixed height
	 */
	height = ropts->scaleToHeight;
	scale = height / nsvgImage->height;
	width = (int) ceil(nsvgImage->width * scale);
    } else if (ropts->scaleToWidth > 0) {
	/*
	 * Fixed width
	 */
	width = ropts->scaleToWidth;
	scale = width / nsvgImage->width;
	height = (int) ceil(nsvgImage->height * scale);
    } else {
	/*
	 * Scale factor
	 */
	scale = ropts->scale;
	width = (int) ceil(nsvgImage->width * scale);
	height = (int) ceil(nsvgImage->height * scale);
    }

    *heightPtr = height;
    *widthPtr = width;
    return scale;
}

/*
 *----------------------------------------------------------------------
 *
 * GetCachePtr --
 *
 *	This function is called to get the per interpreter used
 *	svg image cache.
 *
 * Results:
 * 	Return a pointer to the used cache.
 *
 * Side effects:
 *	Initialize the cache on the first call.
 *
 *----------------------------------------------------------------------
 */

static NSVGcache *
GetCachePtr(
    Tcl_Interp *interp
) {
    NSVGcache *cachePtr = (NSVGcache *)Tcl_GetAssocData(interp, "tksvgnano", NULL);
    if (cachePtr == NULL) {
	cachePtr = (NSVGcache *)ckalloc(sizeof(NSVGcache));
	cachePtr->dataOrChan = NULL;
	Tcl_DStringInit(&cachePtr->formatString);
	cachePtr->nsvgImage = NULL;
	Tcl_SetAssocData(interp, "tksvgnano", FreeCache, cachePtr);
    }
    return cachePtr;
}

/*
 *----------------------------------------------------------------------
 *
 * CacheSVG --
 *
 *	Add the given svg image informations to the cache for further usage.
 *
 * Results:
 *	Return 1 on success, and 0 otherwise.
 *
 * Side effects:
 *
 *----------------------------------------------------------------------
 */

static int
CacheSVG(
    Tcl_Interp *interp,
    ClientData dataOrChan,
    Tcl_Obj *formatObj,
    NSVGimage *nsvgImage,
    RastOpts *ropts)
{
    TkSizeT length;
    const char *data;
    NSVGcache *cachePtr = GetCachePtr(interp);

    if (cachePtr != NULL) {
        cachePtr->dataOrChan = dataOrChan;
	if (formatObj != NULL) {
	    data = Tcl_GetStringFromObj(formatObj, &length);
	    Tcl_DStringAppend(&cachePtr->formatString, data, length);
	}
	cachePtr->nsvgImage = nsvgImage;
	cachePtr->ropts = *ropts;
	return 1;
    }
    return 0;
}

/*
 *----------------------------------------------------------------------
 *
 * GetCachedSVG --
 *
 *	Try to get the NSVGimage from the internal cache.
 *
 * Results:
 *	Return the found NSVGimage on success, and NULL otherwise.
 *
 * Side effects:
 *	Calls the CleanCache() function.
 *
 *----------------------------------------------------------------------
 */

static NSVGimage *
GetCachedSVG(
    Tcl_Interp *interp,
    ClientData dataOrChan,
    Tcl_Obj *formatObj,
    RastOpts *ropts)
{
    TkSizeT length;
    const char *data;
    NSVGcache *cachePtr = GetCachePtr(interp);
    NSVGimage *nsvgImage = NULL;

    if ((cachePtr != NULL) && (cachePtr->nsvgImage != NULL) &&
	(cachePtr->dataOrChan == dataOrChan)) {
        if (formatObj != NULL) {
	    data = Tcl_GetStringFromObj(formatObj, &length);
	    if (strcmp(data, Tcl_DStringValue(&cachePtr->formatString)) == 0) {
	        nsvgImage = cachePtr->nsvgImage;
		*ropts = cachePtr->ropts;
		cachePtr->nsvgImage = NULL;
	    }
	} else if (Tcl_DStringLength(&cachePtr->formatString) == 0) {
	    nsvgImage = cachePtr->nsvgImage;
	    *ropts = cachePtr->ropts;
	    cachePtr->nsvgImage = NULL;
	}
    }
    CleanCache(interp);
    return nsvgImage;
}

/*
 *----------------------------------------------------------------------
 *
 * CleanCache --
 *
 *	Reset the cache and delete the saved image in it.
 *
 * Results:
 *
 * Side effects:
 *
 *----------------------------------------------------------------------
 */

static void
CleanCache(Tcl_Interp *interp)
{
    NSVGcache *cachePtr = GetCachePtr(interp);

    if (cachePtr != NULL) {
        cachePtr->dataOrChan = NULL;
        Tcl_DStringSetLength(&cachePtr->formatString, 0);
	if (cachePtr->nsvgImage != NULL) {
	    nsvgDelete(cachePtr->nsvgImage);
	    cachePtr->nsvgImage = NULL;
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * FreeCache --
 *
 *	This function is called to clean up the internal cache data.
 *
 * Results:
 *
 * Side effects:
 *	Existing image data in the cache and the cache will be deleted.
 *
 *----------------------------------------------------------------------
 */

static void
FreeCache(ClientData clientData, Tcl_Interp *interp)
{
    NSVGcache *cachePtr = (NSVGcache *)clientData;
    (void)interp;

    Tcl_DStringFree(&cachePtr->formatString);
    if (cachePtr->nsvgImage != NULL) {
        nsvgDelete(cachePtr->nsvgImage);
    }
    ckfree(cachePtr);
}

/*
 *----------------------------------------------------------------------
 *
 * RegisterObjCmd --
 *
 *	This function is called to register the "tksvg" photo image
 *	format and to setup per interpreter info.
 *
 * Results:
 *	Standard Tcl result.
 *
 *----------------------------------------------------------------------
 */

int
RegisterObjCmd(
    ClientData clientData,
    Tcl_Interp *interp,
    int objc,
    Tcl_Obj *const *objv)
{
    NSVGcache *cachePtr;
    ThreadSpecificData *tsdPtr;

    if (objc != 1) {
	Tcl_WrongNumArgs(interp, 1, objv, NULL);
	return TCL_ERROR;
    }

    cachePtr = (NSVGcache *) Tcl_GetAssocData(interp, "tksvgnano", NULL);
    if (cachePtr == NULL) {
#ifdef USE_TK_STUBS
	if (Tk_InitStubs(interp, TCL_VERSION, 0) == NULL) {
	    return TCL_ERROR;
	}
#else
	if (Tcl_PkgRequire(interp, "Tk", TK_VERSION, 0) == NULL) {
	    return TCL_ERROR;
	}
#endif
	cachePtr = (NSVGcache *) ckalloc(sizeof(NSVGcache));
	cachePtr->dataOrChan = NULL;
	Tcl_DStringInit(&cachePtr->formatString);
	cachePtr->nsvgImage = NULL;
	Tcl_SetAssocData(interp, "tksvgnano", FreeCache, cachePtr);
	Tk_CreatePhotoImageFormat(&tkImgFmtSVGnano);
    }
    tsdPtr = (ThreadSpecificData *)
	    Tcl_GetThreadData(&dataKey, sizeof(ThreadSpecificData));
    if (!tsdPtr->registered) {
	Tk_CreatePhotoImageFormat(&tkImgFmtSVGnano);
	tsdPtr->registered = 1;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * RenderObjCmd --
 *
 *	This function is called to render SVG into a bytearray which
 *	is returned in a dictionary object.
 *
 * Results:
 *	Standard Tcl result.
 *
 *----------------------------------------------------------------------
 */

int
RenderObjCmd(
    ClientData clientData,
    Tcl_Interp *interp,
    int objc,
    Tcl_Obj *const *objv)
{
    double dpi = 96.0;
    char unit[3], *p;
    char *input, *inputCopy = NULL;
    NSVGimage *nsvgImage = NULL;
    int i, length, parameterScaleSeen = 0;
    static const char *const rendOptions[] = {
	"-dpi", "-scale", "-unit", "-x", "-y",
	"-scaletoheight", "-scaletowidth", NULL
    };
    enum rendOptions {
	OPT_DPI, OPT_SCALE, OPT_UNIT, OPT_X, OPT_Y,
	OPT_SCALE_TO_HEIGHT, OPT_SCALE_TO_WIDTH
    };
    RastOpts ropt[1], *ropts = ropt;
    int w, h;
    NSVGrasterizer *rast = NULL;
    unsigned char *imgData = NULL;
    double scale;
    Tcl_WideUInt wh;
    Tcl_Obj *retObj;

    if (objc < 2) {
	Tcl_WrongNumArgs(interp, 1, objv, "svgstring ?options?");
	return TCL_ERROR;
    }

    /*
     * Process options.
     */

    strcpy(unit, "px");
    ropts->x = ropts->y = 0.0;
    ropts->scale = 1.0;
    ropts->scaleToHeight = 0;
    ropts->scaleToWidth = 0;
    for (i = 2; i < objc; i++) {
	int optIndex;

	if (Tcl_GetIndexFromObjStruct(interp, objv[i], rendOptions,
		sizeof(char *), "option", 0, &optIndex) == TCL_ERROR) {
	    goto error;
	}

	if (++i >= objc) {
	    Tcl_WrongNumArgs(interp, 1, objv, "value");
	    goto error;
	}

	/*
	 * check that only one scale option is given
	 */
	switch ((enum rendOptions) optIndex) {
	case OPT_SCALE:
	case OPT_SCALE_TO_HEIGHT:
	case OPT_SCALE_TO_WIDTH:
	    if (parameterScaleSeen) {
		Tcl_SetObjResult(interp, Tcl_NewStringObj(
			"only one of -scale, -scaletoheight, -scaletowidth may be given", -1));
		Tcl_SetErrorCode(interp, "TK", "IMAGE", "SVG", "BAD_SCALE",
			(char *) NULL);
		goto error;
	    }
	    parameterScaleSeen = 1;
	    break;
	default:
	    break;
	}

	/*
	 * Decode parameters
	 */
	switch ((enum rendOptions) optIndex) {
	case OPT_DPI:
	    if (Tcl_GetDoubleFromObj(interp, objv[i], &dpi) == TCL_ERROR) {
		goto error;
	    }
	    if (dpi < 0.0) {
		Tcl_SetObjResult(interp, Tcl_NewStringObj(
			"-dpi value must be positive", -1));
		Tcl_SetErrorCode(interp, "TK", "IMAGE", "SVG", "BAD_DPI",
			(char *) NULL);
		goto error;
	    }
	    break;
	case OPT_SCALE:
	    if (Tcl_GetDoubleFromObj(interp, objv[i], &ropts->scale) ==
		TCL_ERROR) {
		goto error;
	    }
	    if (ropts->scale <= 0.0) {
		Tcl_SetObjResult(interp, Tcl_NewStringObj(
			"-scale value must be positive", -1));
		Tcl_SetErrorCode(interp, "TK", "IMAGE", "SVG", "BAD_SCALE",
			(char *) NULL);
		goto error;
	    }
	    break;
	case OPT_UNIT:
	    p = Tcl_GetString(objv[i]);
	    if ((p != NULL) && (p[0])) {
		strncpy(unit, p, 3);
		unit[2] = '\0';
	    }
	    break;
	case OPT_X:
	    if (Tcl_GetDoubleFromObj(interp, objv[i], &ropts->x) == TCL_ERROR) {
		goto error;
	    }
	    break;
	case OPT_Y:
	    if (Tcl_GetDoubleFromObj(interp, objv[i], &ropts->y) == TCL_ERROR) {
		goto error;
	    }
	    break;
	case OPT_SCALE_TO_HEIGHT:
	    if (Tcl_GetIntFromObj(interp, objv[i], &ropts->scaleToHeight) ==
		TCL_ERROR) {
		goto error;
	    }
	    if (ropts->scaleToHeight <= 0) {
		Tcl_SetObjResult(interp, Tcl_NewStringObj(
			"-scaletoheight value must be positive", -1));
		Tcl_SetErrorCode(interp, "TK", "IMAGE", "SVG", "BAD_SCALE",
			(char *) NULL);
		goto error;
	    }
	    break;
	case OPT_SCALE_TO_WIDTH:
	    if (Tcl_GetIntFromObj(interp, objv[i], &ropts->scaleToWidth) ==
		TCL_ERROR) {
		goto error;
	    }
	    if (ropts->scaleToWidth <= 0) {
		Tcl_SetObjResult(interp, Tcl_NewStringObj(
			"-scaletowidth value must be positive", -1));
		Tcl_SetErrorCode(interp, "TK", "IMAGE", "SVG", "BAD_SCALE",
			(char *) NULL);
		goto error;
	    }
	    break;
	}
    }

    /*
     * The parser destroys the original input string,
     * therefore first duplicate.
     */

    input = Tcl_GetStringFromObj(objv[1], &length);
    inputCopy = (char *) attemptckalloc(length+1);
    if (inputCopy == NULL) {
	Tcl_SetObjResult(interp,
		Tcl_NewStringObj("cannot alloc data buffer", -1));
	Tcl_SetErrorCode(interp, "TK", "IMAGE", "SVG", "OUT_OF_MEMORY",
		(char *) NULL);
	goto error;
    }
    memcpy(inputCopy, input, length);
    inputCopy[length] = '\0';

    nsvgImage = nsvgParse(inputCopy, unit, dpi);
    if (nsvgImage == NULL) {
	Tcl_SetObjResult(interp,
		Tcl_NewStringObj("cannot parse SVG image", -1));
	Tcl_SetErrorCode(interp, "TK", "IMAGE", "SVG", "PARSE_ERROR",
		(char *) NULL);
	goto error;
    }
    ckfree(inputCopy);
    inputCopy = NULL;

    scale = GetScaleFromParameters(nsvgImage, ropts, &w, &h);
    rast = nsvgCreateRasterizer();
    if (rast == NULL) {
	Tcl_SetObjResult(interp,
		Tcl_NewStringObj("cannot initialize rasterizer", -1));
	Tcl_SetErrorCode(interp, "TK", "IMAGE", "SVG", "RASTERIZER_ERROR",
		(char *) NULL);
	goto error;
    }

    wh = (Tcl_WideUInt) w * (Tcl_WideUInt) h;
    if ((w < 0) || (h < 0) || (w * 4 < 0) || (wh > INT_MAX / 4)) {
	Tcl_SetObjResult(interp, Tcl_NewStringObj("image size overflow", -1));
	Tcl_SetErrorCode(interp, "TK", "IMAGE", "SVG", "IMAGE_SIZE_OVERFLOW",
		(char *) NULL);
	goto error;
    }

    imgData = (unsigned char *) attemptckalloc(wh *4);
    if (imgData == NULL) {
	Tcl_SetObjResult(interp,
		Tcl_NewStringObj("cannot alloc image buffer", -1));
	Tcl_SetErrorCode(interp, "TK", "IMAGE", "SVG", "OUT_OF_MEMORY",
		(char *) NULL);
	goto error;
    }
    nsvgRasterize(rast, nsvgImage, ropts->x, ropts->y,
	    (float) scale, imgData, w, h, w * 4);
    nsvgDeleteRasterizer(rast);
    nsvgDelete(nsvgImage);

    /* make result dictionary */
    retObj = Tcl_NewDictObj();
    Tcl_DictObjPut(interp, retObj, Tcl_NewStringObj("width", -1),
	Tcl_NewIntObj(w));
    Tcl_DictObjPut(interp, retObj, Tcl_NewStringObj("height", -1),
	Tcl_NewIntObj(w));
    Tcl_DictObjPut(interp, retObj, Tcl_NewStringObj("channels", -1),
	Tcl_NewIntObj(4));
    Tcl_DictObjPut(interp, retObj, Tcl_NewStringObj("data", -1),
	Tcl_NewByteArrayObj(imgData, w * h * 4));
    ckfree(imgData);
    Tcl_SetObjResult(interp, retObj);
    return TCL_OK;

error:
    if (inputCopy != NULL) {
	ckfree(inputCopy);
    }
    if (imgData != NULL) {
	ckfree(imgData);
    }
    if (rast != NULL) {
	nsvgDeleteRasterizer(rast);
    }
    if (nsvgImage != NULL) {
	nsvgDelete(nsvgImage);
    }
    return TCL_ERROR;
}

/*
 *----------------------------------------------------------------------
 *
 * Tksvg_Init --
 *
 *	This function is called to initialize the "tksvg" package.
 *
 * Results:
 *	Standard Tcl return code.
 *
 *----------------------------------------------------------------------
 */

int DLLEXPORT
Tksvg_Init(Tcl_Interp *interp)
{
    Tcl_Namespace *nsPtr;
    NSVGcache *cachePtr;
    int haveTk = 0;
    Tcl_Obj *str2[2], *cmdList;
    Tcl_Command cmd;

    if (interp == NULL) {
        return TCL_ERROR;
    }
#ifdef USE_TCL_STUBS
    if (Tcl_InitStubs(interp, TCL_VERSION, 0) == NULL) {
	return TCL_ERROR;
    }
#else
    if (Tcl_PkgRequire(interp, "Tcl", TCL_VERSION, 0) == NULL) {
	return TCL_ERROR;
    }
#endif
    if (Tcl_PkgPresent(interp, "Tk", TK_VERSION, 1) != NULL) {
#ifdef USE_TK_STUBS
	if (Tk_InitStubs(interp, TCL_VERSION, 0) == NULL) {
	    return TCL_ERROR;
	}
#else
	if (Tcl_PkgRequire(interp, "Tk", TK_VERSION, 0) == NULL) {
	    return TCL_ERROR;
	}
#endif
	haveTk = 1;
    }
    nsPtr = Tcl_CreateNamespace(interp, "tksvg", NULL, NULL);
    if (nsPtr == NULL) {
	return TCL_ERROR;
    }
    if (haveTk) {
	cachePtr = (NSVGcache *) Tcl_GetAssocData(interp, "tksvgnano", NULL);
	if (cachePtr == NULL) {
	    ThreadSpecificData *tsdPtr;

    cachePtr = (NSVGcache *) ckalloc(sizeof(NSVGcache));
	    cachePtr->dataOrChan = NULL;
	    Tcl_DStringInit(&cachePtr->formatString);
	    cachePtr->nsvgImage = NULL;
	    Tcl_SetAssocData(interp, "tksvgnano", FreeCache, cachePtr);
	    tsdPtr = (ThreadSpecificData *)
		    Tcl_GetThreadData(&dataKey, sizeof(ThreadSpecificData));
	    if (!tsdPtr->registered) {
		Tk_CreatePhotoImageFormat(&tkImgFmtSVGnano);
		tsdPtr->registered = 1;
	    }
	}
    }
    str2[0] = Tcl_NewStringObj("register", -1);
    str2[1] = Tcl_NewStringObj("render", -1);
    cmdList = Tcl_NewListObj(2, str2);
    cmd = Tcl_CreateEnsemble(interp, "::tksvg", nsPtr, TCL_ENSEMBLE_PREFIX);
    Tcl_SetEnsembleSubcommandList(interp, cmd, cmdList);
    Tcl_CreateObjCommand(interp, "::tksvg::register", RegisterObjCmd,
	    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateObjCommand(interp, "::tksvg::render", RenderObjCmd,
	    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);    Tcl_PkgProvide(interp, PACKAGE_NAME, PACKAGE_VERSION);
    Tcl_PkgProvide(interp, PACKAGE_NAME, PACKAGE_VERSION);
    return TCL_OK;
}