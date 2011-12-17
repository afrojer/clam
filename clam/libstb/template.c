/*
 * Template C file for CLAM backend
 * Jeremy C. Andrus <jeremya@cs.columbia.edu>
 * 2011-12-12
 */

#include <float.h>
#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "clam.h"

#define bail(msg, ...) \
{ \
	fprintf(stderr, "CLAM Runtime ERROR: " msg "\n", ## __VA_ARGS__ ); \
	exit(EXIT_FAILURE); \
}

/* _really_ basic argument handling */
static char *INFILE = NULL;
static char *OUTFILE = NULL;
static char OUTFMT[4] = { 0, 0, 0, 0 };

/* --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- */
/*                                                                 */
/* CLAM heavy lifting functions                                    */
/*                                                                 */
/* --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- */

clam_img *__clam_imgchan_add(clam_img *img, clam_atom type,
			     const char *name, int should_alloc)
{
	clam_imgchan *chan;

DBG(	printf("Adding %s to %s\n", name, img->name);)
	chan = (clam_imgchan *)malloc(sizeof(*chan));
	if (!chan)
		bail("no memory for channel");

	INIT_LIST_HEAD(&chan->list);
	chan->name = name;
	chan->img = img;
	chan->type = type;
	chan->stride = clam_atom_sz(type);
	if (should_alloc) {
		int sz = img->width * img->height * chan->stride;
		chan->p = (unsigned char *)malloc(sz);
		if (!chan->p)
			bail("no memory for channel");
	} else
		chan->p = NULL;

	list_add_tail(&chan->list, &img->chan);
	img->num_chan++;
	return img;
}

void clam_imgchan_del(clam_img *img, const char *name)
{
DBG(	printf("Removing %s from %s\n", name, img->name);)
	clam_imgchan *ch = clam_imgchan_ref(img, name);
	list_del(&ch->list);
	free(ch->p);
	free(ch);
}

int clam_imgchan_exists(clam_img *img, const char *name)
{
	clam_imgchan *ch;
	list_for_each_entry(ch, &img->chan, list) {
		if (strcmp(name, ch->name) == 0)
			return 1;
	}
	return 0;
}


clam_imgchan *clam_imgchan_ref(clam_img *img, const char *name)
{
	clam_imgchan *ch;
	list_for_each_entry(ch, &img->chan, list) {
		if (strcmp(name, ch->name) == 0) {
			goto out;
		}
	}
	bail("Invalid channel: %s", name);
out:
	return ch;
}

#define clam_imgchan_eval(__img, __type, __ch) \
{ \
	int pix, sz; \
	unsigned char *chan_ptr; \
	unsigned char **pp; \
	__type *val; \
	if (!((__ch)->p)) { \
		sz = (__img)->width * (__img)->height; \
		val = (__type *)malloc(sz * (__ch)->stride); \
		clam_alloc_check(val); \
		(__ch)->p = (unsigned char *)val; \
		printf("\tEVAL: %s:%s [%s]\n", (__img)->name, (__ch)->name, #__type); \
		clam_img_setup_calc(__img); \
		for (pix = 0; pix < sz; ++pix) { \
			pp = (__img)->curr_p; \
			*val++ = (__type)( cfunc ); \
			clam_img_next_pix(__img); \
		} \
	} \
}


clam_imgchan *clam_imgchan_copy(clam_img *dst, const char *dname,
				clam_imgchan *schan)
{
	int sz;
	clam_imgchan *dchan;
	clam_img *src = schan->img;

DBG(	printf("Copy-> %s:%s = %s:%s\n",dst->name,dname,schan->img->name,schan->name);)
	if (!clam_img_valid(dst))
		clam_img_resize(dst, src->width, src->height);

	if (dst->width != src->width || dst->height != src->height) {
		bail("incompatible images in chan copy (%s->%s)", schan->name, dname);
	}

	if (clam_imgchan_exists(dst, dname))
		clam_imgchan_del(dst, dname);

	__clam_imgchan_add(dst, schan->type, dname, 1);
	dchan = clam_imgchan_ref(dst, dname);

	sz = src->width * src->height * schan->stride;
	memcpy(dchan->p, schan->p, sz);
}

void clam_img_resize(clam_img *img, int width, int height)
{
	if (!list_empty(&img->chan))
		bail("Can't resize an image with existing channels!");

	img->width = width;
	img->height = height;
	img->num_chan = 0;
	free(img->curr_p); img->curr_p = NULL;
	free(img->curr_s); img->curr_s = NULL;
}

void clam_convolve_matrix(clam_img *outimg,
			  clam_imgchan *ch,
			  clam_calc *calc)
{
	/* switch on destination type (CalcT) */
	switch (calc->type) {
	case UINT8:
		/* switch on source type (ChanT) */
		switch (ch->type) {
		case UINT8:
			__clam_convolve_matrix<uint8_t, uint8_t>(outimg, ch, calc, 0, UINT8_MAX);
			break;
		case UINT16:
			__clam_convolve_matrix<uint8_t, uint16_t>(outimg, ch, calc, 0, UINT8_MAX);
			break;
		case UINT32:
			__clam_convolve_matrix<uint8_t, uint32_t>(outimg, ch, calc, 0, UINT8_MAX);
			break;
		case INT8:
			__clam_convolve_matrix<uint8_t, int8_t>(outimg, ch, calc, 0, UINT8_MAX);
			break;
		case INT16:
			__clam_convolve_matrix<uint8_t, int16_t>(outimg, ch, calc, 0, UINT8_MAX);
			break;
		case INT32:
			__clam_convolve_matrix<uint8_t, int32_t>(outimg, ch, calc, 0, UINT8_MAX);
			break;
		case ANGLE:
			__clam_convolve_matrix<uint8_t, float>(outimg, ch, calc, 0, UINT8_MAX);
			break;
		default:
			bail("invalid channel type?!");
		}
		break;
	case UINT16:
		switch (ch->type) {
		case UINT8:
			__clam_convolve_matrix<uint16_t, uint8_t>(outimg, ch, calc, 0, UINT16_MAX);
			break;
		case UINT16:
			__clam_convolve_matrix<uint16_t, uint16_t>(outimg, ch, calc, 0, UINT16_MAX);
			break;
		case UINT32:
			__clam_convolve_matrix<uint16_t, uint32_t>(outimg, ch, calc, 0, UINT16_MAX);
			break;
		case INT8:
			__clam_convolve_matrix<uint16_t, int8_t>(outimg, ch, calc, 0, UINT16_MAX);
			break;
		case INT16:
			__clam_convolve_matrix<uint16_t, int16_t>(outimg, ch, calc, 0, UINT16_MAX);
			break;
		case INT32:
			break;
			__clam_convolve_matrix<uint16_t, int32_t>(outimg, ch, calc, 0, UINT16_MAX);
		case ANGLE:
			break;
			__clam_convolve_matrix<uint16_t, float>(outimg, ch, calc, 0, UINT16_MAX);
		default:
			bail("invalid channel type?!");
		}
		break;
	case UINT32:
		switch (ch->type) {
		case UINT8:
			__clam_convolve_matrix<uint32_t, uint8_t>(outimg, ch, calc, 0, UINT32_MAX);
			break;
		case UINT16:
			__clam_convolve_matrix<uint32_t, uint16_t>(outimg, ch, calc, 0, UINT32_MAX);
			break;
		case UINT32:
			__clam_convolve_matrix<uint32_t, uint32_t>(outimg, ch, calc, 0, UINT32_MAX);
			break;
		case INT8:
			__clam_convolve_matrix<uint32_t, int8_t>(outimg, ch, calc, 0, UINT32_MAX);
			break;
		case INT16:
			__clam_convolve_matrix<uint32_t, int16_t>(outimg, ch, calc, 0, UINT32_MAX);
			break;
		case INT32:
			__clam_convolve_matrix<uint32_t, int32_t>(outimg, ch, calc, 0, UINT32_MAX);
			break;
		case ANGLE:
			__clam_convolve_matrix<uint32_t, float>(outimg, ch, calc, 0, UINT32_MAX);
			break;
		default:
			bail("invalid channel type?!");
		}
		break;
	case INT8:
		switch (ch->type) {
		case UINT8:
			__clam_convolve_matrix<int8_t, uint8_t>(outimg, ch, calc, INT8_MIN, INT8_MAX);
			break;
		case UINT16:
			__clam_convolve_matrix<int8_t, uint16_t>(outimg, ch, calc, INT8_MIN, INT8_MAX);
			break;
		case UINT32:
			__clam_convolve_matrix<int8_t, uint32_t>(outimg, ch, calc, INT8_MIN, INT8_MAX);
		case INT8:
			break;
			__clam_convolve_matrix<int8_t, int8_t>(outimg, ch, calc, INT8_MIN, INT8_MAX);
		case INT16:
			break;
			__clam_convolve_matrix<int8_t, int16_t>(outimg, ch, calc, INT8_MIN, INT8_MAX);
		case INT32:
			break;
			__clam_convolve_matrix<int8_t, int32_t>(outimg, ch, calc, INT8_MIN, INT8_MAX);
		case ANGLE:
			break;
			__clam_convolve_matrix<int8_t, float>(outimg, ch, calc, INT8_MIN, INT8_MAX);
		default:
			bail("invalid channel type?!");
		}
		break;
	case INT16:
		switch (ch->type) {
		case UINT8:
			__clam_convolve_matrix<int16_t, uint8_t>(outimg, ch, calc, INT16_MIN, INT16_MAX);
			break;
		case UINT16:
			__clam_convolve_matrix<int16_t, uint16_t>(outimg, ch, calc, INT16_MIN, INT16_MAX);
			break;
		case UINT32:
			__clam_convolve_matrix<int16_t, uint32_t>(outimg, ch, calc, INT16_MIN, INT16_MAX);
			break;
		case INT8:
			__clam_convolve_matrix<int16_t, int8_t>(outimg, ch, calc, INT16_MIN, INT16_MAX);
			break;
		case INT16:
			__clam_convolve_matrix<int16_t, int16_t>(outimg, ch, calc, INT16_MIN, INT16_MAX);
			break;
		case INT32:
			__clam_convolve_matrix<int16_t, int32_t>(outimg, ch, calc, INT16_MIN, INT16_MAX);
			break;
		case ANGLE:
			__clam_convolve_matrix<int16_t, float>(outimg, ch, calc, INT16_MIN, INT16_MAX);
			break;
		default:
			bail("invalid channel type?!");
		}
		break;
	case INT32:
		switch (ch->type) {
		case UINT8:
			__clam_convolve_matrix<int32_t, uint8_t>(outimg, ch, calc, INT32_MIN, INT32_MAX);
			break;
		case UINT16:
			__clam_convolve_matrix<int32_t, uint16_t>(outimg, ch, calc, INT32_MIN, INT32_MAX);
			break;
		case UINT32:
			__clam_convolve_matrix<int32_t, uint32_t>(outimg, ch, calc, INT32_MIN, INT32_MAX);
			break;
		case INT8:
			__clam_convolve_matrix<int32_t, int8_t>(outimg, ch, calc, INT32_MIN, INT32_MAX);
			break;
		case INT16:
			__clam_convolve_matrix<int32_t, int16_t>(outimg, ch, calc, INT32_MIN, INT32_MAX);
			break;
		case INT32:
			__clam_convolve_matrix<int32_t, int32_t>(outimg, ch, calc, INT32_MIN, INT32_MAX);
			break;
		case ANGLE:
			__clam_convolve_matrix<int32_t, float>(outimg, ch, calc, INT32_MIN, INT32_MAX);
			break;
		default:
			bail("invalid channel type?!");
		}
		break;
	case ANGLE:
		switch (ch->type) {
		case UINT8:
			__clam_convolve_matrix<float, uint8_t>(outimg, ch, calc, 0, INT32_MAX);
			break;
		case UINT16:
			__clam_convolve_matrix<float, uint16_t>(outimg, ch, calc, 0, INT32_MAX);
			break;
		case UINT32:
			__clam_convolve_matrix<float, uint32_t>(outimg, ch, calc, 0, INT32_MAX);
			break;
		case INT8:
			__clam_convolve_matrix<float, int8_t>(outimg, ch, calc, 0, INT32_MAX);
			break;
		case INT16:
			__clam_convolve_matrix<float, int16_t>(outimg, ch, calc, 0, INT32_MAX);
			break;
		case INT32:
			__clam_convolve_matrix<float, int32_t>(outimg, ch, calc, 0, INT32_MAX);
			break;
		case ANGLE:
			__clam_convolve_matrix<float, float>(outimg, ch, calc, 0, INT32_MAX);
			break;
		default:
			bail("invalid channel type?!");
		}
		break;
	default:
		bail("invalid calculation type?!");
	}
}


#define clam_convolve_cfunc(CALC,TYPE,CFUNC...) \
{ \
	clam_imgchan *__outchanref; \
	__clam_imgchan_add(__IMG, (CALC)->type, (CALC)->name, 0); \
	__outchanref = clam_imgchan_ref(__IMG, (CALC)->name); \
	clam_imgchan_eval(__IMG, TYPE, __outchanref); \
}

void clam_img_cleanup(clam_img *img, clam_kernel *kern)
{
	clam_kcalc *kc;
	list_for_each_entry(kc, &kern->allcalc, list) {
		if (!kc->used)
			clam_imgchan_del(img, kc->calc->name);
	}
}


/* --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- */
/*                                                                 */
/* Everything below this line should be generated                  */
/*                                                                 */
/* --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- */

/* Variable declarations */
char *imgread_arg1;
clam_img *srcimg;
clam_calc *Lum;
clam_calc *sobelG;
clam_calc *sobelTheta;
clam_calc *sobelGx;
clam_calc *sobelGy;
clam_kernel *sobel;
clam_img *edges;
clam_img *output;

/* main program loop */
int main(int argc, char **argv)
{
	/* _really_ basic argument handling */
	if (argc > 1) {
		size_t sz = strlen(argv[1]) + 1;
		INFILE = (char *)malloc(sz);
		if (!INFILE) bail("no space for arguments");
		strncpy(INFILE, argv[1], sz);
	}
	if (argc > 2) {
		size_t sz = strlen(argv[2]) + 1;
		char *fmt;
		OUTFILE = (char *)malloc(sz);
		if (!OUTFILE) bail("no space for arguments");
		strncpy(OUTFILE, argv[2], sz);
		/* find the format */
		fmt = strrchr(OUTFILE, '.');
		if (fmt && (OUTFILE + sz - ++fmt) > 3)
			strncpy(OUTFMT, fmt, 4);
	}

	/* Image srcimage = imgread(...) */
	imgread_arg1 = INFILE;
	srcimg = imgread(imgread_arg1);
	clam_alloc_check(srcimg);
DBG(	srcimg->name = "srcimg";)

	/* Calc Lum := ... */
	Lum = clam_calc_alloc("Lum", UINT8);
	clam_alloc_check(Lum);

	/* Calc sobelG := ... */
	sobelG = clam_calc_alloc("sobelG", UINT8);
	clam_alloc_check(sobelG);

	sobelTheta = clam_calc_alloc("sobelTheta", ANGLE);
	clam_alloc_check(sobelTheta);

	/* srcimg |= Lum */
	clam_imgchan_addcalc(srcimg, Lum);
	{
		clam_imgchan *__EVALCHAN = clam_imgchan_ref(srcimg, "Lum");
		#define Red   clam_img_pix(uint8_t,pp,0)
		#define Green clam_img_pix(uint8_t,pp,1)
		#define Blue  clam_img_pix(uint8_t,pp,2)
		#define cfunc ( (3*Red + 6*Green + 1*Blue)/10 )
		clam_imgchan_eval(srcimg,uint8_t,__EVALCHAN);
		#undef cfunc
		#undef Red
		#undef Green
		#undef Blue
	}

	sobelGx = clam_calc_alloc("sobelGx", UINT8);
	clam_alloc_check(sobelGx);
	clam_calc_setmatrix(sobelGx, uint8_t, 3, 3, 1, 1, { {-1, 0, 1}, {-2, 0, 2}, {-1, 0, 1} });

	sobelGy = clam_calc_alloc("sobelGy", UINT8);
	clam_alloc_check(sobelGy);
	clam_calc_setmatrix(sobelGy, uint8_t, 3, 3, 1, 1, { {1, 2, 1}, {0, 0, 0}, {-1 ,-2, -1} });

	sobel = clam_kernel_alloc();
	clam_alloc_check(sobel);

	clam_kernel_addcalc(
		clam_kernel_addcalc(
		clam_kernel_addcalc(
		clam_kernel_addcalc(sobel, sobelGx, 0), sobelGy, 1), sobelG, 1), sobelTheta, 1);

	/* Image edges = srcimg:Lum ** sobel */
	{
		clam_kcalc *__kc;
		clam_img *__IMG;
		clam_imgchan *__CONVCHAN = clam_imgchan_ref(srcimg, "Lum");
		edges = clam_img_alloc();
		clam_alloc_check(edges);
DBG(		edges->name = "edges";)
		__IMG = edges;
		/* The OCaml will have to do a bit more work here due to
		 * the way the escaped-C strings work...
		 */
		list_for_each_entry_reverse(__kc, &sobel->allcalc, list) {
			int __isused = __kc->used;
			clam_calc *__c = __kc->calc;
DBG(			printf("\tcalc=%s\n", __c->name);)
			if (__c->ismat) {
				clam_convolve_matrix(edges, __CONVCHAN, __c);
			} else {
				/* switch on name, jump to calculation */
				if (strcmp(__c->name,"sobelG") == 0)
					goto do_edges_srcimg_Lum_sobel_sobelG;
				else if (strcmp(__c->name,"sobelTheta") == 0)
					goto do_edges_srcimg_Lum_sobel_sobelTheta;
				else
					goto continue_edges_srcimg_Lum_sobel0;

			do_edges_srcimg_Lum_sobel_sobelG:
				#define sobelGx clam_img_pix(uint8_t,pp,0)
				#define sobelGy clam_img_pix(uint8_t,pp,1)
				#define cfunc ( sqrt(sobelGx*sobelGx + sobelGy*sobelGy) )
				clam_convolve_cfunc(sobelG,uint8_t,cfunc)
				#undef cfunc
				#undef sobelGx
				#undef sobelGy
				goto continue_edges_srcimg_Lum_sobel0;
			do_edges_srcimg_Lum_sobel_sobelTheta:
				#define sobelGx clam_img_pix(uint8_t,pp,0)
				#define sobelGy clam_img_pix(uint8_t,pp,1)
				#define sobelG  clam_img_pix(uint8_t,pp,3)
				#define cfunc ( atan((float)sobelGy/(float)sobelGx) )
				clam_convolve_cfunc(sobelTheta,float,cfunc)
				#undef cfunc
				#undef sobelGx
				#undef sobelGy
				goto continue_edges_srcimg_Lum_sobel0;

			}
			continue_edges_srcimg_Lum_sobel0:
			continue;
		}
		/* cleanup unused channels */
		clam_img_cleanup(__IMG, sobel);
	}

	output = clam_img_alloc();
	clam_alloc_check(output);
DBG(	output->name = "output";)

	clam_imgchan_copy(output, "Red", clam_imgchan_ref(edges, "sobelG"));
	clam_imgchan_copy(output, "Green", clam_imgchan_ref(edges, "sobelG"));
	clam_imgchan_copy(output, "Blue", clam_imgchan_ref(edges, "sobelG"));
	
	if (OUTFILE) {
		printf("Copying to: (%s) %s\n", OUTFMT, OUTFILE);
		imgwrite(output, OUTFMT, OUTFILE);
	}

	return EXIT_SUCCESS;
}

