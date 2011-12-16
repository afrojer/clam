/*
 * Template C file for CLAM backend
 * Jeremy C. Andrus <jeremya@cs.columbia.edu>
 * 2011-12-12
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
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

void __clam_imgchan_add(clam_img *img, clam_atom type,
			const char *name, int should_alloc)
{
	clam_imgchan *chan;

	chan = malloc(sizeof(*chan));
	if (!chan)
		bail("no memory for channel");

	INIT_LIST_HEAD(&chan->list);
	chan->name = name;
	chan->stride = clam_atom_sz(type);
	if (should_alloc) {
		chan->p = malloc(img->width * img->height * chan->stride);
		if (!chan->p)
			bail("no memory for channel");
	} else
		chan->p = NULL;

	list_add_tail(&chan->list, &img->chan);
	img->num_chan++;
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

void clam_imgchan_assign(clam_img *dimg, const char *dname,
		     clam_img *simg, const char *sname)
{
	clam_imgchan *dst, *src;
	int sz;

	if (dimg->width != simg->width ||
	    dimg->height != simg->height)
	{
		/* we could fix this by dynamically resizing... */
		bail("incompatible images\n");
	}

	dst = clam_imgchan_ref(dimg, dname);
	src = clam_imgchan_ref(simg, sname);
	sz = dimg->width * dimg->height;

	if (dst->stride == src->stride) {
		memcpy(dst->p, src->p, sz * dst->stride);
	} else {
		int pix;
		for (pix = 0; pix < sz; ++pix) {
			/* XXX - we have to cast here!! */
			/*       this means we need a set of annoying
			 *       functions to cast up/down between our
			 *       different channel types...
			 */
		}
	}
}

#define clam_imgchan_eval(img, ch) \
{ \
	int pix, sz; \
	unsigned char *chan_ptr; \
	unsigned char **pp; \
	if (!((ch)->p)) { \
		sz = (img)->width * (img)->height; \
		chan_ptr = malloc(sz * ch->stride); \
		clam_alloc_check(chan_ptr); \
		ch->p = chan_ptr; \
		clam_img_setup_calc(img); \
		for (pix = 0; pix < sz; ++pix) { \
			pp = (img)->curr_p; \
			cfunc ; \
			chan_ptr += ch->stride; \
			clam_img_next_pix(img); \
		} \
	} \
}


void clam_imgchan_copy(clam_img *dst, const char *dname,
		       clam_img *src, const char *sname)
{
}

clam_img *clam_convolve(clam_imgchan *ch, clam_kernel *k)
{
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
		INFILE = malloc(sz);
		if (!INFILE) bail("no space for arguments");
		strncpy(INFILE, argv[1], sz);
	}
	if (argc > 2) {
		size_t sz = strlen(argv[2]) + 1;
		char *fmt;
		OUTFILE = malloc(sz);
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

	/* Calc Lum := ... */
	Lum = clam_calc_alloc(UINT8, 0); /* type, ismatrix */
	clam_alloc_check(Lum);

	/* Calc sobelG := ... */
	sobelG = clam_calc_alloc(UINT8, 0);
	clam_alloc_check(sobelG);

	sobelTheta = clam_calc_alloc(ANGLE, 0);
	clam_alloc_check(sobelTheta);

	/* srcimg |= Lum */
	clam_imgchan_add(srcimg, Lum, 0);

	sobelGx = clam_calc_alloc(UINT8, 0);
	clam_alloc_check(sobelGx);
	clam_calc_setmatrix(sobelGx, uint8_t, 3, 3, 1, 1, { {-1, 0, 1}, {-2, 0, 2}, {-1, 0, 1} });

	sobelGy = clam_calc_alloc(UINT8, 0);
	clam_alloc_check(sobelGy);
	clam_calc_setmatrix(sobelGy, uint8_t, 3, 3, 1, 1, { {1, 2, 1}, {0, 0, 0}, {-1 ,-2, -3} });

	sobel = clam_kernel_alloc();
	clam_alloc_check(sobel);

	clam_kernel_addcalc(sobel, sobelGx, 0);
	clam_kernel_addcalc(sobel, sobelGy, 0);
	clam_kernel_addcalc(sobel, sobelG, 1);
	clam_kernel_addcalc(sobel, sobelTheta, 1);

	{
		/* srcimg:Lum */
		clam_imgchan *CONVCHAN = clam_imgchan_ref(srcimg, "Lum");
		#define Red   clam_img_pix(uint8_t,pp,0)
		#define Green clam_img_pix(uint8_t,pp,1)
		#define Blue  clam_img_pix(uint8_t,pp,2)
		#define cfunc ( (3*Red + 6*Green + 1*Blue)/10 )
		clam_imgchan_eval(srcimg,CONVCHAN);
		#undef cfunc
		#undef Red
		#undef Green
		#undef Blue

		/* Image edges = srcimg:Lum ** sobel */
		edges = clam_convolve(CONVCHAN, sobel);
		clam_alloc_check(edges);
	}

	output = clam_img_alloc();
	clam_alloc_check(output);

	clam_imgchan_copy(output, "Red", edges, "sobelG");
	clam_imgchan_copy(output, "Green", edges, "sobelG");
	clam_imgchan_copy(output, "Blue", edges, "sobelG");
	
	if (OUTFILE) {
		printf("Copying to: (%s) %s\n", OUTFMT, OUTFILE);
		imgwrite(output, OUTFMT, OUTFILE);
	}

	return EXIT_SUCCESS;
}

