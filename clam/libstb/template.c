#include "clam.h"
#include "clam.c"

/* _really_ basic argument handling */
static char *INFILE = NULL;
static char *OUTFILE = NULL;
static clam_img_fmt OUTFMT = PNG;

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

clam_convfunc_start(0, srcimg, Lum)
	clam_convfunc_chk(sobelG)
	clam_convfunc_chk(sobelTheta)
	clam_convfunc_lastchk()
		do_sobelG:
			#define sobelGx clam_img_pix(uint8_t,0)
			#define sobelGy clam_img_pix(uint8_t,1)
			#define cfunc ( sqrt(sobelGx*sobelGx + sobelGy*sobelGy) )
			clam_convolve_cfunc(sobelG,uint8_t,cfunc)
			#undef cfunc
			#undef sobelGx
			#undef sobelGy
			continue;
		do_sobelTheta:
			#define sobelGx clam_img_pix(uint8_t,0)
			#define sobelGy clam_img_pix(uint8_t,1)
			#define sobelG  clam_img_pix(uint8_t,3)
			#define cfunc ( atan((float)sobelGy/(float)sobelGx) )
			clam_convolve_cfunc(sobelTheta,float,cfunc)
			#undef cfunc
			#undef sobelGx
			#undef sobelGy
			#undef sobelG
			continue;
clam_convfunc_end(0);

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
	}
	if (argc > 3) {
		OUTFMT = (clam_img_fmt)atoi(argv[3]);
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
		#define Red   clam_img_pix(uint8_t,0)
		#define Green clam_img_pix(uint8_t,1)
		#define Blue  clam_img_pix(uint8_t,2)
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
	edges = __convolution0(sobel);
#if 0
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
#endif

	output = clam_img_alloc();
	clam_alloc_check(output);
DBG(	output->name = "output";)

	clam_imgchan_copy(output, "Red", clam_imgchan_ref(edges, "sobelG"));
	clam_imgchan_copy(output, "Green", clam_imgchan_ref(edges, "sobelG"));
	clam_imgchan_copy(output, "Blue", clam_imgchan_ref(edges, "sobelG"));
	
	if (OUTFILE) {
		printf("Copying to: (%d) %s\n", OUTFMT, OUTFILE);
		imgwrite(output, OUTFMT, OUTFILE);
	}

	return EXIT_SUCCESS;
}

