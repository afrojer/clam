/*
 * Template C file for CLAM backend
 * Jeremy C. Andrus <jeremya@cs.columbia.edu>
 * 2011-12-12
 */

#define bail(msg, ...) \
{ \
	fprintf(stderr, "CLAM Runtime ERROR: " msg "\n", ## __VA_ARGS__ ); \
	exit(EXIT_FAILURE); \
}

/* --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- */
/*                                                                 */
/* CLAM heavy lifting functions                                    */
/*                                                                 */
/* --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- */

clam_img *clam_img_copy(clam_img *src)
{
}

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


clam_imgchan *__clam_imgchan_copy(clam_img *dst, const char *dname,
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

