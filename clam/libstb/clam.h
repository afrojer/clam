#ifndef CLAM_H
#define CLAM_H
/*
 * CLAM C Interface Header
 *
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <float.h>
#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include <stdint.h>


/* --- --- --- --- --- --- --- */
/* stolen from: linux/list.h   */
/* --- --- --- --- --- --- --- */

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _DEBUG
#define DBG(X) X
#else
#define DBG(X)
#endif

struct list_head {
	struct list_head *next, *prev;
};

static inline void INIT_LIST_HEAD(struct list_head *nm)
{
	nm->next = nm;
	nm->prev = nm;
}

static inline void __list_add(struct list_head *newI,
			      struct list_head *prev,
			      struct list_head *next) {
	next->prev = newI;
	newI->next = next;
	newI->prev = prev;
	prev->next = newI;
}

static inline void list_add(struct list_head *newI, struct list_head *head)
{
	__list_add(newI, head, head->next);
}

static inline void list_add_tail(struct list_head *newI, struct list_head *head)
{
	__list_add(newI, head->prev, head);
}

#define __list_del(__prev, __next) \
	(__next)->prev = __prev; \
	(__prev)->next = __next

static inline void list_del(struct list_head *entry)
{
	__list_del(entry->prev, entry->next);
}

static inline void list_del_init(struct list_head *entry)
{
	__list_del(entry->prev, entry->next);
	INIT_LIST_HEAD(entry);
}

static inline void list_move(struct list_head *list, struct list_head *head)
{
	__list_del(list->prev, list->next);
	list_add(list, head);
}

static inline void list_move_tail(struct list_head *list,
				  struct list_head *head)
{
	__list_del(list->prev, list->next);
	list_add_tail(list, head);
}

static inline int list_is_last(const struct list_head *list,
				const struct list_head *head)
{
	return list->next == head;
}

static inline int list_empty(const struct list_head *head)
{
	return head->next == head;
}

#ifndef offsetof
#define offsetof(st, m) \
     ((size_t) ( (char *)&((st *)(0))->m - (char *)0 ))
#endif

#define container_of(ptr, type, member) ({			\
	const typeof( ((type *)0)->member ) *__mptr = (ptr);	\
	(type *)( (char *)__mptr - offsetof(type,member) );})

#define list_entry(ptr, type, member) \
	container_of(ptr, type, member)

#define list_first_entry(ptr, type, member) \
	list_entry((ptr)->next, type, member)

#define list_for_each(pos, head) \
	for (pos = (head)->next; pos != (head); pos = pos->next)

#define list_for_each_prev(pos, head) \
	for (pos = (head)->prev; pos != (head); pos = pos->prev)

#define list_for_each_safe(pos, n, head) \
	for (pos = (head)->next, n = pos->next; pos != (head); \
		pos = n, n = pos->next)

#define list_for_each_prev_safe(pos, n, head) \
	for (pos = (head)->prev, n = pos->prev; \
	     pos != (head); \
	     pos = n, n = pos->prev)

#define list_for_each_entry(pos, head, member)				\
	for (pos = list_entry((head)->next, typeof(*pos), member);	\
	     &pos->member != (head); 	\
	     pos = list_entry(pos->member.next, typeof(*pos), member))

#define list_for_each_entry_reverse(pos, head, member)			\
	for (pos = list_entry((head)->prev, typeof(*pos), member);	\
	     &pos->member != (head); 	\
	     pos = list_entry(pos->member.prev, typeof(*pos), member))


/* --- --- --- --- --- --- --- */
/* CLAM type declarations      */
/* --- --- --- --- --- --- --- */

typedef enum clam_img_fmt_e {
	PNG = 0,
	BMP,
	TGA,
	JPG,
	CLAM_NUMFMTS,
} clam_img_fmt;


typedef enum clam_atom_e {
	UINT8 = 0,
	UINT16,
	UINT32,
	INT8,
	INT16,
	INT32,
	ANGLE,
	CLAM_NUMTYPES,
} clam_atom;

#define clam_type_union \
	uint8_t  u8;  \
	uint16_t u16; \
	uint32_t u32; \
	int8_t   s8;  \
	int16_t  s16; \
	int32_t  s32; \
	float    f32;

static inline int clam_atom_sz(clam_atom a) {
	switch (a) {
	case UINT8:
		return sizeof(uint8_t);
	case UINT16:
		return sizeof(uint16_t);
	case UINT32:
		return sizeof(uint32_t);
	case INT8:
		return sizeof(int8_t);
	case INT16:
		return sizeof(int16_t);
	case INT32:
		return sizeof(int32_t);
	case ANGLE:
		return sizeof(float);
	default:
		return 0;
	}
}

/* ImageT */
typedef struct clam_img {
	unsigned char *p;
	int width;
	int height;
	struct list_head chan;   /* master channel list */
	int num_chan;            /* total number of channels */
	unsigned char **curr_p;  /* channel data pointers: dynamically setup */
	unsigned int   *curr_s;
	const char *name;
} clam_img;

/* clam matrix (for kernel computation) */
typedef struct clam_matrix {
	int32_t rows, cols;
	union { clam_type_union } num;
	union { clam_type_union } denom;
	void *d;
} clam_matrix;

#define clam_calc_setmatrix(_calc, _type, _rows, _cols, _num, _denom, _data...) \
	(_calc)->ismat = 1; \
	(_calc)->m.rows = _rows; \
	(_calc)->m.cols = _cols; \
	*((_type *)(&(_calc)->m.num)) = _num; \
	*((_type *)(&(_calc)->m.denom)) = _denom; \
	static int _calc ## _matdata [_rows][_cols] = _data; \
	(_calc)->m.d = & _calc ## _matdata

/* CalcT */
typedef struct clam_calc {
	const char  *name;
	clam_atom    type;
	int          ismat;
	clam_matrix  m;
} clam_calc;

/* elements of a KernelT */
typedef struct clam_kcalc {
	struct list_head list;
	int used;
	clam_calc *calc;
} clam_kcalc;

/* KernelT */
typedef struct clam_kernel {
	struct list_head allcalc;
} clam_kernel;

/* ImageT channels */
typedef struct clam_imgchan {
	struct list_head  list;
	clam_img         *img;
	const char       *name;
	unsigned char    *p;
	clam_atom         type;
	uint32_t          stride;
} clam_imgchan;


/* --- --- --- --- --- --- --- */
/* internal (compiler) API     */
/* --- --- --- --- --- --- --- */

#define clam_alloc_check(var) \
	if (!var) bail("out of memory for " #var)

static inline clam_img *clam_img_alloc(void)
{
	clam_img *img;
	img = (clam_img *)malloc(sizeof(*img));
	if (!img)
		return NULL;
	/* simple init */
	memset(img, 0, sizeof(*img));
	img->width = img->height = -1;
	img->num_chan = 0;
	INIT_LIST_HEAD(&img->chan);

	return img;
}

static inline void clam_img_free(clam_img *img)
{
	struct list_head *pos, *tmp;
	clam_imgchan *ch;

	if (!img) return;
	list_for_each_safe(pos, tmp, &img->chan) {
		ch = list_entry(pos, typeof(*ch), list);
		list_del(pos);
		free(ch->p);
		free(ch);
	}
	free(img->curr_p);
	free(img->curr_s);
	free(img->p);
	free(img);
}

static inline int clam_img_valid(clam_img *img)
{
	return (img->width > 0) && (img->height > 0);
}

static inline void clam_img_setup_calc(clam_img *img)
{
	clam_imgchan *ch;
	int i;

	free(img->curr_p); /* kill previous setup */
	free(img->curr_s);
	img->curr_p = (unsigned char **)malloc(img->num_chan * sizeof(char *));
	img->curr_s = (unsigned int *)malloc(img->num_chan * sizeof(int));
	if (!img->curr_p || !img->curr_s) {
		fprintf(stderr, "Internal memory alloc error\n");
		return;
	}

	i = 0;
	list_for_each_entry(ch, &img->chan, list) {
		img->curr_p[i] = ch->p;
		img->curr_s[i] = ch->stride;
		i++;
	}
}

#define clam_img_next_pix(img) \
	{ int __chidx__; \
	for (__chidx__ = 0; __chidx__ < img->num_chan; __chidx__++) { \
		img->curr_p[__chidx__] += img->curr_s[__chidx__]; \
	} \
	}

#define clam_img_pix(type, pp, chidx) \
	(*((type *)((pp)[chidx])))

static inline clam_calc *clam_calc_alloc(const char *name, clam_atom type)
{
	clam_calc *c;
	c = (clam_calc *)malloc(sizeof(*c));
	if (!c)
		return NULL;
	memset(c, 0, sizeof(*c));
	c->name = name;
	c->type = type;
	return c;
}

static inline clam_kernel *clam_kernel_alloc(void)
{
	clam_kernel *k;
	k = (clam_kernel *)malloc(sizeof(*k));
	if (!k)
		return NULL;
	memset(k, 0, sizeof(*k));
	INIT_LIST_HEAD(&k->allcalc);
	return k;
}

static inline void clam_kernel_free(clam_kernel *kern)
{
	struct list_head *pos, *tmp;
	clam_kcalc *kc;
	if (!kern) return;

	list_for_each_safe(pos, tmp, &kern->allcalc) {
		kc = list_entry(pos, typeof(*kc), list);
		list_del(pos);
		free(kc);
	}

	free(kern);
}

static clam_kernel *clam_kernel_addcalc(clam_kernel *kern, clam_calc *calc, int used)
{
	clam_kcalc *kc;
	kc = (clam_kcalc *)malloc(sizeof(*kc));
	if (!kc) {
		fprintf(stderr, "out of memory\n");
		return NULL;
	}
	memset(kc, 0, sizeof(*kc));
DBG(	printf("Adding %s to kernel\n", calc->name);)
	INIT_LIST_HEAD(&kc->list);
	kc->calc = calc;
	kc->used = used;
	list_add(&kc->list, &kern->allcalc);

	return kern;
}

/* --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- */
/*                                                                 */
/* CLAM heavy lifting functions                                    */
/* (implemented in generated C file)                               */
/*                                                                 */
/* --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- */

extern clam_img *clam_img_copy(clam_img *src);

extern clam_kernel *__clam_kernel_copy(clam_kernel *kern);

#define clam_kernel_copy(DST, SRC) \
	({if (!DST) \
		DST = clam_kernel_alloc(); \
	__clam_kernel_copy(SRC); })

extern clam_img *__clam_imgchan_add(clam_img *img, clam_atom type,
				    const char *name, int should_alloc);

#define clam_imgchan_addcalc(IMG, CHAN) \
	__clam_imgchan_add(IMG, (CHAN)->type, #CHAN, 0)

#define clam_imgchan_add_empty(IMG, NAME, TYPE) \
	__clam_imgchan_add(IMG, TYPE, #NAME, 1)

extern void clam_imgchan_del(clam_img *img, const char *name);

extern clam_imgchan *clam_imgchan_ref(clam_img *img, const char *name);

#define clam_imgchan_copy(DST, DNAME, SCHAN) \
	({if (!DST) \
		DST = clam_img_alloc(); \
	__clam_imgchan_copy(DST, DNAME, SCHAN); })

extern clam_imgchan *__clam_imgchan_copy(clam_img *dst, const char *dname,
					 clam_imgchan *schan);

extern void clam_img_resize(clam_img *img, int width, int height);

/* Functional Library Interface */
extern clam_img *imgread(const char *filename);
extern int imgwrite(clam_img *img, clam_img_fmt fmt, const char *filename);

#ifdef __cplusplus
} /* extern "C" */

extern void clam_convolve_matrix(clam_img *outimg,
				 clam_imgchan *ch,
				 clam_calc *calc);

/* Convolution (much easier with templates) */
template<typename CalcT, typename ChanT>
void __clam_convolve_matrix(clam_img *outimg, clam_imgchan *ch, clam_calc *calc, int min, int max)
{
	int xx, xstart, xend;
	int yy, ystart, yend;
	int kx, kxstart, kxend;
	int ky, kystart, kyend;
	int k_start, k_stride;
	int width, height;
	clam_imgchan *outchan;
	CalcT *dpix;
	ChanT *spix;

	int *kern, num, denom;

	if (!calc->ismat)
		return;

	if (!clam_img_valid(outimg))
		clam_img_resize(outimg, ch->img->width, ch->img->height);

	/* add the channel to the image
	 * (if it's unused, we'll remove it later)
	 */
	__clam_imgchan_add(outimg, calc->type, calc->name, 1);
	outchan = clam_imgchan_ref(outimg, calc->name);

	width = outimg->width;
	height = outimg->height;

	kern = (int *)(calc->m.d);
	num = (int)*((CalcT *)&(calc->m.num));
	denom = (int)*((CalcT *)&(calc->m.denom));

DBG(	printf("\tConvolve[%d/%d]: %s:%s = %s ** %s\n", num,denom, outimg->name, calc->name, ch->name, calc->name);)
	/* XXX: remove this when we add boundary support! */
	memset(outchan->p, 0, width * height * sizeof(CalcT));

	/* XXX: do first N rows */

	ystart = calc->m.rows/2 + 1;
	yend = height - calc->m.rows;
	xstart = calc->m.cols/2 + 1;
	xend = width - calc->m.cols;

	kystart = -(calc->m.rows/2);
	kyend = kystart + calc->m.rows;
	kxstart = -(calc->m.cols/2);
	kxend = kxstart + calc->m.cols;

	k_start = kystart*width + kxstart;
	k_stride = width - calc->m.cols;

	for (yy = ystart; yy < yend; yy++) {
		/* XXX: do first N cols */

		dpix = &( ((CalcT *)outchan->p)[(yy * width) + xstart] );
		spix = &( ((ChanT *)ch->p)[(yy * width) + xstart]);
		for (xx = xstart; xx < xend; xx++, dpix++, spix++) {
			/* kernel operation */
			int kidx = 0;
			int sidx = k_start;
			int val = 0;
			for (ky = kystart; ky < kyend; ky++, sidx += k_stride) {
				for (kx = kxstart; kx < kxend; kx++, kidx++, sidx++) {
					val += (int)(spix[sidx]) * kern[kidx];
				}
			}
			val *= num;
			val /= denom;
			/* clamp */
			val = (val < min ? min : (val > max ? max : val));
			*dpix = (CalcT)(val);
		}

		/* XXX: do last N cols */
	}

	/* XXX: do last N rows */
}

/* XXX: create a partially specialized function for float... ?! */
#endif

#endif /* CLAM_H */
