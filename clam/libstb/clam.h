/*
 * CLAM C Interface Header
 *
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>


/* --- --- --- --- --- --- --- */
/* stolen from: linux/list.h   */
/* --- --- --- --- --- --- --- */


struct list_head {
	struct list_head *next, *prev;
};

static inline void INIT_LIST_HEAD(struct list_head *nm)
{
	nm->next = nm;
	nm->prev = nm;
}

static inline void __list_add(struct list_head *new,
			      struct list_head *prev,
			      struct list_head *next) {
	next->prev = new;
	new->next = next;
	new->prev = prev;
	prev->next = new;
}

static inline void list_add(struct list_head *new, struct list_head *head)
{
	__list_add(new, head, head->next);
}

static inline void list_add_tail(struct list_head *new, struct list_head *head)
{
	__list_add(new, head->prev, head);
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


typedef enum clam_atom_e {
	UINT8 = 1,
	UINT16,
	UINT32,
	INT8,
	INT16,
	INT32,
	ANGLE,
} clam_atom;

static inline clam_atom_sz(clam_atom a) {
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
} clam_img;

/* clam matrix (for kernel computation) */
#define clam_matrix_def(type) \
	struct clam_mat_ ## type { \
		int32_t rows, cols; \
		type num, denom; \
		type m[]; \
	}
typedef struct clam_matrix {
	int32_t rows, cols;
	clam_atom type;
	union {
		clam_matrix_def(uint8_t)  u8;
		clam_matrix_def(uint16_t) u16;
		clam_matrix_def(uint32_t) u32;
		clam_matrix_def(int8_t)   s8;
		clam_matrix_def(int16_t)  s16;
		clam_matrix_def(int32_t)  s32;
		clam_matrix_def(float)    f32;
	} t;
} clam_matrix;

/* Calc functor */
struct clam_calc;
typedef void (*clam_calcFunc)(unsigned char **pp,
			      size_t num_chan, void *val);

/* CalcT */
typedef struct clam_calc {
	clam_calcFunc  f;
	clam_matrix   *m;
} clam_calc;

/* elements of a KernelT */
typedef struct clam_kcalc {
	struct list_head list;
	clam_calc *calc;
} clam_kcalc;

/* KernelT */
typedef struct clam_kernel {
	struct list_head allcalc;
	struct list_head unused_calc;
} clam_kernel;

/* ImageT channels */
typedef struct clam_imgchan {
	struct list_head  list;
	const char       *name;
	clam_calcFunc     f;
	unsigned char    *p;
	uint32_t          stride;
} clam_imgchan;


/* --- --- --- --- --- --- --- */
/* internal (compiler) API     */
/* --- --- --- --- --- --- --- */


static inline clam_img *clam_img_alloc(void)
{
	clam_img *img;
	img = malloc(sizeof(*img));
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

static inline void clam_img_setup_calc(clam_img *img)
{
	clam_imgchan *ch;
	int i;

	free(img->curr_p); /* kill previous setup */
	free(img->curr_s);
	img->curr_p = malloc(img->num_chan * sizeof(char *));
	img->curr_s = malloc(img->num_chan * sizeof(int));
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

static inline clam_matrix *clam_matrix_alloc(int32_t rows, int32_t cols,
					     clam_atom type)
{
	clam_matrix *m;
	size_t t_sz;
	size_t sz;

	if (rows <= 0 || cols <= 0)
		return NULL;

	t_sz = clam_atom_sz(type);
	if (!t_sz)
		return NULL;

	sz = sizeof(*m) + (t_sz*rows*cols);

	m = (clam_matrix *)malloc(sz);
	if (!m)
		return NULL;

	memset(m, 0, sz);
	m->rows = rows;
	m->cols = cols;
	m->type = type;
}

static inline void clam_matrix_free(clam_matrix *m)
{
	free(m);
}

static inline clam_calc *clam_calc_alloc(void)
{
	clam_calc *c;
	c = (clam_calc *)malloc(sizeof(*c));
	if (!c)
		return NULL;
	return c;
}

/* --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- */
/*                                                                 */
/* CLAM heavy lifting functions                                    */
/* (implemented in generated C file)                               */
/*                                                                 */
/* --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- */
extern void clam_imgchan_add(clam_img *img, clam_calcFunc f,
			     clam_atom type, const char *name);

extern clam_imgchan *clam_imgchan_ref(clam_img *img, const char *name);

extern void clam_imgchan_assign(clam_img *dimg, const char *dname,
				clam_img *simg, const char *sname);

extern void clam_imgchan_eval(clam_img *img, const char *name);


/* Functional interface */
extern clam_img *imgread(const char *filename);
extern int imgwrite(clam_img *img, const char *type, const char *filename);

