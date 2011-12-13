/*
 * CLAM C Interface Header
 *
 */
#include <string.h>
#include <stdlib.h>

typedef struct clam_img {
	unsigned char *p;
	int width;
	int height;
} clam_img;

static inline clam_img *clam_img_alloc(void)
{
	clam_img *img;
	img = malloc(sizeof(*img));
	if (!img)
		return NULL;
	/* simple init */
	img->width = img->height = -1;
	img->p = NULL;
	return img;
}

static inline void clam_img_free(clam_img *img)
{
	if (!img) return;
	free(img->p);
	free(img);
}

/* Functional interface */
extern clam_img *imgread(const char *filename);
extern int imgwrite(const clam_img *img, const char *type, const char *filename);

