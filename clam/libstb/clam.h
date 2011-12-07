/*
 * CLAM C Interface Header
 *
 */

typedef struct clam_img {
	unsigned char *p;
	int width;
	int height;
} clam_img;

static inline clam_img *clam_img_alloc(void)
{
}

static inline void clam_img_free(clam_img *img)
{
	if (!img) return;
	free(img->p);
	free(img);
}
