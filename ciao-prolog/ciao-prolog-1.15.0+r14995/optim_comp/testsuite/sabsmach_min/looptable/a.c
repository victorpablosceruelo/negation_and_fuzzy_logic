typedef struct {
  char *name;
  int a;
} f_t;

//f_t *x[]={&(f_t){"nada", 3}};

//f_t x[]={{"nada", 3}};
//int xc=sizeof(x)/sizeof(f_t);
//f_t *x=(f_t[]){{"nada", 3},{"adan", 5}};
const f_t *const x[]={&(const f_t){name:"nada",a:333},&(const f_t){"adan", 5},0};

extern f_t u[];
int us = sizeof(u);
f_t u[]={{"nada", 3}, {"nada", 5}};
