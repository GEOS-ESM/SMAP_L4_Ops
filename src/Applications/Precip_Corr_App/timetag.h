#define MAXSIZE 1024
/* const int MAXSIZE = 1024; */
int timetag_(int *, int *, char *, const char *,int *,int *);
char *timetag(int, int, const char *, int);
void strtime(char *, const char *, int []);
int tm_offset(const char *,int []);
int tm_token(const char *,int []);
void tm_make(struct tm *,int []);
void tm_grads(char *,const char *);
void tm_resolve(char *,const char *,const struct tm *);
int tm_parse(const char *);
void strdate_(int *,int *,int *,char *,char *);
void strgrads_(int *,int *,int *,char *,char *);
void inctime_(int *,int *,int *,int *,int *);
