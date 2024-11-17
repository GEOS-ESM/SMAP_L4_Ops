#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include "timetag.h"

int timetag_(int *idate, int *itime, char *s1, const char *s2,
                                                    int *ilen, int *type)
{
  int i;
  char *strtmp,*strin;

  if ( (strin = (char *) malloc((size_t)(*ilen+1))) == NULL) return(1);

  strncpy(strin,s2,(size_t)*ilen); strin[*ilen] = '\0';
  if ( (strtmp = timetag(*idate,*itime,strin,*type)) == NULL) return(1);
  for (i=0; i<strlen(strtmp); i++) s1[i] = strtmp[i];

  (void) free(strtmp);
  (void) free(strin );

  return(0);
}

/*****************************************************************************/
char *timetag(int idate, int itime, const char *str,int type)
/******************************************************************************
* English Name: Time Tag
* -------------
*
* Purpose: Resolves time tokens in an input character string according to
* -------- the specified date/time. See the "date" command for a list of
*          valid tokens. Also, see "strtime" in this program for a description
*          of additional time tokens for incrementing/decrementing the
*          date/time.
*
* Language: ANSI C
* ---------
*
* Notes: 1. this program uses the IRIX C library routine, "strftime" to resolve
*           time tokens.
*        2. see "strtime" in this program for a description of allowable
*           tokens and special characters.
*
* Parameters:
* -----------
*
* MAXSIZE - maximum size of input and output character strings.
*
* Variable Description:
* ---------------------
*
*  (o)-output; (i)-input; (io)-input/output
*
*  (i) type - 1: instructs timetag to resolve GRADS tokens
*             0: instructs timetag to resolve "date" command tokens.
*  (i) idate - date including century, year, month, day.
*  (i) itime  - time including hour, minute, second.
*  (i) str - character string containing time tokens.
*  (o) timetag - function return value: 
*
*                NULL: error allocating space for return string
*               !NULL: resolved string
*
* Programmer: Joseph V. Ardizzone
* ----------- (NASA Goddard Space Flight Center)
*             (Data Assimilation Office)
*
* Modified: February 25, 2000 - created.
* ---------
******************************************************************************/
{
   char *strout;
   char strtmp[MAXSIZE];
   int ntime[6] = {0,0,0,0,0,0};

/* Allocate space for returned string
 * ==================================
*/

   if ( (strout = (char *) malloc(MAXSIZE)) == NULL) return(NULL);

/* Extract date/time parameters.
 * =============================
*/
   ntime[0] = idate / 10000;
   ntime[1] = (idate - ntime[0] * 10000) / 100;
   ntime[2] = (idate - ntime[0] * 10000 - ntime[1] * 100);

   ntime[3] = itime / 10000;
   ntime[4] = (itime - ntime[3] * 10000) / 100;
   ntime[5] = (itime - ntime[3] * 10000 - ntime[4] * 100);

/* Resolve time tokens.
 * ====================
*/

   if (type == 1) { tm_grads(strtmp,str); strtime(strout,strtmp,ntime); }
   else strtime(strout,str,ntime);

   return(strout);
}

/*****************************************************************************/
void strtime(char *s1, const char *s2, int t[])
/******************************************************************************
* English Name: String Time
* -------------
*
* Purpose: Resolves time tokens in the input character string and returns
* -------- the expanded string.
*
* Language: ANSI C
* ---------
*
* Notes: 1. see the "date" command for a list of valid time tokens.
* ------ 2. this routine uses the IRIX C library routine, "strftime" to resolve
*           time tokens.
*        3. this routine recognizes the following additional time tokens:
*
*      a.) offset token of the form: %sTxx
*
*       where 
*
*      s - "+" or "-" for addition of subtraction
*      T - time token offset unit. This can be y,m,d,H,M or S for
*          year, month, day, hour, minute, second. No other tokens
*          are recognized for offsets.
*     xx - two digit integer representing the time increment/decrement
*
*  Offsets only apply to the token string immediately preceeding the offset.
*  Any non-token sequence acts as a separator. Brackets "{ }" can be used to
*  encapsulate string segments for an offset:
*
*       Examples:
*
*       timetag 20000228 0 "day:%d, year:%Y, month:%B%+d02"
*     >>day:28, year:2000, month:March
*
*       timetag 20000228 0 "{day:%d, year:%Y, month:%B}%+d02"
*     >>day:01, year:2000, month:March
*
*      b.) the "^" symbol can be used to lower the case of the resolved
*          time token or to remove a leading zero or blank.
*
*     examples:
*
*        timetag 20000228 0 "%d%B%Y"
*      >>28February2000
*
*        timetag 20000228 0 "%d^%B%Y"
*      >>28february2000
*
* Variable Description:
* ---------------------
*
*  (o)-output; (i)-input; (io)-input/output
*
*  (o) s1 - returned character string with expanded tokens.
*  (i) s2 - input character string.
*  (i) t - time parameters:
*
*            t[0] - year
*            t[1] - month
*            t[2] - day
*            t[3] - hour
*            t[4] - minute
*            t[5] - second
*
* Programmer: Joseph V. Ardizzone
* ----------- (NASA Goddard Space Flight Center)
*             (Data Assimilation Office)
*
* Modified: February 25, 2000 - created.
* ---------
******************************************************************************/
{

   char *p1 = s1;
   const char *p2 = s2;
   struct tm time;
   char str[MAXSIZE];
   int i,k,ilen,ntime[6];

   *p1 = '\0';
   for (i=0; i<6; i++) ntime[i] = t[i];

/* Resolve time tokens
 * ===================
*/
   while ( (ilen=tm_parse(p2)) != 0) {

      strncpy(str,p2,ilen); str[ilen] = '\0'; p2 += ilen;
      ilen = tm_offset(p2,ntime); p2 += ilen;
      tm_make(&time,ntime);
      tm_resolve(p1+strlen(p1),str,&time);
      for (i=0; i<6; i++) ntime[i] = t[i];

   }

   i = k = 0;
   ilen = strlen(s1);

/* Resolve special time tokens
 * ===========================
*/
   while (i < ilen) {

      if (s1[i] == '^') {

         if (s1[i+1] == '0') i+=2;
         else if (s1[i+1] == ' ') i+=2;
         else {s1[k++] = tolower(s1[++i]); ++i; }

      }

      else if (s1[i] == '{') ++i;
      else if (s1[i] == '}') ++i;
      else s1[k++] = s1[i++];
   }

   s1[k] = '\0';

}
/*****************************************************************************/
int tm_parse(const char *s)
/******************************************************************************
* English Name: Parse
* -------------
*
* Purpose: Returns the length of the next delimited sub-string containing
* -------- time tokens.
*
* Language: ANSI C
* ---------
*
* Notes: 1. delimiters consist of any non-token sequence not bracketed by
* ------    "{ }". See the "date" command for a list of valid tokens. Also,
*           the "^" symbol is considered to be a time token and will not
*           terminate a sequence (see strtime).
* 
*        2. this routine uses the IRIX C library routine, "strftime" to resolve
*           time tokens.
*
* Variable Description:
* ---------------------
*
*  (o)-output; (i)-input; (io)-input/output
*
*  (i) s - input character string.
*  (o) tm_parse - function return value:
*
*         tm_parse > 0 (length of sub-string)
*                  = 0 (no sub-strings found)
*
* Programmer: Joseph V. Ardizzone
* ----------- (NASA Goddard Space Flight Center)
*             (Data Assimilation Office)
*
* Modified: February 25, 2000 - created.
* ---------
******************************************************************************/
{

   int bracket = 0;
   char s1[3],s2[MAXSIZE];
   const char *p = strpbrk(s,"{%}");
   const char *plast = s + strlen(s);
   static struct tm time = {0,0,0,1,0,1900,0,0,0};

   if (p == NULL) return strlen(s);

   for (; p<=plast; p++) {

      switch (p[0]) {

         case '{': ++bracket;
                   break;

         case '}': if (--bracket == 0) return p-s+1;
                   break;

         case '^': break;

         case '%': switch (p[1]) {

                      case 'f':
                      case 'q':
                      case 'Q': ++p; break;

                      case 'E':
                      case 'O': p+=2; break;

                      default: strncpy(s1,p,2); s1[2] = '\0';
                               (void) strftime(s2,MAXSIZE,s1,&time);
                               if (strcmp(s1,s2) == 0 && bracket == 0)
                               return ( (p-s) > 0 ? p-s : 1);
                               else ++p; break;
                   } break;

         default:  if (bracket == 0) return p-s;
      }

   }

   return strlen(s);

}

/*****************************************************************************/
int tm_offset(const char *str,int ntime[])
/******************************************************************************
* English Name: Offset
* -------------
*
* Purpose: Resolves offset time token(s) starting at the beginning
* -------- of the specified string.
*
* Language: ANSI C
* ---------
*
* Notes: 1. see "tm_token" for a description of the offset token format.
* ------    
*
* Variable Description:
* ---------------------
*
*  (o)-output; (i)-input; (io)-input/output
*
*  (i) s - input character string.
*  (io) ntime - on input, current date/time. On output, incremented date/time.
*
*            ntime[0] - year
*            ntime[1] - month
*            ntime[2] - day
*            ntime[3] - hour
*            ntime[4] - minute
*            ntime[5] - second
*
*  (o) tm_offset - function return value:
*
*         tm_offset > 0 (length of offset token sequence)
*                   = 0 (not an offset token)
*
* Programmer: Joseph V. Ardizzone
* ----------- (NASA Goddard Space Flight Center)
*             (Data Assimilation Office)
*
* Modified: February 25, 2000 - created.
* ---------
******************************************************************************/
{

   int ilen;
   const char *p = str;

   while ( (ilen=tm_token(p,ntime)) != 0) p += ilen;
   return p-str;
}

/*****************************************************************************/
int tm_token(const char *tok,int ntime[])
/******************************************************************************
* English Name: Token
* -------------
*
* Purpose: Resolves the specified offset token.
* -------- 
*
* Language: ANSI C
* ---------
*
* Notes: 1. offset tokens are of the following form: %sTxx
* ------    
*
*       where
*
*      s - "+" or "-" for addition of subtraction
*      T - time token offset unit. This can be y,m,d,H,M or S for
*          year, month, day, hour, minute, second. No other tokens
*          are recognized for offsets.
*     xx - two digit integer representing the time increment/decrement
*
*
* Variable Description:
* ---------------------
*
*  (o)-output; (i)-input; (io)-input/output
*
*  (i) tok - offset token.
*  (io) ntime - on input, current date/time. On output, incremented date/time.
*
*            ntime[0] - year
*            ntime[1] - month
*            ntime[2] - day
*            ntime[3] - hour
*            ntime[4] - minute
*            ntime[5] - second
*
*  (o) tm_token - function return value:
*
*         tm_token > 0 (length of offset token)
*                  = 0 (not an offset token)
*
* Programmer: Joseph V. Ardizzone
* ----------- (NASA Goddard Space Flight Center)
*             (Data Assimilation Office)
*
* Modified: February 25, 2000 - created.
* ---------
******************************************************************************/
{
   int i,sign = 0;

   if (tok[0] != '%') return 0;

   if (tok[1] == '+') sign = 1.0;
   if (tok[1] == '-') sign = -1.0;
   if (sign == 0) return 0;

   if (tok[3] < '0' || tok[3] > '9') return 0;
   if (tok[4] < '0' || tok[4] > '9') return 0;

   (void) sscanf(&tok[3],"%2d",&i);
   i *= sign;

   switch (tok[2]) {

      case 'y': ntime[0] += i; break;

      case 'm': ntime[1] += i; break;

      case 'd': ntime[2] += i; break;

      case 'H': ntime[3] += i; break;

      case 'M': ntime[4] += i; break;

      case 'S': ntime[5] += i; break;

      default:  return 0;

   }

   return 5;
}

/*****************************************************************************/
void tm_make(struct tm *time,int ntime[]) 
/******************************************************************************
* English Name: Make
* -------------
*
* Purpose: Copies time parameters to a "time structure" needed by standard
* -------- "C" lib time routines. Also, time parameters are automatically
*          adjusted to a valid date/time.
*
* Language: ANSI C
* ---------
*
* Notes: See also "mktime" in the IRIX C library.
* ------    
*
* Variable Description:
* ---------------------
*
*  (o)-output; (i)-input; (io)-input/output
*
*  (i) ntime - input time parameters:
*
*            ntime[0] - year
*            ntime[1] - month
*            ntime[2] - day
*            ntime[3] - hour
*            ntime[4] - minute
*            ntime[5] - second
*
*  (o) time - output time structure (see "mktime").
*
* Programmer: Joseph V. Ardizzone
* ----------- (NASA Goddard Space Flight Center)
*             (Data Assimilation Office)
*
* Modified: February 25, 2000 - created.
* ---------
******************************************************************************/
{

   char *tz;

   time->tm_sec   = ntime[5];
   time->tm_min   = ntime[4];
   time->tm_hour  = ntime[3];
   time->tm_mday  = ntime[2];
   time->tm_mon   = ntime[1] - 1;
   time->tm_year  = ntime[0] - 1900;
   time->tm_wday  = 0;
   time->tm_yday  = 0;
   time->tm_isdst = -1;

   tz = getenv("TZ");
   setenv("TZ", "", 1);
   tzset();
   (void) mktime(time);
   if (tz)
     setenv("TZ", tz, 1);
   else
     unsetenv("TZ");
   tzset();

}

/*****************************************************************************/
void tm_resolve(char *s1,const char *format,const struct tm *time)
/******************************************************************************
* English Name: Resolve
* -------------
*
* Purpose: Front-end routine for resolving time tokens. "Season"
* -------- tokens are resolved in addition to "date" command tokens.
*
* Language: ANSI C
* ---------
*
* Notes: 1. The following tokens are resolved in addition to the standard
* ------    "date" command tokens:
*
*                   %f - meteorological season
*                   %q - meteorological season + 2digit year
*                   %Q - meteorological season + 4digit year
*
* Variable Description:
* ---------------------
*
*  (o)-output; (i)-input; (io)-input/output
*
*  (o) s1 - resolved output string.
*  (i) format - input string containing time tokens.
*  (i) time - current date/time structure (see "tm_make").
*  
*
* Programmer: Joseph V. Ardizzone
* ----------- (NASA Goddard Space Flight Center)
*             (Data Assimilation Office)
*
* Modified: March 7, 2000 - created.
* ---------
******************************************************************************/
{
   int ilen;
   char *season;
   char ccyy[5];
   char str[MAXSIZE];
   const char *p = format;
   const char *pprev = format;
   int month = time->tm_mon  + 1;
   int year  = time->tm_year + 1900;

/* Determine meteorological season
 * ===============================
*/
   if (month >= 3 && month <= 5) season = "spring";
   else if (month >= 6 && month <= 8) season = "summer";
   else if (month >= 9 && month <= 11) season = "fall";
   else season = "winter";

   if (month == 12) ++year;
   sprintf(ccyy,"%4d",year);

/* Resolve meteorological season
 * =============================
*/
   str[0] = '\0';
   while ( (p=strchr(p,'%')) != NULL) {

      ilen = strlen(str);
      strncpy(str+ilen,pprev,p-pprev); ilen += p-pprev;
      str[ilen] = '\0';

      switch (p[1]) {

         case 'f': strcat(str,season);
                   pprev = p += 2; break;

         case 'q': strcat(str,season);
                   strcat(str,ccyy+2);
                   pprev = p += 2; break;

         case 'Q': strcat(str,season);
                   strcat(str,ccyy);
                   pprev = p += 2; break;

         case '%': pprev = p; p+= 2; break;

         default:  pprev = p++; break;

      }

   }

   if (pprev-format < strlen(format)) strcat(str,pprev);

/* Resolve standard date command tokens
 * ====================================
*/
   (void) strftime(s1,MAXSIZE,str,time);
}

/*****************************************************************************/
void tm_grads(char *str,const char *format)
/******************************************************************************
* English Name: GRADS
* -------------
*
* Purpose: Convert GRADS time tokens to "date" command tokens.
* --------
*
* Language: ANSI C
* ---------
*
* Notes: 1. See "gtoken[]" for list of recognized GRADS tokens.
* ------
*
* Variable Description:
* ---------------------
*
*  (o)-output; (i)-input; (io)-input/output
*
*  (o) str - resolved output string.
*  (i) format - input string containing time tokens.
*
*
* Programmer: Joseph V. Ardizzone
* ----------- (NASA Goddard Space Flight Center)
*             (Data Assimilation Office)
*
* Modified: March 7, 2000 - created.
* ---------
******************************************************************************/
{
   int i,ilen;
   const char *p = format;
   const char *pprev = format;

   static int ntoken = 10;

   static char 
     *gtoken[] = {"%y2","%y4","%m1","%m2","%mc","%d1","%d2","%h1","%h2","%n2"};

   static char 
     *dtoken[] = {"%y" ,"%Y" ,"^%m","%m" ,"^%b","^%d","%d" ,"^%H","%H" ,"%M" };

/* Locate GRADS tokens and convert
 * ===============================
*/
   str[0] = '\0';
   while ( (p=strchr(p,'%')) != NULL) {

      ilen = strlen(str);
      strncpy(str+ilen,pprev,p-pprev); ilen += p-pprev;
      str[ilen] = '\0';

      for (i=0; i<ntoken; i++) {

         ilen = strlen(gtoken[i]);

         if (strncmp(p,gtoken[i],ilen) == 0) {

            strcat(str,dtoken[i]);
            pprev = p += ilen;
            break;
         }
      }

      if (i >= ntoken) pprev = p++;

   }

   if (pprev-format < strlen(format)) strcat(str,pprev);
}
void strdate_(int *idate,int *itime,int *ilen,char *s1,char *s2)
{
   int i;
   char s3[MAXSIZE],s4[MAXSIZE];
   int ntime[6] = {0,0,0,0,0,0};

/* Extract date/time parameters.
 * =============================
*/

   ntime[0] = *idate / 10000;
   ntime[1] = (*idate - ntime[0] * 10000) / 100;
   ntime[2] = (*idate - ntime[0] * 10000 - ntime[1] * 100);

   ntime[3] = *itime / 10000;
   ntime[4] = (*itime - ntime[3] * 10000) / 100;
   ntime[5] = (*itime - ntime[3] * 10000 - ntime[4] * 100);

/* Resolve time tokens.
 * ====================
*/
   for (i=0; i<MAXSIZE; i++) s3[i] = ' '; s3[MAXSIZE-1] = '\0';
   for (i=0; i<*ilen; i++) s3[i] = s1[i]; s3[*ilen] = '\0';
   strtime(s4,s3,ntime);
   for (i=0; i<strlen(s4); i++) s2[i] = s4[i];
}
void strgrads_(int *idate,int *itime,int *ilen,char *s1,char *s2)
{
   int i;
   char s3[MAXSIZE],s4[MAXSIZE];
   int ntime[6] = {0,0,0,0,0,0};

/* Extract date/time parameters.
 * =============================
*/

   ntime[0] = *idate / 10000;
   ntime[1] = (*idate - ntime[0] * 10000) / 100;
   ntime[2] = (*idate - ntime[0] * 10000 - ntime[1] * 100);

   ntime[3] = *itime / 10000;
   ntime[4] = (*itime - ntime[3] * 10000) / 100;
   ntime[5] = (*itime - ntime[3] * 10000 - ntime[4] * 100);

/* Resolve time tokens.
 * ====================
*/
   strncpy(s3,s1,*ilen);
   tm_grads(s4,s3);
   strtime(s3,s4,ntime);
   for (i=0; i<strlen(s4); i++) s2[i] = s3[i];

}
void inctime_(int *idate, int *itime, int *hours, int *minutes, int *seconds)
{
   int ntime[6];
   struct tm time;

/* Extract date/time parameters. */

   ntime[0] = *idate / 10000;
   ntime[1] = (*idate - ntime[0] * 10000) / 100;
   ntime[2] = (*idate - ntime[0] * 10000 - ntime[1] * 100);
                                                                                
   ntime[3] = *itime / 10000;
   ntime[4] = (*itime - ntime[3] * 10000) / 100;
   ntime[5] = (*itime - ntime[3] * 10000 - ntime[4] * 100);

/* Increment time. */
                                                                                
   ntime[3] += *hours;
   ntime[4] += *minutes;
   ntime[5] += *seconds;

   tm_make(&time,ntime);

   *idate = (time.tm_year + 1900)*10000 + (time.tm_mon+1)*100 + time.tm_mday;
   *itime = time.tm_hour*10000 + time.tm_min*100 + time.tm_sec;

}
