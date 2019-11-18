#include <stdio.h>
#include <string.h>
#include <stdarg.h>

int xetex_sprintf ( char * str, const char * format, ... );
int xetex_snprintf ( char * str, size_t n, const char * format, ... );
int xetex_strcasecmp(const char *s1, const char *s2);

int xetex_sprintf ( char * str, const char * format, ... ) {
    va_list arglist;
    int r;
    va_start(arglist, format);
    r = vsprintf(str, format, arglist);
    va_end(arglist);
    return r;
}

int xetex_snprintf ( char * str, size_t n, const char * format, ... ) {
    va_list arglist;
    int r;
    va_start(arglist, format);
    r = vsnprintf(str, n, format, arglist);
    va_end(arglist);
    return r;
}

int xetex_strcasecmp(const char *s1, const char *s2) {
#ifndef _MSC_VER
    return strcasecmp(s1, s2);
#else
    return _stricmp(s1, s2);
#endif
}
