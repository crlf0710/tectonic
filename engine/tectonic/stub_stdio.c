#include <stdio.h>
#include <string.h>
#include <stdarg.h>

int xetex_sprintf ( char * str, const char * format, ... );

int xetex_sprintf ( char * str, const char * format, ... ) {
    va_list arglist;
    int r;
    va_start(arglist, format);
    r = vsprintf(str, format, arglist);
    va_end(arglist);
    return r;
}
