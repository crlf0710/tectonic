#include <stdio.h>
#include <string.h>
#include <stdarg.h>

int dpx_sprintf ( char * str, const char * format, ... );

// shims

int dpx_sprintf(char *str, const char *format, ...)
{
    va_list arglist;
    int r;
    va_start(arglist, format);
    r = vsprintf(str, format, arglist);
    va_end(arglist);
    return r;
}

#ifdef _MSC_VER

int dpx_win32_mktemp_s(char *nameTemplate,
                       size_t sizeInChars);

int dpx_win32_mktemp_s(char *nameTemplate,
                       size_t sizeInChars)
{
    return _mktemp_s(nameTemplate, sizeInChars);
}

#endif
