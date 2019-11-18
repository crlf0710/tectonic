#include <stdio.h>
#include <string.h>
#include <stdarg.h>

int dpx_sprintf ( char * str, const char * format, ... );
int dpx_snprintf ( char * str, size_t n, const char * format, ... );
int dpx_sscanf ( const char * str, const char * format, ...);

int dpx_strcasecmp(const char *s1, const char *s2);

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

int dpx_snprintf ( char * str, size_t n, const char * format, ... ) {
    va_list arglist;
    int r;
    va_start(arglist, format);
    r = vsnprintf(str, n, format, arglist);
    va_end(arglist);
    return r;
}

int dpx_sscanf ( const char * str, const char * format, ...) {
    va_list arglist;
    int r;
    va_start(arglist, format);
    r = vsscanf(str, format, arglist);
    va_end(arglist);
    return r;
}

int dpx_strcasecmp(const char *s1, const char *s2)
{
#ifndef _MSC_VER
    return strcasecmp(s1, s2);
#else
    return _stricmp(s1, s2);
#endif
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
