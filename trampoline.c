#include <stdarg.h>
#include <stdio.h>

[[noreturn]]
extern void MIRRS_error_handler_rust(unsigned error_type, const char *msg, size_t len);

[[noreturn]]
int MIRRS_error_handler_trampoline(unsigned error_type, const char *fmt, ...) {
    va_list args1;
    va_start(args1, fmt);
    va_list args2;
    va_copy(args2, args1);
    size_t len = vsnprintf(NULL, 0, fmt, args1);
    char buf[1 + len];
    va_end(args1);
    vsnprintf(buf, sizeof buf, fmt, args2);
    va_end(args2);

    MIRRS_error_handler_rust(error_type, buf, len);
}
