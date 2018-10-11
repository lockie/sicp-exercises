/* -*- compile-command: "gcc -std=c11 -Wall -Wextra -Wpedantic 51.c `pkg-config --libs --cflags libedit bdw-gc` -o 51" -*- */

/* Sample code to run

   (+ 2 2)

   (define factorial
       (lambda(n)
           (if (= n 0)
           1
           (* n (factorial (- n 1))))))
   (factorial 10)

   (define (filter pred lst)
       (if (null? lst)
           nil
           (begin
               (if (pred (car lst))
                   (cons (car lst) (filter pred (cdr lst)))
                   (filter pred (cdr lst))))))
   (define (even? x) (= x (* 2 (/ x 2))))
   (filter even? (quote (1 2 3 4 5 6 7 8 9 10)))
 */

#include "gc.h"

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdarg.h>
#include <string.h>
#include <setjmp.h>

#include <histedit.h>


/* aux functions */

static jmp_buf jmpbuf;

static int count_char(const char* string, char ch)
{
    char* s = (char*)string;
    int r;
    for(r = 0; s[r]; s[r] == ch ? r++: *s++);
    return r;
}

/* internal structs and helpers */

typedef enum {
    T_NUMBER = 0,
    T_STRING,
    T_SYMBOL,
    T_PAIR,
    T_INTERNAL,
} expr_type_t;

typedef const struct sexpr_t* (*proc_t)(const struct sexpr_t*);

typedef union {
    int number;
    const char* str;
    struct {
        const struct sexpr_t* car;
        const struct sexpr_t* cdr;
    } list;
    proc_t internal;
} expr_value_t;

typedef struct
{
    expr_type_t type;
    expr_value_t value;
} sexpr_t;

#define CAR(expr) ((sexpr_t*)((sexpr_t*)(expr))->value.list.car)
#define CDR(expr) ((sexpr_t*)((sexpr_t*)(expr))->value.list.cdr)
#define CADR(expr) CAR(CDR(expr))
#define CDDR(expr) CDR(CDR(expr))
#define CAADR(expr) CAR(CAR(CDR(expr)))
#define CADDR(expr) CAR(CDR(CDR(expr)))
#define CDADR(expr) CDR(CAR(CDR(expr)))
#define CDDDR(expr) CDR(CDR(CDR(expr)))
#define CADDDR(expr) CAR(CDR(CDR(CDR(expr))))

static const sexpr_t nil = {T_PAIR, {.list={NULL, NULL}}};

static const sexpr_t true = {T_SYMBOL, {.str="true"}};
static const sexpr_t false = {T_SYMBOL, {.str="false"}};

void scheme_pprint(const sexpr_t* expr, int in_list);

static int is_true(const sexpr_t* expr)
{
    return expr != &false;
}

static int is_null(const sexpr_t* expr)
{
    return expr == &nil;
}

static const sexpr_t* list(size_t n, sexpr_t** elements)
{
    sexpr_t* res = (sexpr_t*)&nil;
    for(size_t i = 0; i < n; i++)
    {
        sexpr_t* tail = res;
        res = (sexpr_t*)GC_MALLOC(sizeof(sexpr_t));
        res->type = T_PAIR;
        res->value.list.car = (struct sexpr_t*)elements[n - i - 1];
        res->value.list.cdr = (struct sexpr_t*)tail;
    }
    return res;
}

static const sexpr_t* vlist(size_t n, ...)
{
    va_list args;
    va_start(args, n);
    sexpr_t** elements = (sexpr_t**)GC_MALLOC(n * sizeof(sexpr_t*));
    for(size_t i = 0; i < n; i++)
        elements[i] = va_arg(args, sexpr_t*);
    va_end(args);

    const sexpr_t* result = list(n, elements);
    GC_FREE(elements);
    return result;
}

static size_t length(const sexpr_t* expr)
{
    size_t result = 0;
    for(sexpr_t* el = (sexpr_t*)expr; !is_null(el); result++, el = CDR(el));
    return result;
}

static inline int is_delimiter(char c)
{
    return isspace(c) || c == '(' || c == ')' || c == 0;
}


/* read */

const sexpr_t* scheme_read(const char* code, int* chars_read);

static const sexpr_t* scheme_read_list(const char* code, int* chars_read)
{
    const sexpr_t* read = scheme_read(code, chars_read);
    if(read)
    {
        int internal_chars_read;
        sexpr_t* result = (sexpr_t*)GC_MALLOC(sizeof(sexpr_t));
        result->type = T_PAIR;
        result->value.list.car = (struct sexpr_t*)read;
        result->value.list.cdr = (struct sexpr_t*)
            scheme_read_list(code + *chars_read, &internal_chars_read);
        *chars_read += internal_chars_read;
        return result;
    }
    else
    {
        return &nil;
    }
}

const sexpr_t* scheme_read(const char* code, int* chars_read)
{
    *chars_read = 0;
    sexpr_t* result = NULL;
    char* s = (char*)code;

    for(;isspace(*s); s++);
    int whitespace = s - code;

    if(isdigit(*s))
    {
        for(;isdigit(*s); s++);
        if(is_delimiter(*s))
        {
            *chars_read = s - code;
            char* val = (char*)malloc(*chars_read);
            strncpy(val, code, *chars_read);
            result = (sexpr_t*)GC_MALLOC(sizeof(sexpr_t));
            result->type = T_NUMBER;
            result->value.number = atoi(val);
            free(val);
        }
    }
    else if(*s == '"')
    {
        char* t = s++;
        for(;(*s != '"') && *s; s++);
        if(*s == '"')
        {
            *chars_read = s - t;
            // TODO : interning of sorts?
            char* val = (char*)GC_MALLOC(*chars_read);
            memset(val, 0, *chars_read);
            strncpy(val, t + 1, *chars_read - 1);
            result = (sexpr_t*)GC_MALLOC(sizeof(sexpr_t));
            result->type = T_STRING;
            result->value.str = val;
        }
    }
    else if(!is_delimiter(*s))
    {
        char* t = s++;
        for(;!is_delimiter(*s); s++);
        if(is_delimiter(*s))
        {
            *chars_read = s - t;
            // TODO : interning of sorts?
            char* val = (char*)GC_MALLOC(*chars_read + 1);
            memset(val, 0, *chars_read + 1);
            strncpy(val, t, *chars_read);
            result = (sexpr_t*)GC_MALLOC(sizeof(sexpr_t));
            result->type = T_SYMBOL;
            result->value.str = val;
        }
    }
    else if(*s == '(')
    {
        char* t = s++;
        int b;
        for(b = 1; (b != 0) && *s; s++)
        {
            if(*s == '(') b++;
            if(*s == ')') b--;
        }
        if(b == 0)
        {
            *chars_read = s - t;
            char* val = (char*)malloc(*chars_read);
            memset(val, 0, *chars_read);
            strncpy(val, t + 1, *chars_read - 2);
            int internal_chars_read;
            result = (sexpr_t*)scheme_read_list(val, &internal_chars_read);
            free(val);
        }
    }

    *chars_read += whitespace;
    return result;
}


/* primitives */

static const struct sexpr_t* primitive_plus(const struct sexpr_t* _arglist)
{
    sexpr_t* arglist = (sexpr_t*)_arglist;
    int sum = 0;
    size_t arg_count = 1;
    while(!is_null(arglist))
    {
        if(CAR(arglist)->type != T_NUMBER)
        {
            fprintf(stderr, "%s: argument %zu is not a number\n",
                    __func__, arg_count);
            longjmp(jmpbuf, 1);
        }
        sum += CAR(arglist)->value.number;
        arglist = CDR(arglist);
        arg_count++;
    }
    sexpr_t* result = (sexpr_t*)GC_MALLOC(sizeof(sexpr_t));
    result->type = T_NUMBER;
    result->value.number = sum;
    return (struct sexpr_t*)result;
}


static const struct sexpr_t* primitive_minus(const struct sexpr_t* _arglist)
{
    sexpr_t* arglist = (sexpr_t*)_arglist;
    int sum = 0;
    size_t arg_count = 1;
    while(!is_null(arglist))
    {
        if(CAR(arglist)->type != T_NUMBER)
        {
            fprintf(stderr, "%s: argument %zu is not a number\n",
                    __func__, arg_count);
            longjmp(jmpbuf, 1);
        }
        sum += CAR(arglist)->value.number * (arg_count == 1 ? 1 : -1);
        arglist = CDR(arglist);
        arg_count++;
    }
    sexpr_t* result = (sexpr_t*)GC_MALLOC(sizeof(sexpr_t));
    result->type = T_NUMBER;
    result->value.number = sum;
    return (struct sexpr_t*)result;
}

static const struct sexpr_t* primitive_mult(const struct sexpr_t* _arglist)
{
    sexpr_t* arglist = (sexpr_t*)_arglist;
    int product = 1;
    size_t arg_count = 1;
    while(!is_null(arglist))
    {
        if(CAR(arglist)->type != T_NUMBER)
        {
            fprintf(stderr, "%s: argument %zu is not a number\n",
                    __func__, arg_count);
            longjmp(jmpbuf, 1);
        }
        product *= CAR(arglist)->value.number;
        arglist = CDR(arglist);
        arg_count++;
    }
    sexpr_t* result = (sexpr_t*)GC_MALLOC(sizeof(sexpr_t));
    result->type = T_NUMBER;
    result->value.number = product;
    return (struct sexpr_t*)result;
}

static const struct sexpr_t* primitive_div(const struct sexpr_t* _arglist)
{
    sexpr_t* arglist = (sexpr_t*)_arglist;
    int quotient = 1;
    size_t arg_count = 1;
    while(!is_null(arglist))
    {
        if(CAR(arglist)->type != T_NUMBER)
        {
            fprintf(stderr, "%s: argument %zu is not a number\n",
                    __func__, arg_count);
            longjmp(jmpbuf, 1);
        }
        if(arg_count == 1)
            quotient = CAR(arglist)->value.number;
        else
            quotient /= CAR(arglist)->value.number;
        arglist = CDR(arglist);
        arg_count++;
    }
    sexpr_t* result = (sexpr_t*)GC_MALLOC(sizeof(sexpr_t));
    result->type = T_NUMBER;
    result->value.number = quotient;
    return (struct sexpr_t*)result;
}

static int _compare(const sexpr_t* val1, const sexpr_t* val2)
{
    switch(val1->type)
    {
    case T_NUMBER:
        return val1->value.number == val2->value.number;
        break;

    case T_STRING:
    case T_SYMBOL:
        return !strcmp(val1->value.str, val2->value.str);
        break;

    case T_PAIR:
        if(is_null(val1) && is_null(val2))
            return 1;
        if(_compare(CAR(val1), CAR(val2)) == 0)
            return 0;
        return _compare(CDR(val1), CDR(val2));
        break;

    case T_INTERNAL:
        return val1->value.internal == val2->value.internal;
        break;

    default:
        fprintf(stderr, "%s: unknown type", __func__);
        longjmp(jmpbuf, 1);
        break;
    }
}

static const struct sexpr_t* primitive_eq(const struct sexpr_t* _arglist)
{
    sexpr_t* arglist = (sexpr_t*)_arglist;
    const sexpr_t* first = CAR(arglist);
    arglist = CDR(arglist);
    size_t arg_count = 2;
    while(!is_null(arglist))
    {
        if(CAR(arglist)->type != first->type)
        {
            fprintf(stderr, "%s: type mismatch for argument %zu\n",
                    __func__, arg_count);
            longjmp(jmpbuf, 1);
        }

        if(_compare(first, CAR(arglist)) == 0)
            return (struct sexpr_t*)&false;

        arglist = CDR(arglist);
        arg_count++;
    }
    return (struct sexpr_t*)&true;
}

static const struct sexpr_t* primitive_more(const struct sexpr_t* _arglist)
{
    sexpr_t* arglist = (sexpr_t*)_arglist;
    const sexpr_t* prev = CAR(arglist);
    arglist = CDR(arglist);
    size_t arg_count = 2;
    while(!is_null(arglist))
    {
        if(CAR(arglist)->type != T_NUMBER)
        {
            fprintf(stderr, "%s: argument %zu is not a number\n",
                    __func__, arg_count);
            longjmp(jmpbuf, 1);
        }

        if(prev->value.number < CAR(arglist)->value.number)
            return (struct sexpr_t*)&false;

        arglist = CDR(arglist);
        arg_count++;
    }
    return (struct sexpr_t*)&true;
}

static const struct sexpr_t* primitive_less(const struct sexpr_t* _arglist)
{
    sexpr_t* arglist = (sexpr_t*)_arglist;
    const sexpr_t* prev = CAR(arglist);
    arglist = CDR(arglist);
    size_t arg_count = 2;
    while(!is_null(arglist))
    {
        if(CAR(arglist)->type != T_NUMBER)
        {
            fprintf(stderr, "%s: argument %zu is not a number\n",
                    __func__, arg_count);
            longjmp(jmpbuf, 1);
        }

        if(prev->value.number > CAR(arglist)->value.number)
            return (struct sexpr_t*)&false;

        arglist = CDR(arglist);
        arg_count++;
    }
    return (struct sexpr_t*)&true;
}

static const struct sexpr_t* primitive_cons(const struct sexpr_t* _arglist)
{
    sexpr_t* arglist = (sexpr_t*)_arglist;
    size_t arglen = length(arglist);
    if(arglen != 2)
    {
        fprintf(stderr, "%s: expected 2 arguments, got %zu", __func__, arglen);
        longjmp(jmpbuf, 1);
    }

    sexpr_t* result = (sexpr_t*)GC_MALLOC(sizeof(sexpr_t));
    result->type = T_PAIR;
    result->value.list.car = (struct sexpr_t*)CAR(arglist);
    result->value.list.cdr = (struct sexpr_t*)CADR(arglist);
    return (struct sexpr_t*)result;
}

static const struct sexpr_t* primitive_car(const struct sexpr_t* _arglist)
{
    sexpr_t* arglist = (sexpr_t*)_arglist;
    size_t arglen = length(arglist);
    if(arglen != 1)
    {
        fprintf(stderr, "%s: expected 1 argument, got %zu", __func__, arglen);
        longjmp(jmpbuf, 1);
    }
    sexpr_t* arg = CAR(arglist);
    if(arg->type != T_PAIR)
    {
        fprintf(stderr, "%s: argument is not a cons cell\n", __func__);
        longjmp(jmpbuf, 1);
    }
    if(is_null(arg))
    {
        fprintf(stderr, "%s: argument is nil\n", __func__);
        longjmp(jmpbuf, 1);
    }

    return (struct sexpr_t*)CAR(arg);
}

static const struct sexpr_t* primitive_cdr(const struct sexpr_t* _arglist)
{
    sexpr_t* arglist = (sexpr_t*)_arglist;
    size_t arglen = length(arglist);
    if(arglen != 1)
    {
        fprintf(stderr, "%s: expected 1 argument, got %zu", __func__, arglen);
        longjmp(jmpbuf, 1);
    }
    sexpr_t* arg = CAR(arglist);
    if(arg->type != T_PAIR)
    {
        fprintf(stderr, "%s: argument is not a cons cell\n", __func__);
        longjmp(jmpbuf, 1);
    }
    if(is_null(arg))
    {
        fprintf(stderr, "%s: argument is nil\n", __func__);
        longjmp(jmpbuf, 1);
    }

    return (struct sexpr_t*)CDR(arg);
}

static const struct sexpr_t* primitive_list(const struct sexpr_t* _arglist)
{
    sexpr_t* arglist = (sexpr_t*)_arglist;
    size_t n = length(arglist);
    sexpr_t** elements = (sexpr_t**)GC_MALLOC(n * sizeof(sexpr_t));
    sexpr_t* e = arglist;
    for(size_t i = 0; i < n; i++, e = CDR(e))
        elements[i] = CAR(e);

    const sexpr_t* result = list(n, elements);

    GC_FREE(elements);
    return (struct sexpr_t*)result;
}

static const struct sexpr_t* primitive_isnull(const struct sexpr_t* _arglist)
{
    sexpr_t* arglist = (sexpr_t*)_arglist;
    size_t arglen = length(arglist);
    if(arglen != 1)
    {
        fprintf(stderr, "%s: expected 1 argument, got %zu", __func__, arglen);
        longjmp(jmpbuf, 1);
    }
    sexpr_t* arg = CAR(arglist);

    return (struct sexpr_t*)(is_null(arg) ? &true : &false);
}

static const struct sexpr_t* primitive_display(const struct sexpr_t* _arglist)
{
    sexpr_t* arglist = (sexpr_t*)_arglist;
    while(!is_null(arglist))
    {
        scheme_pprint(CAR(arglist), 0);
        arglist = CDR(arglist);
    }

    return (struct sexpr_t*)&nil;
}

static const struct sexpr_t* primitive_newline(const struct sexpr_t* _arglist)
{
    sexpr_t* arglist = (sexpr_t*)_arglist;
    size_t arglen = length(arglist);
    if(arglen != 0)
    {
        fprintf(stderr, "%s: no arguments expected, got %zu", __func__, arglen);
        longjmp(jmpbuf, 1);
    }

    printf("\n");
    return (struct sexpr_t*)&nil;
}


/* environment aux functions */

static const sexpr_t* make_frame(const sexpr_t* vars, const sexpr_t* vals)
{
    sexpr_t* result = (sexpr_t*)GC_MALLOC(sizeof(sexpr_t));
    result->type = T_PAIR;
    result->value.list.car = (struct sexpr_t*)vars;
    result->value.list.cdr = (struct sexpr_t*)vals;
    return result;
}

static const sexpr_t* extend_environment(const sexpr_t* vars,
                                         const sexpr_t* vals,
                                         const sexpr_t* base_env)
{
    size_t vars_length = length(vars);
    size_t vals_length = length(vals);
    if(vars_length != vals_length)
    {
        fprintf(stderr, "%s: expected %zu arguments, but %zu supplied\n",
                __func__, vars_length, vals_length);
        longjmp(jmpbuf, 1);
    }
    sexpr_t* result = is_null(base_env) ?
        (sexpr_t*)GC_MALLOC_UNCOLLECTABLE(sizeof(sexpr_t)) :
        (sexpr_t*)GC_MALLOC(sizeof(sexpr_t));
    result->type = T_PAIR;
    result->value.list.car = (struct sexpr_t*)make_frame(vars, vals);
    result->value.list.cdr = (struct sexpr_t*)base_env;
    return result;
}

static const sexpr_t* enclosing_environment(const sexpr_t* env)
{
    return CDR(env);
}

static const sexpr_t* frame_variables(const sexpr_t* frame)
{
    return CAR(frame);
}

static const sexpr_t* frame_values(const sexpr_t* frame)
{
    return CDR(frame);
}

static const sexpr_t* _env_loop(const char* var, const sexpr_t* env);

static const sexpr_t* _lookup_scan(const char* var, const sexpr_t* env,
                            const sexpr_t* vars, const sexpr_t* vals)
{
    // Dear compiler, good luck optimizing this
    if(is_null(vars))
        return _env_loop(var, enclosing_environment(env));
    if(strcmp(var, CAR(vars)->value.str) == 0)
        return CAR(vals);
    return _lookup_scan(var, env, CDR(vars), CDR(vals));
}

static const sexpr_t* _env_loop(const char* var, const sexpr_t* env)
{
    if(is_null(env))
    {
        fprintf(stderr, "%s: unbound variable \"%s\"\n", __func__, var);
        longjmp(jmpbuf, 1);
    }

    sexpr_t* frame = CAR(env);
    return _lookup_scan(var, env, frame_variables(frame), frame_values(frame));
}

static const sexpr_t* lookup_variable_value(const char* var, const sexpr_t* env)
{
    return _env_loop(var, env);
}

static void add_binding_to_frame(const sexpr_t* var, const sexpr_t* val, sexpr_t* frame)
{
    sexpr_t* new_car = (sexpr_t*)GC_MALLOC(sizeof(sexpr_t));
    new_car->type = T_PAIR;
    new_car->value.list.car = (struct sexpr_t*)var;
    new_car->value.list.cdr = (struct sexpr_t*)CAR(frame);
    frame->value.list.car = (struct sexpr_t*)new_car;

    sexpr_t* new_cdr = (sexpr_t*)GC_MALLOC(sizeof(sexpr_t));
    new_cdr->type = T_PAIR;
    new_cdr->value.list.car = (struct sexpr_t*)val;
    new_cdr->value.list.cdr = (struct sexpr_t*)CDR(frame);
    frame->value.list.cdr = (struct sexpr_t*)new_cdr;
}

static void _define_scan(const sexpr_t* var, const sexpr_t* val,
                         sexpr_t* frame,
                         const sexpr_t* vars, sexpr_t* vals)
{
    if(is_null(vars))
    {
        add_binding_to_frame(var, val, frame);
        return;
    }
    if(strcmp(var->value.str, CAR(vars)->value.str) == 0)
    {
        vals->value.list.car = (struct sexpr_t*)val;
        return;
    }
    _define_scan(var, val, frame, CDR(vars), CDR(vals));
}

static const sexpr_t* define_variable(const sexpr_t* var,
                            const sexpr_t* val,
                            const sexpr_t* env)
{
    sexpr_t* frame = CAR(env);
    _define_scan(var, val, frame, frame_variables(frame), (sexpr_t*)frame_values(frame));
    return &nil;
}

static sexpr_t* primitive_proc_names[] = {
    &(sexpr_t){T_STRING, {.str="+"}},
    &(sexpr_t){T_STRING, {.str="-"}},
    &(sexpr_t){T_STRING, {.str="*"}},
    &(sexpr_t){T_STRING, {.str="/"}},
    &(sexpr_t){T_STRING, {.str="="}},
    &(sexpr_t){T_STRING, {.str=">"}},
    &(sexpr_t){T_STRING, {.str="<"}},
    &(sexpr_t){T_STRING, {.str="cons"}},
    &(sexpr_t){T_STRING, {.str="car"}},
    &(sexpr_t){T_STRING, {.str="cdr"}},
    &(sexpr_t){T_STRING, {.str="list"}},
    &(sexpr_t){T_STRING, {.str="null?"}},
    &(sexpr_t){T_STRING, {.str="display"}},
    &(sexpr_t){T_STRING, {.str="newline"}},
};

static proc_t primitive_proc_funcs[] = {
    primitive_plus,
    primitive_minus,
    primitive_mult,
    primitive_div,
    primitive_eq,
    primitive_more,
    primitive_less,
    primitive_cons,
    primitive_car,
    primitive_cdr,
    primitive_list,
    primitive_isnull,
    primitive_display,
    primitive_newline,
};

static const sexpr_t* setup_environment()
{
    size_t primitive_count =
        sizeof(primitive_proc_names) / sizeof(primitive_proc_names[0]);

    sexpr_t** primitive_proc_values =
        (sexpr_t**)GC_MALLOC(primitive_count * sizeof(sexpr_t*));

    sexpr_t* primitive_symbol = (sexpr_t*)GC_MALLOC_UNCOLLECTABLE(sizeof(sexpr_t));
    primitive_symbol->type = T_SYMBOL;
    primitive_symbol->value.str = "primitive";

    for(size_t i = 0; i < primitive_count; i++)
    {
        sexpr_t* internal_func = (sexpr_t*)GC_MALLOC_UNCOLLECTABLE(sizeof(sexpr_t));
        internal_func->type = T_INTERNAL;
        internal_func->value.internal = primitive_proc_funcs[i];
        primitive_proc_values[i] = (sexpr_t*)vlist(2, primitive_symbol, internal_func);
    }

    const sexpr_t* env = extend_environment(
        list(primitive_count, primitive_proc_names),
        list(primitive_count, primitive_proc_values),
        &nil);

    sexpr_t* true_symbol = (sexpr_t*)GC_MALLOC_UNCOLLECTABLE(sizeof(sexpr_t));
    true_symbol->type = T_SYMBOL;
    true_symbol->value.str = "true";
    define_variable(true_symbol, &true, env);

    sexpr_t* false_symbol = (sexpr_t*)GC_MALLOC_UNCOLLECTABLE(sizeof(sexpr_t));
    false_symbol->type = T_SYMBOL;
    false_symbol->value.str = "false";
    define_variable(false_symbol, &false, env);

    sexpr_t* nil_symbol = (sexpr_t*)GC_MALLOC_UNCOLLECTABLE(sizeof(sexpr_t));
    nil_symbol->type = T_SYMBOL;
    nil_symbol->value.str = "nil";
    define_variable(nil_symbol, &nil, env);

    printf("Initialized with %zu primitives\n", primitive_count);

    GC_FREE(primitive_proc_values);

    return env;
}


/* eval aux functions */

static int is_tagged_list(const sexpr_t* expr, const char* tag)
{
    if(expr->type != T_PAIR)
        return 0;
    return
        (CAR(expr)->type == T_SYMBOL) &&
        (strcmp(CAR(expr)->value.str, tag) == 0);
}

static int is_self_evaluating(const sexpr_t* expr)
{
    return (expr->type == T_NUMBER) || (expr->type == T_STRING);
}

static int is_variable(const sexpr_t* expr)
{
    return (expr->type == T_SYMBOL);
}

static int is_quoted(const sexpr_t* expr)
{
    return is_tagged_list(expr, "quote");
}

static int is_definition(const sexpr_t* expr)
{
    return is_tagged_list(expr, "define");
}

static int is_if(const sexpr_t* expr)
{
    return is_tagged_list(expr, "if");
}

static int is_lambda(const sexpr_t* expr)
{
    return is_tagged_list(expr, "lambda");
}

static int is_begin(const sexpr_t* expr)
{
    return is_tagged_list(expr, "begin");
}

static int is_application(const sexpr_t* expr)
{
    return expr->type == T_PAIR;
}

static const sexpr_t* text_of_quotation(const sexpr_t* expr)
{
    return CADR(expr);
}

static sexpr_t lambda_symbol = {T_SYMBOL, {.str="lambda"}};

static const sexpr_t* make_lambda(const sexpr_t* parameters, const sexpr_t* body)
{
    sexpr_t* params_and_body = (sexpr_t*)GC_MALLOC(sizeof(sexpr_t));
    params_and_body->type = T_PAIR;
    params_and_body->value.list.car = (struct sexpr_t*)parameters;
    params_and_body->value.list.cdr = (struct sexpr_t*)body;

    sexpr_t* result = (sexpr_t*)GC_MALLOC(sizeof(sexpr_t));
    result->type = T_PAIR;
    result->value.list.car = (struct sexpr_t*)&lambda_symbol;
    result->value.list.cdr = (struct sexpr_t*)params_and_body;
    return result;
}

static const sexpr_t* lambda_parameters(const sexpr_t* expr)
{
    return CADR(expr);
}

static const sexpr_t* lambda_body(const sexpr_t* expr)
{
    return CDDR(expr);
}

static const sexpr_t* definition_variable(const sexpr_t* expr)
{
    if(CADR(expr)->type == T_SYMBOL)
        return CADR(expr);
    else
        return CAADR(expr);
}

static const sexpr_t* definition_value(const sexpr_t* expr)
{
    if(CADR(expr)->type == T_SYMBOL)
        return CADDR(expr);
    else
        return make_lambda(CDADR(expr), CDDR(expr));
}

static const sexpr_t* if_predicate(const sexpr_t* expr)
{
    return CADR(expr);
}

static const sexpr_t* if_consequent(const sexpr_t* expr)
{
    return CADDR(expr);
}

static const sexpr_t* if_alternative(const sexpr_t* expr)
{
    if(!is_null(CDDDR(expr)))
        return CADDDR(expr);
    return &false;
}

static sexpr_t procedure_symbol = {T_SYMBOL, {.str="procedure"}};

static const sexpr_t* make_procedure(const sexpr_t* parameters,
                                     const sexpr_t* body,
                                     const sexpr_t* env)
{
    return vlist(4, &procedure_symbol, parameters, body, env);
}

static const sexpr_t* begin_actions(const sexpr_t* expr)
{
    return CDR(expr);
}

static const sexpr_t* first_expr(const sexpr_t* seq)
{
    return CAR(seq);
}

static const sexpr_t* rest_exprs(const sexpr_t* seq)
{
    return CDR(seq);
}

static int is_last_expr(const sexpr_t* expr)
{
    return is_null(CDR(expr));
}

static const sexpr_t* operator(const sexpr_t* expr)
{
    return CAR(expr);
}

static const sexpr_t* operands(const sexpr_t* expr)
{
    return CDR(expr);
}

static const sexpr_t* first_operand(const sexpr_t* expr)
{
    return CAR(expr);
}

static const sexpr_t* rest_operands(const sexpr_t* expr)
{
    return CDR(expr);
}

static int is_no_operands(const sexpr_t* expr)
{
    return is_null(expr);
}

static int is_last_operand(const sexpr_t* expr)
{
    return is_null(expr);
}

static sexpr_t* adjoin_arg(const sexpr_t* arg, sexpr_t* arglist)
{
    sexpr_t* tail = NULL;
    if(is_null(arglist))
    {
        arglist = (sexpr_t*)GC_MALLOC(sizeof(sexpr_t));
        arglist->type = T_PAIR;
        tail = arglist;
    }
    else
    {
        sexpr_t* pre_tail = (sexpr_t*)arglist;
        while(CDDR(pre_tail))
            pre_tail = CDR(pre_tail);
        pre_tail->value.list.cdr = (struct sexpr_t*)GC_MALLOC(sizeof(sexpr_t));
        tail = CDR(pre_tail);
    }
    tail->value.list.car = (struct sexpr_t*)arg;
    tail->value.list.cdr = (struct sexpr_t*)&nil;
    return arglist;
}

static const sexpr_t* scheme_eval(const sexpr_t* expr, const sexpr_t* env);

static const sexpr_t* eval_sequence(const sexpr_t* seq, const sexpr_t* env)
{
    const sexpr_t* expr = first_expr(seq);
    if(is_last_expr(seq))
        return scheme_eval(expr, env);
    scheme_eval(expr, env);
    return eval_sequence(rest_exprs(seq), env);
}

/* apply */

static int is_primitive_procedure(const sexpr_t* expr)
{
    return is_tagged_list(expr, "primitive");
}

static int is_compound_procedure(const sexpr_t* expr)
{
    return is_tagged_list(expr, "procedure");
}

proc_t primitive_implementation(const sexpr_t* proc)
{
    return CADR(proc)->value.internal;
}

static const sexpr_t* apply_primitive_procedure(const sexpr_t* proc,
                                                const sexpr_t* args)
{
    proc_t proc_impl = primitive_implementation(proc);
    return (sexpr_t*)proc_impl((const struct sexpr_t*)args);
}

static const sexpr_t* procedure_parameters(const sexpr_t* p)
{
    return CADR(p);
}

static const sexpr_t* procedure_body(const sexpr_t* p)
{
    return CADDR(p);
}

static const sexpr_t* procedure_environment(const sexpr_t* p)
{
    return CADDDR(p);
}

static const sexpr_t* apply(const sexpr_t* proc, const sexpr_t* argl)
{
    if(is_primitive_procedure(proc))
    {
        return apply_primitive_procedure(proc, argl);
    }
    if(is_compound_procedure(proc))
    {
        return eval_sequence(
            procedure_body(proc),
            extend_environment(
                procedure_parameters(proc),
                argl,
                procedure_environment(proc)));
    }

    fprintf(stderr, "%s: unknown procedure type\n", __func__);
    longjmp(jmpbuf, 1);
}

/* eval */

const sexpr_t* scheme_eval(const sexpr_t* expr, const sexpr_t* env)
{
    if(is_self_evaluating(expr))
        return expr;
    if(is_variable(expr))
        return lookup_variable_value(expr->value.str, env);
    if(is_quoted(expr))
        return text_of_quotation(expr);
    if(is_definition(expr))
        return define_variable(definition_variable(expr),
                               scheme_eval(definition_value(expr), env),
                               env);
    if(is_if(expr))
        return is_true(scheme_eval(if_predicate(expr), env)) ?
            scheme_eval(if_consequent(expr), env) :
            scheme_eval(if_alternative(expr), env);
    if(is_lambda(expr))
        return make_procedure(lambda_parameters(expr),
                              lambda_body(expr),
                              env);
    if(is_begin(expr))
        return eval_sequence(begin_actions(expr), env);
    if(is_application(expr))
    {
        const sexpr_t* argl = &nil;
        const sexpr_t* ops = operands(expr);
        if(!is_no_operands(ops))
        {
            do
            {
                argl = adjoin_arg(scheme_eval(first_operand(ops), env),
                                  (sexpr_t*)argl);
                ops = rest_operands(ops);
            } while(!is_last_operand(ops));
        }
        return apply(scheme_eval(operator(expr), env), argl);
    }

    fprintf(stderr, "%s: unknown expression type\n", __func__);
    longjmp(jmpbuf, 1);
}


/* prompt facilities */

void scheme_pprint(const sexpr_t* expr, int in_list)
{
    if(!expr)
        return;

    switch(expr->type)
    {
    case T_NUMBER:
        printf("%d", expr->value.number);
        break;

    case T_STRING:
        printf("\"%s\"", expr->value.str);
        break;

    case T_SYMBOL:
        printf("%s", expr->value.str);
        break;

    case T_PAIR:
        if(is_null(expr))
        {
            printf("nil");
        } else if(is_primitive_procedure(expr))
        {
            printf("<primitive procedure at %p>", CADR(expr)->value.str);
        } else if(is_compound_procedure(expr))
        {
            printf("<compound procedure>");
        } else
        {
            if(!in_list)
                printf("(");
            scheme_pprint(CAR(expr), CDR(expr)->type == T_PAIR ? 0 : 1);
            printf(" . ");
            scheme_pprint(CDR(expr), 1);
            if(!in_list)
                printf(")");
        }
        break;

    case T_INTERNAL:
        printf("<internal %p>", expr->value.str);
        break;
    }
}

static int keepreading = 1;

#define CURRENT_LINE_BUFSIZE 8192
static char current_line[CURRENT_LINE_BUFSIZE];

static char* prompt(__attribute__((unused)) EditLine* e)
{
    return current_line[0] ? ">": "scheme> ";
}


/* entrypoint */

int main(__attribute__((unused)) int argc, char** argv)
{
    EditLine* el = el_init(argv[0], stdin, stdout, stderr);
    el_set(el, EL_PROMPT, &prompt);
    el_set(el, EL_EDITOR, "vi");

    History* hist = history_init();
    if(hist == NULL)
    {
        fprintf(stderr, "History could not be initialized\n");
        return EXIT_FAILURE;
    }
    HistEvent ev;
    history(hist, &ev, H_SETSIZE, 1000);
    el_set(el, EL_HIST, history, hist);

    GC_INIT();

    const sexpr_t* global_environment = setup_environment();

    while(keepreading)
    {
        int count = 0;
        const char* line = el_gets(el, &count);
        if(!line)
        {
            keepreading = 0;
        }
        if(count > 1)
        {
            strncat(current_line, line,
                    CURRENT_LINE_BUFSIZE - strlen(current_line));
            if(count_char(current_line, '(') <= count_char(current_line, ')'))
            {
                history(hist, &ev, H_ENTER, current_line);
                char* str = current_line;
                int chars_read;
                do
                {
                    const sexpr_t* expr = scheme_read(str, &chars_read);
                    if(expr)
                    {
                        if(!setjmp(jmpbuf))
                        {
                            const sexpr_t* result =
                                scheme_eval(expr, global_environment);
                            scheme_pprint(result, 0);
                            printf("\n");
                        }
                        GC_FREE((void*)expr);
                    }
                    str += chars_read;
                } while(chars_read);
                if(str == current_line)
                {
                    fprintf(stderr, "parse error\n");
                }
                memset(current_line, 0, CURRENT_LINE_BUFSIZE);
            }
        }
    }

    history_end(hist);
    el_end(el);
    return EXIT_SUCCESS;
}
