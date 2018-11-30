#ifndef _RUNTIME_H_
#define _RUNTIME_H_

#include <stdio.h>
#include <string.h>
#include <glib.h>


typedef GVariant* (primitive_procedure_t)(GVariant*);

typedef struct
{
    gchar* name;
    GVariant* value;
} variable_t;

variable_t* new_variable(const gchar* name, GVariant* value)
{
    g_assert(name != NULL);
    variable_t* result = g_new(variable_t, 1);
    result->name = g_strdup(name);
    result->value = g_variant_ref_sink(value);
    return result;
}

void free_variable(gpointer _var)
{
    variable_t* var = (variable_t*)_var;
    g_free(var->name);
    g_variant_unref(var->value);
    g_free(var);
}

void free_frame(gpointer frame)
{
    g_sequence_free(frame);
}

gint variable_comparator(gconstpointer a, gconstpointer b,
                         __attribute__((unused)) gpointer data)
{
    const variable_t* var1 = a;
    const variable_t* var2 = b;
    return strcmp(var1->name, var2->name);
}

GVariant* lookup_variable_value(const gchar* var, GSList* env)
{
    for(GSList* it = env; it; it = it->next)
    {
        GSequence* frame = it->data;
        variable_t dummy = {(gchar*)var, NULL};
        GSequenceIter* iter = g_sequence_lookup(frame, &dummy,
                                                variable_comparator, NULL);
        if(iter)
        {
            variable_t* var = g_sequence_get(iter);
            return g_variant_ref_sink(var->value);
        }
    }
    g_error("Unbound variable \"%s\"", var);
}

void set_variable_value(const gchar* var, GVariant* val, GSList* env)
{
    for(GSList* it = env; it; it = it->next)
    {
        GSequence* frame = env->data;
        variable_t dummy = {(gchar*)var, NULL};
        GSequenceIter* iter = g_sequence_lookup(frame, &dummy,
                                                variable_comparator, NULL);
        if(iter)
        {
            variable_t* var = g_sequence_get(iter);
            g_variant_unref(var->value);
            var->value = val;
            return;
        }
    }
    g_error("Unbound variable \"%s\"", var);
}

void define_variable(const gchar* var, GVariant* val, GSList* env)
{
    GSequence* frame = env->data;
    variable_t dummy = {(gchar*)var, NULL};
    GSequenceIter* iter = g_sequence_lookup(frame, &dummy,
                                            variable_comparator, NULL);
    if(!iter)
    {
        g_sequence_insert_sorted(frame, new_variable(var, val),
                                 variable_comparator, NULL);
    }
    else
    {
        // redefenition
        variable_t* var = g_sequence_get(iter);
        g_variant_unref(var->value);
        var->value = val;
    }
}

GSList* extend_environment(GVariant* vars, GVariant* vals, GSList* env)
{
    gsize nvars = g_variant_n_children(vars);
    gsize nvals = g_variant_n_children(vals);
    if(nvars == nvals)
    {
        GSequence* frame = g_sequence_new(free_variable);
        GSequenceIter* iter = g_sequence_get_end_iter(frame);
        for(gsize i = 0; i < nvars; i++)
        {
            GVariant* var = g_variant_get_child_value(vars, i);
            const gchar* var_name = NULL;
            if(g_variant_is_of_type(var, G_VARIANT_TYPE_STRING))
                var_name = g_variant_get_string(var, NULL);
            if(g_variant_is_of_type(var, G_VARIANT_TYPE_HANDLE))
                var_name = g_quark_to_string(g_variant_get_handle(var));
            GVariant* val = g_variant_get_child_value(vals, i);
            g_sequence_insert_before(iter, new_variable(var_name, val));
            g_variant_unref(var);
        }
        g_sequence_sort(frame, variable_comparator, NULL);
        env = g_slist_prepend(env, frame);
    }
    return env;
}

gboolean is_false(GVariant* val)
{
    return g_variant_is_of_type(val, G_VARIANT_TYPE_HANDLE) &&
        (GQuark)g_variant_get_handle(val) == g_quark_from_string("#f");
}

gboolean is_primitive_procedure(GVariant* val)
{
    GVariant* tag = g_variant_get_child_value(val, 0);
    gboolean result = g_variant_is_of_type(tag, G_VARIANT_TYPE_HANDLE) &&
        (GQuark)g_variant_get_handle(g_variant_get_child_value(val, 0)) ==
        g_quark_from_string("primitive-compiled");
    g_variant_unref(tag);
    return result;
}

GString* display(GVariant* expr, GSList* seen);

GVariant* apply_primitive_procedure(GVariant* proc, GVariant* argl)
{
    GVariant* proc_obj = g_variant_get_child_value(proc, 1);
    primitive_procedure_t* _proc =
        (primitive_procedure_t*)g_variant_get_uint64(proc_obj);
    GVariant* res = _proc(argl);
    g_variant_unref(proc_obj);
    return res;
}

GVariant* make_compiled_procedure(gpointer entry, GSList* env)
{
    return g_variant_new(
        "(htt)",
        g_quark_from_string("compiled-procedure"),
        entry,
        env);
}

gpointer compiled_procedure_entry(GVariant* proc)
{
    GVariant* entry = g_variant_get_child_value(proc, 1);
    gpointer result = (gpointer)g_variant_get_uint64(entry);
    g_variant_unref(entry);
    return result;
}

GSList* compiled_procedure_env(GVariant* proc)
{
    GVariant* env = g_variant_get_child_value(proc, 2);
    GSList* result = (GSList*)g_variant_get_uint64(env);
    g_variant_unref(env);
    return result;
}

#define EXPECT_ARGS(n) gsize nargs = g_variant_n_children(args);    \
    if(nargs != n)                                                  \
        g_error("%s: expected %d arguments, got %lu",               \
                __FUNCTION__, n, nargs);

#define EXPECT_ANY_ARGS gsize nargs = g_variant_n_children(args);   \
    if(nargs < 1)                                                   \
        g_error("%s: expected at least one argument, got none",     \
                __FUNCTION__);

#define EXPECT_INTEGER_ARG(arg, n)                                      \
    if(!g_variant_is_of_type(arg, G_VARIANT_TYPE_INT64))                \
        g_error("%s: argument %lu is not an integer", __FUNCTION__, n);

#define EXPECT_NUMERIC_ARG(arg, n)                                      \
    if(!g_variant_is_of_type(arg, G_VARIANT_TYPE_INT64) &&              \
       !g_variant_is_of_type(arg, G_VARIANT_TYPE_DOUBLE))               \
        g_error("%s: argument %lu is not a number", __FUNCTION__, n);

#define EXPECT_PAIR_ARG(arg, n)                                     \
    if(!g_variant_is_of_type(arg, G_VARIANT_TYPE_TUPLE))            \
        g_error("%s: argument %lu is not a pair", __FUNCTION__, n);

#define EXPECT_NONEMPTY_PAIR_ARG(arg, n)                                \
    if(!g_variant_is_of_type(arg, G_VARIANT_TYPE_TUPLE) ||              \
       g_variant_n_children(arg) < 1)                                   \
        g_error("%s: argument %lu is not a non-empty pair", __FUNCTION__, n);

#define GET_NUMERIC_ARG(arg)                                            \
    (g_variant_is_of_type(arg, G_VARIANT_TYPE_INT64) ?                  \
     (gdouble)g_variant_get_int64(arg) :                                \
     g_variant_get_double(arg))

GVariant* primitive_apply(GVariant* args)
{
    EXPECT_ARGS(2);

    GVariant* arg1 = g_variant_get_child_value(args, 0);
    GVariant* arg2 = g_variant_get_child_value(args, 1);
    GVariant* result = NULL;

    EXPECT_NONEMPTY_PAIR_ARG(arg1, 1lu);
    EXPECT_PAIR_ARG(arg2, 2lu);

    if(is_primitive_procedure(arg1))
        result = apply_primitive_procedure(arg1, arg2);

    g_variant_unref(arg2);
    g_variant_unref(arg1);

    if(!result)
        // HACK: not trying to apply compound procedures
        g_error("%s: unknown procedure type", __FUNCTION__);
    return result;
}

GVariant* primitive_plus(GVariant* args)
{
    EXPECT_ANY_ARGS;

    gboolean is_integer = TRUE;
    for(gsize i = 0; i < nargs; i++)
    {
        GVariant* arg = g_variant_get_child_value(args, i);
        EXPECT_NUMERIC_ARG(arg, nargs - i);
        if(g_variant_is_of_type(arg, G_VARIANT_TYPE_DOUBLE))
            is_integer = FALSE;
        g_variant_unref(arg);
    }

    gdouble res = 0;
    int int_res = 0;

    for(gsize i = 0; i < nargs; i++)
    {
        GVariant* arg = g_variant_get_child_value(args, i);
        if(is_integer)
            int_res += g_variant_get_int64(arg);
        else
            res += GET_NUMERIC_ARG(arg);
        g_variant_unref(arg);
    }

    return is_integer ? g_variant_new_int64(int_res) : g_variant_new_double(res);
}

GVariant* primitive_minus(GVariant* args)
{
    EXPECT_ANY_ARGS;

    gboolean is_integer = TRUE;
    for(gsize i = 0; i < nargs; i++)
    {
        GVariant* arg = g_variant_get_child_value(args, i);
        EXPECT_NUMERIC_ARG(arg, nargs - i);
        if(g_variant_is_of_type(arg, G_VARIANT_TYPE_DOUBLE))
            is_integer = FALSE;
        g_variant_unref(arg);
    }

    GVariant* arg1 = g_variant_get_child_value(args, 0);
    gdouble res = nargs == 1 ? -GET_NUMERIC_ARG(arg1) : GET_NUMERIC_ARG(arg1);
    int int_res = nargs == 1 ? -g_variant_get_int64(arg1) : g_variant_get_int64(arg1);
    g_variant_unref(arg1);

    for(gsize i = 1; i < nargs; i++)
    {
        GVariant* arg = g_variant_get_child_value(args, i);
        if(is_integer)
            int_res -= g_variant_get_int64(arg);
        else
            res -= GET_NUMERIC_ARG(arg);
        g_variant_unref(arg);
    }

    return is_integer ? g_variant_new_int64(int_res) : g_variant_new_double(res);
}

GVariant* primitive_mult(GVariant* args)
{
    EXPECT_ANY_ARGS;

    gboolean is_integer = TRUE;
    for(gsize i = 0; i < nargs; i++)
    {
        GVariant* arg = g_variant_get_child_value(args, i);
        EXPECT_NUMERIC_ARG(arg, nargs - i);
        if(g_variant_is_of_type(arg, G_VARIANT_TYPE_DOUBLE))
            is_integer = FALSE;
        g_variant_unref(arg);
    }

    gdouble res = 1;
    int int_res = 1;

    for(gsize i = 0; i < nargs; i++)
    {
        GVariant* arg = g_variant_get_child_value(args, i);
        if(is_integer)
            int_res *= g_variant_get_int64(arg);
        else
            res *= GET_NUMERIC_ARG(arg);
        g_variant_unref(arg);
    }

    return is_integer ? g_variant_new_int64(int_res) : g_variant_new_double(res);
}

GVariant* primitive_div(GVariant* args)
{
    EXPECT_ANY_ARGS;

    for(gsize i = 0; i < nargs; i++)
    {
        GVariant* arg = g_variant_get_child_value(args, i);
        EXPECT_NUMERIC_ARG(arg, nargs - i);
        g_variant_unref(arg);
    }

    GVariant* arg1 = g_variant_get_child_value(args, 0);
    gdouble res = GET_NUMERIC_ARG(arg1);
    g_variant_unref(arg1);

    for(gsize i = 1; i < nargs; i++)
    {
        GVariant* arg = g_variant_get_child_value(args, i);
        res /= GET_NUMERIC_ARG(arg);
        g_variant_unref(arg);
    }

    return g_variant_new_double(res);
}

GVariant* primitive_remainder(GVariant* args)
{
    EXPECT_ARGS(2);

    GVariant* arg1 = g_variant_get_child_value(args, 0);
    GVariant* arg2 = g_variant_get_child_value(args, 1);

    EXPECT_INTEGER_ARG(arg1, 1lu);
    EXPECT_INTEGER_ARG(arg2, 2lu);

    GVariant* result = g_variant_new_int64(g_variant_get_int64(arg1) %
                                           g_variant_get_int64(arg2));

    g_variant_unref(arg2);
    g_variant_unref(arg1);
    return result;
}

GVariant* primitive_not(GVariant* args)
{
    EXPECT_ARGS(1);

    GVariant* arg = g_variant_get_child_value(args, 0);

    GVariant* result = is_false(arg) ?
        g_variant_new_handle(g_quark_from_string("#t")) :
        g_variant_new_handle(g_quark_from_string("#f"));
    g_variant_unref(arg);
    return result;
}

GVariant* primitive_lt(GVariant* args)
{
    EXPECT_ARGS(2);

    GVariant* arg1 = g_variant_get_child_value(args, 0);
    GVariant* arg2 = g_variant_get_child_value(args, 1);

    EXPECT_NUMERIC_ARG(arg1, 1lu);
    EXPECT_NUMERIC_ARG(arg2, 2lu);

    GVariant* result = GET_NUMERIC_ARG(arg1) < GET_NUMERIC_ARG(arg2) ?
        g_variant_new_handle(g_quark_from_string("#t")) :
        g_variant_new_handle(g_quark_from_string("#f"));

    g_variant_unref(arg2);
    g_variant_unref(arg1);
    return result;
}

GVariant* primitive_gt(GVariant* args)
{
    EXPECT_ARGS(2);

    GVariant* arg1 = g_variant_get_child_value(args, 0);
    GVariant* arg2 = g_variant_get_child_value(args, 1);

    EXPECT_NUMERIC_ARG(arg1, 1lu);
    EXPECT_NUMERIC_ARG(arg2, 2lu);

    GVariant* result = GET_NUMERIC_ARG(arg1) > GET_NUMERIC_ARG(arg2) ?
        g_variant_new_handle(g_quark_from_string("#t")) :
        g_variant_new_handle(g_quark_from_string("#f"));

    g_variant_unref(arg2);
    g_variant_unref(arg1);
    return result;
}

GVariant* primitive_eq(GVariant* args)
{
    EXPECT_ARGS(2);

    GVariant* arg1 = g_variant_get_child_value(args, 0);
    GVariant* arg2 = g_variant_get_child_value(args, 1);

    EXPECT_NUMERIC_ARG(arg1, 1lu);
    EXPECT_NUMERIC_ARG(arg2, 2lu);

    GVariant* result = GET_NUMERIC_ARG(arg1) == GET_NUMERIC_ARG(arg2) ?
        g_variant_new_handle(g_quark_from_string("#t")) :
        g_variant_new_handle(g_quark_from_string("#f"));

    g_variant_unref(arg2);
    g_variant_unref(arg1);
    return result;
}

GVariant* primitive_iseq(GVariant* args)
{
    EXPECT_ARGS(2);

    GVariant* arg1 = g_variant_get_child_value(args, 0);
    GVariant* arg2 = g_variant_get_child_value(args, 1);

    GVariant* result = g_variant_equal(arg1, arg2) ?
        g_variant_new_handle(g_quark_from_string("#t")) :
        g_variant_new_handle(g_quark_from_string("#f"));

    g_variant_unref(arg2);
    g_variant_unref(arg1);
    return result;
}

GVariant* primitive_isnumber(GVariant* args)
{
    EXPECT_ARGS(1);

    GVariant* arg = g_variant_get_child_value(args, 0);

    GVariant* result = (g_variant_is_of_type(arg, G_VARIANT_TYPE_INT64) ||
                        g_variant_is_of_type(arg, G_VARIANT_TYPE_DOUBLE)) ?
        g_variant_new_handle(g_quark_from_string("#t")) :
        g_variant_new_handle(g_quark_from_string("#f"));

    g_variant_unref(arg);
    return result;
}

GVariant* primitive_isstring(GVariant* args)
{
    EXPECT_ARGS(1);

    GVariant* arg = g_variant_get_child_value(args, 0);

    GVariant* result = g_variant_is_of_type(arg, G_VARIANT_TYPE_STRING) ?
        g_variant_new_handle(g_quark_from_string("#t")) :
        g_variant_new_handle(g_quark_from_string("#f"));

    g_variant_unref(arg);
    return result;
}

GVariant* primitive_issymbol(GVariant* args)
{
    EXPECT_ARGS(1);

    GVariant* arg = g_variant_get_child_value(args, 0);

    GVariant* result = g_variant_is_of_type(arg, G_VARIANT_TYPE_HANDLE) ?
        g_variant_new_handle(g_quark_from_string("#t")) :
        g_variant_new_handle(g_quark_from_string("#f"));

    g_variant_unref(arg);
    return result;
}

GVariant* primitive_ispair(GVariant* args)
{
    EXPECT_ARGS(1);

    GVariant* arg = g_variant_get_child_value(args, 0);

    GVariant* result = g_variant_is_of_type(arg, G_VARIANT_TYPE_TUPLE) ?
        g_variant_new_handle(g_quark_from_string("#t")) :
        g_variant_new_handle(g_quark_from_string("#f"));

    g_variant_unref(arg);
    return result;
}

#define NIL g_variant_new_tuple(NULL, 0)

GVariant* cons(GVariant* car, GVariant* cdr)
{
    if(g_variant_is_container(cdr))
    {
        gsize n = g_variant_n_children(cdr);
        GVariant** children = g_new(GVariant*, n + 1);
        for(gsize i = 1; i < n + 1; i++)
        {
            children[i] = g_variant_get_child_value(cdr, i - 1);
        }
        children[0] = car;
        GVariant* res = g_variant_new_tuple(children, n + 1);
        g_free(children);
        return res;
    }
    else
    {
        GVariantBuilder builder;
        g_variant_builder_init(&builder, G_VARIANT_TYPE_TUPLE);
        g_variant_builder_add_value(&builder, cdr);
        g_variant_builder_add_value(&builder, car);
        return g_variant_builder_end(&builder);
    }
}

GVariant* primitive_cons(GVariant* args)
{
    EXPECT_ARGS(2);

    GVariant* arg1 = g_variant_get_child_value(args, 0);
    GVariant* arg2 = g_variant_get_child_value(args, 1);

    GVariant* result = cons(arg1, arg2);

    g_variant_unref(arg2);
    g_variant_unref(arg1);
    return result;
}

GVariant* primitive_car(GVariant* args)
{
    EXPECT_ARGS(1);
    GVariant* arg = g_variant_get_child_value(args, 0);
    EXPECT_NONEMPTY_PAIR_ARG(arg, 1lu);
    GVariant* result = g_variant_get_child_value(arg, 0);
    g_variant_unref(arg);
    return result;
}

GVariant* primitive_cdr(GVariant* args)
{
    EXPECT_ARGS(1);
    GVariant* arg = g_variant_get_child_value(args, 0);
    EXPECT_NONEMPTY_PAIR_ARG(arg, 1lu);

    GVariantBuilder builder;
    g_variant_builder_init(&builder, G_VARIANT_TYPE_TUPLE);
    for(gsize i = 1; i < g_variant_n_children(arg); i++)
    {
        g_variant_builder_add_value(&builder, g_variant_get_child_value(arg, i));
    }
    g_variant_unref(arg);
    return g_variant_builder_end(&builder);
}

GVariant* primitive_isnull(GVariant* args)
{
    EXPECT_ARGS(1);
    GVariant* arg = g_variant_get_child_value(args, 0);
    GVariant* result = g_variant_is_of_type(arg, G_VARIANT_TYPE_TUPLE) &&
        g_variant_n_children(arg) == 0 ?
        g_variant_new_handle(g_quark_from_string("#t")) :
        g_variant_new_handle(g_quark_from_string("#f"));

    g_variant_unref(arg);
    return result;
}

GVariant* primitive_list(GVariant* args)
{
    gsize nargs = g_variant_n_children(args);
    GVariant** children = g_new(GVariant*, nargs);
    for(gsize i = 0; i < nargs; i++)
    {
        children[i] = g_variant_get_child_value(args, i);
    }
    GVariant* res = g_variant_new_tuple(children, nargs);
    g_free(children);
    return res;
}

GVariant* primitive_length(GVariant* args)
{
    EXPECT_ARGS(1);
    GVariant* arg = g_variant_get_child_value(args, 0);
    EXPECT_PAIR_ARG(arg, 1lu);
    GVariant* result = g_variant_new_int64(g_variant_n_children(arg));
    g_variant_unref(arg);
    return result;
}

GVariant* primitive_setcar(GVariant* args)
{
    EXPECT_ARGS(2);
    GVariant* arg1 = g_variant_get_child_value(args, 0);
    GVariant* arg2 = g_variant_get_child_value(args, 1);
    EXPECT_NONEMPTY_PAIR_ARG(arg1, 1lu);

    gsize len = g_variant_n_children(arg1);
    GVariant** children = g_new(GVariant*, len);
    for(gsize i = 1; i < len; i++)
    {
        children[i] = g_variant_get_child_value(arg1, i);
    }
    children[0] = arg2;
    GVariant* res = g_variant_new_tuple(children, len);

    // HACK: nice memory leak bro
    memcpy(arg1, res, 6 * sizeof(void*));
    /* g_variant_unref(arg2); */
    /* g_variant_unref(arg1); */
    g_free(children);
    return NIL;
}

GVariant* primitive_setcdr(GVariant* args)
{
    EXPECT_ARGS(2);
    GVariant* arg1 = g_variant_get_child_value(args, 0);
    GVariant* arg2 = g_variant_get_child_value(args, 1);
    EXPECT_NONEMPTY_PAIR_ARG(arg1, 1lu);

    GVariant** children;
    gsize len;
    if(g_variant_is_container(arg2))
    {
        len = g_variant_n_children(arg2) + 1;
        children = g_new(GVariant*, len);
        for(gsize i = 0; i < len - 1; i++)
        {
            children[i + 1] = g_variant_get_child_value(arg2, i);
        }
        children[0] = g_variant_get_child_value(arg1, 0);
    }
    else
    {
        len = 2;
        children = g_new(GVariant*, len);
        children[0] = g_variant_get_child_value(arg1, 0);
        children[1] = arg2;
    }
    GVariant* res = g_variant_new_tuple(children, len);

    // HACK: nice memory leak bro
    memcpy(arg1, res, 6 * sizeof(void*));
    /* g_variant_unref(arg2); */
    /* g_variant_unref(arg1); */
    g_free(children);
    return NIL;
}

GVariant* primitive_display(GVariant* args)
{
    EXPECT_ARGS(1);
    GVariant* arg = g_variant_get_child_value(args, 0);
    GString* str = display(arg, NULL);
    printf("%s", str->str);
    g_string_free(str, TRUE);
    g_variant_unref(arg);
    return NIL;
}

GVariant* primitive_newline(GVariant* args)
{
    EXPECT_ARGS(0);
    printf("\n");
    return NIL;
}

GVariant* read(const char* code, int* chars_read);

GVariant* read_list(const char* code, int* chars_read)
{
    GVariant* took = read(code, chars_read);
    if(took)
    {
        int internal_chars_read;
        GVariant* result = cons(
            took, read_list(code + *chars_read, &internal_chars_read));
        *chars_read += internal_chars_read;
        return result;
    }
    else
    {
        return NIL;
    }
}

int is_delimiter(char c)
{
    return g_ascii_isspace(c) || c == '(' || c == ')' || c == 0;
}

GVariant* read(const char* code, int* chars_read)
{
    *chars_read = 0;
    GVariant* result = NULL;
    char* s = (char*)code;

    for(;g_ascii_isspace(*s); s++);
    int whitespace = s - code;

    if(g_ascii_isdigit(*s))
    {
        for(;g_ascii_isdigit(*s); s++);
        if(is_delimiter(*s))
        {
            *chars_read = s - code;
            char* val = g_malloc(*chars_read);
            strncpy(val, code, *chars_read);
            result = g_variant_new_int64(g_ascii_strtoll(val, NULL, 10));
            g_free(val);
        }
        else if(*s == '.')
        {
            s++;
            for(;g_ascii_isdigit(*s); s++);
            if(is_delimiter(*s))
            {
                *chars_read = s - code;
                char* val = g_malloc(*chars_read);
                strncpy(val, code, *chars_read);
                result = g_variant_new_double(g_ascii_strtod(val, NULL));
                g_free(val);
            }
        }
    }
    else if(*s == '"')
    {
        char* t = s++;
        for(;(*s != '"') && *s; s++);
        if(*s == '"')
        {
            *chars_read = s - t;
            char* val = g_malloc0(*chars_read);
            strncpy(val, t + 1, *chars_read - 1);
            result = g_variant_new_string(val);
            g_free(val);
        }
    }
    else if(!is_delimiter(*s))
    {
        char* t = s++;
        for(;!is_delimiter(*s); s++);
        if(is_delimiter(*s))
        {
            *chars_read = s - t;
            char* val = g_malloc0(*chars_read + 1);
            strncpy(val, t, *chars_read);
            result = g_variant_new_handle(g_quark_from_string(val));
            g_free(val);
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
            char* val = g_malloc0(*chars_read);
            strncpy(val, t + 1, *chars_read - 2);
            int internal_chars_read;
            result = read_list(val, &internal_chars_read);
            g_free(val);
        }
    }

    *chars_read += whitespace;
    return result;
}

#define BUFF_SIZE 8192
char buffer[BUFF_SIZE];

GVariant* primitive_read(GVariant* args)
{
    EXPECT_ARGS(0);

    int chars_read;

    if(!fgets(buffer, BUFF_SIZE, stdin))
        return read("(exit)", &chars_read);  // HACKish

    GVariant* result = read(buffer, &chars_read);
    if(!chars_read)
        g_error("%s: parsing error", __FUNCTION__);
    return result ? result : NIL;
}

GVariant* primitive_error(GVariant* args)
{
    GString* str = display(args, NULL);
    g_error("%s", str->str);
}

const char* primitive_procedure_names[] = {
    "apply",
    "+",
    "-",
    "*",
    "/",
    "remainder",
    "not",
    "<",
    ">",
    "=",
    "eq?",
    "number?",
    "string?",
    "symbol?",
    "pair?",
    "cons",
    "car",
    "cdr",
    "null?",
    "list",
    "length",
    "set-car!",
    "set-cdr!",
    "display",
    "newline",
    "read",
    "error",
};

primitive_procedure_t* primitive_procedure_objects[] = {
    primitive_apply,
    primitive_plus,
    primitive_minus,
    primitive_mult,
    primitive_div,
    primitive_remainder,
    primitive_not,
    primitive_lt,
    primitive_gt,
    primitive_eq,
    primitive_iseq,
    primitive_isnumber,
    primitive_isstring,
    primitive_issymbol,
    primitive_ispair,
    primitive_cons,
    primitive_car,
    primitive_cdr,
    primitive_isnull,
    primitive_list,
    primitive_length,
    primitive_setcar,
    primitive_setcdr,
    primitive_display,
    primitive_newline,
    primitive_read,
    primitive_error,
};

GSList* setup_environment()
{
    gsize count = sizeof(primitive_procedure_names) / sizeof(char*);
    g_assert(count == sizeof(primitive_procedure_objects) / sizeof(void*));

    GVariant** values = g_new(GVariant*, count);
    for(gsize i = 0; i < count; i++)
    {
        values[i] = g_variant_new(
            "(ht)",
            g_quark_from_string("primitive-compiled"),
            primitive_procedure_objects[i]);
    }

    GSList* res = extend_environment(
        g_variant_new_strv(
            primitive_procedure_names,
            count),
        g_variant_new_array(
            G_VARIANT_TYPE("(ht)"),
            values,
            count),
        NULL);

    define_variable("nil", NIL, res);

    g_free(values);
    return res;
}

GString* display(GVariant* expr, GSList* seen)
{
    if(!expr)
    {
        return g_string_new("nil");
    }
    else
    {
        if(g_variant_is_container(expr))
        {
            GVariant* entry = g_variant_get_child_value(expr, 1);
            GString* result = NULL;
            if(is_primitive_procedure(expr) &&
               g_variant_is_of_type(entry, G_VARIANT_TYPE_UINT64))
            {
                primitive_procedure_t* proc = (primitive_procedure_t*)
                    g_variant_get_uint64(entry);
                gsize count = sizeof(primitive_procedure_names) / sizeof(char*);
                for(gsize i = 0; i < count; i++)
                {
                    if(proc == primitive_procedure_objects[i])
                    {
                        GString* result = g_string_new(NULL);
                        g_string_printf(result, "[primitive procedure %s]",
                                        primitive_procedure_names[i]);
                        break;
                    }
                }
                if(!result)
                {
                    result = g_string_new("[unknown primitive procedure");
                    g_string_append_printf(result, " %ld]", (gintptr)proc);
                }
            }
            else
            {
                result = g_string_new("(");
                gsize n = g_variant_n_children(expr);
                for(gsize i = 0; i < n; i++)
                {
                    GVariant* child = g_variant_get_child_value(expr, i);
                    GString* str;
                    if(g_slist_find(seen, child) != NULL)
                    {
                        str = g_string_new("...");
                    }
                    else
                    {
                        str = display(child, g_slist_append(seen, expr));
                    }
                    g_string_append_printf(result,
                                        i == n - 1 ? "%s)" : "%s, ",
                                        str->str);
                    g_string_free(str, TRUE);
                    g_variant_unref(child);
                }
            }
            g_variant_unref(entry);
            return result;
        }
        if(g_variant_is_of_type(expr, G_VARIANT_TYPE_HANDLE))
        {
            return g_string_new(g_quark_to_string(g_variant_get_handle(expr)));
        }
        if(g_variant_is_of_type(expr, G_VARIANT_TYPE_STRING))
        {
            return g_string_new(g_variant_get_string(expr, NULL));
        }
        return g_string_new(g_variant_print(expr, FALSE));
    }
}

#endif  //  _RUNTIME_H_
