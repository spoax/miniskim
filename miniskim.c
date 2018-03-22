/* A mini Scheme Interpreter
 *
 * Serkan Kenar
 * Al Ain, 2017.
 */

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#define car(x)     (x->car)
#define cdr(x)     (x->cdr)
#define caar(x)    (car(car(x)))
#define cdar(x)    (cdr(car(x)))
#define cddr(x)    (cdr(cdr(x)))
#define cadr(x)    (car(cdr(x)))
#define caddr(x)   (car(cddr(x)))
#define cadddr(x)  (car(cdr(cddr(x))))

#define IS_SYMBOL(x) (x->type == SYMBOL)
#define IS_NUM(x)    (x->type == NUM)
#define IS_FUNC(x)   (x->type == FUNCTION)
#define IS_CONS(x)   (x->type == CONS)
#define IS_LAMBDA(x) (x->type == LAMBDA)

#define NEW(x, y)      do { x = (struct val *) malloc(sizeof(struct val)); if (!x) { perror("malloc"); exit(-1); }  x->type = y; } while(0);
#define TYPE(x)        (x->type)
#define ISEQU(x, y)    ( strcmp(x->str, y) == 0 )
#define FUNC(x)        struct val * x(struct val *args)
#define PUSH_ENV(x,y)  (cons(y,x))
#define POP_ENV(x)     (cdr(x))


struct val {
    int type;
    union {
        double d;
        char *str;
        struct val *(*fn)(struct val *);
        struct {
            struct val* car;
            struct val* cdr;
        };
    };
};

enum TokenType { NONE, SYMBOL, NUM, CONS, LAMBDA, FUNCTION, DONE };
double yy_val;
char token[256];
FILE *finput;

struct val *None;       // the None and only object
struct val *True;       // the True value
struct val *False;      // the False value

// some forward declarations
struct val *btbl_insert(struct val *tbl, struct val *key, struct val *val);
void        env_insert(struct val *env, struct val *key, struct val *val);
struct val *env_find(struct val *env, const char *str);
struct val *eval(struct val *exp, struct val *env);
void        pprint(struct val *exp);


int gettok()
{
    int i;

    char ch = getc(finput);
    while (isspace(ch)) { 
        ch = getc(finput);
        if (ch == ';') do { ch = getc(finput); } while (ch != '\n');
    }
    if (ch == '(' || ch == ')') {
        token[0] = ch;
        token[1] = 0;
        return SYMBOL;
    } else if (isdigit(ch)) {
        i = 0;
        while (isdigit(ch) || ch == '.') {
            token[i++] = ch;
            ch = getc(finput);
        }
        ungetc(ch, finput);
        token[i] = 0;
        yy_val = atof(token);
        return NUM;
    } else if (ch == EOF) {
        return DONE;
    } else {
        i = 0;
        while (!isspace(ch) && ch != ')' && ch != '(') {
            token[i++] = ch;
            ch = getc(finput);
        }
        ungetc(ch, finput);
        token[i] = 0;
        return SYMBOL;
    }
}

struct val *make_str(const char *str)
{
    struct val *e;
    NEW(e, SYMBOL);
    e->str = strdup(str);
    return e;
}

struct val *make_num(double d)
{
    struct val *e;
    NEW(e, NUM);
    e->d = d;
    return e;
}

struct val *make_fn(struct val *(*ptr)(struct val *))
{
    struct val *e;
    NEW(e, FUNCTION);
    e->fn = ptr;
    return e;
}

struct val *cons(struct val *car, struct val *cdr)
{
    struct val *e;
    NEW(e, CONS);
    car(e) = car;
    cdr(e) = cdr;
    return e;
}

struct val *make_lambda(struct val *car, struct val *cdr)
{
    struct val *e;
    NEW(e, LAMBDA);
    car(e) = car;
    cdr(e) = cdr;
    return e;
}

/* Parse a scheme expression. */
struct val* parse()
{
    int type;

    type = gettok();

    if (type == DONE) {
        return NULL;
    } else if (type == NUM) {
        return make_num(yy_val);
    } else if (token[0] == '(') {
        struct val *head, *tail, *t = parse();
        if (!t)
            return None;
        tail = head = cons(t, None);
        t = parse();
        while (t != NULL) {
            tail = cdr(tail) = cons(t, None);
            t = parse();
        }
        return head;
    } else if (token[0] == ')') {
        return NULL;
    }

    return make_str(token);
}

struct val *list_of_values(struct val *exps, struct val *env)
{
    if (car(exps) == None) return None;
    return cons(eval(car(exps), env), list_of_values(cdr(exps), env));
}

struct val *bind_params(struct val *params, struct val *vals)
{
    if (car(vals) == None) return None;
    return btbl_insert(bind_params(cdr(params), cdr(vals)),
                       car(params), car(vals));
}

/* Apply */
struct val *apply(struct val *proc, struct val *args)
{
    struct val *nenv;
    if (proc != None) {
        switch (proc->type) {
            case FUNCTION:
                return (proc->fn)(args);
            case LAMBDA:
                nenv = bind_params(car(proc), args);
                nenv = PUSH_ENV(cddr(proc), nenv);
                return eval(cadr(proc), nenv);
        }
    }
    printf("Error: I dunno how to apply?\n");
    return None;
}

/* Eval if expression */
struct val *eval_if(struct val *exp, struct val *env)
{
    struct val *r = eval(cadr(exp), env);
    if (r == False) {
        return eval(cadddr(exp), env);
    } else {
        return eval(caddr(exp), env);
    }
}

/* Eval */
struct val *eval(struct val *exp, struct val *env)
{
    if (exp == NULL) return NULL;
    if (IS_SYMBOL(exp)) {
        return env_find(env, exp->str);
    } else if (IS_NUM(exp) || IS_LAMBDA(exp)) {
        return exp;
    } else if (IS_CONS(exp)) {
        if (IS_SYMBOL(car(exp))) {
            if (ISEQU(car(exp), "define")) {
                env_insert(env, cadr(exp), eval(caddr(exp), env));
                return cadr(exp);
            } else if (ISEQU(car(exp), "set!")) {
                struct val *var = env_find(env, cadr(exp)->str);
                *var = *eval(caddr(exp), env);
                return var;
            } else if (ISEQU(car(exp), "if")) {
                return eval_if(exp, env);
            } else if (ISEQU(car(exp), "quote")) {
                return cadr(exp);
            } else if (ISEQU(car(exp), "lambda")) {
                struct val *lmbd;
                if (!IS_CONS(cadr(exp))) {
                    printf("Error: lambda takes list of params!\n");
                    return None;
                }
                lmbd = cons(cadr(exp), cons(caddr(exp), env));
                lmbd->type = LAMBDA;
                return lmbd;
            }
        }
        if (car(exp) != None) {
            struct val *fn = eval(car(exp), env);
            if (fn == None) {
                printf("Error: Unknown function: ");
                pprint(car(exp));
                printf("\n");
            } else {
                return apply(fn, list_of_values(cdr(exp), env));
            }
        }
    } else {
        printf("Error: shouln't reach here!\n");
    }
    return None;
}


#define TBL_KEY(x) (caar(x))
#define TBL_VAL(x) (cdar(x))
#define TBL_RITE_CHILD(x) (cadr(x))
#define TBL_LEFT_CHILD(x) (cddr(x))

struct val *btbl_find(struct val *tbl, const char *str)
{
    if (tbl == NULL || tbl == None) {
        return None;
    } else {
        int c = strcmp(TBL_KEY(tbl)->str, str);
        if (c == 0) {
            return TBL_VAL(tbl);
        } else if (c < 0) {
            return btbl_find(TBL_LEFT_CHILD(tbl), str);
        } else {
            return btbl_find(TBL_RITE_CHILD(tbl), str);
        }
    }
    return None;
}

struct val *btbl_insert(struct val *tbl, struct val *key, struct val *val)
{
    if (tbl == None) {
        tbl = cons(cons(key, val), cons(None, None));
    } else {
        if (strcmp(TBL_KEY(tbl)->str, key->str) < 0)
            TBL_LEFT_CHILD(tbl) = btbl_insert(TBL_LEFT_CHILD(tbl), key, val);
        else
            TBL_RITE_CHILD(tbl) = btbl_insert(TBL_RITE_CHILD(tbl), key, val);
    }
    return tbl;
}

struct val *env_find(struct val *env, const char *str)
{
    struct val *p, *t;

    for (p = env; p != None; p = POP_ENV(p)) {
        t = btbl_find(car(p), str);
        if (t != None)
            return t;
    }
    return None;
}

void env_insert(struct val *env, struct val *key, struct val *val)
{
    btbl_insert(car(env), key, val);
}


/* Standard Functions */
FUNC(_car) { return caar(args); }
FUNC(_cdr) { return cdar(args); }
FUNC(_cons) { return cons(car(args), cadr(args)); }
FUNC(nullq) { return car(args) == None ? True : False; }
FUNC(zeroq) { return car(args)->d == 0.0 ? True : False; }
FUNC(pairq) { return IS_CONS(car(args)) ? True : False; }

double add_(struct val *l) {
    if (car(l) == None) return 0.0;
    return car(l)->d + add_(cdr(l));
}
FUNC(add) { return make_num(add_(args)); }

double sub_(struct val *l) {
    if (car(l) == None) return 0.0;
    return car(l)->d - sub_(cdr(l));
}
FUNC(sub) { return make_num(sub_(args)); }

double mul_(struct val *l) {
    if (car(l) == None) return 1.0;
    return car(l)->d * mul_(cdr(l));
}
FUNC(mul) { return make_num(mul_(args));
}
double div_(struct val *l) {
    if (car(l) == None) return 1.0;
    return car(l)->d * div_(cdr(l));
}
FUNC(f_div)     { return make_num(div_(args)); }
FUNC(sub1)      { return make_num(car(args)->d - 1); }
FUNC(positiveq) { return car(args)->d > 0.0 ? True : False; }
FUNC(negativeq) { return car(args)->d < 0.0 ? True : False; }
FUNC(equals)    { return (cadr(args)->d - car(args)->d) == 0.0 ? True : False; }
FUNC(display)   { pprint(car(args)); printf("\n"); return None; }

FUNC(begin) {
    struct val *p = args;
    while (cdr(p) != None) {
        p = cdr(p);
    }
    return car(p);
}


typedef struct {
    const char *name;
    struct val *(*fn)(struct val *);
} builtin_entry;

builtin_entry builtins_tbl[] = {
    { "car",     _car     },
    { "cdr",     _cdr     },
    { "cons",    _cons   },
    { "sub1",    sub1    },
    { "zero?",   zeroq   },
    { "positive?", positiveq },
    { "negative?", negativeq },
    { "null?",   nullq   },
    { "pair?",   pairq   },
    { "+",       add     },
    { "-",       sub     },
    { "*",       mul     },
    { "/",       f_div   },
    { "=",       equals  },
    { "begin",   begin   },
    { "display", display },
    { 0, 0 }
};

struct val *setup_global_env()
{
    struct val *p = None;

    builtin_entry *e = &builtins_tbl[0];
    while (e->name) {
        p = btbl_insert(p, make_str(e->name), make_fn(e->fn));
        e++;
    }
    return cons(p, None);
}

/* Pretty print a scheme value. */
void pprint(struct val *exp)
{
    if (exp == 0 || exp == None) {
        printf("#None#");
    } else if (IS_SYMBOL(exp)) {
        printf("%s", exp->str);
    } else if (IS_NUM(exp)) {
        printf("%.2f", exp->d);
    } else if (IS_FUNC(exp)) {
        printf("#FUNC#%p", exp->fn);
    } else if (IS_LAMBDA(exp)) {
        printf("#LAMBDA#");
        pprint(car(exp));
        printf(" -> ");
        pprint(cadr(exp));
    } else if (IS_CONS(exp)) {
        printf("(");
        pprint(car(exp));
        printf(" ");
        pprint(cdr(exp));
        printf(")");
    }
}

void repl(struct val *env)
{
    struct val *exp, *res;

    if (finput == stdin) printf(">> ");
    for (;;) {
        exp = parse();
        if (exp == NULL) break;
        res = eval(exp, env);
        if (finput == stdin) {
            pprint(res);
            printf("\n>> ");
        }
    }
}

void load_file(const char *filename, struct val *env)
{
    finput = fopen(filename, "r");
    if (finput == NULL) {
        printf("Error: unable to open file: %s\n", filename);
        abort();
    }
    repl(env);
    fclose(finput);
}

int main(int argc, char** argv)
{
    struct val *global_env;

    NEW(None, CONS);
    car(None) = cdr(None) = None;
    True = make_num(1.0);
    False = make_num(0.0);

    global_env = setup_global_env();
    load_file("init.scm", global_env);

    if (argc > 1) {
        load_file(argv[1], global_env);
    } else {
        printf("miniskim v0.1 - a mini Scheme interpreter - Serkan Kenar\n\n");
        finput = stdin;
        repl(global_env);
    }

    return 0;
}

