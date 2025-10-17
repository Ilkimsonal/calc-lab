// Ilkim Sonal 211ADB102
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm
//
// -----------------------------------------------------------------------------
// WHAT THIS PROGRAM DOES (brief):
// - Reads arithmetic expressions from .txt files, evaluates, and writes either
//   the numeric result or `ERROR:<pos>` (1-based char index; '\n' counts as 1).
// - Operators: +, -, *, /, ** (right-assoc), parentheses ( ), unary +/-, floats.
// - Pythonic line comments: if the first non-space on a line is '#', that line
//   is ignored.
// - CLI:
//   calc [-d DIR|--dir DIR] [-o OUTDIR|--output-dir OUTDIR] input.txt
//   • If -d is given, processes all *.txt files in DIR (non-recursive).
//   • If -o omitted, output dir becomes: <input_base>_<username>_<STUDENT_ID>/
//   • For each input task1.txt -> task1_<Name>_<Lastname>_<StudentID>.txt
// - Division by zero: we report ERROR at the '/' token position (documented).
// - Single source file; uses only standard C/POSIX headers (no bison/flex).
// -----------------------------------------------------------------------------

#define STUDENT_NAME     "Ilkim"
#define STUDENT_LASTNAME "Sonal"
#define STUDENT_ID       "211ADB102"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <dirent.h>
#include <sys/stat.h>
#include <sys/types.h>

// ============================ Value (int/double) =============================
typedef struct {
    int is_float;      // 0 -> int (long long), 1 -> double
    long long i;
    double d;
} Value;

static Value make_int(long long x){ Value v={0,x,(double)x}; return v; }
static Value make_double(double x){ Value v={1,(long long)x,x}; return v; }
static int   is_zero(Value v){ return v.is_float ? fabs(v.d)==0.0 : (v.i==0); }

static Value v_add(Value a, Value b){
    if(a.is_float || b.is_float) return make_double((a.is_float?a.d:a.i) + (b.is_float?b.d:b.i));
    return make_int(a.i + b.i);
}
static Value v_sub(Value a, Value b){
    if(a.is_float || b.is_float) return make_double((a.is_float?a.d:a.i) - (b.is_float?b.d:b.i));
    return make_int(a.i - b.i);
}
static Value v_mul(Value a, Value b){
    if(a.is_float || b.is_float) return make_double((a.is_float?a.d:a.i) * (b.is_float?b.d:b.i));
    return make_int(a.i * b.i);
}
static Value v_div(Value a, Value b, size_t *err_pos, size_t slash_pos){
    if(is_zero(b)){ if(*err_pos==0) *err_pos = slash_pos; return make_int(0); }
    return make_double((a.is_float?a.d:(double)a.i)/(b.is_float?b.d:(double)b.i));
}
static Value v_pow(Value base, Value exp){
    double bd = base.is_float ? base.d : (double)base.i;
    double ed = exp.is_float ? exp.d : (double)exp.i;
    return make_double(pow(bd, ed));
}

// ================================ Tokenizer =================================
typedef enum {
    T_EOF=0, T_NUM, T_PLUS, T_MINUS, T_STAR, T_SLASH, T_POW, T_LPAREN, T_RPAREN, T_INVALID
} TokType;

typedef struct {
    TokType type;
    size_t start_pos;  // 1-based absolute character index
    int    is_float;   // for T_NUM
    long long i;
    double d;
} Token;

typedef struct {
    const char *src; size_t len;
    size_t pos;   // 1-based next char position
    size_t idx0;  // 0-based index into src
    size_t err_pos;
    Token  cur;
} Scanner;

static void set_error(Scanner *S, size_t p){ if(!S->err_pos) S->err_pos = p; }

static void skip_ws_and_comments(Scanner *S){
    for(;;){
        // whitespace
        while(S->idx0 < S->len && (
            S->src[S->idx0]==' '  ||
            S->src[S->idx0]=='\t' ||
            S->src[S->idx0]=='\r' ||
            S->src[S->idx0]=='\n'
        )){ S->idx0++; S->pos++; }
        // full line comment (first non-space is '#')
        if(S->idx0 < S->len && S->src[S->idx0]=='#'){
            while(S->idx0 < S->len && S->src[S->idx0] != '\n'){ S->idx0++; S->pos++; }
            continue;
        }
        break;
    }
}
static Token make_simple(TokType t, size_t p){ Token x; memset(&x,0,sizeof x); x.type=t; x.start_pos=p; return x; }

static Token scan_number(Scanner *S){
    size_t start = S->pos;
    errno = 0;
    char *end = NULL;
    double dv = strtod(S->src + S->idx0, &end);
    if(end == S->src + S->idx0) return make_simple(T_INVALID, start);
    size_t used = (size_t)(end - (S->src + S->idx0));
    size_t token_start = S->idx0;
    S->idx0 += used; S->pos += used;

    Token t; memset(&t,0,sizeof t);
    t.type = T_NUM; t.start_pos = start;

    int saw_dot_or_exp = 0;
    for(size_t i=0;i<used;i++){
        char c = S->src[token_start + i];
        if(c=='.' || c=='e' || c=='E'){ saw_dot_or_exp = 1; break; }
    }
    if(saw_dot_or_exp){ t.is_float=1; t.d=dv; }
    else{
        errno = 0;
        long long iv = strtoll(S->src + token_start, NULL, 10);
        if(errno==ERANGE){ t.is_float=1; t.d=dv; }
        else { t.is_float=0; t.i=iv; t.d=dv; }
    }
    return t;
}
static Token next_token(Scanner *S){
    skip_ws_and_comments(S);
    if(S->idx0 >= S->len) return make_simple(T_EOF, S->pos);

    char c = S->src[S->idx0];
    size_t p = S->pos;

    if(isdigit((unsigned char)c) || c=='.') return scan_number(S);
    if(c=='+'){ S->idx0++; S->pos++; return make_simple(T_PLUS, p); }
    if(c=='-'){ S->idx0++; S->pos++; return make_simple(T_MINUS, p); }
    if(c=='('){ S->idx0++; S->pos++; return make_simple(T_LPAREN, p); }
    if(c==')'){ S->idx0++; S->pos++; return make_simple(T_RPAREN, p); }
    if(c=='/'){ S->idx0++; S->pos++; return make_simple(T_SLASH, p); }
    if(c=='*'){
        if(S->idx0+1 < S->len && S->src[S->idx0+1]=='*'){ S->idx0+=2; S->pos+=2; return make_simple(T_POW, p); }
        S->idx0++; S->pos++; return make_simple(T_STAR, p);
    }
    // invalid char
    S->idx0++; S->pos++; return make_simple(T_INVALID, p);
}
static void advance(Scanner *S){ S->cur = next_token(S); }

// ================================= Parser ===================================
// Grammar with unary:
//   expr  := term { ('+'|'-') term }
//   term  := power { ('*'|'/') power }
//   power := unary ( '**' power )?      // RIGHT-ASSOCIATIVE
//   unary := ('+'|'-') unary | primary
//   primary := NUMBER | '(' expr ')'

static Value parse_expr(Scanner *S);
static Value parse_term(Scanner *S);
static Value parse_power(Scanner *S);
static Value parse_unary(Scanner *S);
static Value parse_primary(Scanner *S);

static Value parse_expr(Scanner *S){
    Value v = parse_term(S);
    while(S->cur.type==T_PLUS || S->cur.type==T_MINUS){
        TokType op = S->cur.type; advance(S);
        Value r = parse_term(S); if(S->err_pos) return make_int(0);
        v = (op==T_PLUS)? v_add(v,r) : v_sub(v,r);
    }
    return v;
}
static Value parse_term(Scanner *S){
    Value v = parse_power(S);
    while(S->cur.type==T_STAR || S->cur.type==T_SLASH){
        TokType op = S->cur.type; size_t slash_pos = S->cur.start_pos; advance(S);
        Value r = parse_power(S); if(S->err_pos) return make_int(0);
        v = (op==T_STAR)? v_mul(v,r) : v_div(v,r,&S->err_pos,slash_pos);
        if(S->err_pos) return make_int(0);
    }
    return v;
}
static Value parse_power(Scanner *S){
    Value left = parse_unary(S);
    if(S->cur.type==T_POW){ advance(S); Value right = parse_power(S); left = v_pow(left,right); }
    return left;
}
static Value parse_unary(Scanner *S){
    if(S->cur.type==T_PLUS){ advance(S); return parse_unary(S); }
    if(S->cur.type==T_MINUS){ advance(S); Value v=parse_unary(S); return v.is_float? make_double(-v.d) : make_int(-v.i); }
    return parse_primary(S);
}
static Value parse_primary(Scanner *S){
    if(S->cur.type==T_NUM){ Value v=S->cur.is_float? make_double(S->cur.d) : make_int(S->cur.i); advance(S); return v; }
    if(S->cur.type==T_LPAREN){
        advance(S);
        Value inside = parse_expr(S);
        if(S->err_pos) return make_int(0);
        if(S->cur.type!=T_RPAREN){ if(S->cur.type==T_EOF) set_error(S, S->pos); else set_error(S, S->cur.start_pos); return make_int(0); }
        advance(S); return inside;
    }
    set_error(S, S->cur.start_pos); advance(S); return make_int(0);
}

// ============================== Evaluation API ==============================
typedef struct { int ok; Value v; size_t err_pos; } EvalResult;

static EvalResult eval_buffer(const char *buf, size_t len){
    Scanner S; memset(&S,0,sizeof S);
    S.src=buf; S.len=len; S.pos=1; S.idx0=0; S.err_pos=0;
    advance(&S);
    Value v = parse_expr(&S);
    if(S.err_pos){ EvalResult r={0,make_int(0),S.err_pos}; return r; }
    if(S.cur.type != T_EOF){ set_error(&S, S.cur.start_pos); EvalResult r={0,make_int(0),S.err_pos}; return r; }
    EvalResult r={1,v,0}; return r;
}

// =============================== Printing ===================================
static int is_integral_double(double x){ double r = llround(x); return fabs(x - r) < 1e-12; }
static void print_value(FILE *out, Value v){
    if(!v.is_float) fprintf(out, "%lld\n", v.i);
    else if(is_integral_double(v.d)) fprintf(out, "%lld\n", (long long)llround(v.d));
    else fprintf(out, "%.15g\n", v.d);
}

// ================================ File I/O ==================================
static int read_entire_file(const char *path, char **out_buf, size_t *out_len){
    FILE *f = fopen(path, "rb");
    if(!f) return -1;
    if(fseek(f, 0, SEEK_END)!=0){ fclose(f); return -1; }
    long l = ftell(f); if(l<0){ fclose(f); return -1; }
    if(fseek(f, 0, SEEK_SET)!=0){ fclose(f); return -1; }
    *out_buf = (char*)malloc((size_t)l + 1);
    if(!*out_buf){ fclose(f); return -1; }
    size_t rd = fread(*out_buf, 1, (size_t)l, f); (void)rd;
    fclose(f);
    (*out_buf)[l] = '\0'; *out_len = (size_t)l; return 0;
}
static int ensure_dir(const char *path){
    struct stat st;
    if(stat(path,&st)==0){ if(S_ISDIR(st.st_mode)) return 0; errno=ENOTDIR; return -1; }
    return mkdir(path, 0775);
}
static const char* get_username(){ const char *u = getenv("USER"); if(!u||!*u) u="user"; return u; }
static const char* base_name(const char *p){ const char *s = strrchr(p,'/'); return s? s+1 : p; }
static void strip_ext(const char *fname, char *out, size_t outsz){
    snprintf(out, outsz, "%s", fname);
    char *dot = strrchr(out, '.'); if(dot) *dot = '\0';
}
static int ends_with_txt(const char *name){ size_t n=strlen(name); return (n>=4 && strcmp(name+n-4, ".txt")==0); }
static void build_default_outdir(const char *input_path, char *out, size_t outsz){
    char base[256]; strip_ext(base_name(input_path), base, sizeof base);
    snprintf(out, outsz, "%s_%s_%s", base, get_username(), STUDENT_ID);
}
static void build_output_filename(const char *input_path, char *out, size_t outsz){
    char base[256]; strip_ext(base_name(input_path), base, sizeof base);
    snprintf(out, outsz, "%s_%s_%s_%s.txt", base, STUDENT_NAME, STUDENT_LASTNAME, STUDENT_ID);
}

// ================================= CLI ======================================
typedef struct { const char *dir; const char *outdir; const char *input; } Options;

static void usage(const char *prog){
    fprintf(stderr,
      "Usage: %s [-d DIR|--dir DIR] [-o OUTDIR|--output-dir OUTDIR] input.txt\n"
      "If -d is given, processes all *.txt in DIR (non-recursive).\n"
      "If -o omitted, output dir is <input_base>_<username>_%s\n",
      prog, STUDENT_ID);
}
static int parse_args(int argc, char **argv, Options *opt){
    memset(opt,0,sizeof *opt);
    for(int i=1;i<argc;i++){
        if(strcmp(argv[i],"-d")==0 || strcmp(argv[i],"--dir")==0){
            if(i+1>=argc){ usage(argv[0]); return -1; } opt->dir = argv[++i];
        } else if(strcmp(argv[i],"-o")==0 || strcmp(argv[i],"--output-dir")==0){
            if(i+1>=argc){ usage(argv[0]); return -1; } opt->outdir = argv[++i];
        } else if(argv[i][0]=='-'){ usage(argv[0]); return -1; }
        else opt->input = argv[i];
    }
    if(!opt->dir && !opt->input){ usage(argv[0]); return -1; }
    return 0;
}

// =============================== Processing =================================
static int process_one_file(const char *in_path, const char *out_dir){
    char *buf=NULL; size_t len=0;
    if(read_entire_file(in_path,&buf,&len)!=0){ fprintf(stderr,"read fail: %s\n", in_path); return -1; }
    EvalResult R = eval_buffer(buf,len);

    char outname[512]; build_output_filename(in_path, outname, sizeof outname);
    char outpath[1024];
    if(out_dir && *out_dir) snprintf(outpath,sizeof outpath,"%s/%s",out_dir,outname);
    else snprintf(outpath,sizeof outpath,"%s",outname);

    FILE *f = fopen(outpath, "wb");
    if(!f){ fprintf(stderr,"write fail: %s\n", outpath); free(buf); return -1; }
    if(!R.ok) fprintf(f, "ERROR:%zu\n", R.err_pos);
    else      print_value(f, R.v);
    fclose(f);
    free(buf);
    return 0;
}
static int process_dir(const char *dir_path, const char *out_dir){
    DIR *d = opendir(dir_path);
    if(!d){ fprintf(stderr,"open dir fail: %s\n", dir_path); return -1; }
    struct dirent *ent; int rc=0;
    while((ent=readdir(d))!=NULL){
        // portable: skip "." and ".." (avoid DT_DIR / d_type)
        if(strcmp(ent->d_name,".")==0 || strcmp(ent->d_name,"..")==0) continue;
        if(!ends_with_txt(ent->d_name)) continue;
        char inpath[1024]; snprintf(inpath,sizeof inpath,"%s/%s", dir_path, ent->d_name);
        if(process_one_file(inpath,out_dir)!=0) rc=-1;
    }
    closedir(d);
    return rc;
}

// ================================== main ====================================
int main(int argc, char **argv){
    Options opt; if(parse_args(argc,argv,&opt)!=0) return 1;

    char outdir_buf[512]={0};
    const char *outdir = opt.outdir;
    if(!outdir){
        if(opt.dir) build_default_outdir(opt.dir, outdir_buf, sizeof outdir_buf);
        else        build_default_outdir(opt.input, outdir_buf, sizeof outdir_buf);
        outdir = outdir_buf;
    }
    if(ensure_dir(outdir)!=0){ fprintf(stderr,"cannot create/access output dir: %s\n", outdir); return 1; }

    int rc=0;
    if(opt.dir) rc = process_dir(opt.dir, outdir);
    if(opt.input){ if(process_one_file(opt.input,outdir)!=0) rc=1; }

    return rc;
}
