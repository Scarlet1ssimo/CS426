
/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Dont remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
  if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
    YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

int comment_dep=0;
int string_err=0;

/*
  copy string to buffer
  return number of chars copied
  return -1 if buffer full
*/
int appends(const char* c){ 
  const char* cc;
  for(cc=c;*cc;cc++){
    if(string_buf_ptr-string_buf>1024)
      return -1;
    *string_buf_ptr=*cc;
    string_buf_ptr++;
  }
  return cc-c;
}
/*
  copy char to buffer
  return number of chars copied
  return -1 if buffer full (allow c=0 if already full)
*/
int appendc(const char c){
  if(string_buf_ptr-string_buf>1024&&c!=0)
    return -1;
  *string_buf_ptr=c;
  string_buf_ptr++;
  return 1;
}

%}

%option noyywrap

/*
 * Define names for regular expressions here.
 */

digit    [0-9]
typeid   [A-Z][a-zA-Z0-9_]*
objid    [a-z][a-zA-Z0-9_]*

%s COMMENT
%s STRING
%%

 /*
  * Define regular expressions for the tokens of COOL here. Make sure, you
  * handle correctly special cases, like:
  *   - Nested comments
  *   - String constants: They use C like systax and can contain escape
  *     sequences. Escape sequence \c is accepted for all characters c. Except
  *     for \n \t \b \f, the result is c.
  *   - Keywords: They are case-insensitive except for the values true and
  *     false, which must begin with a lower-case letter.
  *   - Multiple-character operators (like <-; The scanner should produce a
  *     single token for every such operator.
  *   - Line counting: You should keep the global variable curr_lineno updated
  *     with the correct line number
  */
 // "(*" if(eatcomment()) return ERROR;

<INITIAL,COMMENT>"(*" {
  comment_dep++;
  BEGIN(COMMENT);
}
<COMMENT>"*)" {
  comment_dep--;
  if(comment_dep==0)
    BEGIN(INITIAL);
}
<COMMENT>.
<COMMENT><<EOF>> {
  yylval.error_msg="EOF in comment";
  BEGIN(INITIAL);
  return ERROR;
}

<INITIAL>\" { //str start
  BEGIN(STRING);
  string_buf_ptr=string_buf;
  string_err=0;
}
<STRING>\" {//str end
  BEGIN(INITIAL);
  if(!string_err){
    if(string_buf_ptr-string_buf>1024){
      yylval.error_msg="String constant too long";
      return ERROR;
    }
    appendc(0);//truncate buffer
    yylval.symbol=stringtable.add_string(string_buf);
    return STR_CONST;
  }
  yylval.error_msg="String contains null character.";
  return ERROR;
}
<STRING>\\\n { //str newline
  ++curr_lineno;
  appendc(yytext[1]);
} 
<STRING>\n { //str 
  ++curr_lineno;
  BEGIN(INITIAL);
  yylval.error_msg="Unterminated string constant";
  return ERROR;
} 
<STRING>\\\0 |
<STRING>\0 {
  string_err=1;
}
<STRING>\\[^\0] { //str escape \b \t \n \f
  char c;
  switch(yytext[1]){
    case 'b': {c='\b';break;}
    case 't': {c='\t';break;}
    case 'n': {c='\n';break;}
    case 'f': {c='\f';break;}
    case 0: YY_FATAL_ERROR("Match Fatal ERR");
    default: c=yytext[1];
  }
  appendc(c);
}
<STRING>[^\\\n\"\0]* { //trivial string content
  appends(yytext);
}

<STRING><<EOF>> { //Wrong String
  yylval.error_msg="EOF in string constant";
  BEGIN(INITIAL);
  return ERROR;
}
<STRING>.


<INITIAL>"--"[^\n]*
<INITIAL>"*)" {
  yylval.error_msg="Unmatched *)";
  return ERROR;
}
<INITIAL>(?i:"class") return CLASS;
<INITIAL>(?i:"else") return ELSE;
<INITIAL>(?i:"fi") return FI;
<INITIAL>(?i:"if") return IF;
<INITIAL>(?i:"in") return IN;
<INITIAL>(?i:"inherits") return INHERITS;
<INITIAL>(?i:"isvoid") return ISVOID;
<INITIAL>(?i:"let") return LET;
<INITIAL>(?i:"loop") return LOOP;
<INITIAL>(?i:"pool") return POOL;
<INITIAL>(?i:"then") return THEN;
<INITIAL>(?i:"while") return WHILE;      
<INITIAL>(?i:"case") return CASE;
<INITIAL>(?i:"esac") return ESAC;
<INITIAL>(?i:"new") return NEW; 
<INITIAL>(?i:"of") return OF;  
<INITIAL>(?i:"not") return NOT; 
<INITIAL>(?i:"=>") return DARROW;     
<INITIAL>(?i:"<=") return LE;  
<INITIAL>(?i:"<-") return ASSIGN;
<INITIAL>t(?i:rue) return yylval.boolean=1,BOOL_CONST;
<INITIAL>f(?i:alse) return yylval.boolean=0,BOOL_CONST;
<INITIAL>"+" return('+');
<INITIAL>"/" return('/');
<INITIAL>"-" return('-');
<INITIAL>"*" return('*');
<INITIAL>"=" return('=');
<INITIAL>"<" return('<');
<INITIAL>"." return('.');
<INITIAL>"~" return('~');
<INITIAL>"," return(',');
<INITIAL>";" return(';');
<INITIAL>":" return(':');
<INITIAL>"(" return('(');
<INITIAL>")" return(')');
<INITIAL>"@" return('@');
<INITIAL>"{" return('{');
<INITIAL>"}" return('}');


<INITIAL>{digit}+ {
  yylval.symbol=inttable.add_string(yytext);
  return INT_CONST;
}

\n {++curr_lineno;}
<INITIAL>[ \f\r\t\v]+ 
<INITIAL>{typeid} {
  yylval.symbol=idtable.add_string(yytext);
  return TYPEID;
}
<INITIAL>{objid} {
  yylval.symbol=idtable.add_string(yytext);
  return OBJECTID;
}
<INITIAL>. {
  yylval.error_msg=yytext;
  return ERROR;
}

<<EOF>> {
   yyterminate();
}

%%