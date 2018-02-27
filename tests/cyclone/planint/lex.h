#ifndef LEX_H
#define LEX_H

extern union keyword {
  void   IF;
  void   IN;
  void   HD;
  void   TL;
  void   OR;
  void   FUN;
  void   VAL;
  void   KEY;
  void   EXN;
  void   DEV;
  void   TRY;
  void   LET;
  void   END;
  void   NOT;
  void   FST;
  void   SND;
  void   ORD;
  void   CHR;
  void   AND;
  void   ORI;
  void   INT;
  void   LIST;
  void   UNIT;
  void   CHAR;
  void   BOOL;
  void   HOST;
  void   PORT;
  void   BLOB;
  void   THEN;
  void   ELSE;
  void   TRUE;
  void   NOTI;
  void   XORI;
  void   ANDI;
  void   CHUNK;
  void   RAISE;
  void   FALSE;
  void   FOLDR;
  void   FOLDL;
  void   HANDLE;
  void   STRING;
  void   EXPLODE;
  void   IMPLODE;
  void   ONREMOTE;
  void   EXCEPTION;
  void   ONNEIGHBOR;
  void   RETRANSONREMOTE;
}

extern union token {
  string  ERROR;
  void    PEOF;
  int     Number;
  keyword Keyword;
  string  Id;
  char    Char;
  string  String;
  void    LPAREN;
  void    RPAREN;
  void    LBRACKET;
  void    RBRACKET;
  void    BAR;
  void    TILDE;
  void    DIV;
  void    PERCENT;
  void    ASTERISK;
  void    PLUS;
  void    MINUS;
  void    LESS;
  void    GREATER;
  void    EQ;
  void    COLON;
  void    SEMICOLON;
  void    HASH;
  void    CARAT;
  void    PERIOD;
  void    COMMA;
}

extern token next_token(string_file);
extern token peek_token(string_file);
extern void  print_token(token);
extern void  print_char(char);
extern void  reset_topt();

#endif

