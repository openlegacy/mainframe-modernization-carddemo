       01  COACCSTI.
           02  FILLER PIC X(12).
           02  CUSTIDINL    COMP  PIC  S9(4).
           02  CUSTIDINF    PICTURE X.
           02  FILLER REDEFINES CUSTIDINF.
             03 CUSTIDINA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  CUSTIDINI  PIC X(9).
           02  ERRMSGL    COMP  PIC  S9(4).
           02  ERRMSGF    PICTURE X.
           02  FILLER REDEFINES ERRMSGF.
             03 ERRMSGA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ERRMSGI  PIC X(78).
       01  COACCSTO REDEFINES COACCSTI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  CUSTIDINC    PICTURE X.
           02  CUSTIDINP    PICTURE X.
           02  CUSTIDINH    PICTURE X.
           02  CUSTIDINV    PICTURE X.
           02  CUSTIDINO  PIC X(9).
           02  FILLER PICTURE X(3).
           02  ERRMSGC    PICTURE X.
           02  ERRMSGP    PICTURE X.
           02  ERRMSGH    PICTURE X.
           02  ERRMSGV    PICTURE X.
           02  ERRMSGO  PIC X(78).