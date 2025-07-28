      ****************************************************************
      *    Linkage Data-structure for TRANsaction record (RECLN = 350)
      *    Used for inter-program communication in transaction wizard
      *    To be used at 05 level under DFHCOMMAREA
      *****************************************************************
       05  LK-TRAN-RECORD.
           10  LK-TRAN-ID                              PIC X(16).
           10  LK-TRAN-TYPE-CD                         PIC X(02).
           10  LK-TRAN-CAT-CD                          PIC 9(04).
           10  LK-TRAN-SOURCE                          PIC X(10).
           10  LK-TRAN-DESC                            PIC X(100).
           10  LK-TRAN-AMT                             PIC X(12).
           10  LK-TRAN-MERCHANT-ID                     PIC X(09).
           10  LK-TRAN-MERCHANT-NAME                   PIC X(50).
           10  LK-TRAN-MERCHANT-CITY                   PIC X(50).
           10  LK-TRAN-MERCHANT-ZIP                    PIC X(10).
           10  LK-TRAN-CARD-NUM                        PIC X(16).
           10  LK-TRAN-ORIG-TS                         PIC X(26).
           10  LK-TRAN-PROC-TS                         PIC X(26).
           10  FILLER                                  PIC X(20).
      *