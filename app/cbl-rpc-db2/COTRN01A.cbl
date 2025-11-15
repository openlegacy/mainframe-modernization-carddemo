000001******************************************************************
      * Program:     COTRN01A                                        *
      * Function:    Transaction view DB2 RPC service                *
      * Description: Stateless RPC program for transaction lookup    *
      *              operations using DB2 database                   *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           COTRN01A.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
      *----------------------------------------------------------------*
      *                     WORKING STORAGE SECTION
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

       01 WS-VARIABLES.
          05 WS-PGMNAME                 PIC X(08) VALUE 'COTRN01A'.

       01 WS-DISPLAY-WORK-FIELDS.
          05 WS-SQLCODE-DISPLAY         PIC S9(9).
          05 WS-NUMERIC-DISPLAY         PIC S9(15)V99.
          05 WS-COUNT-DISPLAY           PIC 9(9).
          05 WS-ID-DISPLAY              PIC 9(15).

       01 WS-TRAN-AMT                   PIC +99999999.99.

      *----------------------------------------------------------------*
      * DB2 HOST VARIABLES FOR TRANSACT TABLE
      *----------------------------------------------------------------*
       01 HOST-TRANSACT-REC.
          05 H-TRAN-ID                  PIC X(16).
          05 H-TRAN-CARD-NUM            PIC X(16).
          05 H-TRAN-TYPE-CD             PIC X(02).
          05 H-TRAN-CAT-CD              PIC S9(04) COMP.
          05 H-TRAN-SOURCE              PIC X(10).
          05 H-TRAN-AMT                 PIC S9(9)V99 COMP-3.
          05 H-TRAN-DESC                PIC X(100).
          05 H-TRAN-ORIG-TS             PIC X(26).
          05 H-TRAN-PROC-TS             PIC X(26).
          05 H-TRAN-MERCHANT-ID         PIC S9(09) COMP.
          05 H-TRAN-MERCHANT-NAME       PIC X(50).
          05 H-TRAN-MERCHANT-CITY       PIC X(50).
          05 H-TRAN-MERCHANT-ZIP        PIC X(10).

      *----------------------------------------------------------------*
      * DB2 NULL INDICATORS
      *----------------------------------------------------------------*
       01 HOST-INDICATOR-VARS.
          05 H-TRAN-ID-IND              PIC S9(4) COMP.
          05 H-CARD-NUM-IND             PIC S9(4) COMP.
          05 H-TYPE-CD-IND              PIC S9(4) COMP.
          05 H-CAT-CD-IND               PIC S9(4) COMP.
          05 H-SOURCE-IND               PIC S9(4) COMP.
          05 H-AMT-IND                  PIC S9(4) COMP.
          05 H-DESC-IND                 PIC S9(4) COMP.
          05 H-ORIG-TS-IND              PIC S9(4) COMP.
          05 H-PROC-TS-IND              PIC S9(4) COMP.
          05 H-MERCHANT-ID-IND          PIC S9(4) COMP.
          05 H-MERCHANT-NAME-IND        PIC S9(4) COMP.
          05 H-MERCHANT-CITY-IND        PIC S9(4) COMP.
          05 H-MERCHANT-ZIP-IND         PIC S9(4) COMP.

      *----------------------------------------------------------------*
      * SQL COMMUNICATION AREA
      *----------------------------------------------------------------*
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *----------------------------------------------------------------*
      *                        LINKAGE SECTION
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01 DFHCOMMAREA.
          05 LK-INPUT-PARMS.
             10 LK-IN-OPERATION         PIC X(01).
                88 OP-READ                       VALUE 'R'.
             10 LK-IN-TRAN-ID           PIC X(16).
          05 LK-OUTPUT-STATUS.
             10 LK-OUT-RETURN-CODE      PIC 9(02).
                88 RC-SUCCESS                    VALUE 00.
                88 RC-NOT-FOUND                  VALUE 01.
                88 RC-INPUT-ERROR                VALUE 03.
                88 RC-DATABASE-ERROR             VALUE 99.
             10 LK-OUT-MESSAGE          PIC X(80).
             10 LK-OUT-ERROR-FIELD      PIC X(30).
          05 LK-OUTPUT-DATA.
             10 LK-OUT-TRAN-ID          PIC X(16).
             10 LK-OUT-CARD-NUM         PIC X(16).
             10 LK-OUT-TYPE-CD          PIC X(02).
             10 LK-OUT-CAT-CD           PIC 9(04).
             10 LK-OUT-SOURCE           PIC X(10).
             10 LK-OUT-AMT              PIC X(13).
             10 LK-OUT-DESC             PIC X(100).
             10 LK-OUT-ORIG-TS          PIC X(26).
             10 LK-OUT-PROC-TS          PIC X(26).
             10 LK-OUT-MERCHANT-ID      PIC 9(09).
             10 LK-OUT-MERCHANT-NAME    PIC X(50).
             10 LK-OUT-MERCHANT-CITY    PIC X(50).
             10 LK-OUT-MERCHANT-ZIP     PIC X(10).

      *----------------------------------------------------------------*
      *                       PROCEDURE DIVISION
      *----------------------------------------------------------------*
       PROCEDURE DIVISION USING DFHCOMMAREA.

       MAIN-PARA.
           INITIALIZE LK-OUTPUT-STATUS
                      LK-OUTPUT-DATA
                      WS-DISPLAY-WORK-FIELDS
                      HOST-TRANSACT-REC
                      HOST-INDICATOR-VARS

           SET RC-SUCCESS TO TRUE
           MOVE SPACES TO LK-OUT-MESSAGE
                          LK-OUT-ERROR-FIELD

           EVALUATE TRUE
               WHEN OP-READ
                   PERFORM 2000-VALID-INPUT-DATA
                      THRU 2000-VALID-INPUT-DATA-EXIT
                   IF RC-SUCCESS
                       PERFORM 1000-PROCESS-READ
                          THRU 1000-PROCESS-READ-EXIT
                   END-IF
               WHEN OTHER
                   SET RC-INPUT-ERROR TO TRUE
                   STRING 'Invalid operation code'
                          DELIMITED BY SIZE
                     INTO LK-OUT-MESSAGE
           END-EVALUATE

           GOBACK.

      *----------------------------------------------------------------*
      *                      2000-VALID-INPUT-DATA
      *----------------------------------------------------------------*
       2000-VALID-INPUT-DATA.

           IF LK-IN-TRAN-ID = SPACES OR LOW-VALUES
               SET RC-INPUT-ERROR TO TRUE
               STRING 'Tran ID can NOT be empty...'
                      DELIMITED BY SIZE
                 INTO LK-OUT-MESSAGE
               MOVE 'TRAN-ID' TO LK-OUT-ERROR-FIELD
           END-IF.

       2000-VALID-INPUT-DATA-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *                      1000-PROCESS-READ
      *----------------------------------------------------------------*
       1000-PROCESS-READ.

           PERFORM 1100-READ-TRANSACT-DB2
              THRU 1100-READ-TRANSACT-DB2-EXIT

           IF RC-SUCCESS
               PERFORM 1200-FMT-OUTPUT-DATA
                  THRU 1200-FMT-OUTPUT-DATA-EXIT
           END-IF.

       1000-PROCESS-READ-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *                      1100-READ-TRANSACT-DB2
      *----------------------------------------------------------------*
       1100-READ-TRANSACT-DB2.

           MOVE LK-IN-TRAN-ID TO H-TRAN-ID

           EXEC SQL
               SELECT TRAN_ID,
                      TRAN_CARD_NUM,
                      TRAN_TYPE_CD,
                      TRAN_CAT_CD,
                      TRAN_SOURCE,
                      TRAN_AMT,
                      TRAN_DESC,
                      TRAN_ORIG_TS,
                      TRAN_PROC_TS,
                      TRAN_MERCHANT_ID,
                      TRAN_MERCHANT_NAME,
                      TRAN_MERCHANT_CITY,
                      TRAN_MERCHANT_ZIP
               INTO  :H-TRAN-ID :H-TRAN-ID-IND,
                     :H-TRAN-CARD-NUM :H-CARD-NUM-IND,
                     :H-TRAN-TYPE-CD :H-TYPE-CD-IND,
                     :H-TRAN-CAT-CD :H-CAT-CD-IND,
                     :H-TRAN-SOURCE :H-SOURCE-IND,
                     :H-TRAN-AMT :H-AMT-IND,
                     :H-TRAN-DESC :H-DESC-IND,
                     :H-TRAN-ORIG-TS :H-ORIG-TS-IND,
                     :H-TRAN-PROC-TS :H-PROC-TS-IND,
                     :H-TRAN-MERCHANT-ID :H-MERCHANT-ID-IND,
                     :H-TRAN-MERCHANT-NAME :H-MERCHANT-NAME-IND,
                     :H-TRAN-MERCHANT-CITY :H-MERCHANT-CITY-IND,
                     :H-TRAN-MERCHANT-ZIP :H-MERCHANT-ZIP-IND
               FROM  TRANSACT
               WHERE TRAN_ID = :H-TRAN-ID
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   SET RC-SUCCESS TO TRUE
               WHEN +100
                   SET RC-NOT-FOUND TO TRUE
                   STRING 'Transaction ID NOT found...'
                          DELIMITED BY SIZE
                     INTO LK-OUT-MESSAGE
                   MOVE 'TRAN-ID' TO LK-OUT-ERROR-FIELD
               WHEN OTHER
                   SET RC-DATABASE-ERROR TO TRUE
                   MOVE SQLCODE TO WS-SQLCODE-DISPLAY
                   STRING 'Unable to lookup Transaction - SQLCODE: '
                          DELIMITED BY SIZE
                          WS-SQLCODE-DISPLAY
                          DELIMITED BY SIZE
                     INTO LK-OUT-MESSAGE
                   MOVE 'TRAN-ID' TO LK-OUT-ERROR-FIELD
           END-EVALUATE.

       1100-READ-TRANSACT-DB2-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *                      1200-FMT-OUTPUT-DATA
      *----------------------------------------------------------------*
       1200-FMT-OUTPUT-DATA.

           MOVE H-TRAN-AMT TO WS-TRAN-AMT

           MOVE H-TRAN-ID            TO LK-OUT-TRAN-ID
           MOVE H-TRAN-CARD-NUM      TO LK-OUT-CARD-NUM
           MOVE H-TRAN-TYPE-CD       TO LK-OUT-TYPE-CD
           MOVE H-TRAN-CAT-CD        TO LK-OUT-CAT-CD
           MOVE H-TRAN-SOURCE        TO LK-OUT-SOURCE
           MOVE WS-TRAN-AMT          TO LK-OUT-AMT
           MOVE H-TRAN-DESC          TO LK-OUT-DESC
           MOVE H-TRAN-ORIG-TS       TO LK-OUT-ORIG-TS
           MOVE H-TRAN-PROC-TS       TO LK-OUT-PROC-TS
           MOVE H-TRAN-MERCHANT-ID   TO LK-OUT-MERCHANT-ID
           MOVE H-TRAN-MERCHANT-NAME TO LK-OUT-MERCHANT-NAME
           MOVE H-TRAN-MERCHANT-CITY TO LK-OUT-MERCHANT-CITY
           MOVE H-TRAN-MERCHANT-ZIP  TO LK-OUT-MERCHANT-ZIP.

       1200-FMT-OUTPUT-DATA-EXIT.
           EXIT.