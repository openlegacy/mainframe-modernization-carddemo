000001******************************************************************
      * Program:     COTRN00A                                          *
      * Function:    Transaction list RPC service with DB2            *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           COTRN00A.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
      *----------------------------------------------------------------*
      *                     WORKING STORAGE SECTION
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

       01 WS-VARIABLES.
         05 WS-PGMNAME                 PIC X(08) VALUE 'COTRN00A'.
         05 WS-TRANSACT-TABLE          PIC X(08) VALUE 'TRANSACT'.
         05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.
           88 ERR-FLG-ON                         VALUE 'Y'.
           88 ERR-FLG-OFF                        VALUE 'N'.
         05 WS-TRANSACT-EOF            PIC X(01) VALUE 'N'.
           88 TRANSACT-EOF                       VALUE 'Y'.
           88 TRANSACT-NOT-EOF                   VALUE 'N'.

         05 WS-IDX                     PIC S9(04) COMP VALUE ZEROS.
         05 WS-TEMP-IDX                PIC S9(04) COMP VALUE ZEROS.

         05 WS-TRAN-AMT                PIC +99999999.99.
         05 WS-TRAN-DATE               PIC X(08) VALUE '00/00/00'.

      * Display conversion fields for SQLCODE and numeric values
       01 WS-DISPLAY-WORK-FIELDS.
          05 WS-SQLCODE-DISPLAY        PIC S9(9).
          05 WS-NUMERIC-DISPLAY        PIC S9(15)V99.

      ******************************************************************
      * DB2 related variables
      ******************************************************************
       01 WS-DB2-VARS.
          05 WS-SQLCODE                PIC S9(09) COMP VALUE ZEROS.
          05 WS-DB2-CURSOR-STATE       PIC X(1) VALUE SPACES.
             88 WS-CURSOR-CLOSED                 VALUE 'C'.
             88 WS-CURSOR-OPEN                   VALUE 'O'.

      ******************************************************************
      * Temporary storage for backward records
      ******************************************************************
       01 WS-TEMP-TRAN-RECORDS.
          05 WS-TEMP-TRAN-COUNT        PIC 9(02) VALUE 0.
          05 WS-TEMP-TRAN-DATA OCCURS 11 TIMES.
             10 WS-TEMP-TRAN-ID        PIC X(16).
             10 WS-TEMP-TRAN-AMT       PIC S9(10)V99 COMP-3.
             10 WS-TEMP-TRAN-DESC      PIC X(50).
             10 WS-TEMP-TRAN-TS        PIC X(26).

      ******************************************************************
      * COPY statements from original program
      ******************************************************************
       COPY CVTRA05Y.

       COPY CSDAT01Y.

      ******************************************************************
      * DB2 SQLCA
      ******************************************************************
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      ******************************************************************
      * DB2 HOST VARIABLE DECLARATIONS
      ******************************************************************
           EXEC SQL BEGIN DECLARE SECTION END-EXEC.

      * Host variables for TRANSACT table
       01  HV-TRAN-VARIABLES.
           05  HV-TRAN-ID                 PIC X(16).
           05  HV-TRAN-TYPE-CD            PIC X(2).
           05  HV-TRAN-CAT-CD             PIC S9(4) COMP.
           05  HV-TRAN-SOURCE             PIC X(10).
           05  HV-TRAN-DESC               PIC X(50).
           05  HV-TRAN-AMT                PIC S9(10)V99 COMP-3.
           05  HV-TRAN-MERCHANT-ID        PIC S9(9) COMP.
           05  HV-TRAN-MERCHANT-NAME      PIC X(50).
           05  HV-TRAN-MERCHANT-CITY      PIC X(50).
           05  HV-TRAN-MERCHANT-ZIP       PIC X(10).
           05  HV-TRAN-CARD-NUM           PIC X(16).
           05  HV-TRAN-ORIG-TS            PIC X(26).
           05  HV-TRAN-PROC-TS            PIC X(26).

      * Host variables for search criteria
       01  HV-SEARCH-VARIABLES.
           05  HV-SEARCH-TRAN-ID          PIC X(16).

           EXEC SQL END DECLARE SECTION END-EXEC.

      ******************************************************************
      * DB2 DECLARE CURSOR - Forward
      ******************************************************************
           EXEC SQL
               DECLARE TRAN_CURSOR CURSOR FOR
               SELECT TRAN_ID,
                      TRAN_TYPE_CD,
                      TRAN_CAT_CD,
                      TRAN_SOURCE,
                      TRAN_DESC,
                      TRAN_AMT,
                      TRAN_MERCHANT_ID,
                      TRAN_MERCHANT_NAME,
                      TRAN_MERCHANT_CITY,
                      TRAN_MERCHANT_ZIP,
                      TRAN_CARD_NUM,
                      TRAN_ORIG_TS,
                      TRAN_PROC_TS
               FROM TRANSACT
               WHERE TRAN_ID >= :HV-SEARCH-TRAN-ID
               ORDER BY TRAN_ID ASC
           END-EXEC.

      ******************************************************************
      * DB2 DECLARE CURSOR - Backward
      ******************************************************************
           EXEC SQL
               DECLARE TRAN_CURSOR_PREV CURSOR FOR
               SELECT TRAN_ID,
                      TRAN_TYPE_CD,
                      TRAN_CAT_CD,
                      TRAN_SOURCE,
                      TRAN_DESC,
                      TRAN_AMT,
                      TRAN_MERCHANT_ID,
                      TRAN_MERCHANT_NAME,
                      TRAN_MERCHANT_CITY,
                      TRAN_MERCHANT_ZIP,
                      TRAN_CARD_NUM,
                      TRAN_ORIG_TS,
                      TRAN_PROC_TS
               FROM TRANSACT
               WHERE TRAN_ID < :HV-SEARCH-TRAN-ID
               ORDER BY TRAN_ID DESC
           END-EXEC.

      *----------------------------------------------------------------*
      *                        LINKAGE SECTION
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01 DFHCOMMAREA.
          05 LK-INPUT-PARMS.
             10 LK-IN-OPERATION         PIC X(01).
                88 OP-READ                       VALUE 'R'.
                88 OP-LIST-FORWARD               VALUE 'F'.
                88 OP-LIST-BACKWARD              VALUE 'B'.
             10 LK-IN-TRAN-ID           PIC X(16).
             10 LK-IN-MAX-RECORDS       PIC S9(04) COMP.
          05 LK-OUTPUT-STATUS.
             10 LK-OUT-RETURN-CODE      PIC 9(02).
                88 RC-SUCCESS                    VALUE 00.
                88 RC-NOT-FOUND                  VALUE 01.
                88 RC-INPUT-ERROR                VALUE 03.
                88 RC-DATABASE-ERROR             VALUE 99.
             10 LK-OUT-MESSAGE          PIC X(80).
          05 LK-OUTPUT-DATA.
             10 LK-OUT-RECORDS-COUNT    PIC S9(04) COMP.
             10 LK-OUT-NEXT-PAGE-FLG    PIC X(01).
                88 LK-NEXT-PAGE-YES              VALUE 'Y'.
                88 LK-NEXT-PAGE-NO               VALUE 'N'.
             10 LK-OUT-TRAN-ARRAY       OCCURS 1 TO 10
                                        DEPENDING ON
                                        LK-OUT-RECORDS-COUNT.
                15 LK-OUT-TRAN-ID       PIC X(16).
                15 LK-OUT-TRAN-AMT      PIC S9(10)V99 COMP-3.
                15 LK-OUT-TRAN-DESC     PIC X(50).
                15 LK-OUT-TRAN-ORIG-TS  PIC X(26).

      *----------------------------------------------------------------*
      *                       PROCEDURE DIVISION
      *----------------------------------------------------------------*
       PROCEDURE DIVISION USING DFHCOMMAREA.

       MAIN-PARA.
           INITIALIZE LK-OUTPUT-STATUS
                      WS-DISPLAY-WORK-FIELDS

           MOVE ZEROS TO LK-OUT-RECORDS-COUNT
           MOVE SPACES TO LK-OUT-NEXT-PAGE-FLG
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 10
               MOVE SPACES TO LK-OUT-TRAN-ID(WS-IDX)
               MOVE ZEROS TO LK-OUT-TRAN-AMT(WS-IDX)
               MOVE SPACES TO LK-OUT-TRAN-DESC(WS-IDX)
               MOVE SPACES TO LK-OUT-TRAN-ORIG-TS(WS-IDX)
           END-PERFORM

           SET RC-SUCCESS TO TRUE
           SET WS-CURSOR-CLOSED TO TRUE
           SET TRANSACT-NOT-EOF TO TRUE
           SET ERR-FLG-OFF TO TRUE

           EVALUATE TRUE
               WHEN OP-READ
                   PERFORM 2000-VALIDATE-INPUT-DATA
                      THRU 2000-VALIDATE-INPUT-DATA-EXIT
                   IF RC-SUCCESS
                       PERFORM 1000--READ
                          THRU 1000--READ-EXIT
                   END-IF
               WHEN OP-LIST-FORWARD
                   PERFORM 2000-VALIDATE-INPUT-DATA
                      THRU 2000-VALIDATE-INPUT-DATA-EXIT
                   IF RC-SUCCESS
                       PERFORM 9000-LIST-FORWARD
                          THRU 9000-LIST-FORWARD-EXIT
                   END-IF
               WHEN OP-LIST-BACKWARD
                   PERFORM 2000-VALIDATE-INPUT-DATA
                      THRU 2000-VALIDATE-INPUT-DATA-EXIT
                   IF RC-SUCCESS
                       PERFORM 9100-LIST-BACKWARD
                          THRU 9100-LIST-BACKWARD-EXIT
                   END-IF
               WHEN OTHER
                   SET RC-INPUT-ERROR TO TRUE
                   STRING 'Invalid operation code'
                          DELIMITED BY SIZE
                     INTO LK-OUT-MESSAGE
           END-EVALUATE

           PERFORM 9999-CLOSE-CURSOR
              THRU 9999-CLOSE-CURSOR-EXIT

           GOBACK.

      *----------------------------------------------------------------*
      *                      1000--READ
      *----------------------------------------------------------------*
       1000--READ.

           IF LK-IN-TRAN-ID = SPACES OR LOW-VALUES
               MOVE LOW-VALUES TO HV-SEARCH-TRAN-ID
           ELSE
               MOVE LK-IN-TRAN-ID TO HV-SEARCH-TRAN-ID
           END-IF

           PERFORM 9200-OPEN-TRAN-CURSOR
              THRU 9200-OPEN-TRAN-CURSOR-EXIT

           IF NOT ERR-FLG-ON
               MOVE 0 TO LK-OUT-RECORDS-COUNT

               PERFORM UNTIL LK-OUT-RECORDS-COUNT >= LK-IN-MAX-RECORDS
                             OR TRANSACT-EOF OR ERR-FLG-ON
                   PERFORM 9300-READNEXT-TRAN-DB2
                      THRU 9300-READNEXT-TRAN-DB2-EXIT
                   IF TRANSACT-NOT-EOF AND ERR-FLG-OFF
                       ADD 1 TO LK-OUT-RECORDS-COUNT
                       PERFORM 9500-POPULATE-TRAN-OUTPUT
                          THRU 9500-POPULATE-TRAN-OUTPUT-EXIT
                   END-IF
               END-PERFORM

               IF LK-OUT-RECORDS-COUNT = 0
                   SET RC-NOT-FOUND TO TRUE
                   STRING 'No transactions found'
                          DELIMITED BY SIZE
                     INTO LK-OUT-MESSAGE
               ELSE
                   PERFORM 9300-READNEXT-TRAN-DB2
                      THRU 9300-READNEXT-TRAN-DB2-EXIT
                   IF TRANSACT-NOT-EOF AND ERR-FLG-OFF
                       SET LK-NEXT-PAGE-YES TO TRUE
                   ELSE
                       SET LK-NEXT-PAGE-NO TO TRUE
                   END-IF
               END-IF
           END-IF.

       1000--READ-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *                      2000-VALIDATE-INPUT-DATA
      *----------------------------------------------------------------*
       2000-VALIDATE-INPUT-DATA.

           IF LK-IN-TRAN-ID NOT = SPACES AND LOW-VALUES
               IF LK-IN-TRAN-ID IS NOT NUMERIC
                   SET RC-INPUT-ERROR TO TRUE
                   STRING 'Transaction ID must be numeric'
                          DELIMITED BY SIZE
                     INTO LK-OUT-MESSAGE
               END-IF
           END-IF

           IF LK-IN-MAX-RECORDS = 0
               MOVE 10 TO LK-IN-MAX-RECORDS
           END-IF.

       2000-VALIDATE-INPUT-DATA-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *                      9000-LIST-FORWARD
      *----------------------------------------------------------------*
       9000-LIST-FORWARD.

           IF LK-IN-TRAN-ID = SPACES OR LOW-VALUES
               MOVE LOW-VALUES TO HV-SEARCH-TRAN-ID
           ELSE
               MOVE LK-IN-TRAN-ID TO HV-SEARCH-TRAN-ID
           END-IF

           PERFORM 9200-OPEN-TRAN-CURSOR
              THRU 9200-OPEN-TRAN-CURSOR-EXIT

           IF NOT ERR-FLG-ON
               IF LK-IN-TRAN-ID NOT = SPACES AND LOW-VALUES
                   PERFORM 9300-READNEXT-TRAN-DB2
                      THRU 9300-READNEXT-TRAN-DB2-EXIT
               END-IF

               MOVE 0 TO LK-OUT-RECORDS-COUNT

               PERFORM UNTIL LK-OUT-RECORDS-COUNT >= LK-IN-MAX-RECORDS
                             OR TRANSACT-EOF OR ERR-FLG-ON
                   PERFORM 9300-READNEXT-TRAN-DB2
                      THRU 9300-READNEXT-TRAN-DB2-EXIT
                   IF TRANSACT-NOT-EOF AND ERR-FLG-OFF
                       ADD 1 TO LK-OUT-RECORDS-COUNT
                       PERFORM 9500-POPULATE-TRAN-OUTPUT
                          THRU 9500-POPULATE-TRAN-OUTPUT-EXIT
                   END-IF
               END-PERFORM

               IF LK-OUT-RECORDS-COUNT = 0
                   SET RC-NOT-FOUND TO TRUE
                   STRING 'No more pages to display'
                          DELIMITED BY SIZE
                     INTO LK-OUT-MESSAGE
               ELSE
                   PERFORM 9300-READNEXT-TRAN-DB2
                      THRU 9300-READNEXT-TRAN-DB2-EXIT
                   IF TRANSACT-NOT-EOF AND ERR-FLG-OFF
                       SET LK-NEXT-PAGE-YES TO TRUE
                   ELSE
                       SET LK-NEXT-PAGE-NO TO TRUE
                   END-IF
               END-IF
           END-IF.

       9000-LIST-FORWARD-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *                      9100-LIST-BACKWARD
      *----------------------------------------------------------------*
       9100-LIST-BACKWARD.

           IF LK-IN-TRAN-ID = SPACES OR LOW-VALUES
               MOVE LOW-VALUES TO HV-SEARCH-TRAN-ID
           ELSE
               MOVE LK-IN-TRAN-ID TO HV-SEARCH-TRAN-ID
           END-IF

           MOVE 0 TO WS-TEMP-TRAN-COUNT

           PERFORM 9250-OPN-TRN-CURSOR-PREV
              THRU 9250-OPN-TRN-CURSOR-PREV-EXIT

           IF NOT ERR-FLG-ON
               PERFORM UNTIL WS-TEMP-TRAN-COUNT >= 11 OR
                             TRANSACT-EOF OR ERR-FLG-ON
                   PERFORM 9350-READPREV-TRAN-DB2
                      THRU 9350-READPREV-TRAN-DB2-EXIT
                   IF TRANSACT-NOT-EOF AND ERR-FLG-OFF
                       ADD 1 TO WS-TEMP-TRAN-COUNT
                       MOVE HV-TRAN-ID TO
                            WS-TEMP-TRAN-ID(WS-TEMP-TRAN-COUNT)
                       MOVE HV-TRAN-AMT TO
                            WS-TEMP-TRAN-AMT(WS-TEMP-TRAN-COUNT)
                       MOVE HV-TRAN-DESC TO
                            WS-TEMP-TRAN-DESC(WS-TEMP-TRAN-COUNT)
                       MOVE HV-TRAN-ORIG-TS TO
                            WS-TEMP-TRAN-TS(WS-TEMP-TRAN-COUNT)
                   END-IF
               END-PERFORM

               IF TRANSACT-EOF OR ERR-FLG-ON OR
                  WS-TEMP-TRAN-COUNT < 11
                   SET TRANSACT-NOT-EOF TO TRUE
                   SET ERR-FLG-OFF TO TRUE
                   PERFORM 9999-CLOSE-CURSOR
                      THRU 9999-CLOSE-CURSOR-EXIT
                   MOVE LOW-VALUES TO HV-SEARCH-TRAN-ID
                   PERFORM 9200-OPEN-TRAN-CURSOR
                      THRU 9200-OPEN-TRAN-CURSOR-EXIT
                   IF ERR-FLG-OFF
                       MOVE 0 TO LK-OUT-RECORDS-COUNT
                       PERFORM UNTIL
                               LK-OUT-RECORDS-COUNT >=
                               LK-IN-MAX-RECORDS
                               OR TRANSACT-EOF OR ERR-FLG-ON
                           PERFORM 9300-READNEXT-TRAN-DB2
                              THRU 9300-READNEXT-TRAN-DB2-EXIT
                           IF TRANSACT-NOT-EOF AND ERR-FLG-OFF
                               ADD 1 TO LK-OUT-RECORDS-COUNT
                               PERFORM 9500-POPULATE-TRAN-OUTPUT
                                  THRU 9500-POPULATE-TRAN-OUTPUT-EXIT
                           END-IF
                       END-PERFORM
                   END-IF
                   STRING 'No previous pages to display'
                          DELIMITED BY SIZE
                     INTO LK-OUT-MESSAGE
               ELSE
                   MOVE 0 TO LK-OUT-RECORDS-COUNT
                   PERFORM VARYING WS-TEMP-IDX FROM
                           WS-TEMP-TRAN-COUNT BY -1
                           UNTIL WS-TEMP-IDX < 2 OR
                                 LK-OUT-RECORDS-COUNT >= 10
                       ADD 1 TO LK-OUT-RECORDS-COUNT
                       MOVE WS-TEMP-TRAN-ID(WS-TEMP-IDX) TO
                            LK-OUT-TRAN-ID(LK-OUT-RECORDS-COUNT)
                       MOVE WS-TEMP-TRAN-AMT(WS-TEMP-IDX) TO
                            LK-OUT-TRAN-AMT(LK-OUT-RECORDS-COUNT)
                       MOVE WS-TEMP-TRAN-DESC(WS-TEMP-IDX) TO
                            LK-OUT-TRAN-DESC(LK-OUT-RECORDS-COUNT)
                       MOVE WS-TEMP-TRAN-TS(WS-TEMP-IDX) TO
                         LK-OUT-TRAN-ORIG-TS(LK-OUT-RECORDS-COUNT)
                   END-PERFORM
               END-IF

               IF LK-OUT-RECORDS-COUNT = 0
                   SET RC-NOT-FOUND TO TRUE
                   STRING 'You are already at the top of the page'
                          DELIMITED BY SIZE
                     INTO LK-OUT-MESSAGE
               END-IF
           END-IF.

       9100-LIST-BACKWARD-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *                      9200-OPEN-TRAN-CURSOR
      *----------------------------------------------------------------*
       9200-OPEN-TRAN-CURSOR.

           EXEC SQL
               OPEN TRAN_CURSOR
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE

           EVALUATE WS-SQLCODE
               WHEN 0
                   SET WS-CURSOR-OPEN TO TRUE
               WHEN OTHER
                   SET TRANSACT-EOF TO TRUE
                   SET ERR-FLG-ON TO TRUE
                   SET RC-DATABASE-ERROR TO TRUE
                   MOVE SQLCODE TO WS-SQLCODE-DISPLAY
                   STRING 'Database error opening cursor - SQLCODE: '
                          DELIMITED BY SIZE
                          WS-SQLCODE-DISPLAY
                          DELIMITED BY SIZE
                     INTO LK-OUT-MESSAGE
           END-EVALUATE.

       9200-OPEN-TRAN-CURSOR-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *                      9250-OPN-TRN-CURSOR-PREV
      *----------------------------------------------------------------*
       9250-OPN-TRN-CURSOR-PREV.

           EXEC SQL
               OPEN TRAN_CURSOR_PREV
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE

           EVALUATE WS-SQLCODE
               WHEN 0
                   SET WS-CURSOR-OPEN TO TRUE
               WHEN OTHER
                   SET TRANSACT-EOF TO TRUE
                   SET ERR-FLG-ON TO TRUE
                   SET RC-DATABASE-ERROR TO TRUE
                   MOVE SQLCODE TO WS-SQLCODE-DISPLAY
                   STRING 'Database error opening cursor - SQLCODE: '
                          DELIMITED BY SIZE
                          WS-SQLCODE-DISPLAY
                          DELIMITED BY SIZE
                     INTO LK-OUT-MESSAGE
           END-EVALUATE.

       9250-OPN-TRN-CURSOR-PREV-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *                      9300-READNEXT-TRAN-DB2
      *----------------------------------------------------------------*
       9300-READNEXT-TRAN-DB2.

           EXEC SQL
               FETCH TRAN_CURSOR
               INTO :HV-TRAN-ID,
                    :HV-TRAN-TYPE-CD,
                    :HV-TRAN-CAT-CD,
                    :HV-TRAN-SOURCE,
                    :HV-TRAN-DESC,
                    :HV-TRAN-AMT,
                    :HV-TRAN-MERCHANT-ID,
                    :HV-TRAN-MERCHANT-NAME,
                    :HV-TRAN-MERCHANT-CITY,
                    :HV-TRAN-MERCHANT-ZIP,
                    :HV-TRAN-CARD-NUM,
                    :HV-TRAN-ORIG-TS,
                    :HV-TRAN-PROC-TS
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE

           EVALUATE WS-SQLCODE
               WHEN 0
                   MOVE HV-TRAN-ID TO TRAN-ID
                   MOVE HV-TRAN-TYPE-CD TO TRAN-TYPE-CD
                   MOVE HV-TRAN-CAT-CD TO TRAN-CAT-CD
                   MOVE HV-TRAN-SOURCE TO TRAN-SOURCE
                   MOVE HV-TRAN-DESC TO TRAN-DESC
                   MOVE HV-TRAN-AMT TO TRAN-AMT
                   MOVE HV-TRAN-MERCHANT-ID TO TRAN-MERCHANT-ID
                   MOVE HV-TRAN-MERCHANT-NAME TO TRAN-MERCHANT-NAME
                   MOVE HV-TRAN-MERCHANT-CITY TO TRAN-MERCHANT-CITY
                   MOVE HV-TRAN-MERCHANT-ZIP TO TRAN-MERCHANT-ZIP
                   MOVE HV-TRAN-CARD-NUM TO TRAN-CARD-NUM
                   MOVE HV-TRAN-ORIG-TS TO TRAN-ORIG-TS
                   MOVE HV-TRAN-PROC-TS TO TRAN-PROC-TS
               WHEN +100
                   SET TRANSACT-EOF TO TRUE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   SET RC-DATABASE-ERROR TO TRUE
                   MOVE SQLCODE TO WS-SQLCODE-DISPLAY
                   STRING 'Database error fetching data - SQLCODE: '
                          DELIMITED BY SIZE
                          WS-SQLCODE-DISPLAY
                          DELIMITED BY SIZE
                     INTO LK-OUT-MESSAGE
           END-EVALUATE.

       9300-READNEXT-TRAN-DB2-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *                      9350-READPREV-TRAN-DB2
      *----------------------------------------------------------------*
       9350-READPREV-TRAN-DB2.

           EXEC SQL
               FETCH TRAN_CURSOR_PREV
               INTO :HV-TRAN-ID,
                    :HV-TRAN-TYPE-CD,
                    :HV-TRAN-CAT-CD,
                    :HV-TRAN-SOURCE,
                    :HV-TRAN-DESC,
                    :HV-TRAN-AMT,
                    :HV-TRAN-MERCHANT-ID,
                    :HV-TRAN-MERCHANT-NAME,
                    :HV-TRAN-MERCHANT-CITY,
                    :HV-TRAN-MERCHANT-ZIP,
                    :HV-TRAN-CARD-NUM,
                    :HV-TRAN-ORIG-TS,
                    :HV-TRAN-PROC-TS
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE

           EVALUATE WS-SQLCODE
               WHEN 0
                   MOVE HV-TRAN-ID TO TRAN-ID
                   MOVE HV-TRAN-TYPE-CD TO TRAN-TYPE-CD
                   MOVE HV-TRAN-CAT-CD TO TRAN-CAT-CD
                   MOVE HV-TRAN-SOURCE TO TRAN-SOURCE
                   MOVE HV-TRAN-DESC TO TRAN-DESC
                   MOVE HV-TRAN-AMT TO TRAN-AMT
                   MOVE HV-TRAN-MERCHANT-ID TO TRAN-MERCHANT-ID
                   MOVE HV-TRAN-MERCHANT-NAME TO TRAN-MERCHANT-NAME
                   MOVE HV-TRAN-MERCHANT-CITY TO TRAN-MERCHANT-CITY
                   MOVE HV-TRAN-MERCHANT-ZIP TO TRAN-MERCHANT-ZIP
                   MOVE HV-TRAN-CARD-NUM TO TRAN-CARD-NUM
                   MOVE HV-TRAN-ORIG-TS TO TRAN-ORIG-TS
                   MOVE HV-TRAN-PROC-TS TO TRAN-PROC-TS
               WHEN +100
                   SET TRANSACT-EOF TO TRUE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   SET RC-DATABASE-ERROR TO TRUE
                   MOVE SQLCODE TO WS-SQLCODE-DISPLAY
                   STRING 'Database error fetching data - SQLCODE: '
                          DELIMITED BY SIZE
                          WS-SQLCODE-DISPLAY
                          DELIMITED BY SIZE
                     INTO LK-OUT-MESSAGE
           END-EVALUATE.

       9350-READPREV-TRAN-DB2-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *                      9500-POPULATE-TRAN-OUTPUT
      *----------------------------------------------------------------*
       9500-POPULATE-TRAN-OUTPUT.

           MOVE TRAN-ID TO
                LK-OUT-TRAN-ID(LK-OUT-RECORDS-COUNT)
           MOVE TRAN-AMT TO
                LK-OUT-TRAN-AMT(LK-OUT-RECORDS-COUNT)
           MOVE TRAN-DESC TO
                LK-OUT-TRAN-DESC(LK-OUT-RECORDS-COUNT)
           MOVE TRAN-ORIG-TS TO
                LK-OUT-TRAN-ORIG-TS(LK-OUT-RECORDS-COUNT).

       9500-POPULATE-TRAN-OUTPUT-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *                      9999-CLOSE-CURSOR
      *----------------------------------------------------------------*
       9999-CLOSE-CURSOR.

           IF WS-CURSOR-OPEN
               EXEC SQL
                   CLOSE TRAN_CURSOR
               END-EXEC

               EXEC SQL
                   CLOSE TRAN_CURSOR_PREV
               END-EXEC

               SET WS-CURSOR-CLOSED TO TRUE
           END-IF.

       9999-CLOSE-CURSOR-EXIT.
           EXIT.
