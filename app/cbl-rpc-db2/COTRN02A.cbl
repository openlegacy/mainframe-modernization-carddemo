000001******************************************************************
      * Program:     COTRN02A                                          *
      * Function:    Transaction add RPC service                       *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COTRN02A.
       AUTHOR.     AWS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
      *----------------------------------------------------------------*
      *                     WORKING STORAGE SECTION
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

       01 WS-VARIABLES.
         05 WS-PGMNAME                 PIC X(08) VALUE 'COTRN02A'.

       01 WS-DISPLAY-WORK-FIELDS.
         05 WS-SQLCODE-DISPLAY         PIC S9(9).
         05 WS-RESP-MSG                PIC X(80) VALUE SPACES.

      *----------------------------------------------------------------*
      *                     DB2 HOST VARIABLES
      *----------------------------------------------------------------*
       01 HOST-XREF-RECORD.
         05 HOST-XREF-ACCT-ID          PIC S9(11) COMP.
         05 HOST-XREF-CARD-NUM         PIC X(16).

       01 HOST-TRAN-RECORD.
         05 HOST-TRAN-ID               PIC X(16).
         05 HOST-TRAN-TYPE-CD          PIC X(02).
         05 HOST-TRAN-CAT-CD           PIC S9(04) COMP.
         05 HOST-TRAN-SOURCE           PIC X(10).
         05 HOST-TRAN-DESC             PIC X(50).
         05 HOST-TRAN-AMT              PIC S9(10)V99 COMP-3.
         05 HOST-TRAN-CARD-NUM         PIC X(16).
         05 HOST-TRAN-MERCHANT-ID      PIC S9(09) COMP.
         05 HOST-TRAN-MERCHANT-NAME    PIC X(50).
         05 HOST-TRAN-MERCHANT-CITY    PIC X(50).
         05 HOST-TRAN-MERCHANT-ZIP     PIC X(10).
         05 HOST-TRAN-ORIG-TS          PIC X(26).
         05 HOST-TRAN-PROC-TS          PIC X(26).

       01 WS-WORK-FIELDS.
         05 WS-TRAN-AMT-N              PIC S9(9)V99 VALUE ZERO.
         05 WS-TRAN-AMT-E              PIC +99999999.99 VALUE ZEROS.
         05 WS-ACCT-ID-N               PIC 9(11) VALUE 0.
         05 WS-CARD-NUM-N              PIC 9(16) VALUE 0.
         05 WS-TRAN-ID-N               PIC 9(16) VALUE ZEROS.
         05 WS-DATE-YYYYMMDD           PIC X(8).
         05 WS-DATE-FORMAT-8           PIC X(8) VALUE 'YYYYMMDD'.
         05 WS-MAX-TRAN-ID             PIC X(16).

      *----------------------------------------------------------------*
      *                     DB2 SQL COMMUNICATION AREA
      *----------------------------------------------------------------*
       EXEC SQL
           INCLUDE SQLCA
       END-EXEC.

       01 WS-RPC-RESP-CD               PIC S9(04) COMP VALUE ZEROS.
         88 RPC-RESP-OK                          VALUE 0.
         88 RPC-RESP-EMPTY-ACCTID                VALUE 1.
         88 RPC-RESP-INVALID-ACCTID              VALUE 2.
         88 RPC-RESP-ACCTID-NOTFOUND             VALUE 3.
         88 RPC-RESP-EMPTY-CARDNUM               VALUE 4.
         88 RPC-RESP-INVALID-CARDNUM             VALUE 5.
         88 RPC-RESP-CARDNUM-NOTFOUND            VALUE 6.
         88 RPC-RESP-EMPTY-TYPECD                VALUE 7.
         88 RPC-RESP-INVALID-TYPECD              VALUE 8.
         88 RPC-RESP-EMPTY-CATCD                 VALUE 9.
         88 RPC-RESP-INVALID-CATCD               VALUE 10.
         88 RPC-RESP-EMPTY-SOURCE                VALUE 11.
         88 RPC-RESP-EMPTY-DESC                  VALUE 12.
         88 RPC-RESP-EMPTY-AMOUNT                VALUE 13.
         88 RPC-RESP-INVALID-AMOUNT              VALUE 14.
         88 RPC-RESP-EMPTY-ORIGDT                VALUE 15.
         88 RPC-RESP-INVALID-ORIGDT              VALUE 16.
         88 RPC-RESP-EMPTY-PROCDT                VALUE 17.
         88 RPC-RESP-INVALID-PROCDT              VALUE 18.
         88 RPC-RESP-EMPTY-MERCHID               VALUE 19.
         88 RPC-RESP-INVALID-MERCHID             VALUE 20.
         88 RPC-RESP-EMPTY-MERCHNAME             VALUE 21.
         88 RPC-RESP-EMPTY-MERCHCITY             VALUE 22.
         88 RPC-RESP-EMPTY-MERCHZIP              VALUE 23.
         88 RPC-RESP-EMPTY-CONFIRM               VALUE 24.
         88 RPC-RESP-INVALID-CONFIRM             VALUE 25.
         88 RPC-RESP-WRITE-ERROR                 VALUE 26.

       01 CSUTLDTC-PARM.
          05 CSUTLDTC-DATE                   PIC X(10).
          05 CSUTLDTC-DATE-FORMAT            PIC X(10).
          05 CSUTLDTC-RESULT.
             10 CSUTLDTC-RESULT-SEV-CD       PIC X(04).
             10 FILLER                       PIC X(11).
             10 CSUTLDTC-RESULT-MSG-NUM      PIC X(04).
             10 CSUTLDTC-RESULT-MSG          PIC X(61).

       COPY CVTRA05Y.
       COPY CVACT01Y.
       COPY CVACT03Y.

      *----------------------------------------------------------------*
      *                        LINKAGE SECTION
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01 DFHCOMMAREA.
          05 LK-INPUT-PARMS.
             10 LK-IN-ACCT-ID           PIC X(11).
             10 LK-IN-CARD-NUM          PIC X(16).
             10 LK-IN-TRAN-TYPE-CD      PIC X(02).
             10 LK-IN-TRAN-CAT-CD       PIC X(04).
             10 LK-IN-TRAN-SOURCE       PIC X(10).
             10 LK-IN-TRAN-AMT          PIC X(12).
             10 LK-IN-TRAN-DESC         PIC X(100).
             10 LK-IN-TRAN-ORIG-DT      PIC X(10).
             10 LK-IN-TRAN-PROC-DT      PIC X(10).
             10 LK-IN-MERCH-ID          PIC X(09).
             10 LK-IN-MERCH-NAME        PIC X(50).
             10 LK-IN-MERCH-CITY        PIC X(50).
             10 LK-IN-MERCH-ZIP         PIC X(10).
             10 LK-IN-CONFIRM           PIC X(01).
          05 LK-OUTPUT-PARMS.
             10 LK-RESP-CODE            PIC S9(04) COMP.
             10 LK-RESP-MSG             PIC X(80).
             10 LK-OUT-TRAN-ID          PIC X(16).

      *----------------------------------------------------------------*
      *                       PROCEDURE DIVISION
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

       MAIN-PARA.

           MOVE ZEROS TO WS-RPC-RESP-CD
           MOVE SPACES TO WS-RESP-MSG
           MOVE SPACES TO LK-OUT-TRAN-ID

           PERFORM VALID-INPUT-KEY-FIELDS

           IF RPC-RESP-OK
               PERFORM VALID-INPUT-DATA-FIELDS
           END-IF

           IF RPC-RESP-OK
               PERFORM ADD-TRANSACTION

           END-IF

           MOVE WS-RPC-RESP-CD TO LK-RESP-CODE
           MOVE WS-RESP-MSG TO LK-RESP-MSG

           GOBACK.


      *----------------------------------------------------------------*
      *                      VALIDATE-INPUT-KEY-FIELDS
      *----------------------------------------------------------------*
       VALID-INPUT-KEY-FIELDS.

           EVALUATE TRUE
               WHEN LK-IN-ACCT-ID NOT = SPACES AND LOW-VALUES
                   IF LK-IN-ACCT-ID IS NOT NUMERIC
                       SET RPC-RESP-INVALID-ACCTID TO TRUE
                       MOVE 'Account ID must be Numeric...'
                         TO WS-RESP-MSG
                   ELSE
                       COMPUTE WS-ACCT-ID-N =
                         FUNCTION NUMVAL(LK-IN-ACCT-ID)
                       MOVE WS-ACCT-ID-N TO XREF-ACCT-ID
                       MOVE WS-ACCT-ID-N TO LK-IN-ACCT-ID
                       PERFORM READ-CXACAIX-FILE
                       IF RPC-RESP-OK
                           MOVE XREF-CARD-NUM TO LK-IN-CARD-NUM
                       END-IF
                   END-IF
               WHEN LK-IN-CARD-NUM NOT = SPACES AND LOW-VALUES
                   IF LK-IN-CARD-NUM IS NOT NUMERIC
                       SET RPC-RESP-INVALID-CARDNUM TO TRUE
                       MOVE 'Card Number must be Numeric...'
                         TO WS-RESP-MSG
                   ELSE
                       COMPUTE WS-CARD-NUM-N =
                         FUNCTION NUMVAL(LK-IN-CARD-NUM)
                       MOVE WS-CARD-NUM-N TO XREF-CARD-NUM
                       MOVE WS-CARD-NUM-N TO LK-IN-CARD-NUM
                       PERFORM READ-CCXREF-FILE
                       IF RPC-RESP-OK
                           MOVE XREF-ACCT-ID TO LK-IN-ACCT-ID
                       END-IF
                   END-IF
               WHEN OTHER
                   SET RPC-RESP-EMPTY-ACCTID TO TRUE
                   STRING 'Account or Card Number must be entered...'
                          DELIMITED BY SIZE
                     INTO WS-RESP-MSG
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      VALID-INPUT-DATA-FIELDS
      *----------------------------------------------------------------*
       VALID-INPUT-DATA-FIELDS.

           EVALUATE TRUE
      *        TYPE-CD validations
               WHEN LK-IN-TRAN-TYPE-CD = SPACES OR LOW-VALUES
                   SET RPC-RESP-EMPTY-TYPECD TO TRUE
                   MOVE 'Type CD can NOT be empty...' TO WS-RESP-MSG
               WHEN LK-IN-TRAN-TYPE-CD NOT NUMERIC
                   SET RPC-RESP-INVALID-TYPECD TO TRUE
                   MOVE 'Type CD must be Numeric...' TO WS-RESP-MSG

      *        CAT-CD validations
               WHEN LK-IN-TRAN-CAT-CD = SPACES OR LOW-VALUES
                   SET RPC-RESP-EMPTY-CATCD TO TRUE
                   MOVE 'Category CD can NOT be empty...' TO WS-RESP-MSG
               WHEN LK-IN-TRAN-CAT-CD NOT NUMERIC
                   SET RPC-RESP-INVALID-CATCD TO TRUE
                   MOVE 'Category CD must be Numeric...' TO WS-RESP-MSG

               WHEN LK-IN-TRAN-SOURCE = SPACES OR LOW-VALUES
                   SET RPC-RESP-EMPTY-SOURCE TO TRUE
                   MOVE 'Source can NOT be empty...' TO WS-RESP-MSG

               WHEN LK-IN-TRAN-DESC = SPACES OR LOW-VALUES
                   SET RPC-RESP-EMPTY-DESC TO TRUE
                   MOVE 'Description can NOT be empty...' TO WS-RESP-MSG

      *        AMOUNT validations
               WHEN LK-IN-TRAN-AMT = SPACES OR LOW-VALUES
                   SET RPC-RESP-EMPTY-AMOUNT TO TRUE
                   MOVE 'Amount can NOT be empty...' TO WS-RESP-MSG
               WHEN LK-IN-TRAN-AMT(1:1) NOT EQUAL '-' AND '+'
                   SET RPC-RESP-INVALID-AMOUNT TO TRUE
                   MOVE 'Amount should be in format -99999999.99'
                     TO WS-RESP-MSG
               WHEN LK-IN-TRAN-AMT(2:8) NOT NUMERIC
                   SET RPC-RESP-INVALID-AMOUNT TO TRUE
                   MOVE 'Amount should be in format -99999999.99'
                     TO WS-RESP-MSG
               WHEN LK-IN-TRAN-AMT(10:1) NOT = '.'
                   SET RPC-RESP-INVALID-AMOUNT TO TRUE
                   MOVE 'Amount should be in format -99999999.99'
                     TO WS-RESP-MSG
               WHEN LK-IN-TRAN-AMT(11:2) IS NOT NUMERIC
                   SET RPC-RESP-INVALID-AMOUNT TO TRUE
                   MOVE 'Amount should be in format -99999999.99'
                     TO WS-RESP-MSG

      *        ORIG DATE format validations
               WHEN LK-IN-TRAN-ORIG-DT = SPACES OR LOW-VALUES
                   SET RPC-RESP-EMPTY-ORIGDT TO TRUE
                   MOVE 'Orig Date can NOT be empty...' TO WS-RESP-MSG
               WHEN LK-IN-TRAN-ORIG-DT(1:4) IS NOT NUMERIC
                   SET RPC-RESP-INVALID-ORIGDT TO TRUE
                   MOVE 'Orig Date should be in format YYYY-MM-DD'
                     TO WS-RESP-MSG
               WHEN LK-IN-TRAN-ORIG-DT(5:1) NOT EQUAL '-'
                   SET RPC-RESP-INVALID-ORIGDT TO TRUE
                   MOVE 'Orig Date should be in format YYYY-MM-DD'
                     TO WS-RESP-MSG
               WHEN LK-IN-TRAN-ORIG-DT(6:2) NOT NUMERIC
                   SET RPC-RESP-INVALID-ORIGDT TO TRUE
                   MOVE 'Orig Date should be in format YYYY-MM-DD'
                     TO WS-RESP-MSG
               WHEN LK-IN-TRAN-ORIG-DT(8:1) NOT EQUAL '-'
                   SET RPC-RESP-INVALID-ORIGDT TO TRUE
                   MOVE 'Orig Date should be in format YYYY-MM-DD'
                     TO WS-RESP-MSG
               WHEN LK-IN-TRAN-ORIG-DT(9:2) NOT NUMERIC
                   SET RPC-RESP-INVALID-ORIGDT TO TRUE
                   MOVE 'Orig Date should be in format YYYY-MM-DD'
                     TO WS-RESP-MSG

      *        PROC DATE format validations
               WHEN LK-IN-TRAN-PROC-DT = SPACES OR LOW-VALUES
                   SET RPC-RESP-EMPTY-PROCDT TO TRUE
                   MOVE 'Proc Date can NOT be empty...' TO WS-RESP-MSG
               WHEN LK-IN-TRAN-PROC-DT(1:4) IS NOT NUMERIC
                   SET RPC-RESP-INVALID-PROCDT TO TRUE
                   MOVE 'Proc Date should be in format YYYY-MM-DD'
                     TO WS-RESP-MSG
               WHEN LK-IN-TRAN-PROC-DT(5:1) NOT EQUAL '-'
                   SET RPC-RESP-INVALID-PROCDT TO TRUE
                   MOVE 'Proc Date should be in format YYYY-MM-DD'
                     TO WS-RESP-MSG
               WHEN LK-IN-TRAN-PROC-DT(6:2) NOT NUMERIC
                   SET RPC-RESP-INVALID-PROCDT TO TRUE
                   MOVE 'Proc Date should be in format YYYY-MM-DD'
                     TO WS-RESP-MSG
               WHEN LK-IN-TRAN-PROC-DT(8:1) NOT EQUAL '-'
                   SET RPC-RESP-INVALID-PROCDT TO TRUE
                   MOVE 'Proc Date should be in format YYYY-MM-DD'
                     TO WS-RESP-MSG
               WHEN LK-IN-TRAN-PROC-DT(9:2) NOT NUMERIC
                   SET RPC-RESP-INVALID-PROCDT TO TRUE
                   MOVE 'Proc Date should be in format YYYY-MM-DD'
                     TO WS-RESP-MSG

      *        MERCHANT ID validations
               WHEN LK-IN-MERCH-ID = SPACES OR LOW-VALUES
                   SET RPC-RESP-EMPTY-MERCHID TO TRUE
                   MOVE 'Merchant ID can NOT be empty...'
                     TO WS-RESP-MSG
               WHEN LK-IN-MERCH-ID IS NOT NUMERIC
                   SET RPC-RESP-INVALID-MERCHID TO TRUE
                   MOVE 'Merchant ID must be Numeric...'
                     TO WS-RESP-MSG

               WHEN LK-IN-MERCH-NAME = SPACES OR LOW-VALUES
                   SET RPC-RESP-EMPTY-MERCHNAME TO TRUE
                   MOVE 'Merchant Name can NOT be empty...'
                     TO WS-RESP-MSG

               WHEN LK-IN-MERCH-CITY = SPACES OR LOW-VALUES
                   SET RPC-RESP-EMPTY-MERCHCITY TO TRUE
                   MOVE 'Merchant City can NOT be empty...'
                     TO WS-RESP-MSG

               WHEN LK-IN-MERCH-ZIP = SPACES OR LOW-VALUES
                   SET RPC-RESP-EMPTY-MERCHZIP TO TRUE
                   MOVE 'Merchant Zip can NOT be empty...'
                     TO WS-RESP-MSG

               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

      *    Now check date VALIDITY (not format) - still in order
           IF RPC-RESP-OK
              MOVE LK-IN-TRAN-ORIG-DT(1:4) TO WS-DATE-YYYYMMDD(1:4)
              MOVE LK-IN-TRAN-ORIG-DT(6:2) TO WS-DATE-YYYYMMDD(5:2)
              MOVE LK-IN-TRAN-ORIG-DT(9:2) TO WS-DATE-YYYYMMDD(7:2)
              MOVE WS-DATE-YYYYMMDD TO CSUTLDTC-DATE
              PERFORM CHECK-DATE-VALIDITY
              IF CSUTLDTC-RESULT-SEV-CD NOT = '0000'
                 IF CSUTLDTC-RESULT-MSG-NUM NOT = '2513'
                    DISPLAY 'CSUTLDTC-RESULT-MSG-NUM: '
                    CSUTLDTC-RESULT-MSG-NUM
                    SET RPC-RESP-INVALID-ORIGDT TO TRUE
                    MOVE 'Orig Date - Not a valid date...'
                       TO WS-RESP-MSG
                 END-IF
              END-IF
           END-IF.

           IF RPC-RESP-OK
              MOVE LK-IN-TRAN-PROC-DT(1:4) TO WS-DATE-YYYYMMDD(1:4)
              MOVE LK-IN-TRAN-PROC-DT(6:2) TO WS-DATE-YYYYMMDD(5:2)
              MOVE LK-IN-TRAN-PROC-DT(9:2) TO WS-DATE-YYYYMMDD(7:2)
              MOVE WS-DATE-YYYYMMDD TO CSUTLDTC-DATE
              PERFORM CHECK-DATE-VALIDITY
              IF CSUTLDTC-RESULT-SEV-CD NOT = '0000'
                 IF CSUTLDTC-RESULT-MSG-NUM NOT = '2513'
                    SET RPC-RESP-INVALID-PROCDT TO TRUE
                    MOVE 'Proc Date - Not a valid date...'
                       TO WS-RESP-MSG
                 END-IF
              END-IF
           END-IF.

      *    CONFIRM validation - last in the story
           IF RPC-RESP-OK
               EVALUATE LK-IN-CONFIRM
                   WHEN 'Y'
                   WHEN 'y'
                       CONTINUE
                   WHEN 'N'
                   WHEN 'n'
                   WHEN SPACES
                   WHEN LOW-VALUES
                       SET RPC-RESP-EMPTY-CONFIRM TO TRUE
                       MOVE 'Confirm to add this transaction...'
                         TO WS-RESP-MSG
                   WHEN OTHER
                       SET RPC-RESP-INVALID-CONFIRM TO TRUE
                       MOVE 'Invalid value. Valid values are (Y/N)...'
                         TO WS-RESP-MSG
               END-EVALUATE
           END-IF.
      *----------------------------------------------------------------*
      *                      CHECK-DATE-VALIDITY
      *----------------------------------------------------------------*
       CHECK-DATE-VALIDITY.
           MOVE WS-DATE-FORMAT-8 TO CSUTLDTC-DATE-FORMAT
           DISPLAY 'CSUTLDTC-DATE-FORMAT: ' CSUTLDTC-DATE-FORMAT
           DISPLAY 'CSUTLDTC-DATE: ' CSUTLDTC-DATE
           MOVE SPACES TO CSUTLDTC-RESULT
           CALL 'CSUTLDTL' USING CSUTLDTC-DATE
                                 CSUTLDTC-DATE-FORMAT
                                 CSUTLDTC-RESULT.

           DISPLAY 'CSUTLDTC-RESULT: ' CSUTLDTC-RESULT.

      *----------------------------------------------------------------*
      *                        ADD-TRANSACTION
      *----------------------------------------------------------------*
       ADD-TRANSACTION.

           EXEC SQL
               SELECT MAX(TRAN_ID)
               INTO :HOST-TRAN-ID
               FROM TRANSACT
           END-EXEC

           IF SQLCODE = 0
               MOVE HOST-TRAN-ID TO WS-TRAN-ID-N
               ADD 1 TO WS-TRAN-ID-N
           ELSE
               MOVE 1 TO WS-TRAN-ID-N
           END-IF

           MOVE WS-TRAN-ID-N       TO HOST-TRAN-ID
           MOVE LK-IN-TRAN-TYPE-CD TO HOST-TRAN-TYPE-CD
           MOVE LK-IN-TRAN-CAT-CD  TO HOST-TRAN-CAT-CD
           MOVE LK-IN-TRAN-SOURCE  TO HOST-TRAN-SOURCE
           MOVE LK-IN-TRAN-DESC    TO HOST-TRAN-DESC
           COMPUTE WS-TRAN-AMT-N =
             FUNCTION NUMVAL-C(LK-IN-TRAN-AMT)
           MOVE WS-TRAN-AMT-N      TO HOST-TRAN-AMT
           MOVE LK-IN-CARD-NUM     TO HOST-TRAN-CARD-NUM
           MOVE LK-IN-MERCH-ID     TO HOST-TRAN-MERCHANT-ID
           MOVE LK-IN-MERCH-NAME   TO HOST-TRAN-MERCHANT-NAME
           MOVE LK-IN-MERCH-CITY   TO HOST-TRAN-MERCHANT-CITY
           MOVE LK-IN-MERCH-ZIP    TO HOST-TRAN-MERCHANT-ZIP
           MOVE LK-IN-TRAN-ORIG-DT TO HOST-TRAN-ORIG-TS
           MOVE LK-IN-TRAN-PROC-DT TO HOST-TRAN-PROC-TS

           PERFORM INSERT-TRANSACT-DB2.

      *----------------------------------------------------------------*
      *                      READ-CXACAIX-FILE
      *----------------------------------------------------------------*
       READ-CXACAIX-FILE.

           MOVE XREF-ACCT-ID TO HOST-XREF-ACCT-ID

           EXEC SQL
               SELECT XREF_CARD_NUM
               INTO :HOST-XREF-CARD-NUM
               FROM CXACAIX
               WHERE XREF_ACCT_ID = :HOST-XREF-ACCT-ID
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   MOVE HOST-XREF-CARD-NUM TO XREF-CARD-NUM
               WHEN 100
                   SET RPC-RESP-ACCTID-NOTFOUND TO TRUE
                   MOVE 'Account ID NOT found...' TO WS-RESP-MSG
               WHEN OTHER
                   SET RPC-RESP-WRITE-ERROR TO TRUE
                   MOVE SQLCODE TO WS-SQLCODE-DISPLAY
                   STRING 'Unable to lookup Acct in XREF - SQLCODE: '
                          DELIMITED BY SIZE
                          WS-SQLCODE-DISPLAY
                          DELIMITED BY SIZE
                     INTO WS-RESP-MSG
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      READ-CCXREF-FILE
      *----------------------------------------------------------------*
       READ-CCXREF-FILE.

           MOVE XREF-CARD-NUM TO HOST-XREF-CARD-NUM

           EXEC SQL
               SELECT XREF_ACCT_ID
               INTO :HOST-XREF-ACCT-ID
               FROM CXACAIX
               WHERE XREF_CARD_NUM = :HOST-XREF-CARD-NUM
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   MOVE HOST-XREF-ACCT-ID TO XREF-ACCT-ID
               WHEN 100
                   SET RPC-RESP-CARDNUM-NOTFOUND TO TRUE
                   MOVE 'Card Number NOT found...' TO WS-RESP-MSG
               WHEN OTHER
                   SET RPC-RESP-WRITE-ERROR TO TRUE
                   MOVE SQLCODE TO WS-SQLCODE-DISPLAY
                   STRING 'Unable to lookup Card in XREF - SQLCODE: '
                          DELIMITED BY SIZE
                          WS-SQLCODE-DISPLAY
                          DELIMITED BY SIZE
                     INTO WS-RESP-MSG
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                    INSERT-TRANSACT-DB2
      *----------------------------------------------------------------*
       INSERT-TRANSACT-DB2.

           EXEC SQL
               INSERT INTO TRANSACT
               (TRAN_ID, TRAN_TYPE_CD, TRAN_CAT_CD, TRAN_SOURCE,
                TRAN_DESC, TRAN_AMT, TRAN_CARD_NUM, TRAN_MERCHANT_ID,
                TRAN_MERCHANT_NAME, TRAN_MERCHANT_CITY,
                TRAN_MERCHANT_ZIP, TRAN_ORIG_TS, TRAN_PROC_TS)
               VALUES
               (:HOST-TRAN-ID, :HOST-TRAN-TYPE-CD, :HOST-TRAN-CAT-CD,
                :HOST-TRAN-SOURCE, :HOST-TRAN-DESC, :HOST-TRAN-AMT,
                :HOST-TRAN-CARD-NUM, :HOST-TRAN-MERCHANT-ID,
                :HOST-TRAN-MERCHANT-NAME, :HOST-TRAN-MERCHANT-CITY,
                :HOST-TRAN-MERCHANT-ZIP, :HOST-TRAN-ORIG-TS,
                :HOST-TRAN-PROC-TS)
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   SET RPC-RESP-OK TO TRUE
                   EXEC SQL COMMIT END-EXEC
                   MOVE HOST-TRAN-ID TO LK-OUT-TRAN-ID
                   STRING 'Transaction added successfully. '
                          DELIMITED BY SIZE
                          ' Your Tran ID is '
                          DELIMITED BY SIZE
                          HOST-TRAN-ID
                          DELIMITED BY SPACE
                          '.'
                          DELIMITED BY SIZE
                     INTO WS-RESP-MSG
               WHEN -803
                   SET RPC-RESP-WRITE-ERROR TO TRUE
                   EXEC SQL ROLLBACK END-EXEC
                   MOVE 'Tran ID already exist...' TO WS-RESP-MSG
               WHEN OTHER
                   SET RPC-RESP-WRITE-ERROR TO TRUE
                   EXEC SQL ROLLBACK END-EXEC
                   MOVE SQLCODE TO WS-SQLCODE-DISPLAY
                   STRING 'Unable to Add Transaction - SQLCODE: '
                          DELIMITED BY SIZE
                          WS-SQLCODE-DISPLAY
                          DELIMITED BY SIZE
                     INTO WS-RESP-MSG
           END-EVALUATE.
