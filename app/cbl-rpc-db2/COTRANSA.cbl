******************************************************************
      * Program     : COTRANSA.CBL
      * Application : CardDemo
      * Type        : COBOL RPC Program
      * Function    : Retrieve transaction list for a given card
      * Description : Returns up to 45 transactions for a card number
      *               (DB2 Version - Simplified COMMAREA)
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     COTRANSA.
       AUTHOR.     AWS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
      *----------------------------------------------------------------*
      *                     WORKING STORAGE SECTION
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------*
      *                     DB2 SQL COMMUNICATION AREA
      *----------------------------------------------------------------*
       EXEC SQL
           INCLUDE SQLCA
       END-EXEC.

       01 WS-VARIABLES.
         05 WS-PGMNAME                 PIC X(08) VALUE 'COTRANSA'.
         05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.
           88 ERR-FLG-ON                         VALUE 'Y'.
           88 ERR-FLG-OFF                        VALUE 'N'.

      ******************************************************************
      * DB2 Related Variables
      ******************************************************************
         05 WS-DB2-VARS.
            07 WS-SQLCODE              PIC S9(09) COMP VALUE ZEROS.

      ******************************************************************
      *      DB2 Host Variables for Transaction Operations
      ******************************************************************
         05  WS-TRAN-HOST-VARS.
           10  HV-CARD-NUM             PIC X(16).
           10  HV-TRAN-ID              PIC X(16).
           10  HV-TRAN-TYPE-CD         PIC X(01).
           10  HV-TRAN-CAT-CD          PIC S9(04) COMP-3.
           10  HV-TRAN-SOURCE          PIC X(02).
           10  HV-TRAN-DESC            PIC X(26).
           10  HV-TRAN-AMT             PIC S9(09)V99 COMP-3.
           10  HV-TRAN-MERCHANT-ID     PIC S9(09) COMP-3.
           10  HV-TRAN-MERCHANT-NAME   PIC X(26).
           10  HV-TRAN-MERCHANT-CITY   PIC X(26).
           10  HV-TRAN-MERCHANT-ZIP    PIC X(10).
           10  HV-TRAN-CARD-NUM        PIC X(16).
           10  HV-TRAN-ORIG-TS         PIC X(26).
           10  HV-TRAN-PROC-TS         PIC X(26).

         05  WS-TRAN-COUNT             PIC S9(02) COMP VALUE ZEROS.
         05  WS-TRAN-INDEX             PIC S9(02) COMP VALUE ZEROS.

      ******************************************************************
      *      Error Message Handling
      ******************************************************************
         05  WS-DB2-ERROR-MESSAGE.
           10  FILLER                  PIC X(12) VALUE 'DB2 Error: '.
           10  ERROR-OPNAME            PIC X(8) VALUE SPACES.
           10  FILLER                  PIC X(4) VALUE ' on '.
           10  ERROR-TABLE             PIC X(9) VALUE SPACES.
           10  FILLER                  PIC X(9) VALUE ' SQLCODE '.
           10  ERROR-SQLCODE           PIC X(10) VALUE SPACES.
           10  FILLER                  PIC X(5) VALUE SPACES.

      ******************************************************************
      *      Literals and Constants
      ******************************************************************
       01 WS-LITERALS.
          05 LIT-TRANTABLENAME         PIC X(8) VALUE 'TRANSACT'.

      *----------------------------------------------------------------*
      *                        LINKAGE SECTION
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01  DFHCOMMAREA.
      *    Input: Just the card number
           05  LK-INPUT-CARD-NUM        PIC X(16).
      *
      *    Output: Return code and message
           05  LK-OUTPUT-STATUS.
               10  LK-OUT-RETURN-CODE   PIC 9(02).
                   88  RC-SUCCESS       VALUE 00.
                   88  RC-NOT-FOUND     VALUE 01.
                   88  RC-VALIDATION-ERROR VALUE 10.
                   88  RC-DATABASE-ERROR VALUE 99.
               10  LK-OUT-MESSAGE       PIC X(80).
      *
      *    Output: Transaction data - up to 45 transactions
           05  LK-OUTPUT-TRANS.
               10  LK-OUT-TRANS-COUNT   PIC S9(02) COMP.
               10  LK-OUT-TRANS-LIST OCCURS 45 TIMES.
                   15  LK-OUT-TRAN-ID   PIC X(16).
                   15  LK-OUT-TRAN-TYPE-CD PIC X(01).
                   15  LK-OUT-TRAN-CAT-CD PIC S9(04) COMP-3.
                   15  LK-OUT-TRAN-SOURCE PIC X(02).
                   15  LK-OUT-TRAN-DESC PIC X(26).
                   15  LK-OUT-TRAN-AMT  PIC S9(09)V99 COMP-3.
                   15  LK-OUT-TRAN-MERCHANT-ID PIC S9(09) COMP-3.
                   15  LK-OUT-TRAN-MERCHANT-NAME PIC X(26).
                   15  LK-OUT-TRAN-MERCHANT-CITY PIC X(26).
                   15  LK-OUT-TRAN-MERCHANT-ZIP PIC X(10).
                   15  LK-OUT-TRAN-CARD-NUM PIC X(16).
                   15  LK-OUT-TRAN-ORIG-TS PIC X(26).
                   15  LK-OUT-TRAN-PROC-TS PIC X(26).

      *----------------------------------------------------------------*
      *                       PROCEDURE DIVISION
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

       MAIN-PARA.
           INITIALIZE LK-OUTPUT-STATUS
                      LK-OUTPUT-TRANS

           SET ERR-FLG-OFF TO TRUE
           SET RC-SUCCESS TO TRUE
           MOVE SPACES TO LK-OUT-MESSAGE

      *    Validate input
           IF LK-INPUT-CARD-NUM = SPACES OR LOW-VALUES
               SET RC-VALIDATION-ERROR TO TRUE
               MOVE 'Card number cannot be empty'
                    TO LK-OUT-MESSAGE
               PERFORM TEMP-DUMP-OUTPUT-STRUCTURE
           ELSE
      *        Get transactions for card
               PERFORM GET-CARD-TRANSACTIONS
           END-IF

           GOBACK.

      *----------------------------------------------------------------*
      *                      GET-CARD-TRANSACTIONS
      *----------------------------------------------------------------*
       GET-CARD-TRANSACTIONS.
      *    Get transactions for card directly from TRANSACT table
           MOVE LK-INPUT-CARD-NUM TO HV-CARD-NUM
           INITIALIZE WS-TRAN-COUNT
           PERFORM READ-TRANSACTIONS-FOR-CARD.

      *----------------------------------------------------------------*
      *                      READ-TRANSACTIONS-FOR-CARD
      *----------------------------------------------------------------*
       READ-TRANSACTIONS-FOR-CARD.
      *    Read transactions for the specified card
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
                FROM   TRANSACT
                WHERE  TRAN_CARD_NUM = :HV-CARD-NUM
                ORDER BY TRAN_PROC_TS DESC
                FETCH FIRST 45 ROWS ONLY
           END-EXEC

           EXEC SQL
                OPEN TRAN_CURSOR
           END-EXEC

           IF SQLCODE NOT = 0
               SET ERR-FLG-ON TO TRUE
               SET RC-DATABASE-ERROR TO TRUE
               MOVE 'OPEN'                TO ERROR-OPNAME
               MOVE LIT-TRANTABLENAME     TO ERROR-TABLE
               MOVE SQLCODE               TO ERROR-SQLCODE
               MOVE WS-DB2-ERROR-MESSAGE  TO LK-OUT-MESSAGE
               PERFORM TEMP-DUMP-OUTPUT-STRUCTURE
               GO TO READ-TRANS-FOR-CARD-EXIT
           END-IF

           PERFORM FETCH-TRANSACTION-LOOP
                   UNTIL SQLCODE = +100 OR WS-TRAN-COUNT >= 45

           EXEC SQL
                CLOSE TRAN_CURSOR
           END-EXEC

           IF WS-TRAN-COUNT = 0
               SET ERR-FLG-ON TO TRUE
               SET RC-NOT-FOUND TO TRUE
               MOVE 'No transactions found for this card'
                    TO LK-OUT-MESSAGE
           ELSE
               MOVE 'Card transactions retrieved successfully'
                    TO LK-OUT-MESSAGE
           END-IF

           MOVE WS-TRAN-COUNT TO LK-OUT-TRANS-COUNT
           PERFORM TEMP-DUMP-OUTPUT-STRUCTURE.

       READ-TRANS-FOR-CARD-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *                      FETCH-TRANSACTION-LOOP
      *----------------------------------------------------------------*
       FETCH-TRANSACTION-LOOP.
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

           EVALUATE SQLCODE
               WHEN 0
                   ADD 1 TO WS-TRAN-COUNT
                   IF WS-TRAN-COUNT <= 45
                       MOVE HV-TRAN-ID TO
                            LK-OUT-TRAN-ID(WS-TRAN-COUNT)
                       MOVE HV-TRAN-TYPE-CD TO
                            LK-OUT-TRAN-TYPE-CD(WS-TRAN-COUNT)
                       MOVE HV-TRAN-CAT-CD TO
                            LK-OUT-TRAN-CAT-CD(WS-TRAN-COUNT)
                       MOVE HV-TRAN-SOURCE TO
                            LK-OUT-TRAN-SOURCE(WS-TRAN-COUNT)
                       MOVE HV-TRAN-DESC TO
                            LK-OUT-TRAN-DESC(WS-TRAN-COUNT)
                       MOVE HV-TRAN-AMT TO
                            LK-OUT-TRAN-AMT(WS-TRAN-COUNT)
                       MOVE HV-TRAN-MERCHANT-ID TO
                            LK-OUT-TRAN-MERCHANT-ID(WS-TRAN-COUNT)
                       MOVE HV-TRAN-MERCHANT-NAME TO
                            LK-OUT-TRAN-MERCHANT-NAME(WS-TRAN-COUNT)
                       MOVE HV-TRAN-MERCHANT-CITY TO
                            LK-OUT-TRAN-MERCHANT-CITY(WS-TRAN-COUNT)
                       MOVE HV-TRAN-MERCHANT-ZIP TO
                            LK-OUT-TRAN-MERCHANT-ZIP(WS-TRAN-COUNT)
                       MOVE HV-TRAN-CARD-NUM TO
                            LK-OUT-TRAN-CARD-NUM(WS-TRAN-COUNT)
                       MOVE HV-TRAN-ORIG-TS TO
                            LK-OUT-TRAN-ORIG-TS(WS-TRAN-COUNT)
                       MOVE HV-TRAN-PROC-TS TO
                            LK-OUT-TRAN-PROC-TS(WS-TRAN-COUNT)
                   END-IF
               WHEN +100
                   CONTINUE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   SET RC-DATABASE-ERROR TO TRUE
                   MOVE 'FETCH'               TO ERROR-OPNAME
                   MOVE LIT-TRANTABLENAME     TO ERROR-TABLE
                   MOVE SQLCODE               TO ERROR-SQLCODE
                   MOVE WS-DB2-ERROR-MESSAGE  TO LK-OUT-MESSAGE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                    TEMP-DUMP-OUTPUT-STRUCTURE
      *----------------------------------------------------------------*
       TEMP-DUMP-OUTPUT-STRUCTURE.
      * TEMP: Remove this paragraph after testing is complete
           DISPLAY 'COTRANSA: === TEMP OUTPUT DUMP ==='
           DISPLAY 'COTRANSA: TRANS-COUNT: ' LK-OUT-TRANS-COUNT
           PERFORM VARYING WS-TRAN-INDEX FROM 1 BY 1
                   UNTIL WS-TRAN-INDEX > LK-OUT-TRANS-COUNT
                   OR WS-TRAN-INDEX > 45
               DISPLAY 'COTRANSA: TRAN[' WS-TRAN-INDEX '] ID: '
                       LK-OUT-TRAN-ID(WS-TRAN-INDEX)
               DISPLAY 'COTRANSA: TRAN[' WS-TRAN-INDEX '] TYPE: '
                       LK-OUT-TRAN-TYPE-CD(WS-TRAN-INDEX)
               DISPLAY 'COTRANSA: TRAN[' WS-TRAN-INDEX '] AMT: '
                       LK-OUT-TRAN-AMT(WS-TRAN-INDEX)
               DISPLAY 'COTRANSA: TRAN[' WS-TRAN-INDEX '] DESC: '
                       LK-OUT-TRAN-DESC(WS-TRAN-INDEX)
               DISPLAY 'COTRANSA: TRAN[' WS-TRAN-INDEX '] MERCHANT: '
                       LK-OUT-TRAN-MERCHANT-NAME(WS-TRAN-INDEX)
               DISPLAY 'COTRANSA: TRAN[' WS-TRAN-INDEX '] PROC-TS: '
                       LK-OUT-TRAN-PROC-TS(WS-TRAN-INDEX)
           END-PERFORM
           DISPLAY 'COTRANSA: MESSAGE: ' LK-OUT-MESSAGE
           DISPLAY 'COTRANSA: === END TEMP DUMP ==='.

      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:12:33 CDT
      *