******************************************************************
      * Program     : COCCARDA.CBL
      * Application : CardDemo
      * Type        : COBOL RPC Program
      * Function    : Card lookup that chains to COTRANSA
      * Description : Retrieves cards for an account then calls
      *               COTRANSA for each card
      *               (DB2 Version - Simplified COMMAREA)
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     COCCARDA.
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
         05 WS-PGMNAME                 PIC X(08) VALUE 'COCCARDA'.
         05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.
           88 ERR-FLG-ON                         VALUE 'Y'.
           88 ERR-FLG-OFF                        VALUE 'N'.
         05 WS-CICS-RESP               PIC S9(08) COMP VALUE ZEROS.
         05 WS-CICS-REAS               PIC S9(08) COMP VALUE ZEROS.



      ******************************************************************
      * DB2 Related Variables
      ******************************************************************
         05 WS-DB2-VARS.
            07 WS-SQLCODE              PIC S9(09) COMP VALUE ZEROS.

      ******************************************************************
      *      DB2 Host Variables for Card Operations
      ******************************************************************
         05  WS-CARD-HOST-VARS.
           10  HV-ACCT-ID              PIC S9(11) COMP-3.
           10  HV-CARD-NUM             PIC X(16).
           10  HV-CARD-ACCT-ID         PIC S9(11) COMP-3.
           10  HV-CARD-ACTIVE-STATUS   PIC X(01).
           10  HV-CARD-CVV-CD          PIC S9(03) COMP-3.
           10  HV-CARD-EMBOSSED-NAME   PIC X(50).
           10  HV-CARD-EXPIRY-DATE     PIC X(10).
           10  HV-CARD-CREAT-DATE      PIC X(10).
           10  HV-CARD-CREAT-TIME      PIC X(8).
           10  HV-CARD-CREAT-USER      PIC X(10).
           10  HV-CARD-UPDATE-DATE     PIC X(10).
           10  HV-CARD-UPDATE_TIME     PIC X(8).
           10  HV-CARD-UPDATE-USER     PIC X(10).

         05  WS-CARD-COUNT             PIC S9(02) COMP VALUE ZEROS.
         05  WS-CARD-INDEX             PIC S9(02) COMP VALUE ZEROS.

      ******************************************************************
      *      COMMAREA for COTRANSA calls
      ******************************************************************
         05  WS-TRANSA-COMMAREA.
           10  WS-TRANSA-INPUT-CARD-NUM  PIC X(16).
           10  WS-TRANSA-OUTPUT-STATUS.
               15  WS-TRANSA-RETURN-CODE PIC 9(02).
               15  WS-TRANSA-MESSAGE     PIC X(80).
           10  WS-TRANSA-OUTPUT-TRANS.
               15  WS-TRANSA-TRANS-COUNT PIC S9(02) COMP.
               15  WS-TRANSA-TRANS-LIST OCCURS 45 TIMES.
                   20  WS-TRANSA-TRAN-ID PIC X(16).
                   20  WS-TRANSA-TRAN-TYPE-CD PIC X(01).
                   20  WS-TRANSA-TRAN-CAT-CD PIC S9(04) COMP-3.
                   20  WS-TRANSA-TRAN-SOURCE PIC X(02).
                   20  WS-TRANSA-TRAN-DESC PIC X(26).
                   20  WS-TRANSA-TRAN-AMT PIC S9(09)V99 COMP-3.
                   20  WS-TRANSA-TRAN-MERCHANT-ID PIC S9(09) COMP-3.
                   20  WS-TRANSA-TRAN-MERCHANT-NAME PIC X(26).
                   20  WS-TRANSA-TRAN-MERCHANT-CITY PIC X(26).
                   20  WS-TRANSA-TRAN-MERCHANT-ZIP PIC X(10).
                   20  WS-TRANSA-TRAN-CARD-NUM PIC X(16).
                   20  WS-TRANSA-TRAN-ORIG-TS PIC X(26).
                   20  WS-TRANSA-TRAN-PROC-TS PIC X(26).

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
          05 LIT-CARDTABLENAME         PIC X(8) VALUE 'CARDDAT '.

      *----------------------------------------------------------------*
      *                        LINKAGE SECTION
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01  DFHCOMMAREA.
      *    Input: Just the account ID
           05  LK-INPUT-ACCT-ID         PIC S9(11) COMP-3.
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
      *    Output: Card data - up to 10 cards
           05  LK-OUTPUT-CARDS.
               10  LK-OUT-CARD-COUNT    PIC S9(02) COMP.
               10  LK-OUT-CARD-LIST OCCURS 10 TIMES.
                   15  LK-OUT-CARD-NUM  PIC X(16).
                   15  LK-OUT-CARD-ACCT-ID PIC S9(11) COMP-3.
                   15  LK-OUT-CARD-STATUS PIC X(01).
                   15  LK-OUT-CARD-CVV-CD PIC S9(03) COMP-3.
                   15  LK-OUT-CARD-NAME PIC X(50).
                   15  LK-OUT-CARD-EXPIRY-DATE PIC X(10).
                   15  LK-OUT-CARD-CREAT-DATE PIC X(10).
                   15  LK-OUT-CARD-CREAT-USER PIC X(10).

      *----------------------------------------------------------------*
      *                       PROCEDURE DIVISION
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

       MAIN-PARA.
           INITIALIZE LK-OUTPUT-STATUS
                      LK-OUTPUT-CARDS

           SET ERR-FLG-OFF TO TRUE
           SET RC-SUCCESS TO TRUE
           MOVE SPACES TO LK-OUT-MESSAGE

      *    Validate input
           IF LK-INPUT-ACCT-ID = ZEROS
               SET RC-VALIDATION-ERROR TO TRUE
               MOVE 'Account ID cannot be zero'
                    TO LK-OUT-MESSAGE
               PERFORM TEMP-DUMP-OUTPUT-STRUCTURE
           ELSE
      *        Get cards for account
               PERFORM GET-ACCOUNT-CARDS
               IF RC-SUCCESS
      *            Chain to COTRANSA for each card
                   PERFORM CALL-COTRANSA-FOR-CARDS
               END-IF
           END-IF

           GOBACK.

      *----------------------------------------------------------------*
      *                      GET-ACCOUNT-CARDS
      *----------------------------------------------------------------*
       GET-ACCOUNT-CARDS.
      *    Get cards for account via INNER JOIN with CXACAIX
           MOVE LK-INPUT-ACCT-ID TO HV-ACCT-ID
           INITIALIZE WS-CARD-COUNT
           PERFORM READ-CARDS-FOR-ACCOUNT.

      *----------------------------------------------------------------*
      *                      READ-CARDS-FOR-ACCOUNT
      *----------------------------------------------------------------*
       READ-CARDS-FOR-ACCOUNT.
      *    Read cards for the specified account
           EXEC SQL
                DECLARE CARD_CURSOR CURSOR FOR
                SELECT C.CARD_NUM,
                       C.CARD_ACCT_ID,
                       C.CARD_ACTIVE_STATUS,
                       C.CARD_CVV_CD,
                       C.CARD_EMBOSSED_NAME,
                       C.CARD_EXPIRY_DATE,
                       C.CARD_CREAT_DATE,
                       C.CARD_CREAT_USER
                FROM   CARDDAT C
                INNER JOIN CXACAIX X ON C.CARD_NUM = X.XREF_CARD_NUM
                WHERE  X.XREF_ACCT_ID = :HV-ACCT-ID
                ORDER BY C.CARD_NUM
                FETCH FIRST 10 ROWS ONLY
           END-EXEC

           EXEC SQL
                OPEN CARD_CURSOR
           END-EXEC

           IF SQLCODE NOT = 0
               SET ERR-FLG-ON TO TRUE
               SET RC-DATABASE-ERROR TO TRUE
               MOVE 'OPEN'                TO ERROR-OPNAME
               MOVE LIT-CARDTABLENAME     TO ERROR-TABLE
               MOVE SQLCODE               TO ERROR-SQLCODE
               MOVE WS-DB2-ERROR-MESSAGE  TO LK-OUT-MESSAGE
               PERFORM TEMP-DUMP-OUTPUT-STRUCTURE
               GO TO READ-CARDS-FOR-ACCT-EXIT
           END-IF

           PERFORM FETCH-CARD-LOOP
                   UNTIL SQLCODE = +100 OR WS-CARD-COUNT >= 10

           EXEC SQL
                CLOSE CARD_CURSOR
           END-EXEC

           IF WS-CARD-COUNT = 0
               SET ERR-FLG-ON TO TRUE
               SET RC-NOT-FOUND TO TRUE
               MOVE 'No cards found for this account'
                    TO LK-OUT-MESSAGE
           ELSE
               MOVE 'Account cards retrieved successfully'
                    TO LK-OUT-MESSAGE
           END-IF

           MOVE WS-CARD-COUNT TO LK-OUT-CARD-COUNT
           PERFORM TEMP-DUMP-OUTPUT-STRUCTURE.

       READ-CARDS-FOR-ACCT-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *                      FETCH-CARD-LOOP
      *----------------------------------------------------------------*
       FETCH-CARD-LOOP.
           EXEC SQL
                FETCH CARD_CURSOR
                INTO :HV-CARD-NUM,
                     :HV-CARD-ACCT-ID,
                     :HV-CARD-ACTIVE-STATUS,
                     :HV-CARD-CVV-CD,
                     :HV-CARD-EMBOSSED-NAME,
                     :HV-CARD-EXPIRY-DATE,
                     :HV-CARD-CREAT-DATE,
                     :HV-CARD-CREAT-USER
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   ADD 1 TO WS-CARD-COUNT
                   IF WS-CARD-COUNT <= 10
                       MOVE HV-CARD-NUM TO
                            LK-OUT-CARD-NUM(WS-CARD-COUNT)
                       MOVE HV-CARD-ACCT-ID TO
                            LK-OUT-CARD-ACCT-ID(WS-CARD-COUNT)
                       MOVE HV-CARD-ACTIVE-STATUS TO
                            LK-OUT-CARD-STATUS(WS-CARD-COUNT)
                       MOVE HV-CARD-CVV-CD TO
                            LK-OUT-CARD-CVV-CD(WS-CARD-COUNT)
                       MOVE HV-CARD-EMBOSSED-NAME TO
                            LK-OUT-CARD-NAME(WS-CARD-COUNT)
                       MOVE HV-CARD-EXPIRY-DATE TO
                            LK-OUT-CARD-EXPIRY-DATE(WS-CARD-COUNT)
                       MOVE HV-CARD-CREAT-DATE TO
                            LK-OUT-CARD-CREAT-DATE(WS-CARD-COUNT)
                       MOVE HV-CARD-CREAT-USER TO
                            LK-OUT-CARD-CREAT-USER(WS-CARD-COUNT)
                   END-IF
               WHEN +100
                   CONTINUE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   SET RC-DATABASE-ERROR TO TRUE
                   MOVE 'FETCH'               TO ERROR-OPNAME
                   MOVE LIT-CARDTABLENAME     TO ERROR-TABLE
                   MOVE SQLCODE               TO ERROR-SQLCODE
                   MOVE WS-DB2-ERROR-MESSAGE  TO LK-OUT-MESSAGE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      CALL-COTRANSA-FOR-CARDS
      *----------------------------------------------------------------*
       CALL-COTRANSA-FOR-CARDS.
           PERFORM VARYING WS-CARD-INDEX FROM 1 BY 1
               UNTIL WS-CARD-INDEX > LK-OUT-CARD-COUNT
                  OR WS-CARD-INDEX > 10
               DISPLAY 'COCCARDA: Calling COTRANSA for CARD-NUM: '
                       LK-OUT-CARD-NUM (WS-CARD-INDEX)
      *        Initialize COTRANSA COMMAREA
               INITIALIZE WS-TRANSA-COMMAREA
               MOVE LK-OUT-CARD-NUM (WS-CARD-INDEX)
                    TO WS-TRANSA-INPUT-CARD-NUM
      *        Call COTRANSA with proper COMMAREA
               EXEC CICS LINK
                    PROGRAM  ('COTRANSA')
                    COMMAREA (WS-TRANSA-COMMAREA)
                    LENGTH   (LENGTH OF WS-TRANSA-COMMAREA)
                    RESP     (WS-CICS-RESP)
                    RESP2    (WS-CICS-REAS)
               END-EXEC
           END-PERFORM.

      *----------------------------------------------------------------*
      *                    TEMP-DUMP-OUTPUT-STRUCTURE
      *----------------------------------------------------------------*
       TEMP-DUMP-OUTPUT-STRUCTURE.
      * TEMP: Remove this paragraph after testing is complete
           DISPLAY 'COCCARDA: === TEMP OUTPUT DUMP ==='
           DISPLAY 'COCCARDA: CARD-COUNT: ' LK-OUT-CARD-COUNT
           PERFORM VARYING WS-CARD-INDEX FROM 1 BY 1
                   UNTIL WS-CARD-INDEX > LK-OUT-CARD-COUNT
                   OR WS-CARD-INDEX > 10
               DISPLAY 'COCCARDA: CARD[' WS-CARD-INDEX '] NUM: '
                       LK-OUT-CARD-NUM(WS-CARD-INDEX)
               DISPLAY 'COCCARDA: CARD[' WS-CARD-INDEX '] ACCT: '
                       LK-OUT-CARD-ACCT-ID(WS-CARD-INDEX)
               DISPLAY 'COCCARDA: CARD[' WS-CARD-INDEX '] STATUS: '
                       LK-OUT-CARD-STATUS(WS-CARD-INDEX)
               DISPLAY 'COCCARDA: CARD[' WS-CARD-INDEX '] CVV: '
                       LK-OUT-CARD-CVV-CD(WS-CARD-INDEX)
               DISPLAY 'COCCARDA: CARD[' WS-CARD-INDEX '] NAME: '
                       LK-OUT-CARD-NAME(WS-CARD-INDEX)
               DISPLAY 'COCCARDA: CARD[' WS-CARD-INDEX '] EXPIRY: '
                       LK-OUT-CARD-EXPIRY-DATE(WS-CARD-INDEX)
               DISPLAY 'COCCARDA: CARD[' WS-CARD-INDEX '] CREAT-DATE: '
                       LK-OUT-CARD-CREAT-DATE(WS-CARD-INDEX)
               DISPLAY 'COCCARDA: CARD[' WS-CARD-INDEX '] CREAT-USER: '
                       LK-OUT-CARD-CREAT-USER(WS-CARD-INDEX)
           END-PERFORM
           DISPLAY 'COCCARDA: MESSAGE: ' LK-OUT-MESSAGE
           DISPLAY 'COCCARDA: === END TEMP DUMP ==='.


      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:12:33 CDT
      *