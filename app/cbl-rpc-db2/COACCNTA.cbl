******************************************************************
      * Program     : COACCNTA.CBL
      * Application : CardDemo
      * Type        : COBOL RPC Program
      * Function    : Account lookup that chains to COCCARDA
      * Description : Retrieves accounts for a customer then calls
      *               COCCARDA for each account
      *               (DB2 Version - Simplified COMMAREA)
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     COACCNTA.
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
         05 WS-PGMNAME                 PIC X(08) VALUE 'COACCNTA'.
         05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.
           88 ERR-FLG-ON                         VALUE 'Y'.
           88 ERR-FLG-OFF                        VALUE 'N'.

      ******************************************************************
      * DB2 Related Variables
      ******************************************************************
         05 WS-DB2-VARS.
            07 WS-SQLCODE              PIC S9(09) COMP VALUE ZEROS.

      ******************************************************************
      *      DB2 Host Variables for Account Operations
      ******************************************************************
         05  WS-ACCT-HOST-VARS.
           10  HV-CUST-ID              PIC S9(09) COMP-3.
           10  HV-ACCT-ID              PIC S9(11) COMP-3.
           10  HV-ACCT-ACTIVE-STATUS   PIC X(01).
           10  HV-ACCT-CURR-BAL        PIC S9(10)V99 COMP-3.
           10  HV-ACCT-CREDIT-LIMIT    PIC S9(10)V99 COMP-3.
           10  HV-ACCT-CASH-LIMIT      PIC S9(10)V99 COMP-3.
           10  HV-ACCT-OPEN-DATE       PIC X(10).
           10  HV-ACCT-EXPIRY-DATE     PIC X(10).
           10  HV-ACCT-REISSUE-DATE    PIC X(10).
           10  HV-ACCT-CURR-CYC-CREDIT PIC S9(10)V99 COMP-3.
           10  HV-ACCT-CURR-CYC-DEBIT  PIC S9(10)V99 COMP-3.
           10  HV-ACCT-ADDR-ZIP        PIC X(10).
           10  HV-ACCT-GROUP-ID        PIC X(10).

         05  WS-ACCT-COUNT             PIC S9(02) COMP VALUE ZEROS.
         05  WS-ACCT-INDEX             PIC S9(02) COMP VALUE ZEROS.

      ******************************************************************
      *      COMMAREA for COCCARDA calls
      ******************************************************************
         05  WS-CCARDA-COMMAREA.
           10  WS-CCARDA-INPUT-ACCT-ID   PIC S9(11) COMP-3.
           10  WS-CCARDA-OUTPUT-STATUS.
               15  WS-CCARDA-RETURN-CODE PIC 9(02).
               15  WS-CCARDA-MESSAGE     PIC X(80).
           10  WS-CCARDA-OUTPUT-CARDS.
               15  WS-CCARDA-CARD-COUNT  PIC S9(02) COMP.
               15  WS-CCARDA-CARD-LIST OCCURS 10 TIMES.
                   20  WS-CCARDA-CARD-NUM  PIC X(16).
                   20  WS-CCARDA-CARD-ACCT-ID PIC S9(11) COMP-3.
                   20  WS-CCARDA-CARD-STATUS PIC X(01).
                   20  WS-CCARDA-CARD-CVV-CD PIC S9(03) COMP-3.
                   20  WS-CCARDA-CARD-NAME PIC X(50).
                   20  WS-CCARDA-CARD-EXPIRY-DATE PIC X(10).
                   20  WS-CCARDA-CARD-CREAT-DATE PIC X(10).
                   20  WS-CCARDA-CARD-CREAT-USER PIC X(10).

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
          05 LIT-ACCTTABLENAME         PIC X(8) VALUE 'ACCTDAT '.

      *----------------------------------------------------------------*
      *                        LINKAGE SECTION
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01  DFHCOMMAREA.
      *    Input: Just the customer ID
           05  LK-INPUT-CUST-ID         PIC S9(09) COMP-3.
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
      *    Output: Account data - up to 10 accounts
           05  LK-OUTPUT-ACCOUNTS.
               10  LK-OUT-ACCT-COUNT    PIC S9(02) COMP.
               10  LK-OUT-ACCT-LIST OCCURS 10 TIMES.
                   15  LK-OUT-ACCT-ID       PIC S9(11) COMP-3.
                   15  LK-OUT-ACCT-STATUS   PIC X(01).
                   15  LK-OUT-CURR-BAL      PIC S9(10)V99 COMP-3.
                   15  LK-OUT-CREDIT-LIMIT  PIC S9(10)V99 COMP-3.
                   15  LK-OUT-CASH-LIMIT    PIC S9(10)V99 COMP-3.
                   15  LK-OUT-OPEN-DATE     PIC X(10).
                   15  LK-OUT-EXPIRY-DATE   PIC X(10).
                   15  LK-OUT-REISSUE-DATE  PIC X(10).
                   15  LK-OUT-CURR-CYC-CREDIT PIC S9(10)V99 COMP-3.
                   15  LK-OUT-CURR-CYC-DEBIT PIC S9(10)V99 COMP-3.
                   15  LK-OUT-ADDR-ZIP      PIC X(10).
                   15  LK-OUT-GROUP-ID      PIC X(10).

      *----------------------------------------------------------------*
      *                       PROCEDURE DIVISION
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

       MAIN-PARA.
           INITIALIZE LK-OUTPUT-STATUS
                      LK-OUTPUT-ACCOUNTS

           SET ERR-FLG-OFF TO TRUE
           SET RC-SUCCESS TO TRUE
           MOVE SPACES TO LK-OUT-MESSAGE
      *    Validate input
           IF LK-INPUT-CUST-ID = ZEROS
               SET RC-VALIDATION-ERROR TO TRUE
               MOVE 'Customer ID cannot be zero'
                    TO LK-OUT-MESSAGE
               PERFORM TEMP-DUMP-OUTPUT-STRUCTURE
           ELSE
      *        Get accounts for customer
               PERFORM GET-CUSTOMER-ACCOUNTS
               IF RC-SUCCESS
      *            Chain to COCCARDA for each account
                   PERFORM CALL-COCCARDA-FOR-ACCOUNTS
               END-IF
           END-IF

           GOBACK.

      *----------------------------------------------------------------*
      *                      GET-CUSTOMER-ACCOUNTS
      *----------------------------------------------------------------*
       GET-CUSTOMER-ACCOUNTS.
      *    Get accounts for customer via INNER JOIN
           MOVE LK-INPUT-CUST-ID TO HV-CUST-ID
           INITIALIZE WS-ACCT-COUNT
           PERFORM READ-ACCOUNTS-FOR-CUSTOMER.

      *----------------------------------------------------------------*
      *                      READ-ACCOUNTS-FOR-CUSTOMER
      *----------------------------------------------------------------*
       READ-ACCOUNTS-FOR-CUSTOMER.
      *    Read accounts for the specified customer
           EXEC SQL
                DECLARE ACCT_CURSOR CURSOR FOR
                SELECT A.ACCT_ID,
                       A.ACCT_ACTIVE_STATUS,
                       A.ACCT_CURR_BAL,
                       A.ACCT_CREDIT_LIMIT,
                       A.ACCT_CASH_CREDIT_LIMIT,
                       A.ACCT_OPEN_DATE,
                       A.ACCT_EXPIRAION_DATE,
                       A.ACCT_REISSUE_DATE,
                       A.ACCT_CURR_CYC_CREDIT,
                       A.ACCT_CURR_CYC_DEBIT,
                       A.ACCT_ADDR_ZIP,
                       A.ACCT_GROUP_ID
                FROM   ACCTDAT A
                INNER JOIN CXACAIX X ON A.ACCT_ID = X.XREF_ACCT_ID
                WHERE  X.XREF_CUST_ID = :HV-CUST-ID
                ORDER BY A.ACCT_ID
                FETCH FIRST 10 ROWS ONLY
           END-EXEC

           EXEC SQL
                OPEN ACCT_CURSOR
           END-EXEC

           IF SQLCODE NOT = 0
               SET ERR-FLG-ON TO TRUE
               SET RC-DATABASE-ERROR TO TRUE
               MOVE 'OPEN'                TO ERROR-OPNAME
               MOVE 'ACCTDAT'             TO ERROR-TABLE
               MOVE SQLCODE               TO ERROR-SQLCODE
               MOVE WS-DB2-ERROR-MESSAGE  TO LK-OUT-MESSAGE
               PERFORM TEMP-DUMP-OUTPUT-STRUCTURE
               GO TO READ-ACCTS-FOR-CUST-EXIT
           END-IF

           PERFORM FETCH-ACCOUNT-LOOP
                   UNTIL SQLCODE = +100 OR WS-ACCT-COUNT >= 10

           EXEC SQL
                CLOSE ACCT_CURSOR
           END-EXEC

           IF WS-ACCT-COUNT = 0
               SET ERR-FLG-ON TO TRUE
               SET RC-NOT-FOUND TO TRUE
               MOVE 'No accounts found for this customer'
                    TO LK-OUT-MESSAGE
           ELSE
               MOVE 'Customer accounts retrieved successfully'
                    TO LK-OUT-MESSAGE
           END-IF

           MOVE WS-ACCT-COUNT TO LK-OUT-ACCT-COUNT
           PERFORM TEMP-DUMP-OUTPUT-STRUCTURE.

       READ-ACCTS-FOR-CUST-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *                      FETCH-ACCOUNT-LOOP
      *----------------------------------------------------------------*
       FETCH-ACCOUNT-LOOP.
           EXEC SQL
                FETCH ACCT_CURSOR
                INTO :HV-ACCT-ID,
                     :HV-ACCT-ACTIVE-STATUS,
                     :HV-ACCT-CURR-BAL,
                     :HV-ACCT-CREDIT-LIMIT,
                     :HV-ACCT-CASH-LIMIT,
                     :HV-ACCT-OPEN-DATE,
                     :HV-ACCT-EXPIRY-DATE,
                     :HV-ACCT-REISSUE-DATE,
                     :HV-ACCT-CURR-CYC-CREDIT,
                     :HV-ACCT-CURR-CYC-DEBIT,
                     :HV-ACCT-ADDR-ZIP,
                     :HV-ACCT-GROUP-ID
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   ADD 1 TO WS-ACCT-COUNT
                   IF WS-ACCT-COUNT <= 10
                       MOVE HV-ACCT-ID TO
                            LK-OUT-ACCT-ID(WS-ACCT-COUNT)
                       MOVE HV-ACCT-ACTIVE-STATUS TO
                            LK-OUT-ACCT-STATUS(WS-ACCT-COUNT)
                       MOVE HV-ACCT-CURR-BAL TO
                            LK-OUT-CURR-BAL(WS-ACCT-COUNT)
                       MOVE HV-ACCT-CREDIT-LIMIT TO
                            LK-OUT-CREDIT-LIMIT(WS-ACCT-COUNT)
                       MOVE HV-ACCT-CASH-LIMIT TO
                            LK-OUT-CASH-LIMIT(WS-ACCT-COUNT)
                       MOVE HV-ACCT-OPEN-DATE TO
                            LK-OUT-OPEN-DATE(WS-ACCT-COUNT)
                       MOVE HV-ACCT-EXPIRY-DATE TO
                            LK-OUT-EXPIRY-DATE(WS-ACCT-COUNT)
                       MOVE HV-ACCT-REISSUE-DATE TO
                            LK-OUT-REISSUE-DATE(WS-ACCT-COUNT)
                       MOVE HV-ACCT-CURR-CYC-CREDIT TO
                            LK-OUT-CURR-CYC-CREDIT(WS-ACCT-COUNT)
                       MOVE HV-ACCT-CURR-CYC-DEBIT TO
                            LK-OUT-CURR-CYC-DEBIT(WS-ACCT-COUNT)
                       MOVE HV-ACCT-ADDR-ZIP TO
                            LK-OUT-ADDR-ZIP(WS-ACCT-COUNT)
                       MOVE HV-ACCT-GROUP-ID TO
                            LK-OUT-GROUP-ID(WS-ACCT-COUNT)
                   END-IF
               WHEN +100
                   CONTINUE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   SET RC-DATABASE-ERROR TO TRUE
                   MOVE 'FETCH'               TO ERROR-OPNAME
                   MOVE 'ACCTDAT'             TO ERROR-TABLE
                   MOVE SQLCODE               TO ERROR-SQLCODE
                   MOVE WS-DB2-ERROR-MESSAGE  TO LK-OUT-MESSAGE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      CALL-COCCARDA-FOR-ACCOUNTS
      *----------------------------------------------------------------*
       CALL-COCCARDA-FOR-ACCOUNTS.
           PERFORM VARYING WS-ACCT-INDEX FROM 1 BY 1
                   UNTIL WS-ACCT-INDEX > LK-OUT-ACCT-COUNT
                   OR WS-ACCT-INDEX > 10
               DISPLAY 'COACCNTA: Calling COCCARDA for ACCT-ID: '
                       LK-OUT-ACCT-ID(WS-ACCT-INDEX)
      *        Initialize COCCARDA COMMAREA
               INITIALIZE WS-CCARDA-COMMAREA
               MOVE LK-OUT-ACCT-ID(WS-ACCT-INDEX) TO
                    WS-CCARDA-INPUT-ACCT-ID
      *        Call COCCARDA with proper COMMAREA
               EXEC CICS LINK
                    PROGRAM('COCCARDA')
                    COMMAREA(WS-CCARDA-COMMAREA)
                    LENGTH(LENGTH OF WS-CCARDA-COMMAREA)
               END-EXEC
      *        Log results from COCCARDA
               DISPLAY 'COACCNTA: COCCARDA returned RC: '
                       WS-CCARDA-RETURN-CODE
               DISPLAY 'COACCNTA: COCCARDA card count: '
                       WS-CCARDA-CARD-COUNT
           END-PERFORM.

      *----------------------------------------------------------------*
      *                    TEMP-DUMP-OUTPUT-STRUCTURE
      *----------------------------------------------------------------*
       TEMP-DUMP-OUTPUT-STRUCTURE.
      * TEMP: Remove this paragraph after testing is complete
           DISPLAY 'COACCNTA: === TEMP OUTPUT DUMP ==='
           DISPLAY 'COACCNTA: ACCT-COUNT: ' LK-OUT-ACCT-COUNT
           PERFORM VARYING WS-ACCT-INDEX FROM 1 BY 1
                   UNTIL WS-ACCT-INDEX > LK-OUT-ACCT-COUNT
                   OR WS-ACCT-INDEX > 10
               DISPLAY 'COACCNTA: ACCT[' WS-ACCT-INDEX '] ID: '
                       LK-OUT-ACCT-ID(WS-ACCT-INDEX)
               DISPLAY 'COACCNTA: ACCT[' WS-ACCT-INDEX '] STATUS: '
                       LK-OUT-ACCT-STATUS(WS-ACCT-INDEX)
               DISPLAY 'COACCNTA: ACCT[' WS-ACCT-INDEX '] BAL: '
                       LK-OUT-CURR-BAL(WS-ACCT-INDEX)
               DISPLAY 'COACCNTA: ACCT[' WS-ACCT-INDEX '] CREDIT-LIM: '
                       LK-OUT-CREDIT-LIMIT(WS-ACCT-INDEX)
               DISPLAY 'COACCNTA: ACCT[' WS-ACCT-INDEX '] CASH-LIM: '
                       LK-OUT-CASH-LIMIT(WS-ACCT-INDEX)
               DISPLAY 'COACCNTA: ACCT[' WS-ACCT-INDEX '] OPEN-DATE: '
                       LK-OUT-OPEN-DATE(WS-ACCT-INDEX)
               DISPLAY 'COACCNTA: ACCT[' WS-ACCT-INDEX '] EXPIRY: '
                       LK-OUT-EXPIRY-DATE(WS-ACCT-INDEX)
               DISPLAY 'COACCNTA: ACCT[' WS-ACCT-INDEX '] REISSUE: '
                       LK-OUT-REISSUE-DATE(WS-ACCT-INDEX)
               DISPLAY 'COACCNTA: ACCT[' WS-ACCT-INDEX '] CYC-CR: '
                       LK-OUT-CURR-CYC-CREDIT(WS-ACCT-INDEX)
               DISPLAY 'COACCNTA: ACCT[' WS-ACCT-INDEX '] CYC-DB: '
                       LK-OUT-CURR-CYC-DEBIT(WS-ACCT-INDEX)
               DISPLAY 'COACCNTA: ACCT[' WS-ACCT-INDEX '] ZIP: '
                       LK-OUT-ADDR-ZIP(WS-ACCT-INDEX)
               DISPLAY 'COACCNTA: ACCT[' WS-ACCT-INDEX '] GROUP: '
                       LK-OUT-GROUP-ID(WS-ACCT-INDEX)
           END-PERFORM
           DISPLAY 'COACCNTA: MESSAGE: ' LK-OUT-MESSAGE
           DISPLAY 'COACCNTA: === END TEMP DUMP ==='.

      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:12:33 CDT
      *