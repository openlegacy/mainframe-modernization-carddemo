******************************************************************
      * Program:     COACTADU.CBL                                     *
      * Layer:       Presentation                                     *
      * Function:    Account Creation Screen - calls COACTADA RPC     *
      * Transaction: AASC                                             *
      * Description: ACCOUNT CREATION SCREEN - CALLS RPC COACTADA     *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           COACTADU.
       DATE-WRITTEN.
           August 2025.
       DATE-COMPILED.
           Today.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01  WS-MISC-STORAGE.
      ******************************************************************
      * General CICS related
      ******************************************************************
         05 WS-CICS-PROCESSNG-VARS.
            07 WS-RESP-CD                          PIC S9(09) COMP
                                                   VALUE ZEROS.
            07 WS-REAS-CD                          PIC S9(09) COMP
                                                   VALUE ZEROS.
            07 WS-TRANID                           PIC X(4)
                                                   VALUE SPACES.

      ******************************************************************
      * Display fields for formatting
      ******************************************************************
         05  WS-DISPLAY-FIELDS.
           10 WS-CREDIT-LIMIT-DISP             PIC  +ZZZ,ZZZ,ZZZ.99.
           10 WS-CASH-LIMIT-DISP               PIC  +ZZZ,ZZZ,ZZZ.99.
           10 WS-CURR-BAL-DISP                 PIC  +ZZZ,ZZZ,ZZZ.99.
           10 WS-CYC-CREDIT-DISP               PIC  +ZZZ,ZZZ,ZZZ.99.
           10 WS-CYC-DEBIT-DISP                PIC  +ZZZ,ZZZ,ZZZ.99.

      ******************************************************************
      * AMOUNT VALIDATION WORK FIELDS - Like UPU pattern
      ******************************************************************
         05  WS-AMOUNT-VALIDATION.
           10 WS-AMOUNT-ERROR-FLAG                 PIC X(01) VALUE 'N'.
              88 AMOUNT-FORMAT-ERROR               VALUE 'Y'.
              88 AMOUNT-FORMAT-OK                  VALUE 'N'.
           10 WS-CONVERTED-AMOUNTS.
              15 WS-CREDIT-LIMIT-NUM               PIC S9(10)V99.
              15 WS-CASH-LIMIT-NUM                 PIC S9(10)V99.
              15 WS-CURR-BAL-NUM                   PIC S9(10)V99.
              15 WS-CYC-CREDIT-NUM                 PIC S9(10)V99.
              15 WS-CYC-DEBIT-NUM                  PIC S9(10)V99.

      ******************************************************************
      * Field persistence storage
      ******************************************************************
         05  WS-FIELD-PERSISTENCE.
           10 WS-PERSIST-CUST-ID                   PIC X(09).
           10 WS-PERSIST-ACCT-STATUS               PIC X(01).
           10 WS-PERSIST-GROUP-ID                  PIC X(10).
           10 WS-PERSIST-ZIP-CODE                  PIC X(10).
           10 WS-PERSIST-OPEN-YEAR                 PIC X(04).
           10 WS-PERSIST-OPEN-MON                  PIC X(02).
           10 WS-PERSIST-OPEN-DAY                  PIC X(02).
           10 WS-PERSIST-EXP-YEAR                  PIC X(04).
           10 WS-PERSIST-EXP-MON                   PIC X(02).
           10 WS-PERSIST-EXP-DAY                   PIC X(02).
           10 WS-PERSIST-REISS-YEAR                PIC X(04).
           10 WS-PERSIST-REISS-MON                 PIC X(02).
           10 WS-PERSIST-REISS-DAY                 PIC X(02).
           10 WS-PERSIST-CREDIT-LIMIT              PIC X(15).
           10 WS-PERSIST-CASH-LIMIT                PIC X(15).
           10 WS-PERSIST-CURR-BAL                  PIC X(15).
           10 WS-PERSIST-CYC-CREDIT                PIC X(15).
           10 WS-PERSIST-CYC-DEBIT                 PIC X(15).

      ******************************************************************
      * SQL Communication Area
      ******************************************************************
       EXEC SQL
           INCLUDE SQLCA
       END-EXEC.

      ******************************************************************
      * Message handling
      ******************************************************************
         05  WS-MESSAGE                            PIC X(80).
         05  WS-ERR-FLG                            PIC X(01) VALUE 'N'.
           88  ERR-FLG-ON                          VALUE 'Y'.
           88  ERR-FLG-OFF                         VALUE 'N'.

      ******************************************************************
      * Constants and literals
      ******************************************************************
       01 WS-LITERALS.
          05 LIT-THISPGM                           PIC X(8)
                                                   VALUE 'COACTADU'.
          05 LIT-THISTRANID                        PIC X(4)
                                                   VALUE 'AASC'.
          05 LIT-THISMAPSET                        PIC X(8)
                                                   VALUE 'COACTAD '.
          05 LIT-THISMAP                           PIC X(7)
                                                   VALUE 'COACTAD'.
          05 LIT-RPC-PROGRAM                       PIC X(8)
                                                   VALUE 'COACTADA'.
          05 LIT-CUST-CREATE-PGM                   PIC X(8)
                                                   VALUE 'COCUSADU'.
          05 LIT-CUST-CREATE-TRAN                  PIC X(4)
                                                   VALUE 'AASD'.
          05 LIT-MENUPGM                           PIC X(8)
                                                   VALUE 'COMEN01U'.
          05 LIT-MENUTRANID                        PIC X(4)
                                                   VALUE 'AAUM'.

      ******************************************************************
      * Copy common work areas
      ******************************************************************
       COPY CVCRD01Y.

      ******************************************************************
      * Application Commarea Copybook
      ******************************************************************
       COPY COCOM01Y.

      ******************************************************************
      * SIMPLE COMMAREA
      ******************************************************************
       01  WS-COMMAREA                             PIC X(2000).

      ******************************************************************
      * IBM SUPPLIED COPYBOOKS
      ******************************************************************
       COPY DFHBMSCA.
       COPY DFHAID.

      ******************************************************************
      * COMMON COPYBOOKS
      ******************************************************************
       COPY COTTL01Y.
       COPY COACTAD.
       COPY CSDAT01Y.
       COPY CSMSG01Y.
       COPY CSMSG02Y.
       COPY CSUSR01Y.

      ******************************************************************
      * RPC Communication Area for Account Creation - NUMERIC AMOUNTS
      ******************************************************************
       01 WS-ACCT-RPC-COMMAREA.
          05 LK-ACCT-INPUT-PARMS.
             10 LK-ACCT-IN-OPERATION       PIC X(01).
                88 ACCT-OP-CREATE          VALUE 'C'.
                88 ACCT-OP-VALIDATE        VALUE 'V'.
             10 LK-ACCT-IN-CUST-ID         PIC X(09).
             10 LK-ACCT-IN-ACCT-DATA.
                15 LK-ACCT-IN-STATUS        PIC X(1).
                15 LK-ACCT-IN-CREDIT-LIMIT  PIC S9(10)V99.
                15 LK-ACCT-IN-CASH-LIMIT    PIC S9(10)V99.
                15 LK-ACCT-IN-CURR-BAL      PIC S9(10)V99.
                15 LK-ACCT-IN-OPEN-DATE.
                   20 LK-ACCT-IN-OPEN-YEAR  PIC X(4).
                   20 LK-ACCT-IN-OPEN-MON   PIC X(2).
                   20 LK-ACCT-IN-OPEN-DAY   PIC X(2).
                15 LK-ACCT-IN-EXPIRY-DATE.
                   20 LK-ACCT-IN-EXP-YEAR   PIC X(4).
                   20 LK-ACCT-IN-EXP-MON    PIC X(2).
                   20 LK-ACCT-IN-EXP-DAY    PIC X(2).
                15 LK-ACCT-IN-REISSUE-DATE.
                   20 LK-ACCT-IN-REISS-YEAR PIC X(4).
                   20 LK-ACCT-IN-REISS-MON  PIC X(2).
                   20 LK-ACCT-IN-REISS-DAY  PIC X(2).
                15 LK-ACCT-IN-CYC-CREDIT    PIC S9(10)V99.
                15 LK-ACCT-IN-CYC-DEBIT     PIC S9(10)V99.
                15 LK-ACCT-IN-GROUP-ID      PIC X(10).
                15 LK-ACCT-IN-ZIP-CODE      PIC X(10).
          05 LK-ACCT-OUTPUT-STATUS.
             10 LK-ACCT-OUT-RETURN-CODE    PIC 9(02).
                88 ACCT-RC-SUCCESS         VALUE 00.
                88 ACCT-RC-NOT-FOUND       VALUE 01.
                88 ACCT-RC-INPUT-ERROR     VALUE 03.
                88 ACCT-RC-DATABASE-ERROR  VALUE 99.
                88 ACCT-RC-CUSTOMER-NOT-FOUND VALUE 04.
                88 ACCT-RC-INVALID-LIMIT   VALUE 05.
             10 LK-ACCT-OUT-MESSAGE        PIC X(80).
             10 LK-ACCT-OUT-ERROR-FIELD    PIC X(25).
          05 LK-ACCT-OUTPUT-DATA.
             10 LK-ACCT-OUT-NEW-ACCT-ID    PIC 9(11).

       LINKAGE SECTION.
       01  DFHCOMMAREA.
         05  FILLER                                PIC X(1)
             OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.

       PROCEDURE DIVISION.

       0000-MAIN.

           EXEC CICS HANDLE ABEND
                     LABEL(ABEND-ROUTINE)
           END-EXEC

           INITIALIZE WS-MISC-STORAGE
                      WS-COMMAREA
                      WS-ACCT-RPC-COMMAREA

           MOVE LIT-THISTRANID       TO WS-TRANID
           SET ERR-FLG-OFF TO TRUE
           MOVE SPACES TO WS-MESSAGE

      ******************************************************************
      * Remap PFkeys and store mapped PF Key
      ******************************************************************
           PERFORM YYYY-STORE-PFKEY
              THRU YYYY-STORE-PFKEY-EXIT

      ******************************************************************
      * SAFE COMMAREA HANDLING WITH DEBUG
      ******************************************************************
           IF EIBCALEN = 0
               MOVE LIT-MENUPGM TO CDEMO-TO-PROGRAM
               PERFORM RETURN-TO-PREV-SCREEN
           ELSE
               MOVE DFHCOMMAREA (1:EIBCALEN) TO CARDDEMO-COMMAREA

      *        CLEAN CUSTOMER INFO FROM MENU POLLUTION
               IF CDEMO-FROM-PROGRAM = 'COMEN01U'
                  INITIALIZE CDEMO-CUSTOMER-INFO
               END-IF

      *        SAVE CUST-ID FROM CUSU IF COMING FROM THERE
               IF CDEMO-FROM-PROGRAM = 'COCUSADU'
               AND CDEMO-CUST-ID NOT = ZEROS
                  MOVE CDEMO-CUST-ID TO WS-PERSIST-CUST-ID
                          WS-PERSIST-CUST-ID
               END-IF

               IF NOT CDEMO-PGM-REENTER
                   SET CDEMO-PGM-REENTER TO TRUE
                   MOVE LOW-VALUES TO COACTADO
      *            FORCE CURSOR ON APPROPRIATE FIELD
                   IF WS-PERSIST-CUST-ID NOT = SPACES
                      MOVE -1 TO ACSTTUSL OF COACTADI
                   ELSE
                      MOVE -1 TO CUSTIDL OF COACTADI
                   END-IF

      *            CHECK FOR CUSTOMER CREATION SUCCESS MESSAGE
                  IF CDEMO-FROM-PROGRAM = 'COACTADU'
                  AND CDEMO-CUST-ID NOT = ZEROS
                  AND WS-PERSIST-CUST-ID = SPACES
                     STRING 'Customer ' CDEMO-CUST-ID ' created'
                     ' - ready for account creation.'
                     DELIMITED BY SIZE INTO WS-MESSAGE
                     SET ERR-FLG-OFF TO TRUE
                     MOVE SPACES TO CDEMO-FROM-PROGRAM
                  END-IF
                   PERFORM SEND-ACCTADD-SCREEN
               ELSE
                   PERFORM RECEIVE-ACCTADD-SCREEN
                   EVALUATE TRUE
                       WHEN CCARD-AID-ENTER
                           PERFORM PROCESS-ENTER-KEY
                       WHEN CCARD-AID-PFK03
                           IF CDEMO-FROM-PROGRAM = 'COCUSADU'
      *                     PRESERVE CUSTOMER ID FROM COMMAREA BEFORE REDISPLAY
                             IF CDEMO-CUST-ID NOT = ZEROS
                                MOVE CDEMO-CUST-ID TO WS-PERSIST-CUST-ID
                                 CDEMO-CUST-ID
                                SET ERR-FLG-OFF TO TRUE
                             END-IF
      *                     INITIALIZE AMOUNTS FOR PROPER DISPLAY
                             INITIALIZE WS-CONVERTED-AMOUNTS
                             INITIALIZE WS-FIELD-PERSISTENCE
                             MOVE WS-PERSIST-CUST-ID TO
                             CUSTIDI OF COACTADI
                             MOVE SPACES TO INFOMSGO OF COACTADO
                             PERFORM SEND-ACCTADD-SCREEN
                             MOVE LIT-THISPGM     TO CDEMO-FROM-PROGRAM
                             GO TO COMMON-RETURN
                           ELSE
                              MOVE LIT-MENUTRANID TO CDEMO-TO-TRANID
                              MOVE LIT-MENUPGM    TO CDEMO-TO-PROGRAM
                              MOVE LIT-THISTRANID TO CDEMO-FROM-TRANID
                              MOVE LIT-THISPGM    TO CDEMO-FROM-PROGRAM
                              SET  CDEMO-PGM-ENTER TO TRUE
                              EXEC CICS XCTL
                                   PROGRAM (CDEMO-TO-PROGRAM)
                                   COMMAREA (CARDDEMO-COMMAREA)
                              END-EXEC
                           END-IF

                       WHEN CCARD-AID-PFK04
                         IF CUSTIDI OF COACTADI = SPACES
                            SET ERR-FLG-ON TO TRUE
                            MOVE 'Enter Customer ID first.'
                            TO WS-MESSAGE
                            MOVE -1 TO CUSTIDL OF COACTADI
                            PERFORM SEND-ACCTADD-SCREEN
                         ELSE
      *                     PASS THE CUSTOMER ID TO CUSU FOR CREATION
                            COMPUTE CDEMO-CUST-ID =
                             FUNCTION NUMVAL(CUSTIDI OF COACTADI)
                            MOVE LIT-CUST-CREATE-TRAN
                            TO CDEMO-TO-TRANID
                            MOVE LIT-CUST-CREATE-PGM
                            TO CDEMO-TO-PROGRAM
                            MOVE LIT-THISTRANID TO CDEMO-FROM-TRANID
                            MOVE LIT-THISPGM TO CDEMO-FROM-PROGRAM
                            SET CDEMO-PGM-ENTER TO TRUE
                            EXEC CICS XCTL
                                 PROGRAM (CDEMO-TO-PROGRAM)
                                 COMMAREA (CARDDEMO-COMMAREA)
                            END-EXEC
                         END-IF
                       WHEN OTHER
                           SET ERR-FLG-ON TO TRUE
                           MOVE -1 TO CUSTIDL OF COACTADI
                           MOVE 'Invalid key pressed.' TO WS-MESSAGE
                           PERFORM SEND-ACCTADD-SCREEN
                   END-EVALUATE
               END-IF
           END-IF.

       COMMON-RETURN.
           EXEC CICS RETURN
                TRANSID (LIT-THISTRANID)
                COMMAREA (CARDDEMO-COMMAREA)
           END-EXEC
           .

      ******************************************************************
      * Process Enter Key - AMOUNT FORMAT VALIDATION ONLY
      ******************************************************************
       PROCESS-ENTER-KEY.

      *    Validate amount formats FIRST in UI layer (like UPU pattern)
           PERFORM VALID-AMOUNT-FORMATS
              THRU VALID-AMOUNT-FORMATS-EXIT

      *    If format errors, send screen with error message
           IF AMOUNT-FORMAT-ERROR
              SET ERR-FLG-ON TO TRUE
              PERFORM SEND-ACCTADD-SCREEN
           ELSE
      *  Format validation passed, call RPC (RPC will check customer)
              PERFORM CALL-RPC-PROGRAM
           END-IF

           .
       PROCESS-ENTER-KEY-EXIT.
           EXIT.

      ******************************************************************
      * Amount format validation in UI layer ONLY - Like UPU
      ******************************************************************
       VALID-AMOUNT-FORMATS.
           SET AMOUNT-FORMAT-OK TO TRUE
           MOVE SPACES TO WS-MESSAGE

      *    Credit Limit - Just check if numeric, convert and display
           PERFORM VALID-CREDIT-LIMIT-FORMAT
              THRU VALID-CREDIT-LIMIT-FORMAT-EXIT
           IF AMOUNT-FORMAT-ERROR
              GO TO VALID-AMOUNT-FORMATS-EXIT
           END-IF

      *    Cash Limit - Just check if numeric, convert and display
           PERFORM VALID-CASH-LIMIT-FORMAT
              THRU VALID-CASH-LIMIT-FORMAT-EXIT
           IF AMOUNT-FORMAT-ERROR
              GO TO VALID-AMOUNT-FORMATS-EXIT
           END-IF

      *    Current Balance - Just check if numeric, convert and display
           PERFORM VALID-CURR-BAL-FORMAT
              THRU VALID-CURR-BAL-FORMAT-EXIT
           IF AMOUNT-FORMAT-ERROR
              GO TO VALID-AMOUNT-FORMATS-EXIT
           END-IF

      *    Cycle amounts - Just check if numeric, convert and display
           PERFORM VALID-CYCLE-FORMATS
              THRU VALID-CYCLE-FORMATS-EXIT

           .
       VALID-AMOUNT-FORMATS-EXIT.
           EXIT.

      ******************************************************************
      * Credit Limit Format Validation - UI Layer
      ******************************************************************
       VALID-CREDIT-LIMIT-FORMAT.
           IF ACRDLIMI OF COACTADI = SPACES
           OR ACRDLIMI OF COACTADI = LOW-VALUES
           OR ACRDLIMI OF COACTADI = '*'
              MOVE ZEROS TO WS-CREDIT-LIMIT-NUM
              GO TO VALID-CREDIT-LIMIT-FORMAT-EXIT
           END-IF

           IF FUNCTION TEST-NUMVAL-C(ACRDLIMI OF COACTADI) = 0
              COMPUTE WS-CREDIT-LIMIT-NUM =
                 FUNCTION NUMVAL-C(ACRDLIMI OF COACTADI)
           ELSE
              SET AMOUNT-FORMAT-ERROR TO TRUE
              MOVE 'Credit Limit is not a valid amount.' TO WS-MESSAGE
              MOVE -1 TO ACRDLIML OF COACTADI
              MOVE ZEROS TO WS-CREDIT-LIMIT-NUM
           END-IF
           .
       VALID-CREDIT-LIMIT-FORMAT-EXIT.
           EXIT.

      ******************************************************************
      * Cash Limit Format Validation - UI Layer
      ******************************************************************
       VALID-CASH-LIMIT-FORMAT.
           IF ACSHLIMI OF COACTADI = SPACES
           OR ACSHLIMI OF COACTADI = LOW-VALUES
           OR ACSHLIMI OF COACTADI = '*'
              MOVE ZEROS TO WS-CASH-LIMIT-NUM
              GO TO VALID-CASH-LIMIT-FORMAT-EXIT
           END-IF

           IF FUNCTION TEST-NUMVAL-C(ACSHLIMI OF COACTADI) = 0
              COMPUTE WS-CASH-LIMIT-NUM =
                 FUNCTION NUMVAL-C(ACSHLIMI OF COACTADI)
           ELSE
              SET AMOUNT-FORMAT-ERROR TO TRUE
              MOVE 'Cash Limit is not a valid amount.' TO WS-MESSAGE
              MOVE -1 TO ACSHLIML OF COACTADI
              MOVE ZEROS TO WS-CASH-LIMIT-NUM
           END-IF
           .
       VALID-CASH-LIMIT-FORMAT-EXIT.
           EXIT.

      ******************************************************************
      * Current Balance Format Validation - UI Layer
      ******************************************************************
       VALID-CURR-BAL-FORMAT.
           IF ACURBALI OF COACTADI = SPACES
           OR ACURBALI OF COACTADI = LOW-VALUES
           OR ACURBALI OF COACTADI = '*'
              MOVE ZEROS TO WS-CURR-BAL-NUM
              GO TO VALID-CURR-BAL-FORMAT-EXIT
           END-IF

           IF FUNCTION TEST-NUMVAL-C(ACURBALI OF COACTADI) = 0
              COMPUTE WS-CURR-BAL-NUM =
                 FUNCTION NUMVAL-C(ACURBALI OF COACTADI)
           ELSE
              SET AMOUNT-FORMAT-ERROR TO TRUE
              MOVE 'Current Balance is not a valid amount.'
              TO WS-MESSAGE
              MOVE -1 TO ACURBALL OF COACTADI
              MOVE ZEROS TO WS-CURR-BAL-NUM
           END-IF
           .
       VALID-CURR-BAL-FORMAT-EXIT.
           EXIT.

      ******************************************************************
      * Cycle Amounts Format Validation - UI Layer
      ******************************************************************
       VALID-CYCLE-FORMATS.
      *    Cycle Credit
           IF ACRCYCRI OF COACTADI = SPACES
           OR ACRCYCRI OF COACTADI = LOW-VALUES
           OR ACRCYCRI OF COACTADI = '*'
              MOVE ZEROS TO WS-CYC-CREDIT-NUM
           ELSE
              IF FUNCTION TEST-NUMVAL-C(ACRCYCRI OF COACTADI) = 0
                 COMPUTE WS-CYC-CREDIT-NUM =
                    FUNCTION NUMVAL-C(ACRCYCRI OF COACTADI)
              ELSE
                 SET AMOUNT-FORMAT-ERROR TO TRUE
                 MOVE 'Cycle Credit is not a valid amount.'
                  TO WS-MESSAGE
                 MOVE -1 TO ACRCYCRL OF COACTADI
                 MOVE ZEROS TO WS-CYC-CREDIT-NUM
                 GO TO VALID-CYCLE-FORMATS-EXIT
              END-IF
           END-IF

      *    Cycle Debit
           IF ACRCYDBI OF COACTADI = SPACES
           OR ACRCYDBI OF COACTADI = LOW-VALUES
           OR ACRCYDBI OF COACTADI = '*'
              MOVE ZEROS TO WS-CYC-DEBIT-NUM
           ELSE
              IF FUNCTION TEST-NUMVAL-C(ACRCYDBI OF COACTADI) = 0
                 COMPUTE WS-CYC-DEBIT-NUM =
                    FUNCTION NUMVAL-C(ACRCYDBI OF COACTADI)
              ELSE
                 SET AMOUNT-FORMAT-ERROR TO TRUE
                 MOVE 'Cycle Debit is not a valid amount.'
                  TO WS-MESSAGE
                 MOVE -1 TO ACRCYDBL OF COACTADI
                 MOVE ZEROS TO WS-CYC-DEBIT-NUM
              END-IF
           END-IF
           .
       VALID-CYCLE-FORMATS-EXIT.
           EXIT.

      ******************************************************************
      * Persist all input fields for screen continuity
      ******************************************************************
       PERSIST-INPUT-FIELDS.
           MOVE CUSTIDI OF COACTADI TO WS-PERSIST-CUST-ID

           MOVE ACSTTUSI OF COACTADI TO WS-PERSIST-ACCT-STATUS
           MOVE ACRDLIMI OF COACTADI TO WS-PERSIST-CREDIT-LIMIT
           MOVE ACSHLIMI OF COACTADI TO WS-PERSIST-CASH-LIMIT
           MOVE ACURBALI OF COACTADI TO WS-PERSIST-CURR-BAL
           MOVE ACRCYCRI OF COACTADI TO WS-PERSIST-CYC-CREDIT
           MOVE ACRCYDBI OF COACTADI TO WS-PERSIST-CYC-DEBIT

           MOVE OPNYEARI OF COACTADI TO WS-PERSIST-OPEN-YEAR
           MOVE OPNMONI OF COACTADI TO WS-PERSIST-OPEN-MON
           MOVE OPNDAYI OF COACTADI TO WS-PERSIST-OPEN-DAY
           MOVE EXPYEARI OF COACTADI TO WS-PERSIST-EXP-YEAR
           MOVE EXPMONI OF COACTADI TO WS-PERSIST-EXP-MON
           MOVE EXPDAYI OF COACTADI TO WS-PERSIST-EXP-DAY
           MOVE RISYEARI OF COACTADI TO WS-PERSIST-REISS-YEAR
           MOVE RISMONI OF COACTADI TO WS-PERSIST-REISS-MON
           MOVE RISDAYI OF COACTADI TO WS-PERSIST-REISS-DAY

           MOVE AADDGRPI OF COACTADI TO WS-PERSIST-GROUP-ID
           MOVE ACZIPI OF COACTADI TO WS-PERSIST-ZIP-CODE
           .

      ******************************************************************
      * Call RPC Program - Pass converted numeric amounts
      ******************************************************************
       CALL-RPC-PROGRAM.
           INITIALIZE WS-ACCT-RPC-COMMAREA
           SET ACCT-OP-CREATE TO TRUE

           MOVE CUSTIDI OF COACTADI TO LK-ACCT-IN-CUST-ID

           MOVE ACSTTUSI OF COACTADI TO LK-ACCT-IN-STATUS

      *    Pass converted numeric amounts to RPC
           MOVE WS-CREDIT-LIMIT-NUM TO LK-ACCT-IN-CREDIT-LIMIT
           MOVE WS-CASH-LIMIT-NUM   TO LK-ACCT-IN-CASH-LIMIT
           MOVE WS-CURR-BAL-NUM     TO LK-ACCT-IN-CURR-BAL
           MOVE WS-CYC-CREDIT-NUM   TO LK-ACCT-IN-CYC-CREDIT
           MOVE WS-CYC-DEBIT-NUM    TO LK-ACCT-IN-CYC-DEBIT

      *    Pass other fields as-is
           MOVE OPNYEARI OF COACTADI TO LK-ACCT-IN-OPEN-YEAR
           MOVE OPNMONI OF COACTADI TO LK-ACCT-IN-OPEN-MON
           MOVE OPNDAYI OF COACTADI TO LK-ACCT-IN-OPEN-DAY
           MOVE EXPYEARI OF COACTADI TO LK-ACCT-IN-EXP-YEAR
           MOVE EXPMONI OF COACTADI TO LK-ACCT-IN-EXP-MON
           MOVE EXPDAYI OF COACTADI TO LK-ACCT-IN-EXP-DAY
           MOVE RISYEARI OF COACTADI TO LK-ACCT-IN-REISS-YEAR
           MOVE RISMONI OF COACTADI TO LK-ACCT-IN-REISS-MON
           MOVE RISDAYI OF COACTADI TO LK-ACCT-IN-REISS-DAY
           MOVE AADDGRPI OF COACTADI TO LK-ACCT-IN-GROUP-ID
           MOVE ACZIPI OF COACTADI TO LK-ACCT-IN-ZIP-CODE

           EXEC CICS LINK
                PROGRAM(LIT-RPC-PROGRAM)
                COMMAREA(WS-ACCT-RPC-COMMAREA)
                LENGTH(LENGTH OF WS-ACCT-RPC-COMMAREA)
                RESP(WS-RESP-CD)
                RESP2(WS-REAS-CD)
           END-EXEC

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   PERFORM PROCESS-RPC-RESPONSE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   MOVE 'Error calling account add service...' TO
                                   WS-MESSAGE
                   MOVE -1 TO CUSTIDL OF COACTADI
                   PERFORM SEND-ACCTADD-SCREEN
           END-EVALUATE
           .

      ******************************************************************
      * Process RPC Response - Enhanced for customer exists case
      ******************************************************************
       PROCESS-RPC-RESPONSE.

           EVALUATE LK-ACCT-OUT-RETURN-CODE
               WHEN 00
                   PERFORM INITIALIZE-ALL-FIELDS
                   MOVE SPACES TO WS-MESSAGE
                   SET ERR-FLG-OFF TO TRUE
                   MOVE LK-ACCT-OUT-MESSAGE TO WS-MESSAGE
                   PERFORM SEND-ACCTADD-SCREEN
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
      *            Check if customer already exists error
                   IF LK-ACCT-OUT-RETURN-CODE = 04
                   AND LK-ACCT-OUT-ERROR-FIELD = 'CUST-ID'
      *                Add F4 suggestion to the error message
                       STRING LK-ACCT-OUT-MESSAGE
                       ' (F4=Create New Customer)'
                              DELIMITED BY SIZE
                              INTO WS-MESSAGE
                       END-STRING
                   ELSE
                       MOVE LK-ACCT-OUT-MESSAGE TO WS-MESSAGE
                   END-IF
                   PERFORM SET-CURSOR-FROM-ERROR-FIELD
                   MOVE WS-PERSIST-CUST-ID TO CUSTIDI OF COACTADI
                   PERFORM SEND-ACCTADD-SCREEN
           END-EVALUATE
           .

      ******************************************************************
      * Set cursor position based on RPC error field
      ******************************************************************
       SET-CURSOR-FROM-ERROR-FIELD.
           EVALUATE LK-ACCT-OUT-ERROR-FIELD
               WHEN 'CUST-ID'
                   MOVE -1 TO CUSTIDL OF COACTADI
               WHEN 'ACCT-STATUS'
                   MOVE -1 TO ACSTTUSL OF COACTADI
               WHEN 'CREDIT-LIMIT'
                   MOVE -1 TO ACRDLIML OF COACTADI
               WHEN 'CASH-LIMIT'
                   MOVE -1 TO ACSHLIML OF COACTADI
               WHEN 'CURR-BAL'
                   MOVE -1 TO ACURBALL OF COACTADI
               WHEN 'OPEN-YEAR'
                   MOVE -1 TO OPNYEARL OF COACTADI
               WHEN 'OPEN-MONTH'
                   MOVE -1 TO OPNMONL OF COACTADI
               WHEN 'OPEN-DAY'
                   MOVE -1 TO OPNDAYL OF COACTADI
               WHEN 'EXP-YEAR'
                   MOVE -1 TO EXPYEARL OF COACTADI
               WHEN 'EXP-MONTH'
                   MOVE -1 TO EXPMONL OF COACTADI
               WHEN 'EXP-DAY'
                   MOVE -1 TO EXPDAYL OF COACTADI
               WHEN 'REISS-YEAR'
                   MOVE -1 TO RISYEARL OF COACTADI
               WHEN 'REISS-MONTH'
                   MOVE -1 TO RISMONL OF COACTADI
               WHEN 'REISS-DAY'
                   MOVE -1 TO RISDAYL OF COACTADI
               WHEN 'CYC-CREDIT'
                   MOVE -1 TO ACRCYCRL OF COACTADI
               WHEN 'CYC-DEBIT'
                   MOVE -1 TO ACRCYDBL OF COACTADI
               WHEN 'GROUP-ID'
                   MOVE -1 TO AADDGRPL OF COACTADI
               WHEN 'ZIP-CODE'
                   MOVE -1 TO ACZIPL OF COACTADI
               WHEN OTHER
                   MOVE -1 TO CUSTIDL OF COACTADI
           END-EVALUATE
           .

      ******************************************************************
      * Return to previous screen
      ******************************************************************
       RETURN-TO-PREV-SCREEN.
           IF CDEMO-TO-PROGRAM = LOW-VALUES OR SPACES
               MOVE LIT-MENUPGM TO CDEMO-TO-PROGRAM
           END-IF
           MOVE LIT-THISTRANID TO CDEMO-FROM-TRANID
           MOVE LIT-THISPGM TO CDEMO-FROM-PROGRAM
           SET CDEMO-PGM-ENTER TO TRUE
           EXEC CICS
               XCTL PROGRAM(CDEMO-TO-PROGRAM)
               COMMAREA(CARDDEMO-COMMAREA)
           END-EXEC
           .

      ******************************************************************
      * Send Account Add Screen
      ******************************************************************
       SEND-ACCTADD-SCREEN.

           PERFORM POPULATE-HEADER-INFO
           PERFORM POPULATE-PERSISTED-FIELDS

           IF ERR-FLG-ON
               MOVE DFHRED TO ERRMSGC OF COACTADO
           ELSE
               MOVE DFHGREEN TO ERRMSGC OF COACTADO
           END-IF

           IF WS-MESSAGE NOT = SPACES
              MOVE WS-MESSAGE TO ERRMSGO OF COACTADO
                      ERRMSGO OF COACTADO
           ELSE
               MOVE WS-MESSAGE TO ERRMSGO OF COACTADO
           END-IF

           EXEC CICS SEND
                     MAP(LIT-THISMAP)
                     MAPSET(LIT-THISMAPSET)
                     FROM(COACTADO)
                     ERASE
                     CURSOR
           END-EXEC

           .

      ******************************************************************
      * Receive Account Add Screen
      ******************************************************************
       RECEIVE-ACCTADD-SCREEN.
           EXEC CICS RECEIVE
                     MAP(LIT-THISMAP)
                     MAPSET(LIT-THISMAPSET)
                     INTO(COACTADI)
                     RESP(WS-RESP-CD)
                     RESP2(WS-REAS-CD)
           END-EXEC

           PERFORM PERSIST-INPUT-FIELDS
           .

      ******************************************************************
      * Populate Header Info
      ******************************************************************
       POPULATE-HEADER-INFO.
           MOVE FUNCTION CURRENT-DATE  TO WS-CURDATE-DATA

           MOVE CCDA-TITLE01           TO TITLE01O OF COACTADO
           MOVE CCDA-TITLE02           TO TITLE02O OF COACTADO
           MOVE LIT-THISTRANID         TO TRNNAMEO OF COACTADO
           MOVE LIT-THISPGM            TO PGMNAMEO OF COACTADO

           MOVE WS-CURDATE-MONTH       TO WS-CURDATE-MM
           MOVE WS-CURDATE-DAY         TO WS-CURDATE-DD
           MOVE WS-CURDATE-YEAR(3:2)   TO WS-CURDATE-YY

           MOVE WS-CURDATE-MM-DD-YY    TO CURDATEO OF COACTADO

           MOVE WS-CURTIME-HOURS       TO WS-CURTIME-HH
           MOVE WS-CURTIME-MINUTE      TO WS-CURTIME-MM
           MOVE WS-CURTIME-SECOND      TO WS-CURTIME-SS

           MOVE WS-CURTIME-HH-MM-SS    TO CURTIMEO OF COACTADO
           .

      ******************************************************************
      * Populate persisted fields on screen - FORMAT AMOUNTS
      ******************************************************************
       POPULATE-PERSISTED-FIELDS.
      *    POPULATE CUST-ID FROM PERSISTENCE FIRST, THEN COMMAREA
           IF WS-PERSIST-CUST-ID NOT = SPACES
           AND WS-PERSIST-CUST-ID NOT = ZEROS
              MOVE WS-PERSIST-CUST-ID TO CUSTIDO OF COACTADO
           ELSE IF CDEMO-CUST-ID NOT = ZEROS
              MOVE CDEMO-CUST-ID TO CUSTIDO OF COACTADO
           END-IF
           MOVE WS-PERSIST-ACCT-STATUS TO ACSTTUSO OF COACTADO

      *    Restore converted amounts from persisted fields
           IF WS-PERSIST-CREDIT-LIMIT NOT = SPACES
           AND FUNCTION TEST-NUMVAL-C(WS-PERSIST-CREDIT-LIMIT) = 0
              COMPUTE WS-CREDIT-LIMIT-NUM =
                 FUNCTION NUMVAL-C(WS-PERSIST-CREDIT-LIMIT)
           END-IF

           IF WS-PERSIST-CASH-LIMIT NOT = SPACES
           AND FUNCTION TEST-NUMVAL-C(WS-PERSIST-CASH-LIMIT) = 0
              COMPUTE WS-CASH-LIMIT-NUM =
                 FUNCTION NUMVAL-C(WS-PERSIST-CASH-LIMIT)
           END-IF

           IF WS-PERSIST-CURR-BAL NOT = SPACES
           AND FUNCTION TEST-NUMVAL-C(WS-PERSIST-CURR-BAL) = 0
              COMPUTE WS-CURR-BAL-NUM =
                 FUNCTION NUMVAL-C(WS-PERSIST-CURR-BAL)
           END-IF

           IF WS-PERSIST-CYC-CREDIT NOT = SPACES
           AND FUNCTION TEST-NUMVAL-C(WS-PERSIST-CYC-CREDIT) = 0
              COMPUTE WS-CYC-CREDIT-NUM =
                 FUNCTION NUMVAL-C(WS-PERSIST-CYC-CREDIT)
           END-IF

           IF WS-PERSIST-CYC-DEBIT NOT = SPACES
           AND FUNCTION TEST-NUMVAL-C(WS-PERSIST-CYC-DEBIT) = 0
              COMPUTE WS-CYC-DEBIT-NUM =
                 FUNCTION NUMVAL-C(WS-PERSIST-CYC-DEBIT)
           END-IF

      *    Format amounts for display amounts
           MOVE WS-CREDIT-LIMIT-NUM TO WS-CREDIT-LIMIT-DISP
           MOVE WS-CREDIT-LIMIT-DISP TO ACRDLIMO OF COACTADO

           MOVE WS-CASH-LIMIT-NUM TO WS-CASH-LIMIT-DISP
           MOVE WS-CASH-LIMIT-DISP TO ACSHLIMO OF COACTADO

           MOVE WS-CURR-BAL-NUM TO WS-CURR-BAL-DISP
           MOVE WS-CURR-BAL-DISP TO ACURBALO OF COACTADO

           MOVE WS-CYC-CREDIT-NUM TO WS-CYC-CREDIT-DISP
           MOVE WS-CYC-CREDIT-DISP TO ACRCYCRO OF COACTADO

           MOVE WS-CYC-DEBIT-NUM TO WS-CYC-DEBIT-DISP
           MOVE WS-CYC-DEBIT-DISP TO ACRCYDBO OF COACTADO

      *    Other fields unchanged
           MOVE WS-PERSIST-OPEN-YEAR TO OPNYEARO OF COACTADO
           MOVE WS-PERSIST-OPEN-MON TO OPNMONO OF COACTADO
           MOVE WS-PERSIST-OPEN-DAY TO OPNDAYO OF COACTADO
           MOVE WS-PERSIST-EXP-YEAR TO EXPYEARO OF COACTADO
           MOVE WS-PERSIST-EXP-MON TO EXPMONO OF COACTADO
           MOVE WS-PERSIST-EXP-DAY TO EXPDAYO OF COACTADO
           MOVE WS-PERSIST-REISS-YEAR TO RISYEARO OF COACTADO
           MOVE WS-PERSIST-REISS-MON TO RISMONO OF COACTADO
           MOVE WS-PERSIST-REISS-DAY TO RISDAYO OF COACTADO
           MOVE WS-PERSIST-GROUP-ID TO AADDGRPO OF COACTADO
           MOVE WS-PERSIST-ZIP-CODE TO ACZIPO OF COACTADO
           .

      ******************************************************************
      * Initialize all fields
      ******************************************************************
       INITIALIZE-ALL-FIELDS.
           MOVE -1 TO CUSTIDL OF COACTADI
           MOVE SPACES TO WS-MESSAGE
           INITIALIZE WS-FIELD-PERSISTENCE
           INITIALIZE WS-CONVERTED-AMOUNTS
           .

      ******************************************************************
      * Copy PFKey handling
      ******************************************************************
       COPY 'CSSTRPFY'.

       ABEND-ROUTINE.
           MOVE 'UNEXPECTED ABEND OCCURRED.' TO WS-MESSAGE
           MOVE LIT-THISPGM TO ABEND-CULPRIT
           EXEC CICS SEND
                            FROM (ABEND-DATA)
                            LENGTH(LENGTH OF ABEND-DATA)
                            NOHANDLE
           END-EXEC
           EXEC CICS HANDLE ABEND
                CANCEL
           END-EXEC
           EXEC CICS ABEND
                ABCODE('9999')
           END-EXEC
           .