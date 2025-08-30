******************************************************************
      * Program:     COACTADA.CBL - BUSINESS RULES ONLY              *
      * Layer:       Business logic                                   *
      * Function:    RPC Service for Account Creation (DB2)           *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           COACTADA.
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

      ******************************************************************
      * Input validation flags
      ******************************************************************
         05  WS-INPUT-FLAG                         PIC X(1).
           88  INPUT-OK                            VALUE '0'.
           88  INPUT-ERROR                         VALUE '1'.

      ******************************************************************
      * Account generation work fields
      ******************************************************************
         05  WS-ACCOUNT-WORK-FIELDS.
           10 WS-NEW-ACCT-ID                       PIC 9(11).
           10 WS-CARD-NUMBER                       PIC X(16).

      ******************************************************************
      * Enhanced Date Validation Work Fields
      ******************************************************************
         05  WS-DATE-WORK-FIELDS.
           10 WS-CALCULATION-VARS.
              15 WS-DIV-BY                         PIC S9(4) COMP-3
                                                   VALUE 4.
              15 WS-DIVIDEND                       PIC S9(4) COMP-3
                                                   VALUE 0.
              15 WS-REMAINDER                      PIC S9(4) COMP-3
                                                   VALUE 0.

      ******************************************************************
      * Error Messages
      ******************************************************************
         05  WS-RETURN-MSG                         PIC X(75).
           88  WS-RETURN-MSG-OFF                   VALUE SPACES.
         05  WS-EDIT-VARIABLE-NAME                 PIC X(30).

      ******************************************************************
      * Literals and Constants
      ******************************************************************
       01 WS-LITERALS.
          05 LIT-THISPGM                           PIC X(8)
                                                   VALUE 'COACTADA'.

      ******************************************************************
      * DB2 SQL COMMUNICATION AREA
      ******************************************************************
       EXEC SQL
           INCLUDE SQLCA
       END-EXEC.

      ******************************************************************
      * DB2 HOST VARIABLES
      ******************************************************************
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  HV-ACCOUNT-ID                        PIC X(11).
       01  HV-CUSTOMER-ID                       PIC S9(09) COMP.
       01  HV-SQLCODE-DISPLAY                   PIC S9(09) DISPLAY.
       01  HV-ACCOUNT-INSERT.
           05  HV-NEW-ACCT-ID                   PIC X(11).
           05  HV-ACCT-STATUS                   PIC X(01).
           05  HV-ACCT-CURR-BAL                 PIC S9(10)V99 COMP-3.
           05  HV-ACCT-CREDIT-LMT               PIC S9(10)V99 COMP-3.
           05  HV-ACCT-CASH-LMT                 PIC S9(10)V99 COMP-3.
           05  HV-ACCT-OPEN-DT                  PIC X(10).
           05  HV-ACCT-EXPIRY-DT                PIC X(10).
           05  HV-ACCT-REISSUE-DT               PIC X(10).
           05  HV-ACCT-CYC-CREDIT               PIC S9(10)V99 COMP-3.
           05  HV-ACCT-CYC-DEBIT                PIC S9(10)V99 COMP-3.
           05  HV-ACCT-ZIP                      PIC X(10).
           05  HV-ACCT-GROUP-ID                 PIC X(10).
       01  HV-CUST-VALIDATION.
           05  HV-CUST-EXISTS-ID                PIC S9(09) COMP.
           05  HV-CUST-COUNT                    PIC S9(04) COMP.
       01  HV-ACCOUNT-SEQUENCE.
           05  HV-MAX-ACCT-ID                   PIC S9(11) COMP.
           05  HV-MAX-ACCT-ID-IND               PIC S9(4) COMP.
       01  HV-XREF-INSERT.
           05  HV-XREF-ACCT-ID                  PIC S9(11) COMP.
           05  HV-XREF-CARD-NUM                 PIC X(16).
           05  HV-XREF-CUST-ID                  PIC S9(09) COMP.
       EXEC SQL END DECLARE SECTION END-EXEC.

      ******************************************************************
      * COMMON COPYBOOKS
      ******************************************************************
       COPY CVACT01Y.
       COPY CVCUS01Y.

      *****************************************************************
      *    Generic date edit variables CCYYMMDD
      ******************************************************************
       01 DATE-VALID.
         05 DATE-VALID-LABEL.
           COPY CSUTLDWY.

       LINKAGE SECTION.
      ******************************************************************
      * COMMAREA Structure for Account Creation RPC Program
      * UI sends NUMERIC amounts already converted
      ******************************************************************
       01 DFHCOMMAREA.
          05 LK-ACCT-INPUT-PARMS.
             10 LK-ACCT-IN-OPERATION       PIC X(01).
                88 ACCT-OP-CREATE          VALUE 'C'.
                88 ACCT-OP-VALIDATE        VALUE 'V'.
             10 LK-ACCT-IN-CUST-ID         PIC X(09).
             10 LK-ACCT-IN-ACCT-DATA.
                15 LK-ACCT-IN-STATUS         PIC X(01).
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

       PROCEDURE DIVISION USING DFHCOMMAREA.

           EXEC CICS HANDLE ABEND
              LABEL(ABEND-ROUTINE)
           END-EXEC.

      ******************************************************************
      * Main processing logic - FIXED ORDER TO FOLLOW SCREEN
      ******************************************************************
       MAIN-PARA.

           INITIALIZE LK-ACCT-OUTPUT-STATUS
                      LK-ACCT-OUTPUT-DATA
                      WS-MISC-STORAGE

           SET ACCT-RC-SUCCESS TO TRUE
           MOVE SPACES TO LK-ACCT-OUT-MESSAGE
           MOVE SPACES TO LK-ACCT-OUT-ERROR-FIELD
           SET INPUT-OK TO TRUE
           SET WS-RETURN-MSG-OFF TO TRUE

      * 1. Customer ID first
           PERFORM 1100-EDIT-CUSTOMER-ID
              THRU 1100-EDIT-CUSTOMER-ID-EXIT
           IF INPUT-ERROR
              SET ACCT-RC-INPUT-ERROR TO TRUE
              MOVE WS-RETURN-MSG TO LK-ACCT-OUT-MESSAGE
              MOVE 'CUST-ID' TO LK-ACCT-OUT-ERROR-FIELD
              GOBACK
           END-IF

      * 2. Check customer exists
           PERFORM 2000-VALID-CUSTOMER
              THRU 2000-VALID-CUSTOMER-EXIT
           IF NOT ACCT-RC-SUCCESS
              MOVE WS-RETURN-MSG TO LK-ACCT-OUT-MESSAGE
              MOVE 'CUST-ID' TO LK-ACCT-OUT-ERROR-FIELD
              GOBACK
           END-IF

      * 3. Account Status (follows customer on screen)
           PERFORM 1310-EDIT-ACCOUNT-STATUS
              THRU 1310-EDIT-ACCOUNT-STATUS-EXIT
           IF INPUT-ERROR
              SET ACCT-RC-INPUT-ERROR TO TRUE
              MOVE WS-RETURN-MSG TO LK-ACCT-OUT-MESSAGE
              MOVE 'ACCT-STATUS' TO LK-ACCT-OUT-ERROR-FIELD
              GOBACK
           END-IF

      * 4. Amounts (Credit, Cash, Current Balance, Cycle amounts)
           PERFORM 1200-VALID-AMOUNTS
              THRU 1200-VALID-AMOUNTS-EXIT
           IF INPUT-ERROR
              SET ACCT-RC-INPUT-ERROR TO TRUE
              MOVE WS-RETURN-MSG TO LK-ACCT-OUT-MESSAGE
              PERFORM 1290-SET-ERROR-FIELD
                 THRU 1290-SET-ERROR-FIELD-EXIT
              GOBACK
           END-IF

      * 5. Dates (Open, Expiry, Reissue)
           PERFORM 1400-EDIT-DATES-ENHANCED
              THRU 1400-EDIT-DATES-ENHANCED-EXIT
           IF INPUT-ERROR
              SET ACCT-RC-INPUT-ERROR TO TRUE
              MOVE WS-RETURN-MSG TO LK-ACCT-OUT-MESSAGE
              PERFORM 1290-SET-ERROR-FIELD
                 THRU 1290-SET-ERROR-FIELD-EXIT
              GOBACK
           END-IF

      * 6. ZIP Code (appears near end of screen)
           PERFORM 1330-EDIT-ZIP-CODE
              THRU 1330-EDIT-ZIP-CODE-EXIT
           IF INPUT-ERROR
              SET ACCT-RC-INPUT-ERROR TO TRUE
              MOVE WS-RETURN-MSG TO LK-ACCT-OUT-MESSAGE
              MOVE 'ZIP-CODE' TO LK-ACCT-OUT-ERROR-FIELD
              GOBACK
           END-IF

      * 7. Account Group (last field)
           PERFORM 1320-EDIT-GROUP-ID
              THRU 1320-EDIT-GROUP-ID-EXIT
           IF INPUT-ERROR
              SET ACCT-RC-INPUT-ERROR TO TRUE
              MOVE WS-RETURN-MSG TO LK-ACCT-OUT-MESSAGE
              MOVE 'GROUP-ID' TO LK-ACCT-OUT-ERROR-FIELD
              GOBACK
           END-IF

      * 8. Create new account
           PERFORM 9000-CREATE-ACCOUNT
              THRU 9000-CREATE-ACCOUNT-EXIT

           IF NOT ACCT-RC-SUCCESS
              MOVE WS-RETURN-MSG TO LK-ACCT-OUT-MESSAGE
              GOBACK
           END-IF

           MOVE WS-NEW-ACCT-ID TO LK-ACCT-OUT-NEW-ACCT-ID
           STRING 'Account ' WS-NEW-ACCT-ID ' created successfully.'
                  DELIMITED BY SIZE
                  INTO LK-ACCT-OUT-MESSAGE
           END-STRING

           GOBACK.

      ******************************************************************
      * Customer ID validation
      ******************************************************************
       1100-EDIT-CUSTOMER-ID.
           MOVE 'Customer ID' TO WS-EDIT-VARIABLE-NAME

           IF LK-ACCT-IN-CUST-ID EQUAL SPACES
           OR LK-ACCT-IN-CUST-ID EQUAL LOW-VALUES
              SET INPUT-ERROR TO TRUE
              STRING
                FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                ' is required.'
                DELIMITED BY SIZE
                INTO WS-RETURN-MSG
              END-STRING
              GO TO 1100-EDIT-CUSTOMER-ID-EXIT
           END-IF

           IF LK-ACCT-IN-CUST-ID IS NOT NUMERIC
              SET INPUT-ERROR TO TRUE
              STRING
                FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                ' must be numeric.'
                DELIMITED BY SIZE
                INTO WS-RETURN-MSG
              END-STRING
              GO TO 1100-EDIT-CUSTOMER-ID-EXIT
           END-IF

           IF FUNCTION NUMVAL(LK-ACCT-IN-CUST-ID) = 0
              SET INPUT-ERROR TO TRUE
              STRING
                FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                ' must not be zero.'
                DELIMITED BY SIZE
                INTO WS-RETURN-MSG
              END-STRING
           END-IF
           .
       1100-EDIT-CUSTOMER-ID-EXIT.
           EXIT.

      ******************************************************************
      * AMOUNT BUSINESS RULE VALIDATION - Core business logic
      ******************************************************************
       1200-VALID-AMOUNTS.

      *    Business Rule 1: Credit Limit is required and > 0
           PERFORM 1210-VALID-CREDIT-LIMIT
              THRU 1210-VALID-CREDIT-LIMIT-EXIT
           IF INPUT-ERROR
              GO TO 1200-VALID-AMOUNTS-EXIT
           END-IF

      *    Business Rule 2: Cash Limit can't be higher than Credit Limit
           PERFORM 1220-VALID-CASH-LIMIT
              THRU 1220-VALID-CASH-LIMIT-EXIT
           IF INPUT-ERROR
              GO TO 1200-VALID-AMOUNTS-EXIT
           END-IF

      *    Business Rule 3: Current Balance can't be higher than Credit Limit
           PERFORM 1230-VALID-CURR-BAL
              THRU 1230-VALID-CURR-BAL-EXIT
           IF INPUT-ERROR
              GO TO 1200-VALID-AMOUNTS-EXIT
           END-IF

      *    Business Rule 4: Cycle amounts validation
           PERFORM 1240-VALID-CYCLE
              THRU 1240-VALID-CYCLE-EXIT

           .
       1200-VALID-AMOUNTS-EXIT.
           EXIT.

      ******************************************************************
      * Credit Limit Business Rules
      ******************************************************************
       1210-VALID-CREDIT-LIMIT.
           MOVE 'Credit Limit' TO WS-EDIT-VARIABLE-NAME

      *    Credit Limit must be greater than zero for new accounts
           IF LK-ACCT-IN-CREDIT-LIMIT <= ZERO
              SET INPUT-ERROR TO TRUE
              STRING
                FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                ' must be greater than zero.'
                DELIMITED BY SIZE
                INTO WS-RETURN-MSG
              END-STRING
           END-IF
           .
       1210-VALID-CREDIT-LIMIT-EXIT.
           EXIT.

      ******************************************************************
      * Cash Limit Business Rules
      ******************************************************************
       1220-VALID-CASH-LIMIT.
           MOVE 'Cash Limit' TO WS-EDIT-VARIABLE-NAME

      *    Cash Limit can't be higher than Credit Limit
           IF LK-ACCT-IN-CASH-LIMIT > LK-ACCT-IN-CREDIT-LIMIT
              SET INPUT-ERROR TO TRUE
              STRING
                'Cash Limit cannot be higher than Credit Limit.'
                DELIMITED BY SIZE
                INTO WS-RETURN-MSG
              END-STRING
           END-IF
           .
       1220-VALID-CASH-LIMIT-EXIT.
           EXIT.

      ******************************************************************
      * Current Balance Business Rules
      ******************************************************************
       1230-VALID-CURR-BAL.
           MOVE 'Current Balance' TO WS-EDIT-VARIABLE-NAME

      *    Current Balance can't be higher than Credit Limit
           IF LK-ACCT-IN-CURR-BAL > LK-ACCT-IN-CREDIT-LIMIT
              SET INPUT-ERROR TO TRUE
              STRING
                'Current Balance cannot be higher than Credit Limit.'
                DELIMITED BY SIZE
                INTO WS-RETURN-MSG
              END-STRING
           END-IF
           .
       1230-VALID-CURR-BAL-EXIT.
           EXIT.

      ******************************************************************
      * Cycle Amount Business Rules
      ******************************************************************
       1240-VALID-CYCLE.

      *    Cycle Credit can't be negative
           IF LK-ACCT-IN-CYC-CREDIT < ZERO
              SET INPUT-ERROR TO TRUE
              MOVE 'Cycle Credit cannot be negative.' TO WS-RETURN-MSG
              MOVE 'Cycle Credit' TO WS-EDIT-VARIABLE-NAME
              GO TO 1240-VALID-CYCLE-EXIT
           END-IF

      *    Cycle Debit can't be negative
           IF LK-ACCT-IN-CYC-DEBIT < ZERO
              SET INPUT-ERROR TO TRUE
              MOVE 'Cycle Debit cannot be negative.' TO WS-RETURN-MSG
              MOVE 'Cycle Debit' TO WS-EDIT-VARIABLE-NAME
              GO TO 1240-VALID-CYCLE-EXIT
           END-IF

      *    Cycle Credit can't be over Current Balance
           IF LK-ACCT-IN-CYC-CREDIT > LK-ACCT-IN-CURR-BAL
              SET INPUT-ERROR TO TRUE
              MOVE 'Cycle Credit cannot exceed Current Balance.'
               TO WS-RETURN-MSG
              MOVE 'Cycle Credit' TO WS-EDIT-VARIABLE-NAME
              GO TO 1240-VALID-CYCLE-EXIT
           END-IF

      *    Cycle Debit can't be over Current Balance
           IF LK-ACCT-IN-CYC-DEBIT > LK-ACCT-IN-CURR-BAL
              SET INPUT-ERROR TO TRUE
              MOVE 'Cycle Debit cannot exceed Current Balance.'
               TO WS-RETURN-MSG
              MOVE 'Cycle Debit' TO WS-EDIT-VARIABLE-NAME
           END-IF
           .
       1240-VALID-CYCLE-EXIT.
           EXIT.

      ******************************************************************
      * Account Status validation - FIXED: NO AUTO DEFAULT
      ******************************************************************
       1310-EDIT-ACCOUNT-STATUS.
           MOVE 'Account Status' TO WS-EDIT-VARIABLE-NAME

      *    Account Status is REQUIRED - no auto-default
           IF LK-ACCT-IN-STATUS = SPACES OR LOW-VALUES
              SET INPUT-ERROR TO TRUE
              STRING
                FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                ' is required.'
                DELIMITED BY SIZE
                INTO WS-RETURN-MSG
              END-STRING
              GO TO 1310-EDIT-ACCOUNT-STATUS-EXIT
           END-IF

           IF LK-ACCT-IN-STATUS NOT = 'Y' AND
              LK-ACCT-IN-STATUS NOT = 'N'
              SET INPUT-ERROR TO TRUE
              STRING
                FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                ' must be Y (Active) or N (Inactive).'
                DELIMITED BY SIZE
                INTO WS-RETURN-MSG
              END-STRING
           END-IF
           .
       1310-EDIT-ACCOUNT-STATUS-EXIT.
           EXIT.

      ******************************************************************
      * Account Group ID is MANDATORY
      ******************************************************************
       1320-EDIT-GROUP-ID.
           MOVE 'Account Group ID' TO WS-EDIT-VARIABLE-NAME

      *    Check if Group ID is provided (MANDATORY)
           IF FUNCTION TRIM(LK-ACCT-IN-GROUP-ID) EQUAL SPACES
              SET INPUT-ERROR TO TRUE
              STRING
                FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                ' is required.'
                DELIMITED BY SIZE
                INTO WS-RETURN-MSG
              END-STRING
           END-IF
           .
       1320-EDIT-GROUP-ID-EXIT.
           EXIT.

      ******************************************************************
      * ZIP Code validation - FIXED: Just trim spaces and validate
      ******************************************************************
       1330-EDIT-ZIP-CODE.
           MOVE 'ZIP Code' TO WS-EDIT-VARIABLE-NAME

      *    Check if ZIP Code is provided (MANDATORY)
           IF FUNCTION TRIM(LK-ACCT-IN-ZIP-CODE) EQUAL SPACES
              SET INPUT-ERROR TO TRUE
              STRING
                FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                ' is required.'
                DELIMITED BY SIZE
                INTO WS-RETURN-MSG
              END-STRING
              GO TO 1330-EDIT-ZIP-CODE-EXIT
           END-IF

      *    ZIP Code must be numeric (after trimming spaces)
           IF FUNCTION TRIM(LK-ACCT-IN-ZIP-CODE) IS NOT NUMERIC
              SET INPUT-ERROR TO TRUE
              STRING
                FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                ' must be numeric.'
                DELIMITED BY SIZE
                INTO WS-RETURN-MSG
              END-STRING
           END-IF
           .
       1330-EDIT-ZIP-CODE-EXIT.
           EXIT.

      ******************************************************************
      * Enhanced Date Validation
      ******************************************************************
       1400-EDIT-DATES-ENHANCED.

           PERFORM 1410-VALID-OPEN-DATE
              THRU 1410-VALID-OPEN-DATE-EXIT
           IF INPUT-ERROR
              GO TO 1400-EDIT-DATES-ENHANCED-EXIT
           END-IF

           PERFORM 1420-VALID-EXPIRY-DATE
              THRU 1420-VALID-EXPIRY-DATE-EXIT
           IF INPUT-ERROR
              GO TO 1400-EDIT-DATES-ENHANCED-EXIT
           END-IF

           PERFORM 1430-VALID-REISSUE-DATE
              THRU 1430-VALID-REISSUE-DATE-EXIT
           IF INPUT-ERROR
              GO TO 1400-EDIT-DATES-ENHANCED-EXIT
           END-IF

           PERFORM 1440-CROSS-VALID-DATES
              THRU 1440-CROSS-VALID-DATES-EXIT

           .
       1400-EDIT-DATES-ENHANCED-EXIT.
           EXIT.

      ******************************************************************
      * Validate Open Date - REQUIRED
      ******************************************************************
       1410-VALID-OPEN-DATE.
           MOVE 'Open Date' TO WS-EDIT-VARIABLE-NAME

           IF LK-ACCT-IN-OPEN-YEAR = SPACES OR LOW-VALUES
           OR LK-ACCT-IN-OPEN-MON = SPACES OR LOW-VALUES
           OR LK-ACCT-IN-OPEN-DAY = SPACES OR LOW-VALUES
              SET INPUT-ERROR TO TRUE
              STRING
                FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                ' is required for new accounts.'
                DELIMITED BY SIZE
                INTO WS-RETURN-MSG
              END-STRING
              GO TO 1410-VALID-OPEN-DATE-EXIT
           END-IF

           STRING LK-ACCT-IN-OPEN-YEAR
                  LK-ACCT-IN-OPEN-MON
                  LK-ACCT-IN-OPEN-DAY
           DELIMITED BY SIZE INTO WS-EDIT-DATE-CCYYMMDD

           PERFORM EDIT-DATE-CCYYMMDD
              THRU EDIT-DATE-CCYYMMDD-EXIT

           IF INPUT-ERROR
              GO TO 1410-VALID-OPEN-DATE-EXIT
           END-IF

           PERFORM 1412-OPEN-DATE-FUTURE
              THRU 1412-OPEN-DATE-FUTURE-EXIT

           .
       1410-VALID-OPEN-DATE-EXIT.
           EXIT.

       1412-OPEN-DATE-FUTURE.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-YYYYMMDD

           COMPUTE WS-EDIT-DATE-BINARY =
               FUNCTION INTEGER-OF-DATE (WS-EDIT-DATE-CCYYMMDD-N)
           COMPUTE WS-CURRENT-DATE-BINARY =
               FUNCTION INTEGER-OF-DATE(WS-CURRENT-DATE-YYYYMMDD-N)

           IF WS-EDIT-DATE-BINARY > WS-CURRENT-DATE-BINARY
              SET INPUT-ERROR TO TRUE
              STRING
                FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                ' cannot be in the future.'
                DELIMITED BY SIZE
                INTO WS-RETURN-MSG
              END-STRING
           END-IF
           .
       1412-OPEN-DATE-FUTURE-EXIT.
           EXIT.

      ******************************************************************
      * Validate Expiry Date - REQUIRED
      ******************************************************************
       1420-VALID-EXPIRY-DATE.
           MOVE 'Expiry Date' TO WS-EDIT-VARIABLE-NAME

           IF LK-ACCT-IN-EXP-YEAR = SPACES OR LOW-VALUES
           OR LK-ACCT-IN-EXP-MON = SPACES OR LOW-VALUES
           OR LK-ACCT-IN-EXP-DAY = SPACES OR LOW-VALUES
              SET INPUT-ERROR TO TRUE
              STRING
                FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                ' is required for new accounts.'
                DELIMITED BY SIZE
                INTO WS-RETURN-MSG
              END-STRING
              GO TO 1420-VALID-EXPIRY-DATE-EXIT
           END-IF

           STRING LK-ACCT-IN-EXP-YEAR
                  LK-ACCT-IN-EXP-MON
                  LK-ACCT-IN-EXP-DAY
           DELIMITED BY SIZE INTO WS-EDIT-DATE-CCYYMMDD

           PERFORM EDIT-DATE-CCYYMMDD
              THRU EDIT-DATE-CCYYMMDD-EXIT

           .
       1420-VALID-EXPIRY-DATE-EXIT.
           EXIT.

      ******************************************************************
      * Validate Reissue Date - OPTIONAL
      ******************************************************************
       1430-VALID-REISSUE-DATE.
           MOVE 'Reissue Date' TO WS-EDIT-VARIABLE-NAME

           IF LK-ACCT-IN-REISS-YEAR = SPACES OR LOW-VALUES
           OR LK-ACCT-IN-REISS-MON = SPACES OR LOW-VALUES
           OR LK-ACCT-IN-REISS-DAY = SPACES OR LOW-VALUES
              GO TO 1430-VALID-REISSUE-DATE-EXIT
           END-IF

           STRING LK-ACCT-IN-REISS-YEAR
                  LK-ACCT-IN-REISS-MON
                  LK-ACCT-IN-REISS-DAY
           DELIMITED BY SIZE INTO WS-EDIT-DATE-CCYYMMDD

           PERFORM EDIT-DATE-CCYYMMDD
              THRU EDIT-DATE-CCYYMMDD-EXIT

           .
       1430-VALID-REISSUE-DATE-EXIT.
           EXIT.

      ******************************************************************
      * Cross-validate dates (business logic)
      ******************************************************************
       1440-CROSS-VALID-DATES.

           IF (LK-ACCT-IN-OPEN-YEAR = SPACES OR LOW-VALUES)
           OR (LK-ACCT-IN-EXP-YEAR = SPACES OR LOW-VALUES)
              GO TO 1440-CROSS-VALID-DATES-EXIT
           END-IF

           STRING LK-ACCT-IN-OPEN-YEAR
                  LK-ACCT-IN-OPEN-MON
                  LK-ACCT-IN-OPEN-DAY
           DELIMITED BY SIZE INTO WS-EDIT-DATE-CCYYMMDD

           COMPUTE WS-EDIT-DATE-BINARY =
               FUNCTION INTEGER-OF-DATE (WS-EDIT-DATE-CCYYMMDD-N)

           STRING LK-ACCT-IN-EXP-YEAR
                  LK-ACCT-IN-EXP-MON
                  LK-ACCT-IN-EXP-DAY
           DELIMITED BY SIZE INTO WS-CURRENT-DATE-YYYYMMDD

           COMPUTE WS-CURRENT-DATE-BINARY =
               FUNCTION INTEGER-OF-DATE (WS-CURRENT-DATE-YYYYMMDD-N)

           IF WS-EDIT-DATE-BINARY <= WS-CURRENT-DATE-BINARY
              CONTINUE
           ELSE
              SET INPUT-ERROR TO TRUE
              MOVE 'Expiry date must be after open date.'
                TO WS-RETURN-MSG
           END-IF

           .
       1440-CROSS-VALID-DATES-EXIT.
           EXIT.

      ******************************************************************
      * Enhanced error field mapping
      ******************************************************************
       1290-SET-ERROR-FIELD.
           MOVE SPACES TO LK-ACCT-OUT-ERROR-FIELD

           EVALUATE TRUE
               WHEN WS-EDIT-VARIABLE-NAME = 'Customer ID'
                   MOVE 'CUST-ID' TO LK-ACCT-OUT-ERROR-FIELD
               WHEN WS-EDIT-VARIABLE-NAME = 'Credit Limit'
                   MOVE 'CREDIT-LIMIT' TO LK-ACCT-OUT-ERROR-FIELD
               WHEN WS-EDIT-VARIABLE-NAME = 'Cash Limit'
                   MOVE 'CASH-LIMIT' TO LK-ACCT-OUT-ERROR-FIELD
               WHEN WS-EDIT-VARIABLE-NAME = 'Current Balance'
                   MOVE 'CURR-BAL' TO LK-ACCT-OUT-ERROR-FIELD
               WHEN WS-EDIT-VARIABLE-NAME = 'Cycle Credit'
                   MOVE 'CYC-CREDIT' TO LK-ACCT-OUT-ERROR-FIELD
               WHEN WS-EDIT-VARIABLE-NAME = 'Cycle Debit'
                   MOVE 'CYC-DEBIT' TO LK-ACCT-OUT-ERROR-FIELD
               WHEN WS-EDIT-VARIABLE-NAME = 'Account Status'
                   MOVE 'ACCT-STATUS' TO LK-ACCT-OUT-ERROR-FIELD
               WHEN WS-EDIT-VARIABLE-NAME = 'Open Date'
                   MOVE 'OPEN-YEAR' TO LK-ACCT-OUT-ERROR-FIELD
               WHEN WS-EDIT-VARIABLE-NAME = 'Expiry Date'
                   MOVE 'EXP-YEAR' TO LK-ACCT-OUT-ERROR-FIELD
               WHEN WS-EDIT-VARIABLE-NAME = 'Reissue Date'
                   MOVE 'REISS-YEAR' TO LK-ACCT-OUT-ERROR-FIELD
               WHEN WS-EDIT-VARIABLE-NAME = 'Account Group ID'
                   MOVE 'GROUP-ID' TO LK-ACCT-OUT-ERROR-FIELD
               WHEN WS-EDIT-VARIABLE-NAME = 'ZIP Code'
                   MOVE 'ZIP-CODE' TO LK-ACCT-OUT-ERROR-FIELD
               WHEN WS-EDIT-VARIABLE-NAME = 'Year'
                   IF LK-ACCT-IN-OPEN-YEAR = SPACES
                       MOVE 'OPEN-YEAR' TO LK-ACCT-OUT-ERROR-FIELD
                   ELSE
                       MOVE 'EXP-YEAR' TO LK-ACCT-OUT-ERROR-FIELD
                   END-IF
               WHEN WS-EDIT-VARIABLE-NAME = 'Month'
                   IF LK-ACCT-IN-OPEN-MON = SPACES
                       MOVE 'OPEN-MONTH' TO LK-ACCT-OUT-ERROR-FIELD
                   ELSE
                       MOVE 'EXP-MONTH' TO LK-ACCT-OUT-ERROR-FIELD
                   END-IF
               WHEN WS-EDIT-VARIABLE-NAME = 'Day'
                   IF LK-ACCT-IN-OPEN-DAY = SPACES
                       MOVE 'OPEN-DAY' TO LK-ACCT-OUT-ERROR-FIELD
                   ELSE
                       MOVE 'EXP-DAY' TO LK-ACCT-OUT-ERROR-FIELD
                   END-IF
               WHEN OTHER
                   MOVE 'CUST-ID' TO LK-ACCT-OUT-ERROR-FIELD
           END-EVALUATE
           .
       1290-SET-ERROR-FIELD-EXIT.
           EXIT.

      ******************************************************************
      * Validate Customer Exists
      ******************************************************************
       2000-VALID-CUSTOMER.
           COMPUTE HV-CUST-EXISTS-ID = FUNCTION
           NUMVAL(LK-ACCT-IN-CUST-ID)

           EXEC SQL
                SELECT COUNT(*)
                INTO :HV-CUST-COUNT
                FROM CUSTDAT
                WHERE CUST_ID = :HV-CUST-EXISTS-ID
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                  IF HV-CUST-COUNT = 0
                     SET ACCT-RC-CUSTOMER-NOT-FOUND TO TRUE
                     MOVE SQLCODE TO HV-SQLCODE-DISPLAY
                     STRING
                     'Customer ID '
                      LK-ACCT-IN-CUST-ID
                     ' not found. Create customer first (F4).'
                     DELIMITED BY SIZE
                     INTO WS-RETURN-MSG
                     END-STRING
                  END-IF
               WHEN OTHER
                  SET ACCT-RC-DATABASE-ERROR TO TRUE
                  MOVE SQLCODE TO HV-SQLCODE-DISPLAY
                  STRING
                  'Error validating customer. SQLCODE:'
                  HV-SQLCODE-DISPLAY
                  DELIMITED BY SIZE
                  INTO WS-RETURN-MSG
                  END-STRING
           END-EVALUATE
           .
       2000-VALID-CUSTOMER-EXIT.
           EXIT.

      ******************************************************************
      * Create New Account
      ******************************************************************
       9000-CREATE-ACCOUNT.

           PERFORM 9100-GENERATE-ACCOUNT-ID
              THRU 9100-GENERATE-ACCOUNT-ID-EXIT

           IF NOT ACCT-RC-SUCCESS
              GO TO 9000-CREATE-ACCOUNT-EXIT
           END-IF

           PERFORM 9200-INSERT-ACCOUNT
              THRU 9200-INSERT-ACCOUNT-EXIT

           IF ACCT-RC-SUCCESS
              PERFORM 9300-UPDATE-CXACAIX
                 THRU 9300-UPDATE-CXACAIX-EXIT
           END-IF

           IF ACCT-RC-SUCCESS
              EXEC SQL COMMIT WORK END-EXEC
           ELSE
              EXEC SQL ROLLBACK WORK END-EXEC
           END-IF
           .
       9000-CREATE-ACCOUNT-EXIT.
           EXIT.

      ******************************************************************
      * Generate new account ID
      ******************************************************************
       9100-GENERATE-ACCOUNT-ID.

           EXEC SQL
                SELECT MAX(ACCT_ID)
                INTO :HV-MAX-ACCT-ID :HV-MAX-ACCT-ID-IND
                FROM ACCTDAT
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
               WHEN 100
                  IF HV-MAX-ACCT-ID-IND = -1
                  OR HV-MAX-ACCT-ID = 0
                     MOVE 10000000000 TO HV-MAX-ACCT-ID
                  END-IF
                  ADD 1 TO HV-MAX-ACCT-ID
                  MOVE HV-MAX-ACCT-ID TO WS-NEW-ACCT-ID
               WHEN OTHER
                  SET ACCT-RC-DATABASE-ERROR TO TRUE
                  MOVE SQLCODE TO HV-SQLCODE-DISPLAY
                  STRING
                  'Error generating account ID. SQLCODE:'
                  HV-SQLCODE-DISPLAY
                  DELIMITED BY SIZE
                  INTO WS-RETURN-MSG
                  END-STRING
           END-EVALUATE
           .
       9100-GENERATE-ACCOUNT-ID-EXIT.
           EXIT.

      ******************************************************************
      * Insert new account record - WITH ALL REQUIRED FIELDS
      ******************************************************************
       9200-INSERT-ACCOUNT.

           MOVE WS-NEW-ACCT-ID       TO HV-NEW-ACCT-ID
           MOVE LK-ACCT-IN-STATUS    TO HV-ACCT-STATUS
           MOVE LK-ACCT-IN-CURR-BAL  TO HV-ACCT-CURR-BAL
           MOVE LK-ACCT-IN-CREDIT-LIMIT TO HV-ACCT-CREDIT-LMT
           MOVE LK-ACCT-IN-CASH-LIMIT TO HV-ACCT-CASH-LMT
           MOVE LK-ACCT-IN-CYC-CREDIT TO HV-ACCT-CYC-CREDIT
           MOVE LK-ACCT-IN-CYC-DEBIT TO HV-ACCT-CYC-DEBIT
           MOVE LK-ACCT-IN-ZIP-CODE  TO HV-ACCT-ZIP
           MOVE LK-ACCT-IN-GROUP-ID  TO HV-ACCT-GROUP-ID

           STRING LK-ACCT-IN-OPEN-YEAR
                  '-'
                  LK-ACCT-IN-OPEN-MON
                  '-'
                  LK-ACCT-IN-OPEN-DAY
           DELIMITED BY SIZE INTO HV-ACCT-OPEN-DT

           STRING LK-ACCT-IN-EXP-YEAR
                  '-'
                  LK-ACCT-IN-EXP-MON
                  '-'
                  LK-ACCT-IN-EXP-DAY
           DELIMITED BY SIZE INTO HV-ACCT-EXPIRY-DT

           IF LK-ACCT-IN-REISS-YEAR NOT = SPACES
              STRING LK-ACCT-IN-REISS-YEAR
                     '-'
                     LK-ACCT-IN-REISS-MON
                     '-'
                     LK-ACCT-IN-REISS-DAY
              DELIMITED BY SIZE INTO HV-ACCT-REISSUE-DT
           ELSE
              MOVE SPACES TO HV-ACCT-REISSUE-DT
           END-IF

           EXEC SQL
                INSERT INTO ACCTDAT (
                    ACCT_ID,
                    ACCT_ACTIVE_STATUS,
                    ACCT_CURR_BAL,
                    ACCT_CREDIT_LIMIT,
                    ACCT_CASH_CREDIT_LIMIT,
                    ACCT_OPEN_DATE,
                    ACCT_EXPIRAION_DATE,
                    ACCT_REISSUE_DATE,
                    ACCT_CURR_CYC_CREDIT,
                    ACCT_CURR_CYC_DEBIT,
                    ACCT_ADDR_ZIP,
                    ACCT_GROUP_ID
                ) VALUES (
                    :HV-NEW-ACCT-ID,
                    :HV-ACCT-STATUS,
                    :HV-ACCT-CURR-BAL,
                    :HV-ACCT-CREDIT-LMT,
                    :HV-ACCT-CASH-LMT,
                    :HV-ACCT-OPEN-DT,
                    :HV-ACCT-EXPIRY-DT,
                    :HV-ACCT-REISSUE-DT,
                    :HV-ACCT-CYC-CREDIT,
                    :HV-ACCT-CYC-DEBIT,
                    :HV-ACCT-ZIP,
                    :HV-ACCT-GROUP-ID
                )
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                  CONTINUE
               WHEN OTHER
                  SET ACCT-RC-DATABASE-ERROR TO TRUE
                  MOVE SQLCODE TO HV-SQLCODE-DISPLAY
                  STRING
                  'Error inserting account. SQLCODE:'
                  HV-SQLCODE-DISPLAY
                  DELIMITED BY SIZE
                  INTO WS-RETURN-MSG
                  END-STRING
           END-EVALUATE
           .
       9200-INSERT-ACCOUNT-EXIT.
           EXIT.

      ******************************************************************
      * Update cross-reference table
      ******************************************************************
       9300-UPDATE-CXACAIX.

           STRING '4000' WS-NEW-ACCT-ID '00000'
                  DELIMITED BY SIZE
                  INTO WS-CARD-NUMBER
           END-STRING


           MOVE WS-NEW-ACCT-ID       TO HV-XREF-ACCT-ID
           MOVE WS-CARD-NUMBER       TO HV-XREF-CARD-NUM
           COMPUTE HV-XREF-CUST-ID = FUNCTION NUMVAL(LK-ACCT-IN-CUST-ID)

           EXEC SQL
                INSERT INTO CXACAIX (
                    XREF_ACCT_ID,
                    XREF_CARD_NUM,
                    XREF_CUST_ID
                ) VALUES (
                    :HV-XREF-ACCT-ID,
                    :HV-XREF-CARD-NUM,
                    :HV-XREF-CUST-ID
                )
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                  CONTINUE
               WHEN OTHER
                  SET ACCT-RC-DATABASE-ERROR TO TRUE
                  MOVE SQLCODE TO HV-SQLCODE-DISPLAY
                  STRING
                  'Error updating cross-reference. SQLCODE:'
                  HV-SQLCODE-DISPLAY
                  DELIMITED BY SIZE
                  INTO WS-RETURN-MSG
                  END-STRING
           END-EVALUATE
           .
       9300-UPDATE-CXACAIX-EXIT.
           EXIT.

      ******************************************************************
      * Include the comprehensive date validation procedures
      ******************************************************************
       COPY CSUTLDPL.

       ABEND-ROUTINE.
           SET ACCT-RC-DATABASE-ERROR TO TRUE
           MOVE 'UNEXPECTED ABEND OCCURRED.' TO LK-ACCT-OUT-MESSAGE
           GOBACK
           .