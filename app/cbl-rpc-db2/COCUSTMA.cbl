******************************************************************
      * Program     : COCUSTMA.CBL
      * Application : CardDemo
      * Type        : COBOL RPC Program
      * Function    : Customer lookup that chains to COACCNTA
      * Description : Gets customer data then calls COACCNTA
      *               (DB2 Version - Simplified COMMAREA)
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     COCUSTMA.
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
         05 WS-PGMNAME                 PIC X(08) VALUE 'COCUSTMA'.
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
      *      DB2 Host Variables for CUSTDAT Operations
      ******************************************************************
         05  WS-CUST-HOST-VARS.
           10  HV-CUST-ID              PIC S9(09) COMP-3.
           10  HV-CUST-FIRST-NAME      PIC X(25).
           10  HV-CUST-MIDDLE-NAME     PIC X(25).
           10  HV-CUST-LAST-NAME       PIC X(25).
           10  HV-CUST-ADDR-LINE-1     PIC X(50).
           10  HV-CUST-ADDR-LINE-2     PIC X(50).
           10  HV-CUST-ADDR-LINE-3     PIC X(50).
           10  HV-CUST-ADDR-STATE-CD   PIC X(02).
           10  HV-CUST-ADDR-COUNTRY-CD PIC X(03).
           10  HV-CUST-ADDR-ZIP        PIC X(10).
           10  HV-CUST-PHONE-NUM-1     PIC X(15).
           10  HV-CUST-PHONE-NUM-2     PIC X(15).
           10  HV-CUST-SSN             PIC S9(09) COMP-3.
           10  HV-CUST-GOVT-ISSUED-ID  PIC X(20).
           10  HV-CUST-DOB-YYYY-MM-DD  PIC X(10).
           10  HV-CUST-EFT-ACCOUNT-ID  PIC X(15).
           10  HV-CUST-PRI-CARD-HOLDER-IND PIC X(01).
           10  HV-CUST-FICO-CREDIT-SCORE PIC S9(03) COMP-3.

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
          05 LIT-CUSTTABLENAME         PIC X(8) VALUE 'CUSTDAT '.

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
      *    Output: Customer data
           05  LK-OUTPUT-CUSTOMER.
               10  LK-OUT-CUST-ID       PIC S9(09) COMP-3.
               10  LK-OUT-FIRST-NAME    PIC X(25).
               10  LK-OUT-MIDDLE-NAME   PIC X(25).
               10  LK-OUT-LAST-NAME     PIC X(25).
               10  LK-OUT-ADDR-LINE-1   PIC X(50).
               10  LK-OUT-ADDR-LINE-2   PIC X(50).
               10  LK-OUT-ADDR-LINE-3   PIC X(50).
               10  LK-OUT-ADDR-STATE-CD PIC X(02).
               10  LK-OUT-ADDR-COUNTRY-CD PIC X(03).
               10  LK-OUT-ADDR-ZIP      PIC X(10).
               10  LK-OUT-PHONE-NUM-1   PIC X(15).
               10  LK-OUT-PHONE-NUM-2   PIC X(15).
               10  LK-OUT-SSN           PIC S9(09) COMP-3.
               10  LK-OUT-GOVT-ISSUED-ID PIC X(20).
               10  LK-OUT-DOB-YYYY-MM-DD PIC X(10).
               10  LK-OUT-EFT-ACCOUNT-ID PIC X(15).
               10  LK-OUT-PRI-CARD-HOLDER-IND PIC X(01).
               10  LK-OUT-FICO-CREDIT-SCORE PIC S9(03) COMP-3.

      *----------------------------------------------------------------*
      *                       PROCEDURE DIVISION
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

       MAIN-PARA.
           INITIALIZE LK-OUTPUT-STATUS
                      LK-OUTPUT-CUSTOMER


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
      *        Get customer details
               MOVE LK-INPUT-CUST-ID TO HV-CUST-ID
               PERFORM GET-CUSTOMER-DETAILS
               IF RC-SUCCESS
      *            Chain to COACCNTA
                   PERFORM CALL-COACCNTA
               END-IF
           END-IF

           GOBACK.

      *----------------------------------------------------------------*
      *                      GET-CUSTOMER-DETAILS
      *----------------------------------------------------------------*
       GET-CUSTOMER-DETAILS.
      *    Retrieve customer details from CUSTDAT table
           EXEC SQL
                SELECT CUST_ID,
                       CUST_FIRST_NAME,
                       CUST_MIDDLE_NAME,
                       CUST_LAST_NAME,
                       CUST_ADDR_LINE_1,
                       CUST_ADDR_LINE_2,
                       CUST_ADDR_LINE_3,
                       CUST_ADDR_STATE_CD,
                       CUST_ADDR_COUNTRY_CD,
                       CUST_ADDR_ZIP,
                       CUST_PHONE_NUM_1,
                       CUST_PHONE_NUM_2,
                       CUST_SSN,
                       CUST_GOVT_ISSUED_ID,
                       CUST_DOB_YYYY_MM_DD,
                       CUST_EFT_ACCOUNT_ID,
                       CUST_PRI_CARD_HOLDER_IND,
                       CUST_FICO_CREDIT_SCORE
                INTO   :HV-CUST-ID,
                       :HV-CUST-FIRST-NAME,
                       :HV-CUST-MIDDLE-NAME,
                       :HV-CUST-LAST-NAME,
                       :HV-CUST-ADDR-LINE-1,
                       :HV-CUST-ADDR-LINE-2,
                       :HV-CUST-ADDR-LINE-3,
                       :HV-CUST-ADDR-STATE-CD,
                       :HV-CUST-ADDR-COUNTRY-CD,
                       :HV-CUST-ADDR-ZIP,
                       :HV-CUST-PHONE-NUM-1,
                       :HV-CUST-PHONE-NUM-2,
                       :HV-CUST-SSN,
                       :HV-CUST-GOVT-ISSUED-ID,
                       :HV-CUST-DOB-YYYY-MM-DD,
                       :HV-CUST-EFT-ACCOUNT-ID,
                       :HV-CUST-PRI-CARD-HOLDER-IND,
                       :HV-CUST-FICO-CREDIT-SCORE
                FROM   CUSTDAT
                WHERE  CUST_ID = :HV-CUST-ID
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
      *            Copy customer data to output
                   MOVE HV-CUST-ID              TO LK-OUT-CUST-ID
                   MOVE HV-CUST-FIRST-NAME      TO LK-OUT-FIRST-NAME
                   MOVE HV-CUST-MIDDLE-NAME     TO LK-OUT-MIDDLE-NAME
                   MOVE HV-CUST-LAST-NAME       TO LK-OUT-LAST-NAME
                   MOVE HV-CUST-ADDR-LINE-1     TO LK-OUT-ADDR-LINE-1
                   MOVE HV-CUST-ADDR-LINE-2     TO LK-OUT-ADDR-LINE-2
                   MOVE HV-CUST-ADDR-LINE-3     TO LK-OUT-ADDR-LINE-3
                   MOVE HV-CUST-ADDR-STATE-CD   TO LK-OUT-ADDR-STATE-CD
                   MOVE HV-CUST-ADDR-COUNTRY-CD
                    TO LK-OUT-ADDR-COUNTRY-CD
                   MOVE HV-CUST-ADDR-ZIP        TO LK-OUT-ADDR-ZIP
                   MOVE HV-CUST-PHONE-NUM-1     TO LK-OUT-PHONE-NUM-1
                   MOVE HV-CUST-PHONE-NUM-2     TO LK-OUT-PHONE-NUM-2
                   MOVE HV-CUST-SSN             TO LK-OUT-SSN
                   MOVE HV-CUST-GOVT-ISSUED-ID  TO LK-OUT-GOVT-ISSUED-ID
                   MOVE HV-CUST-DOB-YYYY-MM-DD  TO LK-OUT-DOB-YYYY-MM-DD
                   MOVE HV-CUST-EFT-ACCOUNT-ID  TO LK-OUT-EFT-ACCOUNT-ID
                   MOVE HV-CUST-PRI-CARD-HOLDER-IND TO
                        LK-OUT-PRI-CARD-HOLDER-IND
                   MOVE HV-CUST-FICO-CREDIT-SCORE TO
                        LK-OUT-FICO-CREDIT-SCORE

                   MOVE 'Customer retrieved successfully'
                   TO LK-OUT-MESSAGE
               WHEN +100
                   SET ERR-FLG-ON TO TRUE
                   SET RC-NOT-FOUND TO TRUE
                   MOVE 'Customer not found' TO LK-OUT-MESSAGE
                   PERFORM TEMP-DUMP-OUTPUT-STRUCTURE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   SET RC-DATABASE-ERROR TO TRUE
                   MOVE 'SELECT'                TO ERROR-OPNAME
                   MOVE LIT-CUSTTABLENAME       TO ERROR-TABLE
                   MOVE SQLCODE                 TO ERROR-SQLCODE
                   MOVE WS-DB2-ERROR-MESSAGE    TO LK-OUT-MESSAGE
                   PERFORM TEMP-DUMP-OUTPUT-STRUCTURE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      CALL-COACCNTA
      *----------------------------------------------------------------*
       CALL-COACCNTA.
           EXEC CICS LINK
                PROGRAM('COACCNTA')
                COMMAREA(LK-INPUT-CUST-ID)
                LENGTH(LENGTH OF LK-INPUT-CUST-ID)
                RESP(WS-CICS-RESP)
                RESP2(WS-CICS-REAS)
           END-EXEC.

      *----------------------------------------------------------------*
      *                    TEMP-DUMP-OUTPUT-STRUCTURE
      *----------------------------------------------------------------*
       TEMP-DUMP-OUTPUT-STRUCTURE.
      * TEMP: Remove this paragraph after testing is complete
           DISPLAY 'COCUSTMA: === TEMP OUTPUT DUMP ==='
           DISPLAY 'COCUSTMA: CUST-ID: ' LK-OUT-CUST-ID
           DISPLAY 'COCUSTMA: FIRST: ' LK-OUT-FIRST-NAME
           DISPLAY 'COCUSTMA: LAST: ' LK-OUT-LAST-NAME
           DISPLAY 'COCUSTMA: ADDR1: ' LK-OUT-ADDR-LINE-1
           DISPLAY 'COCUSTMA: CITY: ' LK-OUT-ADDR-LINE-2
           DISPLAY 'COCUSTMA: STATE: ' LK-OUT-ADDR-STATE-CD
           DISPLAY 'COCUSTMA: ZIP: ' LK-OUT-ADDR-ZIP
           DISPLAY 'COCUSTMA: PHONE1: ' LK-OUT-PHONE-NUM-1
           DISPLAY 'COCUSTMA: SSN: ' LK-OUT-SSN
           DISPLAY 'COCUSTMA: DOB: ' LK-OUT-DOB-YYYY-MM-DD
           DISPLAY 'COCUSTMA: FICO: ' LK-OUT-FICO-CREDIT-SCORE
           DISPLAY 'COCUSTMA: MESSAGE: ' LK-OUT-MESSAGE
           DISPLAY 'COCUSTMA: === END TEMP DUMP ==='
           DISPLAY 'COCUSTMA: Now calling COACCNTA with CUST-ID: '
                   LK-INPUT-CUST-ID.

      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:12:33 CDT
      *