******************************************************************
      * Program:     COACTVWA.CBL                                     *
      * Layer:       Business logic                                   *
      * Function:    RPC Service for Account Read Operations (DB2)    *
      * Description: Stateless read-only service for account/customer *
      ******************************************************************
      * Copyright Amazon.com, Inc. or its affiliates.
      * All Rights Reserved.
      *
      * Licensed under the Apache License, Version 2.0 (the "License").
      * You may not use this file except in compliance with the License.
      * You may obtain a copy of the License at
      *
      *    http://www.apache.org/licenses/LICENSE-2.0
      *
      * Unless required by applicable law or agreed to in writing,
      * software distributed under the License is distributed on an
      * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
      * either express or implied. See the License for the specific
      * language governing permissions and limitations under the License
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           COACTVWA.
       DATE-WRITTEN.
           June 2025.
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
            07 WS-RESP-CD-DISP                   PIC 9(09) VALUE ZEROS.
            07 WS-REAS-CD-DISP                   PIC 9(09) VALUE ZEROS.

      ******************************************************************
      *      Input validation flags
      ******************************************************************
         05  WS-INPUT-FLAG                         PIC X(1).
           88  INPUT-OK                            VALUE '0'.
           88  INPUT-ERROR                         VALUE '1'.

         05  WS-EDIT-ACCT-FLAG                     PIC X(1).
           88  FLG-ACCTFILTER-ISVALID              VALUE '1'.
           88  FLG-ACCTFILTER-NOT-OK               VALUE '0'.
           88  FLG-ACCTFILTER-BLANK                VALUE ' '.

      ******************************************************************
      *      File and data Handling
      ******************************************************************
         05 WS-XREF-RID.
           10  WS-CARD-RID-CARDNUM                 PIC X(16).
           10  WS-CARD-RID-CUST-ID                 PIC 9(09).
           10  WS-CARD-RID-CUST-ID-X REDEFINES
                  WS-CARD-RID-CUST-ID              PIC X(09).
           10  WS-CARD-RID-ACCT-ID                 PIC 9(11).
           10  WS-CARD-RID-ACCT-ID-X REDEFINES
                  WS-CARD-RID-ACCT-ID              PIC X(11).

         05  WS-FILE-READ-FLAGS.
           10 WS-ACCOUNT-MASTER-READ-FLAG          PIC X(1).
              88 FOUND-ACCT-IN-MASTER              VALUE '1'.
           10 WS-CUST-MASTER-READ-FLAG             PIC X(1).
              88 FOUND-CUST-IN-MASTER              VALUE '1'.

      ******************************************************************
      *      Error Messages
      ******************************************************************
         05  WS-RETURN-MSG                         PIC X(75).
           88  WS-RETURN-MSG-OFF                   VALUE SPACES.
           88  DID-NOT-FIND-ACCT-IN-CARDXREF       VALUE
               'Did not find this account in account card xref file'.
           88  DID-NOT-FIND-ACCT-IN-ACCTDAT        VALUE
               'Did not find this account in account master file'.
           88  DID-NOT-FIND-CUST-IN-CUSTDAT        VALUE
               'Did not find associated customer in master file'.

      ******************************************************************
      *      Literals and Constants
      ******************************************************************
       01 WS-LITERALS.
          05 LIT-THISPGM                           PIC X(8)
                                                   VALUE 'COACTVWA'.

      ******************************************************************
      *DB2 SQL COMMUNICATION AREA
      ******************************************************************
       EXEC SQL
           INCLUDE SQLCA
       END-EXEC.

      ******************************************************************
      *DB2 HOST VARIABLES
      ******************************************************************
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  HV-ACCOUNT-ID                        PIC X(11).
       01  HV-CUSTOMER-ID                       PIC S9(09) COMP.
       01  HV-CARD-NUMBER                       PIC X(16).
       01  HV-SQLCODE-DISPLAY                   PIC S9(09) DISPLAY.
       01  HV-ACCOUNT-RECORD.
           05  HV-ACCT-ID                       PIC X(11).
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
       01  HV-CUSTOMER-RECORD.
           05  HV-CUST-ID                       PIC S9(09) COMP.
           05  HV-CUST-FNAME                    PIC X(25).
           05  HV-CUST-MNAME                    PIC X(25).
           05  HV-CUST-LNAME                    PIC X(25).
           05  HV-CUST-ADDR1                    PIC X(50).
           05  HV-CUST-ADDR2                    PIC X(50).
           05  HV-CUST-ADDR3                    PIC X(50).
           05  HV-CUST-STATE                    PIC X(02).
           05  HV-CUST-COUNTRY                  PIC X(03).
           05  HV-CUST-ZIP                      PIC X(10).
           05  HV-CUST-PHONE1                   PIC X(15).
           05  HV-CUST-PHONE2                   PIC X(15).
           05  HV-CUST-SSN                      PIC S9(09) COMP.
           05  HV-CUST-GOVT-ID                  PIC X(20).
           05  HV-CUST-DOB                      PIC X(10).
           05  HV-CUST-EFT-ID                   PIC X(10).
           05  HV-CUST-PRI-HOLDER               PIC X(01).
           05  HV-CUST-FICO                     PIC S9(03) COMP.
       EXEC SQL END DECLARE SECTION END-EXEC.

      *COMMON COPYBOOKS
      *Dataset layouts
      *ACCT RECORD LAYOUT
       COPY CVACT01Y.

      *CARD XREF LAYOUT
       COPY CVACT03Y.

      *CUSTOMER LAYOUT
       COPY CVCUS01Y.

       LINKAGE SECTION.
      ******************************************************************
      * COMMAREA Structure for Account View RPC Program
      ******************************************************************
       01 DFHCOMMAREA.
          05 LK-INPUT-PARMS.
             10 LK-IN-ACCT-ID             PIC X(11).
          05 LK-OUTPUT-STATUS.
             10 LK-OUT-RETURN-CODE        PIC 9(02).
                88 RC-SUCCESS             VALUE 00.
                88 RC-NOT-FOUND           VALUE 01.
                88 RC-INPUT-ERROR         VALUE 03.
                88 RC-DATABASE-ERROR      VALUE 99.
             10 LK-OUT-MESSAGE            PIC X(80).
          05 LK-OUTPUT-DATA.
             10 LK-OUT-ACCT-DATA.
                15 LK-OUT-ACCT-ID              PIC X(11).
                15 LK-OUT-ACCT-ACTIVE-STATUS   PIC X(01).
                15 LK-OUT-ACCT-CREDIT-LIMIT    PIC S9(10)V99.
                15 LK-OUT-ACCT-CASH-LIMIT      PIC S9(10)V99.
                15 LK-OUT-ACCT-CURR-BAL        PIC S9(10)V99.
                15 LK-OUT-ACCT-CURR-CYC-CREDIT PIC S9(10)V99.
                15 LK-OUT-ACCT-CURR-CYC-DEBIT  PIC S9(10)V99.
                15 LK-OUT-ACCT-OPEN-DATE       PIC X(10).
                15 LK-OUT-ACCT-EXPIRATION-DATE PIC X(10).
                15 LK-OUT-ACCT-REISSUE-DATE    PIC X(10).
                15 LK-OUT-ACCT-GROUP-ID        PIC X(10).
                15 LK-OUT-ACCT-CARD-NUM        PIC X(16).
             10 LK-OUT-CUST-DATA.
                15 LK-OUT-CUST-ID              PIC X(9).
                15 LK-OUT-CUST-FIRST-NAME      PIC X(25).
                15 LK-OUT-CUST-MIDDLE-NAME     PIC X(25).
                15 LK-OUT-CUST-LAST-NAME       PIC X(25).
                15 LK-OUT-CUST-SSN             PIC X(9).
                15 LK-OUT-CUST-DOB             PIC X(10).
                15 LK-OUT-CUST-ADDR-LINE-1     PIC X(50).
                15 LK-OUT-CUST-ADDR-LINE-2     PIC X(50).
                15 LK-OUT-CUST-ADDR-LINE-3     PIC X(50).
                15 LK-OUT-CUST-ADDR-STATE-CD   PIC X(2).
                15 LK-OUT-CUST-ADDR-COUNTRY-CD PIC X(3).
                15 LK-OUT-CUST-ADDR-ZIP        PIC X(10).
                15 LK-OUT-CUST-PHONE-NUM-1     PIC X(15).
                15 LK-OUT-CUST-PHONE-NUM-2     PIC X(15).
                15 LK-OUT-CUST-GOVT-ISSUED-ID  PIC X(20).
                15 LK-OUT-CUST-EFT-ACCOUNT-ID  PIC  X(10).
                15 LK-OUT-CUST-PRI-HOLDER-IND  PIC X(1).
                15 LK-OUT-CUST-FICO-SCORE      PIC 9(3).

       PROCEDURE DIVISION USING DFHCOMMAREA.

           EXEC CICS HANDLE ABEND
              LABEL(ABEND-ROUTINE)
           END-EXEC.

      ******************************************************************
      * Main processing logic
      ******************************************************************
       MAIN-PARA.

           INITIALIZE LK-OUTPUT-STATUS
                      LK-OUTPUT-DATA
                      WS-MISC-STORAGE

           SET RC-SUCCESS TO TRUE
           MOVE SPACES TO LK-OUT-MESSAGE
           SET INPUT-OK TO TRUE
           SET WS-RETURN-MSG-OFF TO TRUE

      * Validate input parameters
           PERFORM 1000-VALIDATE-INPUT
              THRU 1000-VALIDATE-INPUT-EXIT

           IF INPUT-ERROR
              SET RC-INPUT-ERROR TO TRUE
              MOVE WS-RETURN-MSG TO LK-OUT-MESSAGE
              GOBACK
           END-IF

      * Read account and customer data
           PERFORM 9000-READ-ACCOUNT-DATA
              THRU 9000-READ-ACCOUNT-DATA-EXIT

           IF NOT RC-SUCCESS
              MOVE WS-RETURN-MSG TO LK-OUT-MESSAGE
              GOBACK
           END-IF

      * Map database data to output structure
           PERFORM 1300-MAP-OUTPUT-DATA
              THRU 1300-MAP-OUTPUT-DATA-EXIT

           MOVE 'Account and customer data retrieved successfully.'
                TO LK-OUT-MESSAGE

           GOBACK.

      ******************************************************************
      * Validate Input Parameters
      ******************************************************************
       1000-VALIDATE-INPUT.
           PERFORM 1100-EDIT-ACCOUNT-ID
              THRU 1100-EDIT-ACCOUNT-ID-EXIT.

       1000-VALIDATE-INPUT-EXIT.
           EXIT.

      ******************************************************************
      * Edit Account ID - similar to COACTUPL validation
      ******************************************************************
       1100-EDIT-ACCOUNT-ID.
           SET FLG-ACCTFILTER-NOT-OK TO TRUE

      *    Not supplied
           IF LK-IN-ACCT-ID EQUAL LOW-VALUES
           OR LK-IN-ACCT-ID EQUAL SPACES
              SET INPUT-ERROR TO TRUE
              SET FLG-ACCTFILTER-BLANK TO TRUE
              MOVE 'Account ID must be supplied' TO WS-RETURN-MSG
              GO TO 1100-EDIT-ACCOUNT-ID-EXIT
           END-IF

      *    Not numeric or zero
           MOVE LK-IN-ACCT-ID TO WS-CARD-RID-ACCT-ID-X
           IF LK-IN-ACCT-ID IS NOT NUMERIC
           OR WS-CARD-RID-ACCT-ID EQUAL ZEROS
              SET INPUT-ERROR TO TRUE
              MOVE 'Account ID must be an 11 digit non-zero number'
                TO WS-RETURN-MSG
              GO TO 1100-EDIT-ACCOUNT-ID-EXIT
           ELSE
              SET FLG-ACCTFILTER-ISVALID TO TRUE
           END-IF
           .

       1100-EDIT-ACCOUNT-ID-EXIT.
           EXIT.

      ******************************************************************
      * Read Account and Customer Data
      ******************************************************************
       9000-READ-ACCOUNT-DATA.
           SET WS-RETURN-MSG-OFF TO TRUE

           PERFORM 9100-GET-CARDXREF-BY-ACCT
              THRU 9100-GET-CARDXREF-BY-ACCT-EXIT

           IF NOT RC-SUCCESS
              GO TO 9000-READ-ACCOUNT-DATA-EXIT
           END-IF

           PERFORM 9200-GET-ACCOUNT-BY-ACCT
              THRU 9200-GET-ACCOUNT-BY-ACCT-EXIT

           IF NOT RC-SUCCESS
              GO TO 9000-READ-ACCOUNT-DATA-EXIT
           END-IF

           PERFORM 9300-GET-CUSTOMER-BY-CUST
              THRU 9300-GET-CUSTOMER-BY-CUST-EXIT

           .

       9000-READ-ACCOUNT-DATA-EXIT.
           EXIT.

      ******************************************************************
      * Get Card Cross Reference by Account ID (DB2)
      ******************************************************************
       9100-GET-CARDXREF-BY-ACCT.
           MOVE WS-CARD-RID-ACCT-ID-X TO HV-ACCOUNT-ID

           EXEC SQL
                SELECT CARD_NUM
                INTO :HV-CARD-NUMBER
                FROM CARDDAT
                WHERE CARD_ACCT_ID = :HV-ACCOUNT-ID
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                  MOVE HV-CARD-NUMBER TO XREF-CARD-NUM
               WHEN 100
                  SET RC-NOT-FOUND TO TRUE
                  MOVE SQLCODE TO HV-SQLCODE-DISPLAY
                  STRING
                  'Account:'
                   WS-CARD-RID-ACCT-ID-X
                  ' not found in'
                  ' Card file.  SQLCODE:'
                  HV-SQLCODE-DISPLAY
                  DELIMITED BY SIZE
                  INTO WS-RETURN-MSG
                  END-STRING
               WHEN OTHER
                  SET RC-DATABASE-ERROR TO TRUE
                  MOVE SQLCODE TO HV-SQLCODE-DISPLAY
                  STRING
                  'Error reading CARDDAT table. SQLCODE:'
                  HV-SQLCODE-DISPLAY
                  DELIMITED BY SIZE
                  INTO WS-RETURN-MSG
                  END-STRING
           END-EVALUATE
           .

       9100-GET-CARDXREF-BY-ACCT-EXIT.
           EXIT.

      ******************************************************************
      * Get Account Data by Account ID (DB2)
      ******************************************************************
       9200-GET-ACCOUNT-BY-ACCT.

           MOVE WS-CARD-RID-ACCT-ID-X TO HV-ACCOUNT-ID.

           EXEC SQL
                SELECT ACCT_ID, ACCT_ACTIVE_STATUS, ACCT_CURR_BAL,
                       ACCT_CREDIT_LIMIT, ACCT_CASH_CREDIT_LIMIT,
                       ACCT_OPEN_DATE, ACCT_EXPIRAION_DATE,
                       ACCT_REISSUE_DATE, ACCT_CURR_CYC_CREDIT,
                       ACCT_CURR_CYC_DEBIT, ACCT_ADDR_ZIP,
                       ACCT_GROUP_ID
                INTO :HV-ACCT-ID, :HV-ACCT-STATUS, :HV-ACCT-CURR-BAL,
                     :HV-ACCT-CREDIT-LMT, :HV-ACCT-CASH-LMT,
                     :HV-ACCT-OPEN-DT, :HV-ACCT-EXPIRY-DT,
                     :HV-ACCT-REISSUE-DT, :HV-ACCT-CYC-CREDIT,
                     :HV-ACCT-CYC-DEBIT, :HV-ACCT-ZIP,
                     :HV-ACCT-GROUP-ID
                FROM ACCTDAT
                WHERE ACCT_ID = :HV-ACCOUNT-ID
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                  SET FOUND-ACCT-IN-MASTER        TO TRUE
                  MOVE HV-ACCT-ID                 TO ACCT-ID
                  MOVE HV-ACCT-STATUS             TO ACCT-ACTIVE-STATUS
                  MOVE HV-ACCT-CURR-BAL           TO ACCT-CURR-BAL
                  MOVE HV-ACCT-CREDIT-LMT         TO ACCT-CREDIT-LIMIT
                  MOVE HV-ACCT-CASH-LMT
                    TO ACCT-CASH-CREDIT-LIMIT
                  MOVE HV-ACCT-OPEN-DT            TO ACCT-OPEN-DATE
                  MOVE HV-ACCT-EXPIRY-DT          TO ACCT-EXPIRAION-DATE
                  MOVE HV-ACCT-REISSUE-DT         TO ACCT-REISSUE-DATE
                  MOVE HV-ACCT-CYC-CREDIT
                    TO ACCT-CURR-CYC-CREDIT
                  MOVE HV-ACCT-CYC-DEBIT          TO ACCT-CURR-CYC-DEBIT
                  MOVE HV-ACCT-ZIP                TO ACCT-ADDR-ZIP
                  MOVE HV-ACCT-GROUP-ID           TO ACCT-GROUP-ID
               WHEN 100
                  SET RC-NOT-FOUND TO TRUE
                  MOVE SQLCODE TO HV-SQLCODE-DISPLAY
                  STRING
                  'Account:'
                   WS-CARD-RID-ACCT-ID-X
                  ' not found in'
                  ' Acct Master file. SQLCODE:'
                  HV-SQLCODE-DISPLAY
                  DELIMITED BY SIZE
                  INTO WS-RETURN-MSG
                  END-STRING
               WHEN OTHER
                  SET RC-DATABASE-ERROR TO TRUE
                  MOVE SQLCODE TO HV-SQLCODE-DISPLAY
                  STRING
                  'Error reading ACCTDAT table. SQLCODE:'
                  HV-SQLCODE-DISPLAY
                  DELIMITED BY SIZE
                  INTO WS-RETURN-MSG
                  END-STRING
           END-EVALUATE
           .

       9200-GET-ACCOUNT-BY-ACCT-EXIT.
           EXIT.

      ******************************************************************
      * Get Customer Data by Customer ID (DB2)
      ******************************************************************
       9300-GET-CUSTOMER-BY-CUST.
      *    For now, we'll assume customer ID is derived from account ID
      *    In a real implementation, this would come from a proper
      *    cross-reference table or account-customer relationship
           MOVE WS-CARD-RID-ACCT-ID TO HV-CUSTOMER-ID

           EXEC SQL
                SELECT CUST_ID, CUST_FIRST_NAME, CUST_MIDDLE_NAME,
                       CUST_LAST_NAME, CUST_ADDR_LINE_1,
                       CUST_ADDR_LINE_2, CUST_ADDR_LINE_3,
                       CUST_ADDR_STATE_CD, CUST_ADDR_COUNTRY_CD,
                       CUST_ADDR_ZIP, CUST_PHONE_NUM_1,
                       CUST_PHONE_NUM_2, CUST_SSN,
                       CUST_GOVT_ISSUED_ID, CUST_DOB_YYYY_MM_DD,
                       CUST_EFT_ACCOUNT_ID, CUST_PRI_CARD_HOLDER_IND,
                       CUST_FICO_CREDIT_SCORE
                INTO :HV-CUST-ID, :HV-CUST-FNAME, :HV-CUST-MNAME,
                     :HV-CUST-LNAME, :HV-CUST-ADDR1,
                     :HV-CUST-ADDR2, :HV-CUST-ADDR3,
                     :HV-CUST-STATE, :HV-CUST-COUNTRY,
                     :HV-CUST-ZIP, :HV-CUST-PHONE1,
                     :HV-CUST-PHONE2, :HV-CUST-SSN,
                     :HV-CUST-GOVT-ID, :HV-CUST-DOB,
                     :HV-CUST-EFT-ID, :HV-CUST-PRI-HOLDER,
                     :HV-CUST-FICO
                FROM CUSTDAT
                WHERE CUST_ID = :HV-CUSTOMER-ID
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                  SET FOUND-CUST-IN-MASTER        TO TRUE
                  MOVE HV-CUST-ID                 TO CUST-ID
                  MOVE HV-CUST-FNAME              TO CUST-FIRST-NAME
                  MOVE HV-CUST-MNAME              TO CUST-MIDDLE-NAME
                  MOVE HV-CUST-LNAME              TO CUST-LAST-NAME
                  MOVE HV-CUST-ADDR1              TO CUST-ADDR-LINE-1
                  MOVE HV-CUST-ADDR2              TO CUST-ADDR-LINE-2
                  MOVE HV-CUST-ADDR3              TO CUST-ADDR-LINE-3
                  MOVE HV-CUST-STATE              TO CUST-ADDR-STATE-CD
                  MOVE HV-CUST-COUNTRY
                    TO CUST-ADDR-COUNTRY-CD
                  MOVE HV-CUST-ZIP                TO CUST-ADDR-ZIP
                  MOVE HV-CUST-PHONE1             TO CUST-PHONE-NUM-1
                  MOVE HV-CUST-PHONE2             TO CUST-PHONE-NUM-2
                  MOVE HV-CUST-SSN                TO CUST-SSN
                  MOVE HV-CUST-GOVT-ID            TO CUST-GOVT-ISSUED-ID
                  MOVE HV-CUST-DOB                TO CUST-DOB-YYYY-MM-DD
                  MOVE HV-CUST-EFT-ID             TO CUST-EFT-ACCOUNT-ID
                  MOVE HV-CUST-PRI-HOLDER
                    TO CUST-PRI-CARD-HOLDER-IND
                  MOVE HV-CUST-FICO
                    TO CUST-FICO-CREDIT-SCORE
               WHEN 100
                  SET RC-NOT-FOUND TO TRUE
                  MOVE SQLCODE TO HV-SQLCODE-DISPLAY
                  STRING
                  'CustId not found'
                  ' in customer master. SQLCODE: '
                  HV-SQLCODE-DISPLAY
                  DELIMITED BY SIZE
                  INTO WS-RETURN-MSG
                  END-STRING
               WHEN OTHER
                  SET RC-DATABASE-ERROR TO TRUE
                  MOVE SQLCODE TO HV-SQLCODE-DISPLAY
                  STRING
                  'Error reading CUSTDAT table. SQLCODE:'
                  HV-SQLCODE-DISPLAY
                  DELIMITED BY SIZE
                  INTO WS-RETURN-MSG
                  END-STRING
           END-EVALUATE
           .

       9300-GET-CUSTOMER-BY-CUST-EXIT.
           EXIT.

      ******************************************************************
      * Map Database Data to Output Structure
      ******************************************************************
       1300-MAP-OUTPUT-DATA.
      * Account data
           MOVE ACCT-ID                   TO LK-OUT-ACCT-ID
           MOVE ACCT-ACTIVE-STATUS        TO LK-OUT-ACCT-ACTIVE-STATUS
           MOVE ACCT-CREDIT-LIMIT         TO LK-OUT-ACCT-CREDIT-LIMIT
           MOVE ACCT-CASH-CREDIT-LIMIT    TO LK-OUT-ACCT-CASH-LIMIT
           MOVE ACCT-CURR-BAL             TO LK-OUT-ACCT-CURR-BAL
           MOVE ACCT-CURR-CYC-CREDIT      TO LK-OUT-ACCT-CURR-CYC-CREDIT
           MOVE ACCT-CURR-CYC-DEBIT       TO LK-OUT-ACCT-CURR-CYC-DEBIT
           MOVE ACCT-OPEN-DATE            TO LK-OUT-ACCT-OPEN-DATE
           MOVE ACCT-EXPIRAION-DATE       TO LK-OUT-ACCT-EXPIRATION-DATE
           MOVE ACCT-REISSUE-DATE         TO LK-OUT-ACCT-REISSUE-DATE
           MOVE ACCT-GROUP-ID             TO LK-OUT-ACCT-GROUP-ID
           MOVE XREF-CARD-NUM             TO LK-OUT-ACCT-CARD-NUM

      * Customer data
           MOVE CUST-ID                   TO LK-OUT-CUST-ID
           MOVE CUST-FIRST-NAME           TO LK-OUT-CUST-FIRST-NAME
           MOVE CUST-MIDDLE-NAME          TO LK-OUT-CUST-MIDDLE-NAME
           MOVE CUST-LAST-NAME            TO LK-OUT-CUST-LAST-NAME
           MOVE CUST-SSN                  TO LK-OUT-CUST-SSN
           MOVE CUST-DOB-YYYY-MM-DD       TO LK-OUT-CUST-DOB
           MOVE CUST-ADDR-LINE-1          TO LK-OUT-CUST-ADDR-LINE-1
           MOVE CUST-ADDR-LINE-2          TO LK-OUT-CUST-ADDR-LINE-2
           MOVE CUST-ADDR-LINE-3          TO LK-OUT-CUST-ADDR-LINE-3
           MOVE CUST-ADDR-STATE-CD        TO LK-OUT-CUST-ADDR-STATE-CD
           MOVE CUST-ADDR-COUNTRY-CD      TO LK-OUT-CUST-ADDR-COUNTRY-CD
           MOVE CUST-ADDR-ZIP             TO LK-OUT-CUST-ADDR-ZIP
           MOVE CUST-PHONE-NUM-1          TO LK-OUT-CUST-PHONE-NUM-1
           MOVE CUST-PHONE-NUM-2          TO LK-OUT-CUST-PHONE-NUM-2
           MOVE CUST-GOVT-ISSUED-ID       TO LK-OUT-CUST-GOVT-ISSUED-ID
           MOVE CUST-EFT-ACCOUNT-ID       TO LK-OUT-CUST-EFT-ACCOUNT-ID
           MOVE CUST-PRI-CARD-HOLDER-IND  TO LK-OUT-CUST-PRI-HOLDER-IND
           MOVE CUST-FICO-CREDIT-SCORE    TO LK-OUT-CUST-FICO-SCORE
           .

       1300-MAP-OUTPUT-DATA-EXIT.
           EXIT.

       ABEND-ROUTINE.
           SET RC-DATABASE-ERROR TO TRUE
           MOVE 'UNEXPECTED ABEND OCCURRED.' TO LK-OUT-MESSAGE
           GOBACK
           .