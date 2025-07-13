      ******************************************************************
      * Program:     COACTVWL.CBL                                     *
      * Layer:       Business logic                                   *
      * Function:    RPC Service for Account Read Operations          *
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
           COACTVWL.
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
                                                   VALUE 'COACTVWL'.
          05 LIT-ACCTFILENAME                      PIC X(8)
                                                   VALUE 'ACCTDAT '.
          05 LIT-CUSTFILENAME                      PIC X(8)
                                                   VALUE 'CUSTDAT '.
          05 LIT-CARDXREFNAME-ACCT-PATH            PIC X(8)
                                                   VALUE 'CXACAIX '.

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
      * Get Card Cross Reference by Account ID
      ******************************************************************
       9100-GET-CARDXREF-BY-ACCT.
           EXEC CICS READ
                DATASET   (LIT-CARDXREFNAME-ACCT-PATH)
                RIDFLD    (WS-CARD-RID-ACCT-ID-X)
                KEYLENGTH (LENGTH OF WS-CARD-RID-ACCT-ID-X)
                INTO      (CARD-XREF-RECORD)
                LENGTH    (LENGTH OF CARD-XREF-RECORD)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC

           MOVE WS-RESP-CD TO WS-RESP-CD-DISP.
           MOVE WS-REAS-CD TO WS-REAS-CD-DISP.


           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                  CONTINUE
               WHEN DFHRESP(NOTFND)
                  SET RC-NOT-FOUND TO TRUE
                  SET DID-NOT-FIND-ACCT-IN-CARDXREF TO TRUE
               WHEN OTHER
                  SET RC-DATABASE-ERROR TO TRUE
                  STRING 'Error reading card cross reference. RESP='
                         WS-RESP-CD-DISP
                         ' RESP2='
                         WS-REAS-CD-DISP
                    DELIMITED BY SIZE
                    INTO WS-RETURN-MSG
                  END-STRING
           END-EVALUATE
           .

       9100-GET-CARDXREF-BY-ACCT-EXIT.
           EXIT.

      ******************************************************************
      * Get Account Data by Account ID
      ******************************************************************
       9200-GET-ACCOUNT-BY-ACCT.
           EXEC CICS READ
                DATASET   (LIT-ACCTFILENAME)
                RIDFLD    (WS-CARD-RID-ACCT-ID-X)
                KEYLENGTH (LENGTH OF WS-CARD-RID-ACCT-ID-X)
                INTO      (ACCOUNT-RECORD)
                LENGTH    (LENGTH OF ACCOUNT-RECORD)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC

           MOVE WS-RESP-CD TO WS-RESP-CD-DISP.
           MOVE WS-REAS-CD TO WS-REAS-CD-DISP.


           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                  SET FOUND-ACCT-IN-MASTER TO TRUE
               WHEN DFHRESP(NOTFND)
                  SET RC-NOT-FOUND TO TRUE
                  SET DID-NOT-FIND-ACCT-IN-ACCTDAT TO TRUE
               WHEN OTHER
                  SET RC-DATABASE-ERROR TO TRUE
                  STRING 'Error reading account master. RESP='
                         WS-RESP-CD-DISP
                         ' RESP2='
                         WS-REAS-CD-DISP
                    DELIMITED BY SIZE
                    INTO WS-RETURN-MSG
                  END-STRING
           END-EVALUATE
           .

       9200-GET-ACCOUNT-BY-ACCT-EXIT.
           EXIT.

      ******************************************************************
      * Get Customer Data by Customer ID
      ******************************************************************
       9300-GET-CUSTOMER-BY-CUST.
           MOVE XREF-CUST-ID TO WS-CARD-RID-CUST-ID

           EXEC CICS READ
                DATASET   (LIT-CUSTFILENAME)
                RIDFLD    (WS-CARD-RID-CUST-ID-X)
                KEYLENGTH (LENGTH OF WS-CARD-RID-CUST-ID-X)
                INTO      (CUSTOMER-RECORD)
                LENGTH    (LENGTH OF CUSTOMER-RECORD)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC

           MOVE WS-RESP-CD TO WS-RESP-CD-DISP.
           MOVE WS-REAS-CD TO WS-REAS-CD-DISP.


           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                  SET FOUND-CUST-IN-MASTER TO TRUE
               WHEN DFHRESP(NOTFND)
                  SET RC-NOT-FOUND TO TRUE
                  SET DID-NOT-FIND-CUST-IN-CUSTDAT TO TRUE
               WHEN OTHER
                  SET RC-DATABASE-ERROR TO TRUE
                  STRING 'Error reading customer master. RESP='
                         WS-RESP-CD-DISP
                         ' RESP2='
                         WS-REAS-CD-DISP
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