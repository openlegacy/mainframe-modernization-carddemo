******************************************************************
      * Program     : COCRDSLL.CBL
      * Application : CardDemo
      * Type        : COBOL RPC Program
      * Function    : Card lookup/read with reverse API to account
      * Description : Receives card number, returns card data
      *               PLUS account data via reverse API call to COACTVWL
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
       PROGRAM-ID.     COCRDSLL.
       AUTHOR.     AWS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
      *----------------------------------------------------------------*
      *                     WORKING STORAGE SECTION
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

       01 WS-VARIABLES.
         05 WS-PGMNAME                 PIC X(08) VALUE 'COCRDSLL'.
         05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.
           88 ERR-FLG-ON                         VALUE 'Y'.
           88 ERR-FLG-OFF                        VALUE 'N'.
         05 WS-RESP-CD                 PIC S9(09) COMP VALUE ZEROS.
         05 WS-REAS-CD                 PIC S9(09) COMP VALUE ZEROS.
         05 WS-RESP-DISP               PIC 9(09) VALUE ZEROS.
         05 WS-REAS-DISP               PIC 9(09) VALUE ZEROS.

      * File access variables
       01 WS-FILE-VARS.
         05 WS-CARDFILENAME             PIC X(08) VALUE 'CARDDAT '.
         05 WS-CARD-RID.
           10 WS-CARD-RID-CARDNUM       PIC X(16).
         05 WS-FILE-ERROR-MESSAGE.
           10 FILLER                    PIC X(12)
                                        VALUE 'File Error: '.
           10 ERROR-OPNAME              PIC X(8)
                                        VALUE SPACES.
           10 FILLER                    PIC X(4)
                                        VALUE ' on '.
           10 ERROR-FILE                PIC X(9)
                                        VALUE SPACES.
           10 FILLER                    PIC X(15)
                                        VALUE
                                        ' returned RESP '.
           10 ERROR-RESP                PIC X(10)
                                        VALUE SPACES.
           10 FILLER                    PIC X(7)
                                        VALUE ',RESP2 '.
           10 ERROR-RESP2               PIC X(10)
                                        VALUE SPACES.
           10 FILLER                    PIC X(5)
                                        VALUE SPACES.

      * Reverse API - COACTVWL Communication Area
       01 WS-VWL-COMMAREA.
          05 VWL-INPUT-PARMS.
             10 VWL-IN-ACCT-ID             PIC X(11).
          05 VWL-OUTPUT-STATUS.
             10 VWL-OUT-RETURN-CODE        PIC 9(02).
                88 VWL-RC-SUCCESS             VALUE 00.
                88 VWL-RC-NOT-FOUND           VALUE 01.
                88 VWL-RC-INPUT-ERROR         VALUE 03.
                88 VWL-RC-DATABASE-ERROR      VALUE 99.
             10 VWL-OUT-MESSAGE            PIC X(80).
          05 VWL-OUTPUT-DATA.
             10 VWL-OUT-ACCT-DATA.
                15 VWL-OUT-ACCT-ID              PIC X(11).
                15 VWL-OUT-ACCT-ACTIVE-STATUS   PIC X(01).
                15 VWL-OUT-ACCT-CREDIT-LIMIT    PIC S9(10)V99.
                15 VWL-OUT-ACCT-CASH-LIMIT      PIC S9(10)V99.
                15 VWL-OUT-ACCT-CURR-BAL        PIC S9(10)V99.
                15 VWL-OUT-ACCT-CURR-CYC-CREDIT PIC S9(10)V99.
                15 VWL-OUT-ACCT-CURR-CYC-DEBIT  PIC S9(10)V99.
                15 VWL-OUT-ACCT-OPEN-DATE       PIC X(10).
                15 VWL-OUT-ACCT-EXPIRATION-DATE PIC X(10).
                15 VWL-OUT-ACCT-REISSUE-DATE    PIC X(10).
                15 VWL-OUT-ACCT-GROUP-ID        PIC X(10).
                15 VWL-OUT-ACCT-CARD-NUM        PIC X(16).
             10 VWL-OUT-CUST-DATA.
                15 VWL-OUT-CUST-ID              PIC X(9).
                15 VWL-OUT-CUST-FIRST-NAME      PIC X(25).
                15 VWL-OUT-CUST-MIDDLE-NAME     PIC X(25).
                15 VWL-OUT-CUST-LAST-NAME       PIC X(25).
                15 VWL-OUT-CUST-SSN             PIC X(9).
                15 VWL-OUT-CUST-DOB             PIC X(10).
                15 VWL-OUT-CUST-ADDR-LINE-1     PIC X(50).
                15 VWL-OUT-CUST-ADDR-LINE-2     PIC X(50).
                15 VWL-OUT-CUST-ADDR-LINE-3     PIC X(50).
                15 VWL-OUT-CUST-ADDR-STATE-CD   PIC X(2).
                15 VWL-OUT-CUST-ADDR-COUNTRY-CD PIC X(3).
                15 VWL-OUT-CUST-ADDR-ZIP        PIC X(10).
                15 VWL-OUT-CUST-PHONE-NUM-1     PIC X(15).
                15 VWL-OUT-CUST-PHONE-NUM-2     PIC X(15).
                15 VWL-OUT-CUST-GOVT-ISSUED-ID  PIC X(20).
                15 VWL-OUT-CUST-EFT-ACCOUNT-ID  PIC  X(10).
                15 VWL-OUT-CUST-PRI-HOLDER-IND  PIC X(1).
                15 VWL-OUT-CUST-FICO-SCORE      PIC 9(3).

       01 WS-LITERALS.
          05 LIT-VWL-PROGRAM            PIC X(08) VALUE 'COACTVWL'.

      *Dataset layouts
       COPY CVACT02Y.

      *----------------------------------------------------------------*
      *                        LINKAGE SECTION
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01  DFHCOMMAREA.
           05  LK-INPUT-CRITERIA.
               10  LK-IN-CARD-NUM            PIC 9(16).
           05  LK-OUTPUT-STATUS.
               10  LK-OUT-RETURN-CODE        PIC 9(02).
                   88  RC-SUCCESS             VALUE 00.
                   88  RC-NOT-FOUND           VALUE 01.
                   88  RC-VALIDATION-ERROR    VALUE 10.
                   88  RC-DATABASE-ERROR      VALUE 99.
               10  LK-OUT-MESSAGE            PIC X(80).
           05  LK-OUTPUT-CARD-DATA.
               10  LK-OUT-CARD-NUM           PIC X(16).
               10  LK-OUT-CARD-ACCT-ID       PIC 9(11).
               10  LK-OUT-CARD-CVV-CD        PIC 9(03).
               10  LK-OUT-CARD-EMBOSSED-NAME PIC X(50).
               10  LK-OUT-CARD-EXPIRY-DATE   PIC X(10).
               10  LK-OUT-CARD-STATUS        PIC X(01).
           05  LK-OUTPUT-ACCT-DATA.
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



      *----------------------------------------------------------------*
      *                       PROCEDURE DIVISION
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

       MAIN-PARA.

           INITIALIZE LK-OUTPUT-STATUS
                      LK-OUTPUT-CARD-DATA
                      LK-OUTPUT-ACCT-DATA

           SET ERR-FLG-OFF TO TRUE
           SET RC-SUCCESS TO TRUE

           MOVE SPACES TO LK-OUT-MESSAGE

      *    Validate input
           IF LK-IN-CARD-NUM = ZEROS
               SET RC-VALIDATION-ERROR TO TRUE
               MOVE 'Card number cannot be zero'
                    TO LK-OUT-MESSAGE
               GOBACK
           END-IF

      *    Read card data
           MOVE LK-IN-CARD-NUM TO WS-CARD-RID-CARDNUM
           PERFORM READ-CARD-BY-CARDNUM

           IF ERR-FLG-ON
               GOBACK
           END-IF

      *    Populate card output
           PERFORM POPULATE-CARD-OUTPUT-DATA

      *    Call reverse API for account data
           PERFORM CALL-COACTVWL-FOR-ACCOUNT

           IF NOT ERR-FLG-ON
               MOVE 'Card and account data retrieved successfully'
                    TO LK-OUT-MESSAGE
           END-IF

           GOBACK.

      *----------------------------------------------------------------*
      *                      READ-CARD-BY-CARDNUM
      *----------------------------------------------------------------*
       READ-CARD-BY-CARDNUM.

           EXEC CICS READ
                FILE      (WS-CARDFILENAME)
                RIDFLD    (WS-CARD-RID-CARDNUM)
                KEYLENGTH (LENGTH OF WS-CARD-RID-CARDNUM)
                INTO      (CARD-RECORD)
                LENGTH    (LENGTH OF CARD-RECORD)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC


           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                    CONTINUE
               WHEN DFHRESP(NOTFND)
                   SET ERR-FLG-ON TO TRUE
                   SET RC-NOT-FOUND TO TRUE
                   MOVE 'Card not found' TO LK-OUT-MESSAGE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   SET RC-DATABASE-ERROR TO TRUE
                   MOVE 'read'                     TO ERROR-OPNAME
                   MOVE WS-CARDFILENAME            TO ERROR-FILE
                   MOVE WS-RESP-CD                 TO ERROR-RESP
                   MOVE WS-REAS-CD                 TO ERROR-RESP2
                   MOVE WS-FILE-ERROR-MESSAGE      TO LK-OUT-MESSAGE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      POPULATE-CARD-OUTPUT-DATA
      *----------------------------------------------------------------*
       POPULATE-CARD-OUTPUT-DATA.

           MOVE CARD-NUM              TO LK-OUT-CARD-NUM
           MOVE CARD-ACCT-ID          TO LK-OUT-CARD-ACCT-ID
           MOVE CARD-CVV-CD           TO LK-OUT-CARD-CVV-CD
           MOVE CARD-EMBOSSED-NAME    TO LK-OUT-CARD-EMBOSSED-NAME
           MOVE CARD-EXPIRAION-DATE   TO LK-OUT-CARD-EXPIRY-DATE
           MOVE CARD-ACTIVE-STATUS    TO LK-OUT-CARD-STATUS.

      *----------------------------------------------------------------*
      *                   CALL-COACTVWL-FOR-ACCOUNT
      *         REVERSE API - Call account read RPC program
      *----------------------------------------------------------------*
       CALL-COACTVWL-FOR-ACCOUNT.

      *    Initialize the VWL commarea
           INITIALIZE WS-VWL-COMMAREA

      *    Set the account ID from the card record
           MOVE CARD-ACCT-ID TO VWL-IN-ACCT-ID

      *    Call COACTVWL to get account data
           EXEC CICS LINK
                PROGRAM(LIT-VWL-PROGRAM)
                COMMAREA(WS-VWL-COMMAREA)
                LENGTH(LENGTH OF WS-VWL-COMMAREA)
                RESP(WS-RESP-CD)
                RESP2(WS-REAS-CD)
           END-EXEC

           MOVE WS-RESP-CD TO WS-RESP-DISP
           MOVE WS-REAS-CD TO WS-REAS-DISP

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   IF VWL-RC-SUCCESS
      *                Map account data from VWL to output
                       PERFORM MAP-ACCOUNT-DATA-TO-OUTPUT
                   ELSE
      *                VWL returned error
                       SET ERR-FLG-ON TO TRUE
                       MOVE VWL-OUT-RETURN-CODE TO LK-OUT-RETURN-CODE
                       STRING 'Account lookup failed: '
                              VWL-OUT-MESSAGE
                              DELIMITED BY SIZE
                              INTO LK-OUT-MESSAGE
                       END-STRING
                   END-IF
               WHEN DFHRESP(PGMIDERR)
                   SET ERR-FLG-ON TO TRUE
                   SET RC-DATABASE-ERROR TO TRUE
                   MOVE 'COACTVWL program not found' TO LK-OUT-MESSAGE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   SET RC-DATABASE-ERROR TO TRUE
                   STRING 'Error calling COACTVWL. RESP='
                          WS-RESP-DISP
                          ' RESP2='
                          WS-REAS-DISP
                     DELIMITED BY SIZE
                     INTO LK-OUT-MESSAGE
                   END-STRING
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                   MAP-ACCOUNT-DATA-TO-OUTPUT
      *     Map account data from COACTVWL output to screen output
      *----------------------------------------------------------------*
       MAP-ACCOUNT-DATA-TO-OUTPUT.

           MOVE VWL-OUT-ACCT-ID              TO LK-OUT-ACCT-ID
           MOVE VWL-OUT-ACCT-ACTIVE-STATUS  TO LK-OUT-ACCT-ACTIVE-STATUS
           MOVE VWL-OUT-ACCT-CREDIT-LIMIT    TO LK-OUT-ACCT-CREDIT-LIMIT
           MOVE VWL-OUT-ACCT-CASH-LIMIT      TO LK-OUT-ACCT-CASH-LIMIT
           MOVE VWL-OUT-ACCT-CURR-BAL        TO LK-OUT-ACCT-CURR-BAL
           MOVE VWL-OUT-ACCT-CURR-CYC-CREDIT TO
                LK-OUT-ACCT-CURR-CYC-CREDIT
           MOVE VWL-OUT-ACCT-CURR-CYC-DEBIT  TO
                LK-OUT-ACCT-CURR-CYC-DEBIT
           MOVE VWL-OUT-ACCT-OPEN-DATE       TO LK-OUT-ACCT-OPEN-DATE
           MOVE VWL-OUT-ACCT-EXPIRATION-DATE TO
                LK-OUT-ACCT-EXPIRATION-DATE
           MOVE VWL-OUT-ACCT-REISSUE-DATE    TO LK-OUT-ACCT-REISSUE-DATE
           MOVE VWL-OUT-ACCT-GROUP-ID        TO LK-OUT-ACCT-GROUP-ID.

      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:12:33 CDT
      *