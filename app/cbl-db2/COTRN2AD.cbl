******************************************************************
      * Program     : COTRN2AD.CBL
      * Application : CardDemo
      * Type        : CICS COBOL Program
      * Function    : Add Transaction - Screen A (Transaction Details)
      *               DB2 Version with LK-TRAN-RECORD in COMMAREA
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
       PROGRAM-ID. COTRN2AD.
       AUTHOR.     AWS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
      *----------------------------------------------------------------*
      *                     WORKING STORAGE SECTION
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

       01 WS-VARIABLES.
         05 WS-PGMNAME                 PIC X(08) VALUE 'COTRN2AD'.
         05 WS-TRANID                  PIC X(04) VALUE 'ADT1'.
         05 WS-RESP-CD                 PIC S9(09) COMP VALUE ZEROS.
         05 WS-REAS-CD                 PIC S9(09) COMP  VALUE ZEROS.

         05 WS-MESSAGE                 PIC X(80) VALUE SPACES.
         05 WS-SUB                     PIC 99    VALUE ZEROES.

         05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.
           88 ERR-FLG-ON                         VALUE 'Y'.
           88 ERR-FLG-OFF                        VALUE 'N'.

         05 WS-ACCT-ID-N               PIC 9(11) VALUE 0.
         05 WS-CARD-NUM-N              PIC 9(16) VALUE 0.

      *----------------------------------------------------------------*
      * DB2 Host Variables
      *----------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA END-EXEC.

       01 HV-CXACAIX-VARS.
         05 HV-XREF-ACCT-ID            PIC S9(11) COMP-3.
         05 HV-XREF-CARD-NUM           PIC X(16).
         05 HV-XREF-CUST-ID            PIC X(09).

      *----------------------------------------------------------------*
      * Transaction Type Lookup Table
      *----------------------------------------------------------------*
       01 WS-TYPE-TABLE.
         05 WS-TYPE-ENTRIES            PIC 9(02) VALUE 09.
         05 WS-TYPE-ENTRY OCCURS 9 TIMES.
           10 WS-TYPE-CODE             PIC X(02).
           10 WS-TYPE-DESC             PIC X(20).

       01 WS-TYPE-DATA.
         05 FILLER PIC X(22) VALUE 'DBDebit Transaction '.
         05 FILLER PIC X(22) VALUE 'CRCredit Transaction'.
         05 FILLER PIC X(22) VALUE 'RFRefund Transaction'.
         05 FILLER PIC X(22) VALUE 'ADAdjustment        '.
         05 FILLER PIC X(22) VALUE 'FEFee Transaction   '.
         05 FILLER PIC X(22) VALUE 'CHChargeback        '.
         05 FILLER PIC X(22) VALUE 'AUAuthorization     '.
         05 FILLER PIC X(22) VALUE 'VOVoid Transaction  '.
         05 FILLER PIC X(22) VALUE 'RTRetail/Sales      '.

       01 WS-TYPE-DATA-R REDEFINES WS-TYPE-DATA.
         05 WS-TYPE-ENTRY-R OCCURS 9 TIMES.
           10 WS-TYPE-CODE-R           PIC X(02).
           10 WS-TYPE-DESC-R           PIC X(20).

      *----------------------------------------------------------------*
      * Category Lookup Table (13 entries, 4-digit numeric codes)
      *----------------------------------------------------------------*
       01 WS-CAT-TABLE.
         05 WS-CAT-ENTRIES             PIC 9(02) VALUE 13.
         05 WS-CAT-ENTRY OCCURS 13 TIMES.
           10 WS-CAT-CODE              PIC 9(04).
           10 WS-CAT-DESC              PIC X(26).

        01 WS-CAT-DATA.
          05 FILLER PIC X(30) VALUE '5699Retail/General Merchandise'.
          05 FILLER PIC X(30) VALUE '5812Restaurants/Food Service  '.
          05 FILLER PIC X(30) VALUE '5541Gas Stations/Fuel         '.
          05 FILLER PIC X(30) VALUE '5411Grocery Stores            '.
          05 FILLER PIC X(30) VALUE '5311Department Stores         '.
          05 FILLER PIC X(30) VALUE '5912Pharmacy/Medical          '.
          05 FILLER PIC X(30) VALUE '4900Utilities                 '.
          05 FILLER PIC X(30) VALUE '4814Telecommunications        '.
          05 FILLER PIC X(30) VALUE '4511Transportation/Travel     '.
          05 FILLER PIC X(30) VALUE '7011Hotels/Lodging            '.
          05 FILLER PIC X(30) VALUE '7832Entertainment             '.
          05 FILLER PIC X(30) VALUE '5691Clothing/Apparel          '.
          05 FILLER PIC X(30) VALUE '5732Electronics               '.

       01 WS-CAT-DATA-R REDEFINES WS-CAT-DATA.
         05 WS-CAT-ENTRY-R OCCURS 13 TIMES.
           10 WS-CAT-CODE-R            PIC 9(04).
           10 WS-CAT-DESC-R            PIC X(26).

       COPY COCOM01Y.
       COPY COTRN2A.
       COPY COTYPLK.
       COPY COCATLK.
       COPY COTTL01Y.
       COPY CSDAT01Y.
       COPY CSMSG01Y.
       COPY DFHAID.
       COPY DFHBMSCA.

      *----------------------------------------------------------------*
      *                        LINKAGE SECTION
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01  DFHCOMMAREA.
         05  LK-CARDDEMO-COMMAREA            PIC X(300).
         COPY CVTRA05L.

      *----------------------------------------------------------------*
      *                       PROCEDURE DIVISION
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       MAIN-PARA.

           SET ERR-FLG-OFF     TO TRUE
           MOVE SPACES TO WS-MESSAGE
                          ERRMSGO OF COTRN2AO

           PERFORM LOAD-LOOKUP-TABLES

           IF EIBCALEN = 0
               MOVE 'COSGN00D' TO CDEMO-TO-PROGRAM
               PERFORM RETURN-TO-PREV-SCREEN
           ELSE
               MOVE DFHCOMMAREA(1:300) TO LK-CARDDEMO-COMMAREA
               MOVE LK-CARDDEMO-COMMAREA TO CARDDEMO-COMMAREA
               IF NOT CDEMO-PGM-REENTER
                   SET CDEMO-PGM-REENTER    TO TRUE
                   MOVE LOW-VALUES          TO COTRN2AO
                   MOVE -1       TO ACTIDINL OF COTRN2AI
                   IF CDEMO-FROM-PROGRAM = 'COTRN2BD'
                      PERFORM RESTORE-SCREEN-DATA
                   END-IF
                   PERFORM SEND-TRNADD-SCREEN
               ELSE
                   PERFORM RECEIVE-TRNADD-SCREEN
                   PERFORM RESTORE-SCREEN-DATA
                   EVALUATE EIBAID
                       WHEN DFHENTER
                           PERFORM PROCESS-ENTER-KEY
                       WHEN DFHPF1
                           PERFORM PROCESS-F1-LOOKUP
                       WHEN DFHPF3
                           MOVE 'COMEN01D' TO CDEMO-TO-PROGRAM
                           PERFORM RETURN-TO-PREV-SCREEN
                       WHEN DFHPF4
                           PERFORM CLEAR-CURRENT-SCREEN
                       WHEN DFHPF8
                           PERFORM PROCEED-TO-MERCHANT-SCREEN
                       WHEN OTHER
                           MOVE 'Y'                       TO WS-ERR-FLG
                           MOVE CCDA-MSG-INVALID-KEY      TO WS-MESSAGE
                           PERFORM SEND-TRNADD-SCREEN
                   END-EVALUATE
               END-IF
           END-IF

           EXEC CICS RETURN
                     TRANSID (WS-TRANID)
                     COMMAREA (DFHCOMMAREA)
                     LENGTH (650)
           END-EXEC.

      *----------------------------------------------------------------*
      *                      LOAD-LOOKUP-TABLES
      *----------------------------------------------------------------*
       LOAD-LOOKUP-TABLES.
           PERFORM VARYING WS-SUB FROM 1 BY 1
               UNTIL WS-SUB > WS-TYPE-ENTRIES
               MOVE WS-TYPE-CODE-R(WS-SUB) TO WS-TYPE-CODE(WS-SUB)
               MOVE WS-TYPE-DESC-R(WS-SUB) TO WS-TYPE-DESC(WS-SUB)
           END-PERFORM

           PERFORM VARYING WS-SUB FROM 1 BY 1
               UNTIL WS-SUB > WS-CAT-ENTRIES
               MOVE WS-CAT-CODE-R(WS-SUB) TO WS-CAT-CODE(WS-SUB)
               MOVE WS-CAT-DESC-R(WS-SUB) TO WS-CAT-DESC(WS-SUB)
           END-PERFORM.

      *----------------------------------------------------------------*
      *                      PROCESS-ENTER-KEY
      *----------------------------------------------------------------*
       PROCESS-ENTER-KEY.
           PERFORM VALIDATE-INPUT-KEY-FIELDS
           PERFORM VALIDATE-INPUT-DATA-FIELDS.

           IF NOT ERR-FLG-ON
               PERFORM PROCEED-TO-MERCHANT-SCREEN
           ELSE
               PERFORM SEND-TRNADD-SCREEN
           END-IF.

      *----------------------------------------------------------------*
      *                      PROCEED-TO-MERCHANT-SCREEN
      *----------------------------------------------------------------*
       PROCEED-TO-MERCHANT-SCREEN.
      *    Save transaction data to LK-TRAN-RECORD
           MOVE TTYPCDI OF COTRN2AI     TO LK-TRAN-TYPE-CD
           MOVE TCATCDI OF COTRN2AI     TO LK-TRAN-CAT-CD
           MOVE TRNSRCI OF COTRN2AI     TO LK-TRAN-SOURCE
           MOVE TDESCI OF COTRN2AI      TO LK-TRAN-DESC
           MOVE TRNAMTI OF COTRN2AI     TO LK-TRAN-AMT
           MOVE CARDNINI OF COTRN2AI    TO LK-TRAN-CARD-NUM
           MOVE TORIGDTI OF COTRN2AI    TO LK-TRAN-ORIG-TS
           MOVE TPROCDTI OF COTRN2AI    TO LK-TRAN-PROC-TS

           MOVE 'COTRN2BD' TO CDEMO-TO-PROGRAM
           MOVE WS-TRANID  TO CDEMO-FROM-TRANID
           MOVE WS-PGMNAME TO CDEMO-FROM-PROGRAM
           PERFORM RETURN-TO-PREV-SCREEN.

       PROCESS-F1-LOOKUP.
           EVALUATE EIBCPOSN
               WHEN 734 THRU 735
                   PERFORM SHOW-TYPE-LOOKUP
               WHEN 767 THRU 770
                   PERFORM SHOW-CATEGORY-LOOKUP
               WHEN OTHER
                   MOVE 'F1 only available on Type or Category fields'
                        TO ERRMSGO OF COTRN2AO
                   EXEC CICS SEND
                             MAP('COTRN2A')
                             MAPSET('COTRN2A')
                             FROM(COTRN2AO)
                             DATAONLY
                             CURSOR(EIBCPOSN)
                   END-EXEC
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      SHOW-TYPE-LOOKUP (UPDATED)
      *----------------------------------------------------------------*
       SHOW-TYPE-LOOKUP.
           MOVE LOW-VALUES TO TYPELKO
           MOVE -1 TO TYPE1L OF TYPELKI

           EXEC CICS SEND
                     MAP('TYPELK')
                     MAPSET('COTYPLK')
                     FROM(TYPELKO)
                     CURSOR
           END-EXEC

           EXEC CICS RECEIVE
                     MAP('TYPELK')
                     MAPSET('COTYPLK')
                     INTO(TYPELKI)
                     RESP(WS-RESP-CD)
           END-EXEC

           IF WS-RESP-CD = DFHRESP(NORMAL)
               PERFORM PROCESS-TYPE-SELECTION
               MOVE -1 TO TCATCDL OF COTRN2AI
           END-IF

           PERFORM SEND-TRNADD-SCREEN.

      *----------------------------------------------------------------*
      *                      SHOW-CATEGORY-LOOKUP (UPDATED)
      *----------------------------------------------------------------*
       SHOW-CATEGORY-LOOKUP.
           MOVE LOW-VALUES TO CATEGLKO
           MOVE -1 TO CAT1L OF CATEGLKI

           EXEC CICS SEND
                     MAP('CATEGLK')
                     MAPSET('COCATLK')
                     FROM(CATEGLKO)
                     CURSOR
           END-EXEC

           EXEC CICS RECEIVE
                     MAP('CATEGLK')
                     MAPSET('COCATLK')
                     INTO(CATEGLKI)
                     RESP(WS-RESP-CD)
           END-EXEC

           IF WS-RESP-CD = DFHRESP(NORMAL)
               PERFORM PROCESS-CATEGORY-SELECTION
               MOVE -1 TO TRNSRCL OF COTRN2AI
           END-IF

           PERFORM SEND-TRNADD-SCREEN.

      *----------------------------------------------------------------*
      *                      PROCESS-TYPE-SELECTION
      *----------------------------------------------------------------*
       PROCESS-TYPE-SELECTION.
      *    Need to determine which field was entered
      *    Check each TYPE field to see which one has data
           EVALUATE TRUE
               WHEN TYPE1I OF TYPELKI NOT = SPACES AND LOW-VALUES
                   MOVE 'DB' TO TTYPCDI OF COTRN2AI
               WHEN TYPE2I OF TYPELKI NOT = SPACES AND LOW-VALUES
                   MOVE 'CR' TO TTYPCDI OF COTRN2AI
               WHEN TYPE3I OF TYPELKI NOT = SPACES AND LOW-VALUES
                   MOVE 'RF' TO TTYPCDI OF COTRN2AI
               WHEN TYPE4I OF TYPELKI NOT = SPACES AND LOW-VALUES
                   MOVE 'AD' TO TTYPCDI OF COTRN2AI
               WHEN TYPE5I OF TYPELKI NOT = SPACES AND LOW-VALUES
                   MOVE 'FE' TO TTYPCDI OF COTRN2AI
               WHEN TYPE6I OF TYPELKI NOT = SPACES AND LOW-VALUES
                   MOVE 'CH' TO TTYPCDI OF COTRN2AI
               WHEN TYPE7I OF TYPELKI NOT = SPACES AND LOW-VALUES
                   MOVE 'AU' TO TTYPCDI OF COTRN2AI
               WHEN TYPE8I OF TYPELKI NOT = SPACES AND LOW-VALUES
                   MOVE 'VO' TO TTYPCDI OF COTRN2AI
               WHEN TYPE9I OF TYPELKI NOT = SPACES AND LOW-VALUES
                   MOVE 'RT' TO TTYPCDI OF COTRN2AI
               WHEN OTHER
                   MOVE 'Invalid selection. Please try again.'
                        TO WS-MESSAGE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      PROCESS-CATEGORY-SELECTION
      *----------------------------------------------------------------*
       PROCESS-CATEGORY-SELECTION.
      *    Need to determine which field was entered
      *    Check each CAT field to see which one has data
           EVALUATE TRUE
               WHEN CAT1I OF CATEGLKI NOT = SPACES AND LOW-VALUES
                   MOVE 5699 TO TCATCDI OF COTRN2AI
               WHEN CAT2I OF CATEGLKI NOT = SPACES AND LOW-VALUES
                   MOVE 5812 TO TCATCDI OF COTRN2AI
               WHEN CAT3I OF CATEGLKI NOT = SPACES AND LOW-VALUES
                   MOVE 5541 TO TCATCDI OF COTRN2AI
               WHEN CAT4I OF CATEGLKI NOT = SPACES AND LOW-VALUES
                   MOVE 5411 TO TCATCDI OF COTRN2AI
               WHEN CAT5I OF CATEGLKI NOT = SPACES AND LOW-VALUES
                   MOVE 5311 TO TCATCDI OF COTRN2AI
               WHEN CAT6I OF CATEGLKI NOT = SPACES AND LOW-VALUES
                   MOVE 5912 TO TCATCDI OF COTRN2AI
               WHEN CAT7I OF CATEGLKI NOT = SPACES AND LOW-VALUES
                   MOVE 4900 TO TCATCDI OF COTRN2AI
               WHEN CAT8I OF CATEGLKI NOT = SPACES AND LOW-VALUES
                   MOVE 4814 TO TCATCDI OF COTRN2AI
               WHEN CAT9I OF CATEGLKI NOT = SPACES AND LOW-VALUES
                   MOVE 4511 TO TCATCDI OF COTRN2AI
               WHEN CAT10I OF CATEGLKI NOT = SPACES AND LOW-VALUES
                   MOVE 7011 TO TCATCDI OF COTRN2AI
               WHEN CAT11I OF CATEGLKI NOT = SPACES AND LOW-VALUES
                   MOVE 7832 TO TCATCDI OF COTRN2AI
               WHEN CAT12I OF CATEGLKI NOT = SPACES AND LOW-VALUES
                   MOVE 5691 TO TCATCDI OF COTRN2AI
               WHEN CAT13I OF CATEGLKI NOT = SPACES AND LOW-VALUES
                   MOVE 5732 TO TCATCDI OF COTRN2AI
               WHEN OTHER
                   MOVE 'Invalid selection. Please try again.'
                       TO WS-MESSAGE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      VALIDATE-INPUT-KEY-FIELDS
      *----------------------------------------------------------------*
       VALIDATE-INPUT-KEY-FIELDS.
           EVALUATE TRUE
               WHEN ACTIDINI OF COTRN2AI NOT = SPACES AND LOW-VALUES
                   IF ACTIDINI OF COTRN2AI IS NOT NUMERIC
                       MOVE 'Y'     TO WS-ERR-FLG
                       MOVE 'Account ID must be Numeric...' TO
                                       WS-MESSAGE
                       MOVE -1       TO ACTIDINL OF COTRN2AI
                       PERFORM SEND-TRNADD-SCREEN
                   END-IF
                   COMPUTE WS-ACCT-ID-N = FUNCTION NUMVAL(ACTIDINI OF
                   COTRN2AI)
                   MOVE WS-ACCT-ID-N            TO HV-XREF-ACCT-ID
                                                ACTIDINI OF COTRN2AI
                   PERFORM READ-CXACAIX-BY-ACCT
                   MOVE HV-XREF-CARD-NUM        TO CARDNINI OF COTRN2AI
               WHEN CARDNINI OF COTRN2AI NOT = SPACES AND LOW-VALUES
                   IF CARDNINI OF COTRN2AI IS NOT NUMERIC
                       MOVE 'Y'     TO WS-ERR-FLG
                       MOVE 'Card Number must be Numeric...' TO
                                       WS-MESSAGE
                       MOVE -1       TO CARDNINL OF COTRN2AI
                       PERFORM SEND-TRNADD-SCREEN
                   END-IF
                   COMPUTE WS-CARD-NUM-N = FUNCTION NUMVAL(CARDNINI OF
                   COTRN2AI)
                   MOVE WS-CARD-NUM-N        TO HV-XREF-CARD-NUM
                                                CARDNINI OF COTRN2AI
                   PERFORM READ-CXACAIX-BY-CARD
                   MOVE HV-XREF-ACCT-ID         TO ACTIDINI OF COTRN2AI
               WHEN OTHER
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Account or Card Number must be entered...' TO
                                   WS-MESSAGE
                   MOVE -1       TO ACTIDINL OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                 VALIDATE-INPUT-DATA-FIELDS
      *----------------------------------------------------------------*
       VALIDATE-INPUT-DATA-FIELDS.
           IF ERR-FLG-ON
               MOVE SPACES      TO TTYPCDI  OF COTRN2AI
                                   TCATCDI  OF COTRN2AI
                                   TRNSRCI  OF COTRN2AI
                                   TRNAMTI  OF COTRN2AI
                                   TDESCI   OF COTRN2AI
                                   TORIGDTI OF COTRN2AI
                                   TPROCDTI OF COTRN2AI
           END-IF.

           EVALUATE TRUE
               WHEN TTYPCDI OF COTRN2AI = SPACES OR LOW-VALUES
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Type CD can NOT be empty. Press F1 for list'
                                TO WS-MESSAGE
                   MOVE -1       TO TTYPCDL OF COTRN2AI
               WHEN TCATCDI OF COTRN2AI = SPACES OR LOW-VALUES
                   MOVE 'Y'     TO WS-ERR-FLG
                  MOVE 'Category CD can NOT be empty. Press F1 for list'
                                TO WS-MESSAGE
                   MOVE -1       TO TCATCDL OF COTRN2AI
               WHEN TRNSRCI OF COTRN2AI = SPACES OR LOW-VALUES
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Source can NOT be empty...' TO WS-MESSAGE
                   MOVE -1       TO TRNSRCL OF COTRN2AI
               WHEN TDESCI OF COTRN2AI = SPACES OR LOW-VALUES
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Description can NOT be empty...' TO WS-MESSAGE
                   MOVE -1       TO TDESCL OF COTRN2AI
               WHEN TRNAMTI OF COTRN2AI = SPACES OR LOW-VALUES
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Amount can NOT be empty...' TO WS-MESSAGE
                   MOVE -1       TO TRNAMTL OF COTRN2AI
               WHEN TORIGDTI OF COTRN2AI = SPACES OR LOW-VALUES
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Orig Date can NOT be empty...' TO WS-MESSAGE
                   MOVE -1       TO TORIGDTL OF COTRN2AI
               WHEN TPROCDTI OF COTRN2AI = SPACES OR LOW-VALUES
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Proc Date can NOT be empty...' TO WS-MESSAGE
                   MOVE -1       TO TPROCDTL OF COTRN2AI
               WHEN OTHER
                   PERFORM VALIDATE-TYPE-CODE
                   PERFORM VALIDATE-CATEGORY-CODE
                   PERFORM VALIDATE-AMOUNT-FORMAT
                   PERFORM VALIDATE-DATE-FORMATS
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      VALIDATE-TYPE-CODE
      *----------------------------------------------------------------*
       VALIDATE-TYPE-CODE.
           PERFORM VARYING WS-SUB FROM 1 BY 1
               UNTIL WS-SUB > WS-TYPE-ENTRIES
               IF TTYPCDI OF COTRN2AI = WS-TYPE-CODE(WS-SUB)
                   EXIT PARAGRAPH
               END-IF
           END-PERFORM

           IF WS-SUB > WS-TYPE-ENTRIES
               MOVE 'Y' TO WS-ERR-FLG
               MOVE 'Invalid Type Code. Press F1 for valid codes'
                    TO WS-MESSAGE
               MOVE -1 TO TTYPCDL OF COTRN2AI
           END-IF.

      *----------------------------------------------------------------*
      *                      VALIDATE-CATEGORY-CODE
      *----------------------------------------------------------------*
       VALIDATE-CATEGORY-CODE.
           PERFORM VARYING WS-SUB FROM 1 BY 1
               UNTIL WS-SUB > WS-CAT-ENTRIES
               IF TCATCDI OF COTRN2AI = WS-CAT-CODE(WS-SUB)
                   EXIT PARAGRAPH
               END-IF
           END-PERFORM

           IF WS-SUB > WS-CAT-ENTRIES
               MOVE 'Y' TO WS-ERR-FLG
               MOVE 'Invalid Category Code. Press F2 for valid codes'
                    TO WS-MESSAGE
               MOVE -1 TO TCATCDL OF COTRN2AI
           END-IF.

      *----------------------------------------------------------------*
      *                      VALIDATE-AMOUNT-FORMAT
      *----------------------------------------------------------------*
       VALIDATE-AMOUNT-FORMAT.
           EVALUATE TRUE
               WHEN TRNAMTI OF COTRN2AI(1:1) NOT EQUAL '-' AND '+'
               WHEN TRNAMTI OF COTRN2AI(2:8) NOT NUMERIC
               WHEN TRNAMTI OF COTRN2AI(10:1) NOT = '.'
               WHEN TRNAMTI OF COTRN2AI(11:2) IS NOT NUMERIC
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Amount should be in format -99999999.99' TO
                                   WS-MESSAGE
                   MOVE -1       TO TRNAMTL OF COTRN2AI
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      VALIDATE-DATE-FORMATS
      *----------------------------------------------------------------*
       VALIDATE-DATE-FORMATS.
           EVALUATE TRUE
               WHEN TORIGDTI OF COTRN2AI(1:4) IS NOT NUMERIC
               WHEN TORIGDTI OF COTRN2AI(5:1) NOT EQUAL '-'
               WHEN TORIGDTI OF COTRN2AI(6:2) NOT NUMERIC
               WHEN TORIGDTI OF COTRN2AI(8:1) NOT EQUAL '-'
               WHEN TORIGDTI OF COTRN2AI(9:2) NOT NUMERIC
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Orig Date should be in format YYYY-MM-DD' TO
                                   WS-MESSAGE
                   MOVE -1       TO TORIGDTL OF COTRN2AI
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           EVALUATE TRUE
               WHEN TPROCDTI OF COTRN2AI(1:4) IS NOT NUMERIC
               WHEN TPROCDTI OF COTRN2AI(5:1) NOT EQUAL '-'
               WHEN TPROCDTI OF COTRN2AI(6:2) NOT NUMERIC
               WHEN TPROCDTI OF COTRN2AI(8:1) NOT EQUAL '-'
               WHEN TPROCDTI OF COTRN2AI(9:2) NOT NUMERIC
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Proc Date should be in format YYYY-MM-DD' TO
                                   WS-MESSAGE
                   MOVE -1       TO TPROCDTL OF COTRN2AI
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      RETURN-TO-PREV-SCREEN
      *----------------------------------------------------------------*
       RETURN-TO-PREV-SCREEN.
           IF CDEMO-TO-PROGRAM = LOW-VALUES OR SPACES
               MOVE 'COSGN00D' TO CDEMO-TO-PROGRAM
           END-IF
           MOVE WS-TRANID    TO CDEMO-FROM-TRANID
           MOVE WS-PGMNAME   TO CDEMO-FROM-PROGRAM
           MOVE ZEROS        TO CDEMO-PGM-CONTEXT

           MOVE CARDDEMO-COMMAREA TO LK-CARDDEMO-COMMAREA

           EXEC CICS
               XCTL PROGRAM(CDEMO-TO-PROGRAM)
               COMMAREA(DFHCOMMAREA)
               LENGTH(650)
           END-EXEC.

      *----------------------------------------------------------------*
      *                      SEND-TRNADD-SCREEN
      *----------------------------------------------------------------*
       SEND-TRNADD-SCREEN.
           PERFORM POPULATE-HEADER-INFO
           MOVE WS-MESSAGE TO ERRMSGO OF COTRN2AO

           EXEC CICS SEND
                     MAP('COTRN2A')
                     MAPSET('COTRN2A')
                     FROM(COTRN2AO)
                     ERASE
                     CURSOR
           END-EXEC.

           MOVE CARDDEMO-COMMAREA TO LK-CARDDEMO-COMMAREA

           EXEC CICS RETURN
                     TRANSID (WS-TRANID)
                     COMMAREA (DFHCOMMAREA)
                     LENGTH (650)
           END-EXEC.

      *----------------------------------------------------------------*
      *                      RECEIVE-TRNADD-SCREEN
      *----------------------------------------------------------------*
       RECEIVE-TRNADD-SCREEN.
           EXEC CICS RECEIVE
                     MAP('COTRN2A')
                     MAPSET('COTRN2A')
                     INTO(COTRN2AI)
                     RESP(WS-RESP-CD)
                     RESP2(WS-REAS-CD)
           END-EXEC.

      *----------------------------------------------------------------*
      *                      POPULATE-HEADER-INFO
      *----------------------------------------------------------------*
       POPULATE-HEADER-INFO.
           MOVE FUNCTION CURRENT-DATE  TO WS-CURDATE-DATA
           MOVE CCDA-TITLE01           TO TITLE01O OF COTRN2AO
           MOVE CCDA-TITLE02           TO TITLE02O OF COTRN2AO
           MOVE WS-TRANID              TO TRNNAMEO OF COTRN2AO
           MOVE WS-PGMNAME             TO PGMNAMEO OF COTRN2AO

           MOVE WS-CURDATE-MONTH       TO WS-CURDATE-MM
           MOVE WS-CURDATE-DAY         TO WS-CURDATE-DD
           MOVE WS-CURDATE-YEAR(3:2)   TO WS-CURDATE-YY
           MOVE WS-CURDATE-MM-DD-YY    TO CURDATEO OF COTRN2AO

           MOVE WS-CURTIME-HOURS       TO WS-CURTIME-HH
           MOVE WS-CURTIME-MINUTE      TO WS-CURTIME-MM
           MOVE WS-CURTIME-SECOND      TO WS-CURTIME-SS
           MOVE WS-CURTIME-HH-MM-SS    TO CURTIMEO OF COTRN2AO.

      *----------------------------------------------------------------*
      *                      CLEAR-CURRENT-SCREEN
      *----------------------------------------------------------------*
       CLEAR-CURRENT-SCREEN.
           PERFORM INITIALIZE-ALL-FIELDS.
           PERFORM SEND-TRNADD-SCREEN.

      *----------------------------------------------------------------*
      *                      INITIALIZE-ALL-FIELDS
      *----------------------------------------------------------------*
       INITIALIZE-ALL-FIELDS.
           MOVE -1              TO ACTIDINL OF COTRN2AI
           MOVE SPACES          TO ACTIDINI OF COTRN2AI
                                   CARDNINI OF COTRN2AI
                                   TTYPCDI  OF COTRN2AI
                                   TCATCDI  OF COTRN2AI
                                   TRNSRCI  OF COTRN2AI
                                   TRNAMTI  OF COTRN2AI
                                   TDESCI   OF COTRN2AI
                                   TORIGDTI OF COTRN2AI
                                   TPROCDTI OF COTRN2AI
                                   WS-MESSAGE

           INITIALIZE LK-TRAN-RECORD.

      *----------------------------------------------------------------*
      *                      RESTORE-SCREEN-DATA
      *----------------------------------------------------------------*
       RESTORE-SCREEN-DATA.
      *    Restore screen fields from LK-TRAN-RECORD when returning
           IF LK-TRAN-TYPE-CD NOT = SPACES AND LOW-VALUES
               MOVE LK-TRAN-TYPE-CD TO TTYPCDI OF COTRN2AI
           END-IF
           IF LK-TRAN-CAT-CD NOT = ZERO
               MOVE LK-TRAN-CAT-CD TO TCATCDI OF COTRN2AI
           END-IF
           IF LK-TRAN-SOURCE NOT = SPACES AND LOW-VALUES
               MOVE LK-TRAN-SOURCE TO TRNSRCI OF COTRN2AI
           END-IF
           IF LK-TRAN-DESC NOT = SPACES AND LOW-VALUES
               MOVE LK-TRAN-DESC TO TDESCI OF COTRN2AI
           END-IF
           IF LK-TRAN-AMT NOT = SPACES AND LOW-VALUES
               MOVE LK-TRAN-AMT TO TRNAMTI OF COTRN2AI
           END-IF
           IF LK-TRAN-CARD-NUM NOT = SPACES AND LOW-VALUES
               MOVE LK-TRAN-CARD-NUM TO CARDNINI OF COTRN2AI
      *        Also restore account ID when card number is restored
               IF LK-TRAN-CARD-NUM IS NUMERIC
                   COMPUTE WS-CARD-NUM-N =
                       FUNCTION NUMVAL(LK-TRAN-CARD-NUM)
                   MOVE WS-CARD-NUM-N TO HV-XREF-CARD-NUM
                   PERFORM READ-CXACAIX-BY-CARD
                   MOVE HV-XREF-ACCT-ID TO ACTIDINI OF COTRN2AI
               END-IF
           END-IF
           IF LK-TRAN-ORIG-TS NOT = SPACES AND LOW-VALUES
               MOVE LK-TRAN-ORIG-TS TO TORIGDTI OF COTRN2AI
           END-IF
           IF LK-TRAN-PROC-TS NOT = SPACES AND LOW-VALUES
               MOVE LK-TRAN-PROC-TS TO TPROCDTI OF COTRN2AI
           END-IF.

      *----------------------------------------------------------------*
      *                      READ-CXACAIX-BY-ACCT
      *----------------------------------------------------------------*
       READ-CXACAIX-BY-ACCT.
           EXEC SQL
               SELECT XREF_ACCT_ID, XREF_CARD_NUM, XREF_CUST_ID
               INTO :HV-XREF-ACCT-ID, :HV-XREF-CARD-NUM                
               FROM CXACAIX
               WHERE XREF_ACCT_ID = :HV-XREF-ACCT-ID
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Account ID NOT found...' TO WS-MESSAGE
                   MOVE -1       TO ACTIDINL OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
               WHEN OTHER
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Unable to lookup Card # in XREF file...' TO
                                   WS-MESSAGE
                   MOVE -1       TO CARDNINL OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      READ-CXACAIX-BY-CARD
      *----------------------------------------------------------------*
       READ-CXACAIX-BY-CARD.
           EXEC SQL
               SELECT XREF_ACCT_ID, XREF_CARD_NUM, XREF_CUST_ID
               INTO :HV-XREF-ACCT-ID, :HV-XREF-CARD-NUM
               WHERE XREF_CARD_NUM = :HV-XREF-CARD-NUM
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Card Number NOT found...' TO WS-MESSAGE
                   MOVE -1       TO CARDNINL OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
               WHEN OTHER
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Unable to lookup Acct in XREF AIX file...' TO
                                   WS-MESSAGE
                   MOVE -1       TO ACTIDINL OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
           END-EVALUATE.

      * END OF PGM
