******************************************************************
      * Program     : COTRN2CD.CBL
      * Application : CardDemo
      * Type        : CICS COBOL Program
      * Function    : Add Transaction - Screen C (Review/Confirmation)
      *               DB2 Version with TRAN-RECORD in COMMAREA
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
       PROGRAM-ID. COTRN2CD.
       AUTHOR.     AWS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
      *----------------------------------------------------------------*
      *                     WORKING STORAGE SECTION
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

       01 WS-VARIABLES.
         05 WS-PGMNAME                 PIC X(08) VALUE 'COTRN2CD'.
         05 WS-TRANID                  PIC X(04) VALUE 'ADT3'.
         05 WS-MESSAGE                 PIC X(80) VALUE SPACES.
         05 WS-RESP-CD                 PIC S9(09) COMP VALUE ZEROS.
         05 WS-REAS-CD                 PIC S9(09) COMP VALUE ZEROS.

         05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.
           88 ERR-FLG-ON                         VALUE 'Y'.
           88 ERR-FLG-OFF                        VALUE 'N'.

         05 WS-TRAN-ID-N               PIC 9(16) VALUE ZEROS.
         05 WS-TRAN-AMT-N              PIC S9(9)V99 VALUE ZERO.
         05 WS-TRAN-AMT-E              PIC +99999999.99 VALUE ZEROS.

      *----------------------------------------------------------------*
      * DB2 Host Variables
      *----------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA END-EXEC.

       01 HV-TRANSACT-VARS.
         05 HV-TRAN-ID                 PIC X(16).
         05 HV-TRAN-TYPE-CD            PIC X(02).
         05 HV-TRAN-CAT-CD             PIC S9(04) COMP-3.
         05 HV-TRAN-SOURCE             PIC X(10).
         05 HV-TRAN-DESC               PIC X(100).
         05 HV-TRAN-AMT                PIC S9(11)V99 COMP-3.
         05 HV-TRAN-MERCHANT-ID        PIC S9(09) COMP-3.
         05 HV-TRAN-MERCHANT-NAME      PIC X(50).
         05 HV-TRAN-MERCHANT-CITY      PIC X(50).
         05 HV-TRAN-MERCHANT-ZIP       PIC X(10).
         05 HV-TRAN-CARD-NUM           PIC X(16).
         05 HV-TRAN-ORIG-TS            PIC X(26).
         05 HV-TRAN-PROC-TS            PIC X(26).
         05 HV-MAX-TRAN-ID             PIC X(16).

       COPY COCOM01Y.
       COPY COTRN2C.
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
                          ERRMSGO OF COTRN2CO

           IF EIBCALEN = 0
               MOVE 'COSGN00D' TO CDEMO-TO-PROGRAM
               PERFORM RETURN-TO-PREV-SCREEN
           ELSE
               MOVE DFHCOMMAREA(1:300) TO LK-CARDDEMO-COMMAREA
               MOVE LK-CARDDEMO-COMMAREA TO CARDDEMO-COMMAREA
               IF NOT CDEMO-PGM-REENTER
                   SET CDEMO-PGM-REENTER    TO TRUE
                   MOVE LOW-VALUES          TO COTRN2CO
                   MOVE -1       TO CONFIRML OF COTRN2CI
                   PERFORM POPULATE-REVIEW-DATA
                   PERFORM SEND-REVIEW-SCREEN
               ELSE
                   PERFORM RECEIVE-REVIEW-SCREEN
                   EVALUATE EIBAID
                       WHEN DFHENTER
                           PERFORM PROCESS-ENTER-KEY
                       WHEN DFHPF3
      *                    Save current confirmation state before going back
                           PERFORM SAVE-CURRENT-REVIEW-DATA
                           PERFORM RETURN-TO-MERCHANT-SCREEN
                       WHEN DFHPF4
                           PERFORM CLEAR-AND-START-OVER
                       WHEN OTHER
                           MOVE 'Y'                       TO WS-ERR-FLG
                           MOVE CCDA-MSG-INVALID-KEY      TO WS-MESSAGE
                           PERFORM SEND-REVIEW-SCREEN
                   END-EVALUATE
               END-IF
           END-IF

           EXEC CICS RETURN
                     TRANSID (WS-TRANID)
                     COMMAREA (DFHCOMMAREA)
                     LENGTH (650)
           END-EXEC.

      *----------------------------------------------------------------*
      *                      PROCESS-ENTER-KEY
      *----------------------------------------------------------------*
       PROCESS-ENTER-KEY.
           EVALUATE CONFIRMI OF COTRN2CI
               WHEN 'Y'
               WHEN 'y'
                   PERFORM ADD-TRANSACTION
               WHEN 'N'
               WHEN 'n'
               WHEN SPACES
               WHEN LOW-VALUES
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Confirm to add this transaction (Y/N)...'
                                TO WS-MESSAGE
                   MOVE -1      TO CONFIRML OF COTRN2CI
                   PERFORM SEND-REVIEW-SCREEN
               WHEN OTHER
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Invalid value. Valid values are (Y/N)...'
                                TO WS-MESSAGE
                   MOVE -1      TO CONFIRML OF COTRN2CI
                   PERFORM SEND-REVIEW-SCREEN
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      SAVE-CURRENT-REVIEW-DATA
      *----------------------------------------------------------------*
       SAVE-CURRENT-REVIEW-DATA.
      *    Save the confirmation status if user entered something
      *    The transaction data is already in LK-TRAN-RECORD
      *    We just need to preserve any confirmation input
           CONTINUE.

      *----------------------------------------------------------------*
      *                      RETURN-TO-MERCHANT-SCREEN
      *----------------------------------------------------------------*
       RETURN-TO-MERCHANT-SCREEN.
      *    Transaction data is already in LK-TRAN-RECORD, just navigate back
           MOVE 'COTRN2BD' TO CDEMO-TO-PROGRAM
           MOVE WS-TRANID  TO CDEMO-FROM-TRANID
           MOVE WS-PGMNAME TO CDEMO-FROM-PROGRAM
           PERFORM RETURN-TO-PREV-SCREEN.

      *----------------------------------------------------------------*
      *                      CLEAR-AND-START-OVER
      *----------------------------------------------------------------*
       CLEAR-AND-START-OVER.
           INITIALIZE LK-TRAN-RECORD
           MOVE 'COTRN2AD' TO CDEMO-TO-PROGRAM
           MOVE WS-TRANID  TO CDEMO-FROM-TRANID
           MOVE WS-PGMNAME TO CDEMO-FROM-PROGRAM
           PERFORM RETURN-TO-PREV-SCREEN.

      *----------------------------------------------------------------*
      *                      POPULATE-REVIEW-DATA
      *----------------------------------------------------------------*
       POPULATE-REVIEW-DATA.
      *    Use data from LK-TRAN-RECORD to populate review screen
           MOVE LK-TRAN-CARD-NUM        TO RACCTNBRO OF COTRN2CO
           MOVE LK-TRAN-CARD-NUM        TO RCARDNBRO OF COTRN2CO
           MOVE LK-TRAN-TYPE-CD         TO RTYPECDO OF COTRN2CO
           MOVE LK-TRAN-CAT-CD          TO RCATCDO OF COTRN2CO
           MOVE LK-TRAN-SOURCE          TO RSOURCEO OF COTRN2CO
           MOVE LK-TRAN-AMT             TO RAMOUNTO OF COTRN2CO
           MOVE LK-TRAN-ORIG-TS         TO RORIGDTO OF COTRN2CO
           MOVE LK-TRAN-PROC-TS         TO RPROCDTO OF COTRN2CO
           MOVE LK-TRAN-DESC            TO RDESCO OF COTRN2CO
           MOVE LK-TRAN-MERCHANT-ID     TO RMIDO OF COTRN2CO
           MOVE LK-TRAN-MERCHANT-NAME   TO RMNAMEO OF COTRN2CO
           MOVE LK-TRAN-MERCHANT-CITY   TO RMCITYO OF COTRN2CO
           MOVE LK-TRAN-MERCHANT-ZIP    TO RMZIPO OF COTRN2CO.

      *----------------------------------------------------------------*
      *                      ADD-TRANSACTION
      *----------------------------------------------------------------*
       ADD-TRANSACTION.
           PERFORM GET-NEXT-TRAN-ID
           ADD 1 TO WS-TRAN-ID-N

           MOVE WS-TRAN-ID-N            TO HV-TRAN-ID
           MOVE LK-TRAN-TYPE-CD         TO HV-TRAN-TYPE-CD
           MOVE LK-TRAN-CAT-CD          TO HV-TRAN-CAT-CD
           MOVE LK-TRAN-SOURCE          TO HV-TRAN-SOURCE
           MOVE LK-TRAN-DESC            TO HV-TRAN-DESC
           IF LK-TRAN-AMT IS NUMERIC OR LK-TRAN-AMT(1:1) = '-' OR '+'
               COMPUTE HV-TRAN-AMT = FUNCTION NUMVAL-C(LK-TRAN-AMT)
           ELSE
               MOVE ZERO TO HV-TRAN-AMT
           END-IF
           MOVE LK-TRAN-CARD-NUM        TO HV-TRAN-CARD-NUM
           IF LK-TRAN-MERCHANT-ID IS NUMERIC
               COMPUTE HV-TRAN-MERCHANT-ID =
               FUNCTION NUMVAL(LK-TRAN-MERCHANT-ID)
           ELSE
               MOVE ZERO TO HV-TRAN-MERCHANT-ID
           END-IF
           MOVE LK-TRAN-MERCHANT-NAME   TO HV-TRAN-MERCHANT-NAME
           MOVE LK-TRAN-MERCHANT-CITY   TO HV-TRAN-MERCHANT-CITY
           MOVE LK-TRAN-MERCHANT-ZIP    TO HV-TRAN-MERCHANT-ZIP
           MOVE LK-TRAN-ORIG-TS         TO HV-TRAN-ORIG-TS
           MOVE LK-TRAN-PROC-TS         TO HV-TRAN-PROC-TS

           PERFORM WRITE-TRANSACT-DB2.

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
      *                      SEND-REVIEW-SCREEN
      *----------------------------------------------------------------*
       SEND-REVIEW-SCREEN.
           PERFORM POPULATE-HEADER-INFO
           MOVE WS-MESSAGE TO ERRMSGO OF COTRN2CO

           EXEC CICS SEND
                     MAP('COTRN2C')
                     MAPSET('COTRN2C')
                     FROM(COTRN2CO)
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
      *                      RECEIVE-REVIEW-SCREEN
      *----------------------------------------------------------------*
       RECEIVE-REVIEW-SCREEN.
           EXEC CICS RECEIVE
                     MAP('COTRN2C')
                     MAPSET('COTRN2C')
                     INTO(COTRN2CI)
                     RESP(WS-RESP-CD)
                     RESP2(WS-REAS-CD)
           END-EXEC.

      *----------------------------------------------------------------*
      *                      POPULATE-HEADER-INFO
      *----------------------------------------------------------------*
       POPULATE-HEADER-INFO.
           MOVE FUNCTION CURRENT-DATE  TO WS-CURDATE-DATA
           MOVE CCDA-TITLE01           TO TITLE01O OF COTRN2CO
           MOVE CCDA-TITLE02           TO TITLE02O OF COTRN2CO
           MOVE WS-TRANID              TO TRNNAMEO OF COTRN2CO
           MOVE WS-PGMNAME             TO PGMNAMEO OF COTRN2CO

           MOVE WS-CURDATE-MONTH       TO WS-CURDATE-MM
           MOVE WS-CURDATE-DAY         TO WS-CURDATE-DD
           MOVE WS-CURDATE-YEAR(3:2)   TO WS-CURDATE-YY
           MOVE WS-CURDATE-MM-DD-YY    TO CURDATEO OF COTRN2CO

           MOVE WS-CURTIME-HOURS       TO WS-CURTIME-HH
           MOVE WS-CURTIME-MINUTE      TO WS-CURTIME-MM
           MOVE WS-CURTIME-SECOND      TO WS-CURTIME-SS
           MOVE WS-CURTIME-HH-MM-SS    TO CURTIMEO OF COTRN2CO.

      *----------------------------------------------------------------*
      *                    GET-NEXT-TRAN-ID
      *----------------------------------------------------------------*
       GET-NEXT-TRAN-ID.
           EXEC SQL
               SELECT MAX(TRAN_ID)
               INTO :HV-MAX-TRAN-ID
               FROM ALAINL.TRANSACT
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   IF HV-MAX-TRAN-ID = SPACES OR LOW-VALUES
                       MOVE ZEROS TO WS-TRAN-ID-N
                   ELSE
                       IF HV-MAX-TRAN-ID IS NUMERIC
                           COMPUTE WS-TRAN-ID-N =
                               FUNCTION NUMVAL(HV-MAX-TRAN-ID)
                       ELSE
                           MOVE ZEROS TO WS-TRAN-ID-N
                       END-IF
                   END-IF
               WHEN +100
                   MOVE ZEROS TO WS-TRAN-ID-N
               WHEN OTHER
                   DISPLAY 'SQLCODE:' SQLCODE
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Unable to get next Transaction ID...' TO
                    WS-MESSAGE
                   PERFORM SEND-REVIEW-SCREEN
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                    WRITE-TRANSACT-DB2
      *----------------------------------------------------------------*
       WRITE-TRANSACT-DB2.
           EXEC SQL
               INSERT INTO ALAINL.TRANSACT
               (TRAN_ID, TRAN_TYPE_CD, TRAN_CAT_CD, TRAN_SOURCE,
                TRAN_DESC, TRAN_AMT, TRAN_MERCHANT_ID,
                TRAN_MERCHANT_NAME,
                TRAN_MERCHANT_CITY, TRAN_MERCHANT_ZIP, TRAN_CARD_NUM,
                TRAN_ORIG_TS, TRAN_PROC_TS)
               VALUES
               (:HV-TRAN-ID, :HV-TRAN-TYPE-CD, :HV-TRAN-CAT-CD,
                :HV-TRAN-SOURCE, :HV-TRAN-DESC, :HV-TRAN-AMT,
                :HV-TRAN-MERCHANT-ID, :HV-TRAN-MERCHANT-NAME,
                :HV-TRAN-MERCHANT-CITY, :HV-TRAN-MERCHANT-ZIP,
                :HV-TRAN-CARD-NUM, :HV-TRAN-ORIG-TS, :HV-TRAN-PROC-TS)
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   INITIALIZE LK-TRAN-RECORD
                   MOVE SPACES             TO WS-MESSAGE
                   MOVE DFHGREEN           TO ERRMSGC  OF COTRN2CO
                   STRING 'Transaction added successfully. '
                                               DELIMITED BY SIZE
                     ' Your Tran ID is ' DELIMITED BY SIZE
                          HV-TRAN-ID  DELIMITED BY SPACE
                          '.' DELIMITED BY SIZE
                     INTO WS-MESSAGE
                   PERFORM SEND-REVIEW-SCREEN
               WHEN -803
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Tran ID already exist...' TO WS-MESSAGE
                   PERFORM SEND-REVIEW-SCREEN
               WHEN OTHER
                   DISPLAY 'SQLCODE:' SQLCODE
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Unable to Add Transaction...' TO WS-MESSAGE
                   PERFORM SEND-REVIEW-SCREEN
           END-EVALUATE.

      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:12:34 CDT
      *