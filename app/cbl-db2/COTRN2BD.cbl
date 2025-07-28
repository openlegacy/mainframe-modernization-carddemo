******************************************************************
      * Program     : COTRN2BD.CBL
      * Application : CardDemo
      * Type        : CICS COBOL Program
      * Function    : Add Transaction - Screen B (Merchant Details)
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
       PROGRAM-ID. COTRN2BD.
       AUTHOR.     AWS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
      *----------------------------------------------------------------*
      *                     WORKING STORAGE SECTION
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

       01 WS-VARIABLES.
         05 WS-PGMNAME                 PIC X(08) VALUE 'COTRN2BD'.
         05 WS-TRANID                  PIC X(04) VALUE 'ADT2'.
         05 WS-MESSAGE                 PIC X(80) VALUE SPACES.

         05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.
           88 ERR-FLG-ON                         VALUE 'Y'.
           88 ERR-FLG-OFF                        VALUE 'N'.
         05 WS-RESP-CD                 PIC S9(09) COMP VALUE ZEROS.
         05 WS-REAS-CD                 PIC S9(09) COMP VALUE ZEROS.

         05 WS-MERCHANT-ID-N           PIC 9(09) VALUE 0.


       COPY COCOM01Y.
       COPY COTRN2B.
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
                          ERRMSGO OF COTRN2BO

           IF EIBCALEN = 0
               MOVE 'COSGN00D' TO CDEMO-TO-PROGRAM
               PERFORM RETURN-TO-PREV-SCREEN
           ELSE
               MOVE DFHCOMMAREA(1:300) TO LK-CARDDEMO-COMMAREA
               MOVE LK-CARDDEMO-COMMAREA TO CARDDEMO-COMMAREA
               IF NOT CDEMO-PGM-REENTER
                   SET CDEMO-PGM-REENTER    TO TRUE
                   MOVE LOW-VALUES          TO COTRN2BO
                   MOVE -1       TO MIDL OF COTRN2BI
                   PERFORM RESTORE-SCREEN-DATA
                   PERFORM SEND-MERCHANT-SCREEN
               ELSE
                   PERFORM RECEIVE-MERCHANT-SCREEN
                   EVALUATE EIBAID
                       WHEN DFHENTER
                           PERFORM PROCESS-ENTER-KEY
                       WHEN DFHPF3
                           PERFORM RETURN-TO-TRANSACTION-SCREEN
                       WHEN DFHPF8
                           PERFORM PROCEED-TO-REVIEW-SCREEN
                       WHEN DFHPF4
                           PERFORM CLEAR-CURRENT-SCREEN
                       WHEN OTHER
                           MOVE 'Y'                       TO WS-ERR-FLG
                           MOVE CCDA-MSG-INVALID-KEY      TO WS-MESSAGE
                           PERFORM SEND-MERCHANT-SCREEN
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
           PERFORM VALIDATE-MERCHANT-FIELDS.

           IF NOT ERR-FLG-ON
               PERFORM PROCEED-TO-REVIEW-SCREEN
           ELSE
               PERFORM SEND-MERCHANT-SCREEN
           END-IF.

      *----------------------------------------------------------------*
      *                      RETURN-TO-TRANSACTION-SCREEN
      *----------------------------------------------------------------*
       RETURN-TO-TRANSACTION-SCREEN.
           DISPLAY 'BD - BEFORE XCTL TO: ' CDEMO-TO-PROGRAM
              DISPLAY 'BD - LK-TRAN-RECORD: [' LK-TRAN-RECORD ']'
              DISPLAY 'BD - LK-TRAN-TYPE-CD: [' LK-TRAN-TYPE-CD ']'

           MOVE 'COTRN2AD' TO CDEMO-TO-PROGRAM
           MOVE WS-TRANID  TO CDEMO-FROM-TRANID
           MOVE WS-PGMNAME TO CDEMO-FROM-PROGRAM
           PERFORM RETURN-TO-PREV-SCREEN.

      *----------------------------------------------------------------*
      *                      PROCEED-TO-REVIEW-SCREEN
      *----------------------------------------------------------------*
       PROCEED-TO-REVIEW-SCREEN.
      *    Add merchant data to LK-TRAN-RECORD
           MOVE MIDI OF COTRN2BI            TO LK-TRAN-MERCHANT-ID
           MOVE MNAMEI OF COTRN2BI          TO LK-TRAN-MERCHANT-NAME
           MOVE MCITYI OF COTRN2BI          TO LK-TRAN-MERCHANT-CITY
           MOVE MZIPI OF COTRN2BI           TO LK-TRAN-MERCHANT-ZIP

           MOVE 'COTRN2CD' TO CDEMO-TO-PROGRAM
           MOVE WS-TRANID  TO CDEMO-FROM-TRANID
           MOVE WS-PGMNAME TO CDEMO-FROM-PROGRAM
           PERFORM RETURN-TO-PREV-SCREEN.

      *----------------------------------------------------------------*
      *                      VALIDATE-MERCHANT-FIELDS
      *----------------------------------------------------------------*
       VALIDATE-MERCHANT-FIELDS.
           EVALUATE TRUE
               WHEN MIDI OF COTRN2BI = SPACES OR LOW-VALUES
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Merchant ID can NOT be empty...' TO
                                   WS-MESSAGE
                   MOVE -1       TO MIDL OF COTRN2BI
               WHEN MNAMEI OF COTRN2BI = SPACES OR LOW-VALUES
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Merchant Name can NOT be empty...' TO
                                   WS-MESSAGE
                   MOVE -1       TO MNAMEL OF COTRN2BI
               WHEN MCITYI OF COTRN2BI = SPACES OR LOW-VALUES
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Merchant City can NOT be empty...' TO
                                   WS-MESSAGE
                   MOVE -1       TO MCITYL OF COTRN2BI
               WHEN MZIPI OF COTRN2BI = SPACES OR LOW-VALUES
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Merchant Zip can NOT be empty...' TO
                                   WS-MESSAGE
                   MOVE -1       TO MZIPL OF COTRN2BI
               WHEN OTHER
                   PERFORM VALIDATE-MERCHANT-ID-NUMERIC
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      VALIDATE-MERCHANT-ID-NUMERIC
      *----------------------------------------------------------------*
       VALIDATE-MERCHANT-ID-NUMERIC.
           IF MIDI OF COTRN2BI IS NOT NUMERIC
               MOVE 'Y'     TO WS-ERR-FLG
               MOVE 'Merchant ID must be Numeric...' TO
                               WS-MESSAGE
               MOVE -1       TO MIDL OF COTRN2BI
           END-IF.

      *----------------------------------------------------------------*
      *                      RESTORE-SCREEN-DATA
      *----------------------------------------------------------------*
       RESTORE-SCREEN-DATA.
      *    Restore merchant fields from LK-TRAN-RECORD when returning
           IF LK-TRAN-MERCHANT-ID NOT = SPACES AND LOW-VALUES
               MOVE LK-TRAN-MERCHANT-ID TO MIDI OF COTRN2BI
           END-IF
           IF LK-TRAN-MERCHANT-NAME NOT = SPACES AND LOW-VALUES
               MOVE LK-TRAN-MERCHANT-NAME TO MNAMEI OF COTRN2BI
           END-IF
           IF LK-TRAN-MERCHANT-CITY NOT = SPACES AND LOW-VALUES
               MOVE LK-TRAN-MERCHANT-CITY TO MCITYI OF COTRN2BI
           END-IF
           IF LK-TRAN-MERCHANT-ZIP NOT = SPACES AND LOW-VALUES
               MOVE LK-TRAN-MERCHANT-ZIP TO MZIPI OF COTRN2BI
           END-IF.

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
      *                      SEND-MERCHANT-SCREEN
      *----------------------------------------------------------------*
       SEND-MERCHANT-SCREEN.
           PERFORM POPULATE-HEADER-INFO
           MOVE WS-MESSAGE TO ERRMSGO OF COTRN2BO

           EXEC CICS SEND
                     MAP('COTRN2B')
                     MAPSET('COTRN2B')
                     FROM(COTRN2BO)
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
      *                      RECEIVE-MERCHANT-SCREEN
      *----------------------------------------------------------------*
       RECEIVE-MERCHANT-SCREEN.
           EXEC CICS RECEIVE
                     MAP('COTRN2B')
                     MAPSET('COTRN2B')
                     INTO(COTRN2BI)
                     RESP(WS-RESP-CD)
                     RESP2(WS-REAS-CD)
           END-EXEC.

      *----------------------------------------------------------------*
      *                      POPULATE-HEADER-INFO
      *----------------------------------------------------------------*
       POPULATE-HEADER-INFO.
           MOVE FUNCTION CURRENT-DATE  TO WS-CURDATE-DATA
           MOVE CCDA-TITLE01           TO TITLE01O OF COTRN2BO
           MOVE CCDA-TITLE02           TO TITLE02O OF COTRN2BO
           MOVE WS-TRANID              TO TRNNAMEO OF COTRN2BO
           MOVE WS-PGMNAME             TO PGMNAMEO OF COTRN2BO

           MOVE WS-CURDATE-MONTH       TO WS-CURDATE-MM
           MOVE WS-CURDATE-DAY         TO WS-CURDATE-DD
           MOVE WS-CURDATE-YEAR(3:2)   TO WS-CURDATE-YY
           MOVE WS-CURDATE-MM-DD-YY    TO CURDATEO OF COTRN2BO

           MOVE WS-CURTIME-HOURS       TO WS-CURTIME-HH
           MOVE WS-CURTIME-MINUTE      TO WS-CURTIME-MM
           MOVE WS-CURTIME-SECOND      TO WS-CURTIME-SS
           MOVE WS-CURTIME-HH-MM-SS    TO CURTIMEO OF COTRN2BO.

      *----------------------------------------------------------------*
      *                      CLEAR-CURRENT-SCREEN
      *----------------------------------------------------------------*
       CLEAR-CURRENT-SCREEN.
           PERFORM INITIALIZE-ALL-FIELDS.
           PERFORM SEND-MERCHANT-SCREEN.

      *----------------------------------------------------------------*
      *                      INITIALIZE-ALL-FIELDS
      *----------------------------------------------------------------*
       INITIALIZE-ALL-FIELDS.
           MOVE -1              TO MIDL OF COTRN2BI
           MOVE SPACES          TO MIDI     OF COTRN2BI
                                   MNAMEI   OF COTRN2BI
                                   MCITYI   OF COTRN2BI
                                   MZIPI    OF COTRN2BI
                                   WS-MESSAGE

           INITIALIZE LK-TRAN-MERCHANT-ID
                      LK-TRAN-MERCHANT-NAME
                      LK-TRAN-MERCHANT-CITY
                      LK-TRAN-MERCHANT-ZIP.

      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:12:34 CDT
      *