      ************************************************************
      * Program     : COUSR01S.CBL
      * Application : CardDemo
      * Type        : CICS COBOL Screen Program
      * Function    : Add a new Regular/Admin user - Screen Interface
      * Transaction:   ALS1
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
      * Modification History
      * 2025-06-01 - Created screen program to call RPC program COUSR01L
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COUSR01S.
       AUTHOR.     AWS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
      *----------------------------------------------------------------*
      *                     WORKING STORAGE SECTION
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

       01 WS-VARIABLES.
         05 WS-PGMNAME                 PIC X(08) VALUE 'COUSR01S'.
         05 WS-TRANID                  PIC X(04) VALUE 'ALS1'.
         05 WS-MESSAGE                 PIC X(80) VALUE SPACES.
         05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.
           88 ERR-FLG-ON                         VALUE 'Y'.
           88 ERR-FLG-OFF                        VALUE 'N'.
         05 WS-RESP-CD                 PIC S9(09) COMP VALUE ZEROS.
         05 WS-REAS-CD                 PIC S9(09) COMP VALUE ZEROS.

      *----------------------------------------------------------------*
      *                     RPC CALL STRUCTURE
      *----------------------------------------------------------------*
       01 WS-RPC-COMMAREA.
          05 WS-RPC-INPUT-PARMS.
             10 WS-RPC-USER-FNAME       PIC X(20).
             10 WS-RPC-USER-LNAME       PIC X(20).
             10 WS-RPC-USER-ID          PIC X(08).
             10 WS-RPC-USER-PASSWORD    PIC X(08).
             10 WS-RPC-USER-TYPE        PIC X(01).

          05 WS-RPC-OUTPUT-PARMS.
             10 WS-RPC-RESP-CODE        PIC S9(04) COMP.
             10 WS-RPC-RESP-MSG         PIC X(80).

       COPY COCOM01Y.

       COPY COUSR01.

       COPY COTTL01Y.
       COPY CSDAT01Y.
       COPY CSMSG01Y.
       COPY CSUSR01Y.

       COPY DFHAID.
       COPY DFHBMSCA.
      *COPY DFHATTR.

      *----------------------------------------------------------------*
      *                        LINKAGE SECTION
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01  DFHCOMMAREA.
         05  LK-COMMAREA                           PIC X(01)
             OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.

      *----------------------------------------------------------------*
      *                      PROCEDURE DIVISION
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       MAIN-PARA.

           SET ERR-FLG-OFF TO TRUE

           MOVE SPACES TO WS-MESSAGE
                          ERRMSGO OF COUSR1AO

           IF EIBCALEN = 0
               MOVE 'COSGN00C' TO CDEMO-TO-PROGRAM
               PERFORM RETURN-TO-PREV-SCREEN
           ELSE
               MOVE DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA
               IF NOT CDEMO-PGM-REENTER
                   SET CDEMO-PGM-REENTER    TO TRUE
                   MOVE LOW-VALUES          TO COUSR1AO
                   MOVE -1       TO FNAMEL OF COUSR1AI
                   PERFORM SEND-USRADD-SCREEN
               ELSE
                   PERFORM RECEIVE-USRADD-SCREEN
                   EVALUATE EIBAID
                       WHEN DFHENTER
                           PERFORM PROCESS-ENTER-KEY
                       WHEN DFHPF3
                           MOVE 'COADM01C' TO CDEMO-TO-PROGRAM
                           PERFORM RETURN-TO-PREV-SCREEN
                       WHEN DFHPF4
                           PERFORM CLEAR-CURRENT-SCREEN
                       WHEN OTHER
                           MOVE 'Y'                       TO WS-ERR-FLG
                           MOVE -1       TO FNAMEL OF COUSR1AI
                           MOVE CCDA-MSG-INVALID-KEY      TO WS-MESSAGE
                           PERFORM SEND-USRADD-SCREEN
                   END-EVALUATE
               END-IF
           END-IF

           EXEC CICS RETURN
                     TRANSID (WS-TRANID)
                     COMMAREA (CARDDEMO-COMMAREA)
           END-EXEC.

      *----------------------------------------------------------------*
      *                      PROCESS-ENTER-KEY
      *----------------------------------------------------------------*
       PROCESS-ENTER-KEY.

           PERFORM CALL-RPC-PROGRAM.

      *----------------------------------------------------------------*
      *                      CALL-RPC-PROGRAM
      *----------------------------------------------------------------*
       CALL-RPC-PROGRAM.

      *    Prepare input parameters for RPC call
           MOVE FNAMEI   OF COUSR1AI TO WS-RPC-USER-FNAME
           MOVE LNAMEI   OF COUSR1AI TO WS-RPC-USER-LNAME
           MOVE USERIDI  OF COUSR1AI TO WS-RPC-USER-ID
           MOVE PASSWDI  OF COUSR1AI TO WS-RPC-USER-PASSWORD
           MOVE USRTYPEI OF COUSR1AI TO WS-RPC-USER-TYPE

      *    Initialize output parameters
           MOVE ZEROS TO WS-RPC-RESP-CODE
           MOVE SPACES TO WS-RPC-RESP-MSG

      *    Call the RPC program
           EXEC CICS LINK
                PROGRAM('COUSR01L')
                COMMAREA(WS-RPC-COMMAREA)
                LENGTH(LENGTH OF WS-RPC-COMMAREA)
                RESP(WS-RESP-CD)
                RESP2(WS-REAS-CD)
           END-EXEC.

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   PERFORM PROCESS-RPC-RESPONSE
               WHEN OTHER
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE 'Error calling user add service...' TO
                                   WS-MESSAGE
                   MOVE -1 TO FNAMEL OF COUSR1AI
                   PERFORM SEND-USRADD-SCREEN
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      PROCESS-RPC-RESPONSE
      *----------------------------------------------------------------*
       PROCESS-RPC-RESPONSE.

           EVALUATE WS-RPC-RESP-CODE
               WHEN 0
      *            Success - clear fields and show success message
                   PERFORM INITIALIZE-ALL-FIELDS
                   MOVE SPACES TO WS-MESSAGE
                   MOVE DFHGREEN TO ERRMSGC OF COUSR1AO
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   PERFORM SEND-USRADD-SCREEN
               WHEN 1
      *            Empty First Name
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO FNAMEL OF COUSR1AI
                   PERFORM SEND-USRADD-SCREEN
               WHEN 2
      *            Empty Last Name
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO LNAMEL OF COUSR1AI
                   PERFORM SEND-USRADD-SCREEN
               WHEN 3
      *            Empty User ID
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO USERIDL OF COUSR1AI
                   PERFORM SEND-USRADD-SCREEN
               WHEN 4
      *            Empty Password
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO PASSWDL OF COUSR1AI
                   PERFORM SEND-USRADD-SCREEN
               WHEN 5
      *            Empty/Invalid User Type
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO USRTYPEL OF COUSR1AI
                   PERFORM SEND-USRADD-SCREEN
               WHEN 6
      *            Duplicate User
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO USERIDL OF COUSR1AI
                   PERFORM SEND-USRADD-SCREEN
               WHEN 7
      *            Write Error
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO FNAMEL OF COUSR1AI
                   PERFORM SEND-USRADD-SCREEN
               WHEN OTHER
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE 'Unknown error occurred...' TO WS-MESSAGE
                   MOVE -1 TO FNAMEL OF COUSR1AI
                   PERFORM SEND-USRADD-SCREEN
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      RETURN-TO-PREV-SCREEN
      *----------------------------------------------------------------*
       RETURN-TO-PREV-SCREEN.

           IF CDEMO-TO-PROGRAM = LOW-VALUES OR SPACES
               MOVE 'COSGN00C' TO CDEMO-TO-PROGRAM
           END-IF
           MOVE WS-TRANID    TO CDEMO-FROM-TRANID
           MOVE WS-PGMNAME   TO CDEMO-FROM-PROGRAM
      *    MOVE WS-USER-ID   TO CDEMO-USER-ID
      *    MOVE SEC-USR-TYPE TO CDEMO-USER-TYPE
           MOVE ZEROS        TO CDEMO-PGM-CONTEXT
           EXEC CICS
               XCTL PROGRAM(CDEMO-TO-PROGRAM)
               COMMAREA(CARDDEMO-COMMAREA)
           END-EXEC.

      *----------------------------------------------------------------*
      *                      SEND-USRADD-SCREEN
      *----------------------------------------------------------------*
       SEND-USRADD-SCREEN.

           PERFORM POPULATE-HEADER-INFO

           MOVE WS-MESSAGE TO ERRMSGO OF COUSR1AO

           EXEC CICS SEND
                     MAP('COUSR1A')
                     MAPSET('COUSR01')
                     FROM(COUSR1AO)
                     ERASE
                     CURSOR
           END-EXEC.

      *----------------------------------------------------------------*
      *                      RECEIVE-USRADD-SCREEN
      *----------------------------------------------------------------*
       RECEIVE-USRADD-SCREEN.

           EXEC CICS RECEIVE
                     MAP('COUSR1A')
                     MAPSET('COUSR01')
                     INTO(COUSR1AI)
                     RESP(WS-RESP-CD)
                     RESP2(WS-REAS-CD)
           END-EXEC.

      *----------------------------------------------------------------*
      *                      POPULATE-HEADER-INFO
      *----------------------------------------------------------------*
       POPULATE-HEADER-INFO.

           MOVE FUNCTION CURRENT-DATE  TO WS-CURDATE-DATA

           MOVE CCDA-TITLE01           TO TITLE01O OF COUSR1AO
           MOVE CCDA-TITLE02           TO TITLE02O OF COUSR1AO
           MOVE WS-TRANID              TO TRNNAMEO OF COUSR1AO
           MOVE WS-PGMNAME             TO PGMNAMEO OF COUSR1AO

           MOVE WS-CURDATE-MONTH       TO WS-CURDATE-MM
           MOVE WS-CURDATE-DAY         TO WS-CURDATE-DD
           MOVE WS-CURDATE-YEAR(3:2)   TO WS-CURDATE-YY

           MOVE WS-CURDATE-MM-DD-YY    TO CURDATEO OF COUSR1AO

           MOVE WS-CURTIME-HOURS       TO WS-CURTIME-HH
           MOVE WS-CURTIME-MINUTE      TO WS-CURTIME-MM
           MOVE WS-CURTIME-SECOND      TO WS-CURTIME-SS

           MOVE WS-CURTIME-HH-MM-SS    TO CURTIMEO OF COUSR1AO.

      *----------------------------------------------------------------*
      *                      CLEAR-CURRENT-SCREEN
      *----------------------------------------------------------------*
       CLEAR-CURRENT-SCREEN.

           PERFORM INITIALIZE-ALL-FIELDS.
           PERFORM SEND-USRADD-SCREEN.

      *----------------------------------------------------------------*
      *                      INITIALIZE-ALL-FIELDS
      *----------------------------------------------------------------*
       INITIALIZE-ALL-FIELDS.

           MOVE -1              TO FNAMEL OF COUSR1AI
           MOVE SPACES          TO USERIDI  OF COUSR1AI
                                   FNAMEI   OF COUSR1AI
                                   LNAMEI   OF COUSR1AI
                                   PASSWDI  OF COUSR1AI
                                   USRTYPEI OF COUSR1AI
                                   WS-MESSAGE.

      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:12:34 CDT
      *