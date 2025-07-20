******************************************************************
      * Program     : COUSR00A.CBL
      * Application : CardDemo
      * Type        : COBOL RPC Program
      * Function    : Simple paginated user list starting from
      *               specified user ID using DB2
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
       PROGRAM-ID. COUSR00A.
       AUTHOR.     AWS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
      *----------------------------------------------------------------*
      *                     WORKING STORAGE SECTION
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

       01 WS-VARIABLES.
         05 WS-PGMNAME                 PIC X(08) VALUE 'COUSR00A'.
         05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.
           88 ERR-FLG-ON                         VALUE 'Y'.
           88 ERR-FLG-OFF                        VALUE 'N'.
         05 WS-USER-SEC-EOF            PIC X(01) VALUE 'N'.
           88 USER-SEC-EOF                       VALUE 'Y'.
           88 USER-SEC-NOT-EOF                   VALUE 'N'.
         05 WS-IDX                     PIC S9(04) COMP VALUE ZEROS.
         05 WS-OUT-IDX                 PIC S9(04) COMP VALUE ZEROS.
         05 WS-STARTING-KEY            PIC X(08).

      * Temporary storage for backward records
       01 WS-TEMP-RECORDS.
          05 WS-TEMP-COUNT             PIC 9(02) VALUE 0.
          05 WS-TEMP-DATA OCCURS 10 TIMES.
             10 WS-TEMP-ID             PIC X(08).
             10 WS-TEMP-FNAME          PIC X(20).
             10 WS-TEMP-LNAME          PIC X(20).
             10 WS-TEMP-TYPE           PIC X(01).

      * DB2 SQL Communication Area
           EXEC SQL INCLUDE SQLCA END-EXEC.

      * User Security Record Structure (DB2 Host Variables)
       01 SEC-USER-DATA.
         05 SEC-USR-ID                 PIC X(08).
         05 SEC-USR-FNAME              PIC X(20).
         05 SEC-USR-LNAME              PIC X(20).
         05 SEC-USR-TYPE               PIC X(01).

      * DB2 Cursor Declarations
           EXEC SQL DECLARE USER_CURSOR CURSOR FOR
               SELECT USR_ID, USR_FNAME, USR_LNAME, USR_TYPE
               FROM USERSEC
               WHERE USR_ID >= :SEC-USR-ID
               ORDER BY USR_ID
           END-EXEC.

           EXEC SQL DECLARE USER_CURSOR_PREV CURSOR FOR
               SELECT USR_ID, USR_FNAME, USR_LNAME, USR_TYPE
               FROM USERSEC
               WHERE USR_ID <= :SEC-USR-ID
               ORDER BY USR_ID DESC
           END-EXEC.

       01 WS-CURSOR-STATUS           PIC X(01) VALUE 'C'.
           88 CURSOR-OPEN                        VALUE 'O'.
           88 CURSOR-CLOSED                      VALUE 'C'.

      *----------------------------------------------------------------*
      *                        LINKAGE SECTION
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01 DFHCOMMAREA.
           05 LK-INPUT-PARMS.
               10 LK-IN-USER-ID             PIC X(08).
               10 LK-IN-PAGE-DIR            PIC X(01).

           05 LK-OUTPUT-PARMS.
               10 LK-OUT-MESSAGE            PIC X(80).
               10 LK-OUT-USER-COUNT         PIC 9(02).
               10 LK-OUT-USER-DATA OCCURS 0 TO 10 TIMES
                  DEPENDING ON LK-OUT-USER-COUNT.
                   15 LK-OUT-USR-ID         PIC X(08).
                   15 LK-OUT-USR-FNAME      PIC X(20).
                   15 LK-OUT-USR-LNAME      PIC X(20).
                   15 LK-OUT-USR-TYPE       PIC X(01).

      *----------------------------------------------------------------*
      *                       PROCEDURE DIVISION
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

       MAIN-PARA.
           SET ERR-FLG-OFF TO TRUE
           SET USER-SEC-NOT-EOF TO TRUE
           SET CURSOR-CLOSED TO TRUE

           MOVE SPACES TO LK-OUTPUT-PARMS
           MOVE 0 TO LK-OUT-USER-COUNT

      * DEBUG: Show input parms

      * Position to starting user ID
           IF LK-IN-USER-ID = SPACES OR LOW-VALUES
               MOVE LOW-VALUES TO SEC-USR-ID
               MOVE LOW-VALUES TO WS-STARTING-KEY
           ELSE
               MOVE LK-IN-USER-ID TO SEC-USR-ID
               MOVE LK-IN-USER-ID TO WS-STARTING-KEY
           END-IF

           IF LK-IN-PAGE-DIR = 'F'
               PERFORM PROCESS-PAGE-FORWARD
           ELSE
               IF LK-IN-PAGE-DIR = 'B'
                   PERFORM PROCESS-PAGE-BACKWARD
               ELSE
                   PERFORM PROCESS-LIST
               END-IF
           END-IF

           PERFORM CLOSE-CURSOR-IF-OPEN

           GOBACK.

      *----------------------------------------------------------------*
      *                      PROCESS-LIST (Records 1-10) -> Enter
      *----------------------------------------------------------------*
       PROCESS-LIST.
           MOVE 0 TO LK-OUT-USER-COUNT

           PERFORM OPEN-USER-CURSOR

           IF NOT ERR-FLG-ON
      * First read the starting record itself
               PERFORM FETCH-USER-CURSOR
               IF USER-SEC-NOT-EOF AND ERR-FLG-OFF
                   ADD 1 TO LK-OUT-USER-COUNT
                   MOVE LK-OUT-USER-COUNT TO WS-OUT-IDX
                   PERFORM POPULATE-USER-OUTPUT
               END-IF

      * Read remaining records up to 10 total
               PERFORM UNTIL LK-OUT-USER-COUNT >= 10 OR
                             USER-SEC-EOF OR ERR-FLG-ON
                   PERFORM FETCH-USER-CURSOR
                   IF USER-SEC-NOT-EOF AND ERR-FLG-OFF
                       ADD 1 TO LK-OUT-USER-COUNT
                       MOVE LK-OUT-USER-COUNT TO WS-OUT-IDX
                       PERFORM POPULATE-USER-OUTPUT
                   END-IF
               END-PERFORM

               PERFORM CLOSE-USER-CURSOR
           END-IF.

      *----------------------------------------------------------------*
      *                      PROCESS-PAGE-FORWARD (Records 11-20) -> F8
      *----------------------------------------------------------------*
       PROCESS-PAGE-FORWARD.
           MOVE 0 TO LK-OUT-USER-COUNT

           PERFORM OPEN-USER-CURSOR

           IF NOT ERR-FLG-ON
      * Skip the first 10 records
               PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 10
                       OR USER-SEC-EOF OR ERR-FLG-ON
                   PERFORM FETCH-USER-CURSOR
               END-PERFORM

      * If EOF hit during skip, just call PROCESS-LIST from start key
               IF USER-SEC-EOF AND WS-IDX <= 10
                   SET USER-SEC-NOT-EOF TO TRUE
                   PERFORM CLOSE-USER-CURSOR
                   MOVE WS-STARTING-KEY TO SEC-USR-ID
                   PERFORM PROCESS-LIST
               ELSE
      * Normal case - read next 10 records after skip
                   PERFORM UNTIL LK-OUT-USER-COUNT >= 10 OR
                                 USER-SEC-EOF OR ERR-FLG-ON
                       PERFORM FETCH-USER-CURSOR
                       IF USER-SEC-NOT-EOF AND ERR-FLG-OFF
                           ADD 1 TO LK-OUT-USER-COUNT
                           MOVE LK-OUT-USER-COUNT TO WS-OUT-IDX
                           PERFORM POPULATE-USER-OUTPUT
                       END-IF
                   END-PERFORM
                   PERFORM CLOSE-USER-CURSOR
               END-IF

               IF LK-OUT-USER-COUNT < 10 OR USER-SEC-EOF
                   MOVE 'You have reached the bottom of the page...'
                   TO LK-OUT-MESSAGE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      *              PROCESS-PAGE-BACKWARD (Records -10 to 1) -> F7
      *----------------------------------------------------------------*
       PROCESS-PAGE-BACKWARD.
           MOVE 0 TO LK-OUT-USER-COUNT
           MOVE 0 TO WS-TEMP-COUNT

           PERFORM OPEN-USER-CURSOR-PREV

           IF NOT ERR-FLG-ON
      * Read 11 records backward into temp storage (including current)
               PERFORM UNTIL WS-TEMP-COUNT >= 11 OR
                             USER-SEC-EOF OR ERR-FLG-ON
                   PERFORM FETCH-USER-CURSOR-PREV
                   IF USER-SEC-NOT-EOF AND ERR-FLG-OFF
                       ADD 1 TO WS-TEMP-COUNT
                       MOVE SEC-USR-ID TO WS-TEMP-ID(WS-TEMP-COUNT)
                       MOVE SEC-USR-FNAME TO
                            WS-TEMP-FNAME(WS-TEMP-COUNT)
                       MOVE SEC-USR-LNAME TO
                            WS-TEMP-LNAME(WS-TEMP-COUNT)
                       MOVE SEC-USR-TYPE TO WS-TEMP-TYPE(WS-TEMP-COUNT)
                   END-IF
               END-PERFORM

               PERFORM CLOSE-USER-CURSOR-PREV

      * Check if we hit EOF or error during backward read (at top)
               IF USER-SEC-EOF OR ERR-FLG-ON OR WS-TEMP-COUNT < 11
                   SET USER-SEC-NOT-EOF TO TRUE
                   SET ERR-FLG-OFF TO TRUE
                   MOVE LOW-VALUES TO SEC-USR-ID
                   PERFORM PROCESS-LIST
                   MOVE 'You are already at the top of the page...'
                   TO LK-OUT-MESSAGE
               ELSE
                   PERFORM VARYING WS-IDX FROM WS-TEMP-COUNT BY -1
                           UNTIL WS-IDX < 2 OR LK-OUT-USER-COUNT >= 10
                       ADD 1 TO LK-OUT-USER-COUNT
                       MOVE LK-OUT-USER-COUNT TO WS-OUT-IDX
                       MOVE WS-TEMP-ID(WS-IDX) TO
                            LK-OUT-USR-ID(WS-OUT-IDX)
                       MOVE WS-TEMP-FNAME(WS-IDX) TO
                            LK-OUT-USR-FNAME(WS-OUT-IDX)
                       MOVE WS-TEMP-LNAME(WS-IDX) TO
                            LK-OUT-USR-LNAME(WS-OUT-IDX)
                       MOVE WS-TEMP-TYPE(WS-IDX) TO
                            LK-OUT-USR-TYPE(WS-OUT-IDX)
                   END-PERFORM
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      *                      POPULATE-USER-OUTPUT
      *----------------------------------------------------------------*
       POPULATE-USER-OUTPUT.
           MOVE SEC-USR-ID TO LK-OUT-USR-ID(WS-OUT-IDX)
           MOVE SEC-USR-FNAME TO LK-OUT-USR-FNAME(WS-OUT-IDX)
           MOVE SEC-USR-LNAME TO LK-OUT-USR-LNAME(WS-OUT-IDX)
           MOVE SEC-USR-TYPE TO LK-OUT-USR-TYPE(WS-OUT-IDX).

      *----------------------------------------------------------------*
      *                      OPEN-USER-CURSOR
      *----------------------------------------------------------------*
       OPEN-USER-CURSOR.

           EXEC SQL OPEN USER_CURSOR END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   SET CURSOR-OPEN TO TRUE
                   CONTINUE
               WHEN OTHER
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE 'Unable to open user cursor...'
                   TO LK-OUT-MESSAGE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      OPEN-USER-CURSOR-PREV
      *----------------------------------------------------------------*
       OPEN-USER-CURSOR-PREV.

           EXEC SQL OPEN USER_CURSOR_PREV END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   SET CURSOR-OPEN TO TRUE
                   CONTINUE
               WHEN OTHER
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE 'Unable to open user cursor...'
                   TO LK-OUT-MESSAGE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      FETCH-USER-CURSOR
      *----------------------------------------------------------------*
       FETCH-USER-CURSOR.

           EXEC SQL FETCH USER_CURSOR
               INTO :SEC-USR-ID, :SEC-USR-FNAME,
                    :SEC-USR-LNAME, :SEC-USR-TYPE
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   SET USER-SEC-EOF TO TRUE
               WHEN OTHER
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE 'Unable to fetch user data...' TO LK-OUT-MESSAGE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      FETCH-USER-CURSOR-PREV
      *----------------------------------------------------------------*
       FETCH-USER-CURSOR-PREV.

           EXEC SQL FETCH USER_CURSOR_PREV
               INTO :SEC-USR-ID, :SEC-USR-FNAME,
                    :SEC-USR-LNAME, :SEC-USR-TYPE
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN +100
                   SET USER-SEC-EOF TO TRUE
               WHEN OTHER
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE 'Unable to fetch user data...' TO LK-OUT-MESSAGE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      CLOSE-USER-CURSOR
      *----------------------------------------------------------------*
       CLOSE-USER-CURSOR.

           IF CURSOR-OPEN
               EXEC SQL CLOSE USER_CURSOR END-EXEC
               SET CURSOR-CLOSED TO TRUE
           END-IF.

      *----------------------------------------------------------------*
      *                      CLOSE-USER-CURSOR-PREV
      *----------------------------------------------------------------*
       CLOSE-USER-CURSOR-PREV.

           IF CURSOR-OPEN
               EXEC SQL CLOSE USER_CURSOR_PREV END-EXEC
               SET CURSOR-CLOSED TO TRUE
           END-IF.

      *----------------------------------------------------------------*
      *                      CLOSE-CURSOR-IF-OPEN
      *----------------------------------------------------------------*
       CLOSE-CURSOR-IF-OPEN.

           IF CURSOR-OPEN
               EXEC SQL CLOSE USER_CURSOR END-EXEC
               EXEC SQL CLOSE USER_CURSOR_PREV END-EXEC
               SET CURSOR-CLOSED TO TRUE
           END-IF.

      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:12:34 CDT
      *