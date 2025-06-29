      ******************************************************************
      * Program     : COUSR00L.CBL
      * Application : CardDemo
      * Type        : COBOL RPC Program
      * Function    : Simple paginated user list starting from
      *               specified user ID
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COUSR00L.
       AUTHOR.     AWS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
      *----------------------------------------------------------------*
      *                     WORKING STORAGE SECTION
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

       01 WS-VARIABLES.
         05 WS-PGMNAME                 PIC X(08) VALUE 'COUSR00L'.
         05 WS-USRSEC-FILE             PIC X(08) VALUE 'USRSEC  '.
         05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.
           88 ERR-FLG-ON                         VALUE 'Y'.
           88 ERR-FLG-OFF                        VALUE 'N'.
         05 WS-USER-SEC-EOF            PIC X(01) VALUE 'N'.
           88 USER-SEC-EOF                       VALUE 'Y'.
           88 USER-SEC-NOT-EOF                   VALUE 'N'.
         05 WS-RESP-CD                 PIC S9(09) COMP VALUE ZEROS.
         05 WS-REAS-CD                 PIC S9(09) COMP VALUE ZEROS.
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

      * User Security Record Structure
       01 SEC-USER-DATA.
         05 SEC-USR-ID                 PIC X(08).
         05 SEC-USR-FNAME              PIC X(20).
         05 SEC-USR-LNAME              PIC X(20).
         05 SEC-USR-PWD                PIC X(08).
         05 SEC-USR-TYPE               PIC X(01).
         05 SEC-USR-FILLER             PIC X(23).

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

           MOVE SPACES TO LK-OUTPUT-PARMS
           MOVE 0 TO LK-OUT-USER-COUNT

      * Position to starting user ID
           IF LK-IN-USER-ID = SPACES OR LOW-VALUES
               MOVE LOW-VALUES TO SEC-USR-ID
               MOVE LOW-VALUES TO WS-STARTING-KEY
           ELSE
               MOVE LK-IN-USER-ID TO SEC-USR-ID
               MOVE LK-IN-USER-ID TO WS-STARTING-KEY
           END-IF

           PERFORM STARTBR-USER-SEC-FILE

           IF NOT ERR-FLG-ON
              IF LK-IN-PAGE-DIR = 'F'
                  PERFORM    PROCESS-PAGE-FORWARD
              ELSE
                  IF LK-IN-PAGE-DIR = 'B'
                      PERFORM   PROCESS-PAGE-BACKWARD
                  ELSE
                      PERFORM PROCESS-LIST
                  END-IF
              END-IF
              PERFORM ENDBR-USER-SEC-FILE
           END-IF

           GOBACK.

      *----------------------------------------------------------------*
      *                      PROCESS-LIST (Records 1-10) -­> Enter
      *----------------------------------------------------------------*
       PROCESS-LIST.
           MOVE 0 TO LK-OUT-USER-COUNT

      * First read the starting record itself
           PERFORM READNEXT-USER-SEC-FILE
           IF USER-SEC-NOT-EOF AND ERR-FLG-OFF
               ADD 1 TO LK-OUT-USER-COUNT
               MOVE LK-OUT-USER-COUNT TO WS-OUT-IDX
               PERFORM POPULATE-USER-OUTPUT
           END-IF

      * Read remaining records up to 10 total
           PERFORM UNTIL LK-OUT-USER-COUNT >= 10 OR
                         USER-SEC-EOF OR ERR-FLG-ON
               PERFORM READNEXT-USER-SEC-FILE
               IF USER-SEC-NOT-EOF AND ERR-FLG-OFF
                   ADD 1 TO LK-OUT-USER-COUNT
                   MOVE LK-OUT-USER-COUNT TO WS-OUT-IDX
                   PERFORM POPULATE-USER-OUTPUT
               END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
      *                      PROCESS-PAGE-FORWARD (Records 11-20) -> F8
      *----------------------------------------------------------------*
       PROCESS-PAGE-FORWARD.
           MOVE 0 TO LK-OUT-USER-COUNT

      * Skip the first 10 records
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 10
                   OR USER-SEC-EOF OR ERR-FLG-ON
               PERFORM READNEXT-USER-SEC-FILE
           END-PERFORM

      * If EOF hit during skip, just call PROCESS-LIST from start key
           IF USER-SEC-EOF AND WS-IDX <= 10
               SET USER-SEC-NOT-EOF TO TRUE
               PERFORM ENDBR-USER-SEC-FILE
               MOVE WS-STARTING-KEY TO SEC-USR-ID
               PERFORM STARTBR-USER-SEC-FILE
               IF NOT ERR-FLG-ON
                   PERFORM PROCESS-LIST
               END-IF
           ELSE
      * Normal case - read next 10 records after skip
               PERFORM UNTIL LK-OUT-USER-COUNT >= 10 OR
                             USER-SEC-EOF OR ERR-FLG-ON
                   PERFORM READNEXT-USER-SEC-FILE
                   IF USER-SEC-NOT-EOF AND ERR-FLG-OFF
                       ADD 1 TO LK-OUT-USER-COUNT
                       MOVE LK-OUT-USER-COUNT TO WS-OUT-IDX
                       PERFORM POPULATE-USER-OUTPUT
                   END-IF
               END-PERFORM
           END-IF

           IF LK-OUT-USER-COUNT < 10 OR USER-SEC-EOF
               MOVE 'You have reached the bottom of the page...'
               TO LK-OUT-MESSAGE
           END-IF.

      *----------------------------------------------------------------*
      *              PROCESS-PAGE-BACKWARD (Records -10 to 1) -> F7
      *----------------------------------------------------------------*
       PROCESS-PAGE-BACKWARD.
           MOVE 0 TO LK-OUT-USER-COUNT
           MOVE 0 TO WS-TEMP-COUNT

      * Read 11 records backward into temp storage (including current)
           PERFORM UNTIL WS-TEMP-COUNT >= 11 OR
                         USER-SEC-EOF OR ERR-FLG-ON
               PERFORM READPREV-USER-SEC-FILE
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

      * Check if we hit EOF or error during backward read (at top)
           IF USER-SEC-EOF OR ERR-FLG-ON OR WS-TEMP-COUNT < 11
      * We're at the top - display first 10 records from beginning
               SET USER-SEC-NOT-EOF TO TRUE
               SET ERR-FLG-OFF TO TRUE
               PERFORM ENDBR-USER-SEC-FILE
               MOVE LOW-VALUES TO SEC-USR-ID
               PERFORM STARTBR-USER-SEC-FILE
               IF NOT ERR-FLG-ON
                   PERFORM PROCESS-LIST
               END-IF
               MOVE 'You are already at the top of the page...'
               TO LK-OUT-MESSAGE
           ELSE
      * Normal case - display the first 10 in reverse order
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
      *                      STARTBR-USER-SEC-FILE
      *----------------------------------------------------------------*
       STARTBR-USER-SEC-FILE.
           EXEC CICS STARTBR
                DATASET   (WS-USRSEC-FILE)
                RIDFLD    (SEC-USR-ID)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   SET USER-SEC-EOF TO TRUE
                   MOVE 'You are already at the top of the page...' TO
                        LK-OUT-MESSAGE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   MOVE 'Error accessing user database.'
                   TO LK-OUT-MESSAGE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      READNEXT-USER-SEC-FILE
      *----------------------------------------------------------------*
       READNEXT-USER-SEC-FILE.
           EXEC CICS READNEXT
                DATASET   (WS-USRSEC-FILE)
                INTO      (SEC-USER-DATA)
                RIDFLD    (SEC-USR-ID)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(ENDFILE)
                   SET USER-SEC-EOF TO TRUE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   MOVE 'Error reading user database.' TO LK-OUT-MESSAGE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      READPREV-USER-SEC-FILE
      *----------------------------------------------------------------*
       READPREV-USER-SEC-FILE.
           EXEC CICS READPREV
                DATASET   (WS-USRSEC-FILE)
                INTO      (SEC-USER-DATA)
                RIDFLD    (SEC-USR-ID)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(ENDFILE)
                   SET USER-SEC-EOF TO TRUE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   MOVE 'Error reading user database.' TO LK-OUT-MESSAGE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      ENDBR-USER-SEC-FILE
      *----------------------------------------------------------------*
       ENDBR-USER-SEC-FILE.
           EXEC CICS ENDBR
                DATASET   (WS-USRSEC-FILE)
           END-EXEC.