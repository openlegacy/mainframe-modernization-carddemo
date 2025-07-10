******************************************************************
      * Program     :  COUSR00S.CBL
      * Function    : List Users with RPC calls - Fixed Navigation
      *               Full functionality: Display, Search, Paginate, Select
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COUSR00S.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-VARIABLES.
         05 WS-PGMNAME                 PIC X(08) VALUE 'COUSR00S'.
         05 WS-TRANID                  PIC X(04) VALUE 'ALS0'.
         05 WS-MESSAGE                 PIC X(80) VALUE SPACES.
         05 WS-RPC-PROGRAM             PIC X(08) VALUE 'COUSR00L'.
         05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.
           88 ERR-FLG-ON                         VALUE 'Y'.
           88 ERR-FLG-OFF                        VALUE 'N'.
         05 WS-RESP-CD                 PIC S9(09) COMP VALUE ZEROS.
         05 WS-IDX                     PIC S9(04) COMP VALUE ZEROS.
         05 WS-PAGE-NUM                PIC S9(04) COMP VALUE 1.
         05 WS-SELECTED-USER           PIC X(08) VALUE SPACES.
         05 WS-SELECTED-ACTION         PIC X(01) VALUE SPACES.

      * Simple constants for header display
       COPY COTTL01Y.
       COPY CSDAT01Y.
       COPY CSMSG01Y.
       COPY CSUSR01Y.

      * ADDED: Common area structure like COUSR00C
       COPY COCOM01Y.
          05 CDEMO-CU00-INFO.
             10 CDEMO-CU00-USRID-FIRST     PIC X(08).
             10 CDEMO-CU00-USRID-LAST      PIC X(08).
             10 CDEMO-CU00-PAGE-NUM        PIC 9(08).
             10 CDEMO-CU00-NEXT-PAGE-FLG   PIC X(01) VALUE 'N'.
                88 NEXT-PAGE-YES                     VALUE 'Y'.
                88 NEXT-PAGE-NO                      VALUE 'N'.
             10 CDEMO-CU00-USR-SEL-FLG     PIC X(01).
             10 CDEMO-CU00-USR-SELECTED    PIC X(08).

      * RPC Communication Area - MUST MATCH COUSR00L EXACTLY
       01 WS-RPC-COMMAREA.
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

       COPY COUSR00.
       COPY DFHAID.
       COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
         05  LK-COMMAREA                           PIC X(01)
             OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.

       PROCEDURE DIVISION.
       MAIN-PARA.

           SET ERR-FLG-OFF TO TRUE
           SET NEXT-PAGE-NO TO TRUE
           MOVE SPACES TO WS-MESSAGE
           MOVE -1 TO USRIDINL OF COUSR0AI

      * FIXED: Check EIBCALEN and handle commarea like COUSR00C
           IF EIBCALEN = 0
      * No commarea - return to signon
               MOVE 'COSGN00S' TO CDEMO-TO-PROGRAM
               PERFORM RETURN-TO-PREV-SCREEN
           ELSE
      * Has commarea - process it
               MOVE DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA
               IF NOT CDEMO-PGM-REENTER
      * First time with commarea - display initial screen
                   SET CDEMO-PGM-REENTER TO TRUE
                   MOVE LOW-VALUES TO COUSR0AO
                   PERFORM PROCESS-ENTER-KEY
                   PERFORM SEND-SCREEN
               ELSE
      * Subsequent calls - process user input
                   PERFORM RECEIVE-SCREEN
                   EVALUATE EIBAID
                       WHEN DFHENTER
                           PERFORM PROCESS-ENTER-KEY
                       WHEN DFHPF3
      * FIXED: Return to admin menu like original
                           MOVE 'COADM01S' TO CDEMO-TO-PROGRAM
                           PERFORM RETURN-TO-PREV-SCREEN
                       WHEN DFHPF7
                           PERFORM PROCESS-PF7-KEY
                       WHEN DFHPF8
                           PERFORM PROCESS-PF8-KEY
                       WHEN DFHPF12
                            MOVE 'Y' TO WS-ERR-FLG
                            MOVE -1 TO USRIDINL OF COUSR0AI
                            MOVE CCDA-MSG-INVALID-KEY TO WS-MESSAGE
                            PERFORM SEND-SCREEN
                        WHEN OTHER
                            MOVE 'Y' TO WS-ERR-FLG
                            MOVE -1 TO USRIDINL OF COUSR0AI
                            MOVE CCDA-MSG-INVALID-KEY TO WS-MESSAGE
                            PERFORM SEND-SCREEN
                        END-EVALUATE
                   END-IF
           END-IF

      * FIXED: Return with proper commarea
           EXEC CICS RETURN
                     TRANSID (WS-TRANID)
                     COMMAREA (CARDDEMO-COMMAREA)
           END-EXEC.

      *----------------------------------------------------------------*
      *                      PROCESS-ENTER-KEY
      *----------------------------------------------------------------*
       PROCESS-ENTER-KEY.

      * ADDED: Handle user selection like COUSR00C
           EVALUATE TRUE
               WHEN SEL0001I OF COUSR0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0001I OF COUSR0AI TO CDEMO-CU00-USR-SEL-FLG
                   MOVE USRID01I OF COUSR0AI TO CDEMO-CU00-USR-SELECTED
               WHEN SEL0002I OF COUSR0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0002I OF COUSR0AI TO CDEMO-CU00-USR-SEL-FLG
                   MOVE USRID02I OF COUSR0AI TO CDEMO-CU00-USR-SELECTED
               WHEN SEL0003I OF COUSR0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0003I OF COUSR0AI TO CDEMO-CU00-USR-SEL-FLG
                   MOVE USRID03I OF COUSR0AI TO CDEMO-CU00-USR-SELECTED
               WHEN SEL0004I OF COUSR0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0004I OF COUSR0AI TO CDEMO-CU00-USR-SEL-FLG
                   MOVE USRID04I OF COUSR0AI TO CDEMO-CU00-USR-SELECTED
               WHEN SEL0005I OF COUSR0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0005I OF COUSR0AI TO CDEMO-CU00-USR-SEL-FLG
                   MOVE USRID05I OF COUSR0AI TO CDEMO-CU00-USR-SELECTED
               WHEN SEL0006I OF COUSR0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0006I OF COUSR0AI TO CDEMO-CU00-USR-SEL-FLG
                   MOVE USRID06I OF COUSR0AI TO CDEMO-CU00-USR-SELECTED
               WHEN SEL0007I OF COUSR0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0007I OF COUSR0AI TO CDEMO-CU00-USR-SEL-FLG
                   MOVE USRID07I OF COUSR0AI TO CDEMO-CU00-USR-SELECTED
               WHEN SEL0008I OF COUSR0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0008I OF COUSR0AI TO CDEMO-CU00-USR-SEL-FLG
                   MOVE USRID08I OF COUSR0AI TO CDEMO-CU00-USR-SELECTED
               WHEN SEL0009I OF COUSR0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0009I OF COUSR0AI TO CDEMO-CU00-USR-SEL-FLG
                   MOVE USRID09I OF COUSR0AI TO CDEMO-CU00-USR-SELECTED
               WHEN SEL0010I OF COUSR0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0010I OF COUSR0AI TO CDEMO-CU00-USR-SEL-FLG
                   MOVE USRID10I OF COUSR0AI TO CDEMO-CU00-USR-SELECTED
               WHEN OTHER
                   MOVE SPACES TO CDEMO-CU00-USR-SEL-FLG
                   MOVE SPACES TO CDEMO-CU00-USR-SELECTED
           END-EVALUATE

      * FIXED: Use XCTL to transfer to update/delete programs with data
           IF (CDEMO-CU00-USR-SEL-FLG NOT = SPACES AND LOW-VALUES) AND
              (CDEMO-CU00-USR-SELECTED NOT = SPACES AND LOW-VALUES)
               EVALUATE CDEMO-CU00-USR-SEL-FLG
                   WHEN 'U'
                   WHEN 'u'

                        MOVE 'COUSR02S' TO CDEMO-TO-PROGRAM
                        MOVE WS-TRANID TO CDEMO-FROM-TRANID
                        MOVE WS-PGMNAME TO CDEMO-FROM-PROGRAM
                        MOVE 0 TO CDEMO-PGM-CONTEXT
                        EXEC CICS
                            XCTL PROGRAM(CDEMO-TO-PROGRAM)
                            COMMAREA(CARDDEMO-COMMAREA)
                        END-EXEC
                   WHEN 'D'
                   WHEN 'd'

                        MOVE 'COUSR03S' TO CDEMO-TO-PROGRAM
                        MOVE WS-TRANID TO CDEMO-FROM-TRANID
                        MOVE WS-PGMNAME TO CDEMO-FROM-PROGRAM
                        MOVE 0 TO CDEMO-PGM-CONTEXT
                        EXEC CICS
                            XCTL PROGRAM(CDEMO-TO-PROGRAM)
                            COMMAREA(CARDDEMO-COMMAREA)
                        END-EXEC
                   WHEN OTHER
                    MOVE 'Invalid selection. Valid values are U and D'
                        TO WS-MESSAGE
                       MOVE -1 TO USRIDINL OF COUSR0AI
               END-EVALUATE
           END-IF

      * Handle search/filter when no selection
           IF USRIDINI OF COUSR0AI = SPACES OR LOW-VALUES
               MOVE SPACES TO WS-SELECTED-USER
           ELSE
               MOVE USRIDINI OF COUSR0AI TO WS-SELECTED-USER
           END-IF

           MOVE -1 TO USRIDINL OF COUSR0AI
           PERFORM LOAD-INITIAL-DATA

           IF NOT ERR-FLG-ON
               MOVE SPACE TO USRIDINO OF COUSR0AO
           END-IF.

      *----------------------------------------------------------------*
      *                      PROCESS-PF7-KEY (Page Backward)
      *----------------------------------------------------------------*
       PROCESS-PF7-KEY.

           MOVE SPACES TO LK-INPUT-PARMS LK-OUTPUT-PARMS
           MOVE 'B' TO LK-IN-PAGE-DIR

      * Always use first displayed user as starting point
           IF USRID01I OF COUSR0AI NOT = SPACES
               MOVE USRID01I OF COUSR0AI TO LK-IN-USER-ID
           ELSE
               MOVE SPACES TO LK-IN-USER-ID
           END-IF

           PERFORM CALL-RPC-PROGRAM

           IF NOT ERR-FLG-ON
               PERFORM POPULATE-SCREEN-FROM-RPC
               IF LK-OUT-MESSAGE NOT = SPACES
                   MOVE LK-OUT-MESSAGE TO WS-MESSAGE
               END-IF
               PERFORM SEND-SCREEN
           ELSE
               MOVE LK-OUT-MESSAGE TO WS-MESSAGE
               PERFORM SEND-SCREEN
           END-IF.

      *----------------------------------------------------------------*
      *                      PROCESS-PF8-KEY (Page Forward)
      *----------------------------------------------------------------*
       PROCESS-PF8-KEY.

           MOVE SPACES TO LK-INPUT-PARMS LK-OUTPUT-PARMS
           MOVE 'F' TO LK-IN-PAGE-DIR

      * Always use first displayed user as starting point
           IF USRID01I OF COUSR0AI NOT = SPACES
               MOVE USRID01I OF COUSR0AI TO LK-IN-USER-ID
           ELSE
               MOVE SPACES TO LK-IN-USER-ID
           END-IF

           PERFORM CALL-RPC-PROGRAM

           IF NOT ERR-FLG-ON
               PERFORM POPULATE-SCREEN-FROM-RPC
               IF LK-OUT-MESSAGE NOT = SPACES
                   MOVE LK-OUT-MESSAGE TO WS-MESSAGE
               END-IF
               PERFORM SEND-SCREEN
           ELSE
               MOVE LK-OUT-MESSAGE TO WS-MESSAGE
               PERFORM SEND-SCREEN
           END-IF.

      *----------------------------------------------------------------*
      *                      LOAD-INITIAL-DATA
      *----------------------------------------------------------------*
       LOAD-INITIAL-DATA.

           MOVE SPACES TO LK-INPUT-PARMS LK-OUTPUT-PARMS

           IF WS-SELECTED-USER NOT = SPACES
               MOVE WS-SELECTED-USER TO LK-IN-USER-ID
           ELSE
               MOVE SPACES TO LK-IN-USER-ID
           END-IF

           MOVE SPACES TO LK-IN-PAGE-DIR
           MOVE 1 TO WS-PAGE-NUM

           PERFORM CALL-RPC-PROGRAM

           IF NOT ERR-FLG-ON
               PERFORM POPULATE-SCREEN-FROM-RPC
           ELSE
               MOVE 'Error loading initial data' TO WS-MESSAGE
           END-IF.

      *----------------------------------------------------------------*
      *                      CALL-RPC-PROGRAM
      *----------------------------------------------------------------*
       CALL-RPC-PROGRAM.

           EXEC CICS LINK
                PROGRAM(WS-RPC-PROGRAM)
                COMMAREA(WS-RPC-COMMAREA)
                LENGTH(LENGTH OF WS-RPC-COMMAREA)
                RESP(WS-RESP-CD)
           END-EXEC

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(PGMIDERR)
                   SET ERR-FLG-ON TO TRUE
                   MOVE 'COUSR00L program not found' TO WS-MESSAGE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   MOVE 'Error calling RPC program' TO WS-MESSAGE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      POPULATE-SCREEN-FROM-RPC
      *----------------------------------------------------------------*
       POPULATE-SCREEN-FROM-RPC.

      * Clear all user data first
           PERFORM INITIALIZE-ALL-USER-DATA

      * Track first and last user IDs for pagination
           MOVE SPACES TO CDEMO-CU00-USRID-FIRST
           MOVE SPACES TO CDEMO-CU00-USRID-LAST

      * Populate screen with returned data
           PERFORM VARYING WS-IDX FROM 1 BY 1
             UNTIL WS-IDX > LK-OUT-USER-COUNT

               IF WS-IDX = 1
                   MOVE LK-OUT-USR-ID(WS-IDX) TO CDEMO-CU00-USRID-FIRST
               END-IF

               IF WS-IDX = LK-OUT-USER-COUNT
                   MOVE LK-OUT-USR-ID(WS-IDX) TO CDEMO-CU00-USRID-LAST
               END-IF

               EVALUATE WS-IDX
                   WHEN 1
                       MOVE LK-OUT-USR-ID(WS-IDX) TO
                            USRID01I OF COUSR0AI
                       MOVE LK-OUT-USR-FNAME(WS-IDX) TO
                            FNAME01I OF COUSR0AI
                       MOVE LK-OUT-USR-LNAME(WS-IDX) TO
                            LNAME01I OF COUSR0AI
                       MOVE LK-OUT-USR-TYPE(WS-IDX) TO
                            UTYPE01I OF COUSR0AI
                   WHEN 2
                       MOVE LK-OUT-USR-ID(WS-IDX) TO
                            USRID02I OF COUSR0AI
                       MOVE LK-OUT-USR-FNAME(WS-IDX) TO
                            FNAME02I OF COUSR0AI
                       MOVE LK-OUT-USR-LNAME(WS-IDX) TO
                            LNAME02I OF COUSR0AI
                       MOVE LK-OUT-USR-TYPE(WS-IDX) TO
                            UTYPE02I OF COUSR0AI
                   WHEN 3
                       MOVE LK-OUT-USR-ID(WS-IDX) TO
                            USRID03I OF COUSR0AI
                       MOVE LK-OUT-USR-FNAME(WS-IDX) TO
                            FNAME03I OF COUSR0AI
                       MOVE LK-OUT-USR-LNAME(WS-IDX) TO
                            LNAME03I OF COUSR0AI
                       MOVE LK-OUT-USR-TYPE(WS-IDX) TO
                            UTYPE03I OF COUSR0AI
                   WHEN 4
                       MOVE LK-OUT-USR-ID(WS-IDX) TO
                            USRID04I OF COUSR0AI
                       MOVE LK-OUT-USR-FNAME(WS-IDX) TO
                            FNAME04I OF COUSR0AI
                       MOVE LK-OUT-USR-LNAME(WS-IDX) TO
                            LNAME04I OF COUSR0AI
                       MOVE LK-OUT-USR-TYPE(WS-IDX) TO
                            UTYPE04I OF COUSR0AI
                   WHEN 5
                       MOVE LK-OUT-USR-ID(WS-IDX) TO
                            USRID05I OF COUSR0AI
                       MOVE LK-OUT-USR-FNAME(WS-IDX) TO
                            FNAME05I OF COUSR0AI
                       MOVE LK-OUT-USR-LNAME(WS-IDX) TO
                            LNAME05I OF COUSR0AI
                       MOVE LK-OUT-USR-TYPE(WS-IDX) TO
                            UTYPE05I OF COUSR0AI
                   WHEN 6
                       MOVE LK-OUT-USR-ID(WS-IDX) TO
                            USRID06I OF COUSR0AI
                       MOVE LK-OUT-USR-FNAME(WS-IDX) TO
                            FNAME06I OF COUSR0AI
                       MOVE LK-OUT-USR-LNAME(WS-IDX) TO
                            LNAME06I OF COUSR0AI
                       MOVE LK-OUT-USR-TYPE(WS-IDX) TO
                            UTYPE06I OF COUSR0AI
                   WHEN 7
                       MOVE LK-OUT-USR-ID(WS-IDX) TO
                            USRID07I OF COUSR0AI
                       MOVE LK-OUT-USR-FNAME(WS-IDX) TO
                            FNAME07I OF COUSR0AI
                       MOVE LK-OUT-USR-LNAME(WS-IDX) TO
                            LNAME07I OF COUSR0AI
                       MOVE LK-OUT-USR-TYPE(WS-IDX) TO
                            UTYPE07I OF COUSR0AI
                   WHEN 8
                       MOVE LK-OUT-USR-ID(WS-IDX) TO
                            USRID08I OF COUSR0AI
                       MOVE LK-OUT-USR-FNAME(WS-IDX) TO
                            FNAME08I OF COUSR0AI
                       MOVE LK-OUT-USR-LNAME(WS-IDX) TO
                            LNAME08I OF COUSR0AI
                       MOVE LK-OUT-USR-TYPE(WS-IDX) TO
                            UTYPE08I OF COUSR0AI
                   WHEN 9
                       MOVE LK-OUT-USR-ID(WS-IDX) TO
                            USRID09I OF COUSR0AI
                       MOVE LK-OUT-USR-FNAME(WS-IDX) TO
                            FNAME09I OF COUSR0AI
                       MOVE LK-OUT-USR-LNAME(WS-IDX) TO
                            LNAME09I OF COUSR0AI
                       MOVE LK-OUT-USR-TYPE(WS-IDX) TO
                            UTYPE09I OF COUSR0AI
                   WHEN 10
                       MOVE LK-OUT-USR-ID(WS-IDX) TO
                            USRID10I OF COUSR0AI
                       MOVE LK-OUT-USR-FNAME(WS-IDX) TO
                            FNAME10I OF COUSR0AI
                       MOVE LK-OUT-USR-LNAME(WS-IDX) TO
                            LNAME10I OF COUSR0AI
                       MOVE LK-OUT-USR-TYPE(WS-IDX) TO
                            UTYPE10I OF COUSR0AI
               END-EVALUATE
           END-PERFORM.

      *----------------------------------------------------------------*
      *                      INITIALIZE-ALL-USER-DATA
      *----------------------------------------------------------------*
       INITIALIZE-ALL-USER-DATA.

           MOVE SPACES TO USRID01I OF COUSR0AI FNAME01I OF COUSR0AI
                          LNAME01I OF COUSR0AI UTYPE01I OF COUSR0AI
                          USRID02I OF COUSR0AI FNAME02I OF COUSR0AI
                          LNAME02I OF COUSR0AI UTYPE02I OF COUSR0AI
                          USRID03I OF COUSR0AI FNAME03I OF COUSR0AI
                          LNAME03I OF COUSR0AI UTYPE03I OF COUSR0AI
                          USRID04I OF COUSR0AI FNAME04I OF COUSR0AI
                          LNAME04I OF COUSR0AI UTYPE04I OF COUSR0AI
                          USRID05I OF COUSR0AI FNAME05I OF COUSR0AI
                          LNAME05I OF COUSR0AI UTYPE05I OF COUSR0AI
                          USRID06I OF COUSR0AI FNAME06I OF COUSR0AI
                          LNAME06I OF COUSR0AI UTYPE06I OF COUSR0AI
                          USRID07I OF COUSR0AI FNAME07I OF COUSR0AI
                          LNAME07I OF COUSR0AI UTYPE07I OF COUSR0AI
                          USRID08I OF COUSR0AI FNAME08I OF COUSR0AI
                          LNAME08I OF COUSR0AI UTYPE08I OF COUSR0AI
                          USRID09I OF COUSR0AI FNAME09I OF COUSR0AI
                          LNAME09I OF COUSR0AI UTYPE09I OF COUSR0AI
                          USRID10I OF COUSR0AI FNAME10I OF COUSR0AI
                          LNAME10I OF COUSR0AI UTYPE10I OF COUSR0AI

           MOVE SPACES TO SEL0001I OF COUSR0AI SEL0002I OF COUSR0AI
                          SEL0003I OF COUSR0AI SEL0004I OF COUSR0AI
                          SEL0005I OF COUSR0AI SEL0006I OF COUSR0AI
                          SEL0007I OF COUSR0AI SEL0008I OF COUSR0AI
                          SEL0009I OF COUSR0AI SEL0010I OF COUSR0AI.

      *----------------------------------------------------------------*
      *                      RETURN-TO-PREV-SCREEN
      *----------------------------------------------------------------*
       RETURN-TO-PREV-SCREEN.

           IF CDEMO-TO-PROGRAM = LOW-VALUES OR SPACES
               MOVE 'COSGN00S' TO CDEMO-TO-PROGRAM
           END-IF
           MOVE WS-TRANID TO CDEMO-FROM-TRANID
           MOVE WS-PGMNAME TO CDEMO-FROM-PROGRAM
           MOVE ZEROS TO CDEMO-PGM-CONTEXT
           EXEC CICS
               XCTL PROGRAM(CDEMO-TO-PROGRAM)
               COMMAREA(CARDDEMO-COMMAREA)
           END-EXEC.

      *----------------------------------------------------------------*
      *                      SEND-SCREEN
      *----------------------------------------------------------------*
       SEND-SCREEN.

           PERFORM POPULATE-HEADER-INFO
           MOVE WS-MESSAGE TO ERRMSGO OF COUSR0AO

           EXEC CICS SEND
                     MAP('COUSR0A')
                     MAPSET('COUSR00')
                     FROM(COUSR0AO)
                     ERASE
                     CURSOR
           END-EXEC.

      *----------------------------------------------------------------*
      *                      RECEIVE-SCREEN
      *----------------------------------------------------------------*
       RECEIVE-SCREEN.

           EXEC CICS RECEIVE
                     MAP('COUSR0A')
                     MAPSET('COUSR00')
                     INTO(COUSR0AI)
                     RESP(WS-RESP-CD)
           END-EXEC.

      *----------------------------------------------------------------*
      *                      POPULATE-HEADER-INFO
      *----------------------------------------------------------------*
       POPULATE-HEADER-INFO.

           MOVE FUNCTION CURRENT-DATE  TO WS-CURDATE-DATA

           MOVE CCDA-TITLE01           TO TITLE01O OF COUSR0AO
           MOVE CCDA-TITLE02           TO TITLE02O OF COUSR0AO
           MOVE WS-TRANID              TO TRNNAMEO OF COUSR0AO
           MOVE WS-PGMNAME             TO PGMNAMEO OF COUSR0AO

           MOVE WS-CURDATE-MONTH       TO WS-CURDATE-MM
           MOVE WS-CURDATE-DAY         TO WS-CURDATE-DD
           MOVE WS-CURDATE-YEAR(3:2)   TO WS-CURDATE-YY

           MOVE WS-CURDATE-MM-DD-YY    TO CURDATEO OF COUSR0AO

           MOVE WS-CURTIME-HOURS       TO WS-CURTIME-HH
           MOVE WS-CURTIME-MINUTE      TO WS-CURTIME-MM
           MOVE WS-CURTIME-SECOND      TO WS-CURTIME-SS

           MOVE WS-CURTIME-HH-MM-SS    TO CURTIMEO OF COUSR0AO.

