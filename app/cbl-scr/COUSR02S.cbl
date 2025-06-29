******************************************************************
      * Program     : COUSR02S.CBL
      * Function    : Update User Screen with RPC calls
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COUSR02S.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-VARIABLES.
         05 WS-PGMNAME                 PIC X(08) VALUE 'COUSR02S'.
         05 WS-TRANID                  PIC X(04) VALUE 'ALS2'.
         05 WS-MESSAGE                 PIC X(80) VALUE SPACES.
         05 WS-RPC-PROGRAM             PIC X(08) VALUE 'COUSR02L'.
         05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.
           88 ERR-FLG-ON                         VALUE 'Y'.
           88 ERR-FLG-OFF                        VALUE 'N'.
         05 WS-RESP-CD                 PIC S9(09) COMP VALUE ZEROS.



       COPY COCOM01Y.
          05 CDEMO-CU02-INFO.
             10 CDEMO-CU02-USRID-FIRST     PIC X(08).
             10 CDEMO-CU02-USRID-LAST      PIC X(08).
             10 CDEMO-CU02-PAGE-NUM        PIC 9(08).
             10 CDEMO-CU02-NEXT-PAGE-FLG   PIC X(01) VALUE 'N'.
                88 NEXT-PAGE-YES                     VALUE 'Y'.
                88 NEXT-PAGE-NO                      VALUE 'N'.
             10 CDEMO-CU02-USR-SEL-FLG     PIC X(01).
             10 CDEMO-CU02-USR-SELECTED    PIC X(08).


      * ADDED: Proper header copybooks
       COPY COTTL01Y.
       COPY CSDAT01Y.
       COPY CSMSG01Y.
       COPY CSUSR01Y.

      * RPC Communication Area for updates
       01 WS-RPC-COMMAREA.
           05 LK-OPERATION               PIC X(01).
               88 LK-OP-LOOKUP           VALUE 'L'.
               88 LK-OP-UPDATE           VALUE 'U'.
           05 LK-INPUT-USER.
               10 LK-IN-USER-ID             PIC X(08).
               10 LK-IN-USER-FNAME          PIC X(20).
               10 LK-IN-USER-LNAME          PIC X(20).
               10 LK-IN-USER-PWD            PIC X(08).
               10 LK-IN-USER-TYPE           PIC X(01).
           05 LK-OUTPUT-STATUS.
               10 LK-OUT-RETURN-CODE        PIC 9(02).
                   88 RC-SUCCESS             VALUE 00.
                   88 RC-NOT-FOUND           VALUE 01.
                   88 RC-NO-CHANGES          VALUE 02.
                   88 RC-VALIDATION-ERROR    VALUE 10.
                   88 RC-DATABASE-ERROR      VALUE 99.
               10 LK-OUT-MESSAGE            PIC X(80).

       COPY COUSR02.
       COPY DFHAID.
       COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
         05  LK-COMMAREA                           PIC X(01)
             OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.

       PROCEDURE DIVISION.
       MAIN-PARA.

           SET ERR-FLG-OFF TO TRUE
           MOVE SPACES TO WS-MESSAGE
           MOVE SPACES TO ERRMSGO OF COUSR2AO



           IF EIBCALEN = 0
               MOVE 'COSGN00S' TO CDEMO-TO-PROGRAM
               PERFORM RETURN-TO-PREV-SCREEN
           ELSE
               MOVE DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA
               IF NOT CDEMO-PGM-REENTER
                   SET CDEMO-PGM-REENTER TO TRUE
                   MOVE LOW-VALUES TO COUSR2AO
                   MOVE -1 TO USRIDINL OF COUSR2AI
                   IF CDEMO-CU02-USR-SELECTED
                   NOT = SPACES AND LOW-VALUES
                       MOVE CDEMO-CU02-USR-SELECTED
                       TO USRIDINI OF COUSR2AI
                       PERFORM PROCESS-ENTER-KEY
                   END-IF
                   PERFORM SEND-SCREEN
               ELSE
                   PERFORM RECEIVE-SCREEN
                   EVALUATE EIBAID
                       WHEN DFHENTER
                           PERFORM PROCESS-ENTER-KEY
                       WHEN DFHPF3
                           PERFORM UPDATE-USER-INFO
                           IF CDEMO-FROM-PROGRAM = SPACES OR LOW-VALUES
                            MOVE 'COADM01S' TO CDEMO-TO-PROGRAM
                           ELSE
                            MOVE CDEMO-FROM-PROGRAM TO CDEMO-TO-PROGRAM
                           END-IF
                           PERFORM RETURN-TO-PREV-SCREEN
                       WHEN DFHPF4
                           PERFORM CLEAR-CURRENT-SCREEN
                       WHEN DFHPF5
                           PERFORM UPDATE-USER-INFO
                       WHEN DFHPF12
                           MOVE 'COADM01S' TO CDEMO-TO-PROGRAM
                           PERFORM RETURN-TO-PREV-SCREEN
                       WHEN OTHER
                           MOVE 'Y' TO WS-ERR-FLG
                           MOVE CCDA-MSG-INVALID-KEY TO WS-MESSAGE
                           PERFORM SEND-SCREEN
                   END-EVALUATE
               END-IF
           END-IF

           EXEC CICS RETURN
                     TRANSID (WS-TRANID)
                     COMMAREA (CARDDEMO-COMMAREA)
           END-EXEC.

       PROCESS-ENTER-KEY.

           EVALUATE TRUE
               WHEN USRIDINI OF COUSR2AI = SPACES OR LOW-VALUES
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE 'User ID cannot be empty' TO WS-MESSAGE
                   MOVE -1 TO USRIDINL OF COUSR2AI
                   PERFORM SEND-SCREEN
               WHEN OTHER
                   MOVE -1 TO USRIDINL OF COUSR2AI
                   CONTINUE
           END-EVALUATE

           IF NOT ERR-FLG-ON
               MOVE SPACES TO FNAMEI OF COUSR2AI
                              LNAMEI OF COUSR2AI
                              PASSWDI OF COUSR2AI
                              USRTYPEI OF COUSR2AI
               PERFORM LOOKUP-USER-VIA-RPC
           END-IF.

      *----------------------------------------------------------------*
      *                      LOOKUP-USER-VIA-RPC
      *----------------------------------------------------------------*
       LOOKUP-USER-VIA-RPC.

           MOVE SPACES TO LK-INPUT-USER LK-OUTPUT-STATUS
           SET LK-OP-LOOKUP TO TRUE
           MOVE USRIDINI OF COUSR2AI TO LK-IN-USER-ID

           PERFORM CALL-RPC-PROGRAM

           IF NOT ERR-FLG-ON
               EVALUATE TRUE
                   WHEN RC-SUCCESS
                       MOVE LK-IN-USER-FNAME TO FNAMEI OF COUSR2AI
                       MOVE LK-IN-USER-LNAME TO LNAMEI OF COUSR2AI
                       MOVE LK-IN-USER-PWD TO PASSWDI OF COUSR2AI
                       MOVE LK-IN-USER-TYPE TO USRTYPEI OF COUSR2AI
                       MOVE 'Press PF5 to save updates' TO WS-MESSAGE
                   WHEN RC-NOT-FOUND
                       MOVE 'User ID not found' TO WS-MESSAGE
                       MOVE -1 TO USRIDINL OF COUSR2AI
                   WHEN OTHER
                       MOVE LK-OUT-MESSAGE TO WS-MESSAGE
               END-EVALUATE
               PERFORM SEND-SCREEN
           END-IF.
      *----------------------------------------------------------------*
      *                      UPDATE-USER-INFO
      *----------------------------------------------------------------*
       UPDATE-USER-INFO.

           EVALUATE TRUE
               WHEN USRIDINI OF COUSR2AI = SPACES OR LOW-VALUES
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE 'User ID cannot be empty' TO WS-MESSAGE
                   MOVE -1 TO USRIDINL OF COUSR2AI
                   PERFORM SEND-SCREEN
               WHEN FNAMEI OF COUSR2AI = SPACES OR LOW-VALUES
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE 'First Name cannot be empty' TO WS-MESSAGE
                   MOVE -1 TO USRIDINL OF COUSR2AI
                   PERFORM SEND-SCREEN
               WHEN LNAMEI OF COUSR2AI = SPACES OR LOW-VALUES
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE 'Last Name cannot be empty' TO WS-MESSAGE
                   MOVE -1 TO USRIDINL OF COUSR2AI
                   PERFORM SEND-SCREEN
               WHEN PASSWDI OF COUSR2AI = SPACES OR LOW-VALUES
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE 'Password cannot be empty' TO WS-MESSAGE
                   MOVE -1 TO USRIDINL OF COUSR2AI
                   PERFORM SEND-SCREEN
               WHEN USRTYPEI OF COUSR2AI = SPACES OR LOW-VALUES
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE 'User Type cannot be empty' TO WS-MESSAGE
                   MOVE -1 TO USRIDINL OF COUSR2AI
                   PERFORM SEND-SCREEN
               WHEN OTHER
                   MOVE -1 TO USRIDINL OF COUSR2AI
                   CONTINUE
           END-EVALUATE

           IF NOT ERR-FLG-ON
               MOVE SPACES TO LK-INPUT-USER LK-OUTPUT-STATUS
               SET LK-OP-UPDATE TO TRUE
               MOVE USRIDINI OF COUSR2AI TO LK-IN-USER-ID
               MOVE FNAMEI   OF COUSR2AI TO LK-IN-USER-FNAME
               MOVE LNAMEI   OF COUSR2AI TO LK-IN-USER-LNAME
               MOVE PASSWDI  OF COUSR2AI TO LK-IN-USER-PWD
               MOVE USRTYPEI OF COUSR2AI TO LK-IN-USER-TYPE

               PERFORM CALL-RPC-PROGRAM

               MOVE LK-OUT-MESSAGE TO WS-MESSAGE
               PERFORM SEND-SCREEN
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
                   MOVE 'COUSR02L program not found' TO WS-MESSAGE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   MOVE 'Error calling RPC program' TO WS-MESSAGE
           END-EVALUATE.

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
           MOVE WS-MESSAGE TO ERRMSGO OF COUSR2AO

           EXEC CICS SEND
                MAP('COUSR2A')
                MAPSET('COUSR02')
                FROM(COUSR2AO)
                ERASE
                CURSOR
           END-EXEC.

      *----------------------------------------------------------------*
      *                      RECEIVE-SCREEN
      *----------------------------------------------------------------*
       RECEIVE-SCREEN.

           EXEC CICS RECEIVE
                MAP('COUSR2A')
                MAPSET('COUSR02')
                INTO(COUSR2AI)
                RESP(WS-RESP-CD)
           eND-EXEC.


      *----------------------------------------------------------------*
      *                      POPULATE-HEADER-INFO
      *----------------------------------------------------------------*
       POPULATE-HEADER-INFO.

           MOVE FUNCTION CURRENT-DATE  TO WS-CURDATE-DATA

           MOVE CCDA-TITLE01           TO TITLE01O OF COUSR2AO
           MOVE CCDA-TITLE02           TO TITLE02O OF COUSR2AO
           MOVE WS-TRANID              TO TRNNAMEO OF COUSR2AO
           MOVE WS-PGMNAME             TO PGMNAMEO OF COUSR2AO

           MOVE WS-CURDATE-MONTH       TO WS-CURDATE-MM
           MOVE WS-CURDATE-DAY         TO WS-CURDATE-DD
           MOVE WS-CURDATE-YEAR(3:2)   TO WS-CURDATE-YY

           MOVE WS-CURDATE-MM-DD-YY    TO CURDATEO OF COUSR2AO

           MOVE WS-CURTIME-HOURS       TO WS-CURTIME-HH
           MOVE WS-CURTIME-MINUTE      TO WS-CURTIME-MM
           MOVE WS-CURTIME-SECOND      TO WS-CURTIME-SS

           MOVE WS-CURTIME-HH-MM-SS    TO CURTIMEO OF COUSR2AO.

      *----------------------------------------------------------------*
      *                      CLEAR-CURRENT-SCREEN
      *----------------------------------------------------------------*
       CLEAR-CURRENT-SCREEN.

           PERFORM INITIALIZE-ALL-FIELDS
           PERFORM SEND-SCREEN.

      *----------------------------------------------------------------*
      *                      INITIALIZE-ALL-FIELDS
      *----------------------------------------------------------------*
       INITIALIZE-ALL-FIELDS.

           MOVE -1 TO USRIDINL OF COUSR2AI
           MOVE SPACES TO USRIDINI OF COUSR2AI
                          FNAMEI   OF COUSR2AI
                          LNAMEI   OF COUSR2AI
                          PASSWDI  OF COUSR2AI
                          USRTYPEI OF COUSR2AI
                          WS-MESSAGE.