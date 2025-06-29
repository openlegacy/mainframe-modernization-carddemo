******************************************************************
      * Program     : COUSR03S.CBL
      * Function    : Delete User Screen with RPC calls
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COUSR03S.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-VARIABLES.
         05 WS-PGMNAME                 PIC X(08) VALUE 'COUSR03S'.
         05 WS-TRANID                  PIC X(04) VALUE 'ALS3'.
         05 WS-MESSAGE                 PIC X(80) VALUE SPACES.
         05 WS-RPC-PROGRAM             PIC X(08) VALUE 'COUSR03L'.
         05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.
           88 ERR-FLG-ON                         VALUE 'Y'.
           88 ERR-FLG-OFF                        VALUE 'N'.
         05 WS-RESP-CD                 PIC S9(09) COMP VALUE ZEROS.

       COPY COCOM01Y.
          05 CDEMO-CU03-INFO.
             10 CDEMO-CU03-USRID-FIRST     PIC X(08).
             10 CDEMO-CU03-USRID-LAST      PIC X(08).
             10 CDEMO-CU03-PAGE-NUM        PIC 9(08).
             10 CDEMO-CU03-NEXT-PAGE-FLG   PIC X(01) VALUE 'N'.
                88 NEXT-PAGE-YES                     VALUE 'Y'.
                88 NEXT-PAGE-NO                      VALUE 'N'.
             10 CDEMO-CU03-USR-SEL-FLG     PIC X(01).
             10 CDEMO-CU03-USR-SELECTED    PIC X(08).

       COPY COTTL01Y.
       COPY CSDAT01Y.
       COPY CSMSG01Y.
       COPY CSUSR01Y.

      * RPC Communication Area for delete operations
       01 WS-RPC-COMMAREA.
           05 LK-OPERATION               PIC X(01).
               88 LK-OP-LOOKUP           VALUE 'L'.
               88 LK-OP-DELETE           VALUE 'D'.
           05 LK-INPUT-PARMS.
              10 LK-IN-USER-ID           PIC X(08).
           05 LK-OUTPUT-STATUS.
              10 LK-OUT-RETURN-CODE      PIC 9(02).
                 88 RC-SUCCESS           VALUE 00.
                 88 RC-NOT-FOUND         VALUE 01.
                 88 RC-VALIDATION-ERROR  VALUE 10.
                 88 RC-DATABASE-ERROR    VALUE 99.
              10 LK-OUT-MESSAGE          PIC X(80).
           05 LK-OUTPUT-USER.
              10 LK-OUT-USER-ID          PIC X(08).
              10 LK-OUT-USER-FNAME       PIC X(20).
              10 LK-OUT-USER-LNAME       PIC X(20).
              10 LK-OUT-USER-TYPE        PIC X(01).

       COPY COUSR03.
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
                          ERRMSGO OF COUSR3AO

           IF EIBCALEN = 0
               MOVE 'COSGN00S' TO CDEMO-TO-PROGRAM
               PERFORM RETURN-TO-PREV-SCREEN
           ELSE
               MOVE DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA
               IF NOT CDEMO-PGM-REENTER
                   SET CDEMO-PGM-REENTER TO TRUE
                   MOVE LOW-VALUES TO COUSR3AO
                   MOVE -1 TO USRIDINL OF COUSR3AI
                   IF CDEMO-CU03-USR-SELECTED
                   NOT = SPACES AND LOW-VALUES
                       MOVE CDEMO-CU03-USR-SELECTED TO
                            USRIDINI OF COUSR3AI
                       PERFORM PROCESS-ENTER-KEY
                   END-IF
                   PERFORM SEND-USRDEL-SCREEN
               ELSE
                   PERFORM RECEIVE-USRDEL-SCREEN
                   EVALUATE EIBAID
                       WHEN DFHENTER
                           PERFORM PROCESS-ENTER-KEY
                       WHEN DFHPF3
                           IF CDEMO-FROM-PROGRAM = SPACES OR LOW-VALUES
                               MOVE 'COADM01S' TO CDEMO-TO-PROGRAM
                           ELSE
                               MOVE CDEMO-FROM-PROGRAM TO
                                    CDEMO-TO-PROGRAM
                           END-IF
                           PERFORM RETURN-TO-PREV-SCREEN
                       WHEN DFHPF4
                           PERFORM CLEAR-CURRENT-SCREEN
                       WHEN DFHPF5
                           PERFORM DELETE-USER-INFO
                       WHEN DFHPF12
                           MOVE 'COADM01S' TO CDEMO-TO-PROGRAM
                           PERFORM RETURN-TO-PREV-SCREEN
                       WHEN OTHER
                           MOVE 'Y' TO WS-ERR-FLG
                           MOVE CCDA-MSG-INVALID-KEY TO WS-MESSAGE
                           PERFORM SEND-USRDEL-SCREEN
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

           EVALUATE TRUE
               WHEN USRIDINI OF COUSR3AI = SPACES OR LOW-VALUES
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE 'User ID can NOT be empty...' TO WS-MESSAGE
                   MOVE -1 TO USRIDINL OF COUSR3AI
                   PERFORM SEND-USRDEL-SCREEN
               WHEN OTHER
                   MOVE -1 TO USRIDINL OF COUSR3AI
                   CONTINUE
           END-EVALUATE

           IF NOT ERR-FLG-ON
               MOVE SPACES TO FNAMEI   OF COUSR3AI
                              LNAMEI   OF COUSR3AI
                              USRTYPEI OF COUSR3AI
               PERFORM LOOKUP-USER-VIA-RPC
           END-IF.

      *----------------------------------------------------------------*
      *                      DELETE-USER-INFO
      *----------------------------------------------------------------*
       DELETE-USER-INFO.

           EVALUATE TRUE
               WHEN USRIDINI OF COUSR3AI = SPACES OR LOW-VALUES
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE 'User ID can NOT be empty...' TO WS-MESSAGE
                   MOVE -1 TO USRIDINL OF COUSR3AI
                   PERFORM SEND-USRDEL-SCREEN
               WHEN OTHER
                   MOVE -1 TO USRIDINL OF COUSR3AI
                   CONTINUE
           END-EVALUATE

           IF NOT ERR-FLG-ON
               PERFORM DELETE-USER-VIA-RPC
           END-IF.

      *----------------------------------------------------------------*
      *                      LOOKUP-USER-VIA-RPC
      *----------------------------------------------------------------*
       LOOKUP-USER-VIA-RPC.

           MOVE SPACES TO LK-INPUT-PARMS LK-OUTPUT-STATUS LK-OUTPUT-USER
           SET LK-OP-LOOKUP TO TRUE
           MOVE USRIDINI OF COUSR3AI TO LK-IN-USER-ID

           PERFORM CALL-RPC-PROGRAM

           IF NOT ERR-FLG-ON
               EVALUATE TRUE
                   WHEN RC-SUCCESS
                       MOVE LK-OUT-USER-FNAME TO FNAMEI  OF COUSR3AI
                       MOVE LK-OUT-USER-LNAME TO LNAMEI  OF COUSR3AI
                       MOVE LK-OUT-USER-TYPE  TO USRTYPEI OF COUSR3AI
                       MOVE 'Press PF5 key to delete this user ...'
                            TO WS-MESSAGE
                       MOVE DFHNEUTR TO ERRMSGC OF COUSR3AO
                       PERFORM SEND-USRDEL-SCREEN
                   WHEN RC-NOT-FOUND
                       MOVE 'Y' TO WS-ERR-FLG
                       MOVE 'User ID NOT found...' TO WS-MESSAGE
                       MOVE -1 TO USRIDINL OF COUSR3AI
                       PERFORM SEND-USRDEL-SCREEN
                   WHEN OTHER
                       MOVE 'Y' TO WS-ERR-FLG
                       MOVE 'Unable to lookup User...' TO WS-MESSAGE
                       MOVE -1 TO USRIDINL OF COUSR3AI
                       PERFORM SEND-USRDEL-SCREEN
               END-EVALUATE
           END-IF.

      *----------------------------------------------------------------*
      *                      DELETE-USER-VIA-RPC
      *----------------------------------------------------------------*
       DELETE-USER-VIA-RPC.

           MOVE SPACES TO LK-INPUT-PARMS LK-OUTPUT-STATUS LK-OUTPUT-USER
           SET LK-OP-DELETE TO TRUE
           MOVE USRIDINI OF COUSR3AI TO LK-IN-USER-ID

           PERFORM CALL-RPC-PROGRAM

           IF NOT ERR-FLG-ON
               EVALUATE TRUE
                   WHEN RC-SUCCESS
                       PERFORM INITIALIZE-ALL-FIELDS
                       MOVE SPACES TO WS-MESSAGE
                       MOVE DFHGREEN TO ERRMSGC OF COUSR3AO
                       STRING 'User ' DELIMITED BY SIZE
                              LK-IN-USER-ID DELIMITED BY SPACE
                              ' has been deleted ...' DELIMITED BY SIZE
                         INTO WS-MESSAGE
                       PERFORM SEND-USRDEL-SCREEN
                   WHEN RC-NOT-FOUND
                       MOVE 'Y' TO WS-ERR-FLG
                       MOVE 'User ID NOT found...' TO WS-MESSAGE
                       MOVE -1 TO USRIDINL OF COUSR3AI
                       PERFORM SEND-USRDEL-SCREEN
                   WHEN OTHER
                       MOVE 'Y' TO WS-ERR-FLG
                       MOVE 'Unable to Update User...' TO WS-MESSAGE
                       MOVE -1 TO USRIDINL OF COUSR3AI
                       PERFORM SEND-USRDEL-SCREEN
               END-EVALUATE
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
                   MOVE 'Unable to lookup User...' TO WS-MESSAGE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   MOVE 'Unable to lookup User...' TO WS-MESSAGE
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
      *                      SEND-USRDEL-SCREEN
      *----------------------------------------------------------------*
       SEND-USRDEL-SCREEN.

           PERFORM POPULATE-HEADER-INFO
           MOVE WS-MESSAGE TO ERRMSGO OF COUSR3AO

           EXEC CICS SEND
                MAP('COUSR3A')
                MAPSET('COUSR03')
                FROM(COUSR3AO)
                ERASE
                CURSOR
           END-EXEC.

      *----------------------------------------------------------------*
      *                      RECEIVE-USRDEL-SCREEN
      *----------------------------------------------------------------*
       RECEIVE-USRDEL-SCREEN.

           EXEC CICS RECEIVE
                MAP('COUSR3A')
                MAPSET('COUSR03')
                INTO(COUSR3AI)
                RESP(WS-RESP-CD)
           END-EXEC.

      *----------------------------------------------------------------*
      *                      POPULATE-HEADER-INFO
      *----------------------------------------------------------------*
       POPULATE-HEADER-INFO.

           MOVE FUNCTION CURRENT-DATE  TO WS-CURDATE-DATA

           MOVE CCDA-TITLE01           TO TITLE01O OF COUSR3AO
           MOVE CCDA-TITLE02           TO TITLE02O OF COUSR3AO
           MOVE WS-TRANID              TO TRNNAMEO OF COUSR3AO
           MOVE WS-PGMNAME             TO PGMNAMEO OF COUSR3AO

           MOVE WS-CURDATE-MONTH       TO WS-CURDATE-MM
           MOVE WS-CURDATE-DAY         TO WS-CURDATE-DD
           MOVE WS-CURDATE-YEAR(3:2)   TO WS-CURDATE-YY

           MOVE WS-CURDATE-MM-DD-YY    TO CURDATEO OF COUSR3AO

           MOVE WS-CURTIME-HOURS       TO WS-CURTIME-HH
           MOVE WS-CURTIME-MINUTE      TO WS-CURTIME-MM
           MOVE WS-CURTIME-SECOND      TO WS-CURTIME-SS

           MOVE WS-CURTIME-HH-MM-SS    TO CURTIMEO OF COUSR3AO.

      *----------------------------------------------------------------*
      *                      CLEAR-CURRENT-SCREEN
      *----------------------------------------------------------------*
       CLEAR-CURRENT-SCREEN.

           PERFORM INITIALIZE-ALL-FIELDS
           PERFORM SEND-USRDEL-SCREEN.

      *----------------------------------------------------------------*
      *                      INITIALIZE-ALL-FIELDS
      *----------------------------------------------------------------*
       INITIALIZE-ALL-FIELDS.

           MOVE -1 TO USRIDINL OF COUSR3AI
           MOVE SPACES TO USRIDINI OF COUSR3AI
                          FNAMEI   OF COUSR3AI
                          LNAMEI   OF COUSR3AI
                          USRTYPEI OF COUSR3AI
                          WS-MESSAGE.