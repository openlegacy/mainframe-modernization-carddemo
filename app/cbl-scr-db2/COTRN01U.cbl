000001******************************************************************
      * Program     : COTRN01U.CBL
      * Application : CardDemo
      * Type        : CICS COBOL Screen Program
      * Function    : View a Transaction from DB2 TRANSACT table
      * Description : Screen handling program that calls COTRN01A RPC
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COTRN01U.
       AUTHOR.     AWS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
      *----------------------------------------------------------------*
      *                     WORKING STORAGE SECTION
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

       01 WS-VARIABLES.
          05 WS-PGMNAME                 PIC X(08) VALUE 'COTRN01U'.
          05 WS-TRANID                  PIC X(04) VALUE 'AASF'.
          05 WS-MESSAGE                 PIC X(80) VALUE SPACES.
          05 WS-RPC-PROGRAM             PIC X(08) VALUE 'COTRN01A'.
          05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.
             88 ERR-FLG-ON                         VALUE 'Y'.
             88 ERR-FLG-OFF                        VALUE 'N'.
          05 WS-USR-MODIFIED            PIC X(01) VALUE 'N'.
             88 USR-MODIFIED-YES                   VALUE 'Y'.
             88 USR-MODIFIED-NO                    VALUE 'N'.

       COPY COCOM01Y.
          05 CDEMO-CT01-INFO.
             10 CDEMO-CT01-TRNID-FIRST     PIC X(16).
             10 CDEMO-CT01-TRNID-LAST      PIC X(16).
             10 CDEMO-CT01-PAGE-NUM        PIC 9(08).
             10 CDEMO-CT01-NEXT-PAGE-FLG   PIC X(01) VALUE 'N'.
                88 NEXT-PAGE-YES                     VALUE 'Y'.
                88 NEXT-PAGE-NO                      VALUE 'N'.
             10 CDEMO-CT01-TRN-SEL-FLG     PIC X(01).
             10 CDEMO-CT01-TRN-SELECTED    PIC X(16).

       COPY COTRN01.

       COPY COTTL01Y.
       COPY CSDAT01Y.
       COPY CSMSG01Y.

       01 WS-RPC-COMMAREA.
          05 WS-RPC-INPUT-PARMS.
             10 WS-RPC-IN-OPERATION     PIC X(01).
                88 WS-OP-READ                     VALUE 'R'.
             10 WS-RPC-IN-TRAN-ID       PIC X(16).
          05 WS-RPC-OUTPUT-STATUS.
             10 WS-RPC-OUT-RETURN-CODE  PIC 9(02).
                88 WS-RC-SUCCESS                  VALUE 00.
                88 WS-RC-NOT-FOUND                VALUE 01.
                88 WS-RC-INPUT-ERROR              VALUE 03.
                88 WS-RC-DATABASE-ERROR           VALUE 99.
             10 WS-RPC-OUT-MESSAGE      PIC X(80).
             10 WS-RPC-OUT-ERROR-FIELD  PIC X(30).
          05 WS-RPC-OUTPUT-DATA.
             10 WS-RPC-OUT-TRAN-ID      PIC X(16).
             10 WS-RPC-OUT-CARD-NUM     PIC X(16).
             10 WS-RPC-OUT-TYPE-CD      PIC X(02).
             10 WS-RPC-OUT-CAT-CD       PIC 9(04).
             10 WS-RPC-OUT-SOURCE       PIC X(10).
             10 WS-RPC-OUT-AMT          PIC X(13).
             10 WS-RPC-OUT-DESC         PIC X(100).
             10 WS-RPC-OUT-ORIG-TS      PIC X(26).
             10 WS-RPC-OUT-PROC-TS      PIC X(26).
             10 WS-RPC-OUT-MERCHANT-ID  PIC 9(09).
             10 WS-RPC-OUT-MERCHANT-NAME PIC X(50).
             10 WS-RPC-OUT-MERCHANT-CITY PIC X(50).
             10 WS-RPC-OUT-MERCHANT-ZIP PIC X(10).

       COPY DFHAID.
       COPY DFHBMSCA.

      *----------------------------------------------------------------*
      *                        LINKAGE SECTION
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01  DFHCOMMAREA.
          05  LK-COMMAREA                           PIC X(01)
              OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.

      *----------------------------------------------------------------*
      *                       PROCEDURE DIVISION
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       MAIN-PARA.

           SET ERR-FLG-OFF     TO TRUE
           SET USR-MODIFIED-NO TO TRUE

           MOVE SPACES TO WS-MESSAGE
                          ERRMSGO OF COTRN1AO

           IF EIBCALEN = 0
               MOVE 'COSGN00S' TO CDEMO-TO-PROGRAM
               PERFORM RETURN-TO-PREV-SCREEN
           ELSE
               MOVE DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA
               IF NOT CDEMO-PGM-REENTER
                   SET CDEMO-PGM-REENTER    TO TRUE
                   MOVE LOW-VALUES          TO COTRN1AO
                   MOVE -1       TO TRNIDINL OF COTRN1AI
                   IF CDEMO-CT01-TRN-SELECTED NOT =
                                              SPACES AND LOW-VALUES
                       MOVE CDEMO-CT01-TRN-SELECTED TO
                            TRNIDINI OF COTRN1AI
                       PERFORM PROCESS-ENTER-KEY
                   END-IF
                   PERFORM SEND-TRNVIEW-SCREEN
               ELSE
                   PERFORM RECEIVE-TRNVIEW-SCREEN
                   EVALUATE EIBAID
                       WHEN DFHENTER
                           PERFORM PROCESS-ENTER-KEY
                       WHEN DFHPF3
                           IF CDEMO-FROM-PROGRAM = SPACES OR
                              LOW-VALUES
                               MOVE 'COMEN01S' TO CDEMO-TO-PROGRAM
                           ELSE
                               MOVE CDEMO-FROM-PROGRAM TO
                                    CDEMO-TO-PROGRAM
                           END-IF
                           PERFORM RETURN-TO-PREV-SCREEN
                       WHEN DFHPF4
                           PERFORM CLEAR-CURRENT-SCREEN
                       WHEN DFHPF5
                           MOVE 'COTRN00U' TO CDEMO-TO-PROGRAM
                           PERFORM RETURN-TO-PREV-SCREEN
                       WHEN OTHER
                           MOVE 'Y' TO WS-ERR-FLG
                           MOVE CCDA-MSG-INVALID-KEY TO WS-MESSAGE
                           PERFORM SEND-TRNVIEW-SCREEN
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

           MOVE -1 TO TRNIDINL OF COTRN1AI
           PERFORM CALL-RPC-READ-TRAN.

      *----------------------------------------------------------------*
      *                   CALL-RPC-READ-TRAN
      *----------------------------------------------------------------*
       CALL-RPC-READ-TRAN.

           INITIALIZE WS-RPC-COMMAREA

           SET WS-OP-READ TO TRUE
           MOVE TRNIDINI OF COTRN1AI TO WS-RPC-IN-TRAN-ID

           EXEC CICS LINK
                PROGRAM  (WS-RPC-PROGRAM)
                COMMAREA (WS-RPC-COMMAREA)
                LENGTH   (LENGTH OF WS-RPC-COMMAREA)
           END-EXEC

           EVALUATE TRUE
               WHEN WS-RC-SUCCESS
                   PERFORM FILL-SCREEN-FIELDS
                   PERFORM SEND-TRNVIEW-SCREEN
               WHEN WS-RC-NOT-FOUND
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-OUT-MESSAGE TO WS-MESSAGE
                   IF WS-RPC-OUT-ERROR-FIELD = 'TRAN-ID'
                       MOVE -1 TO TRNIDINL OF COTRN1AI
                   END-IF
                   PERFORM SEND-TRNVIEW-SCREEN
               WHEN WS-RC-INPUT-ERROR
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-OUT-MESSAGE TO WS-MESSAGE
                   IF WS-RPC-OUT-ERROR-FIELD = 'TRAN-ID'
                       MOVE -1 TO TRNIDINL OF COTRN1AI
                   END-IF
                   PERFORM SEND-TRNVIEW-SCREEN
               WHEN WS-RC-DATABASE-ERROR
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-OUT-MESSAGE TO WS-MESSAGE
                   PERFORM SEND-TRNVIEW-SCREEN
               WHEN OTHER
                   MOVE 'Y' TO WS-ERR-FLG
                   STRING 'Unexpected RPC error occurred'
                          DELIMITED BY SIZE
                     INTO WS-MESSAGE
                   PERFORM SEND-TRNVIEW-SCREEN
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      FILL-SCREEN-FIELDS
      *----------------------------------------------------------------*
       FILL-SCREEN-FIELDS.

           MOVE SPACES TO TRNIDI   OF COTRN1AI
                          CARDNUMI OF COTRN1AI
                          TTYPCDI  OF COTRN1AI
                          TCATCDI  OF COTRN1AI
                          TRNSRCI  OF COTRN1AI
                          TRNAMTI  OF COTRN1AI
                          TDESCI   OF COTRN1AI
                          TORIGDTI OF COTRN1AI
                          TPROCDTI OF COTRN1AI
                          MIDI     OF COTRN1AI
                          MNAMEI   OF COTRN1AI
                          MCITYI   OF COTRN1AI
                          MZIPI    OF COTRN1AI

           MOVE WS-RPC-OUT-TRAN-ID TO TRNIDI OF COTRN1AI
           MOVE WS-RPC-OUT-CARD-NUM TO CARDNUMI OF COTRN1AI
           MOVE WS-RPC-OUT-TYPE-CD TO TTYPCDI OF COTRN1AI
           MOVE WS-RPC-OUT-CAT-CD TO TCATCDI OF COTRN1AI
           MOVE WS-RPC-OUT-SOURCE TO TRNSRCI OF COTRN1AI
           MOVE WS-RPC-OUT-AMT TO TRNAMTI OF COTRN1AI
           MOVE WS-RPC-OUT-DESC TO TDESCI OF COTRN1AI
           MOVE WS-RPC-OUT-ORIG-TS TO TORIGDTI OF COTRN1AI
           MOVE WS-RPC-OUT-PROC-TS TO TPROCDTI OF COTRN1AI
           MOVE WS-RPC-OUT-MERCHANT-ID TO MIDI OF COTRN1AI
           MOVE WS-RPC-OUT-MERCHANT-NAME TO MNAMEI OF COTRN1AI
           MOVE WS-RPC-OUT-MERCHANT-CITY TO MCITYI OF COTRN1AI
           MOVE WS-RPC-OUT-MERCHANT-ZIP TO MZIPI OF COTRN1AI.

      *----------------------------------------------------------------*
      *                      RETURN-TO-PREV-SCREEN
      *----------------------------------------------------------------*
       RETURN-TO-PREV-SCREEN.

           IF CDEMO-TO-PROGRAM = LOW-VALUES OR SPACES
               MOVE 'COSGN00S' TO CDEMO-TO-PROGRAM
           END-IF
           MOVE WS-TRANID    TO CDEMO-FROM-TRANID
           MOVE WS-PGMNAME   TO CDEMO-FROM-PROGRAM
           MOVE ZEROS        TO CDEMO-PGM-CONTEXT
           EXEC CICS
               XCTL PROGRAM(CDEMO-TO-PROGRAM)
               COMMAREA(CARDDEMO-COMMAREA)
           END-EXEC.

      *----------------------------------------------------------------*
      *                      SEND-TRNVIEW-SCREEN
      *----------------------------------------------------------------*
       SEND-TRNVIEW-SCREEN.

           PERFORM FILL-HEADER-INFO

           MOVE WS-MESSAGE TO ERRMSGO OF COTRN1AO

           EXEC CICS SEND
                     MAP('COTRN1A')
                     MAPSET('COTRN01')
                     FROM(COTRN1AO)
                     ERASE
                     CURSOR
           END-EXEC.

      *----------------------------------------------------------------*
      *                      RECEIVE-TRNVIEW-SCREEN
      *----------------------------------------------------------------*
       RECEIVE-TRNVIEW-SCREEN.

           EXEC CICS RECEIVE
                     MAP('COTRN1A')
                     MAPSET('COTRN01')
                     INTO(COTRN1AI)
           END-EXEC.

      *----------------------------------------------------------------*
      *                      FILL-HEADER-INFO
      *----------------------------------------------------------------*
       FILL-HEADER-INFO.

           MOVE FUNCTION CURRENT-DATE  TO WS-CURDATE-DATA

           MOVE CCDA-TITLE01           TO TITLE01O OF COTRN1AO
           MOVE CCDA-TITLE02           TO TITLE02O OF COTRN1AO
           MOVE WS-TRANID              TO TRNNAMEO OF COTRN1AO
           MOVE WS-PGMNAME             TO PGMNAMEO OF COTRN1AO

           MOVE WS-CURDATE-MONTH       TO WS-CURDATE-MM
           MOVE WS-CURDATE-DAY         TO WS-CURDATE-DD
           MOVE WS-CURDATE-YEAR(3:2)   TO WS-CURDATE-YY

           MOVE WS-CURDATE-MM-DD-YY    TO CURDATEO OF COTRN1AO

           MOVE WS-CURTIME-HOURS       TO WS-CURTIME-HH
           MOVE WS-CURTIME-MINUTE      TO WS-CURTIME-MM
           MOVE WS-CURTIME-SECOND      TO WS-CURTIME-SS

           MOVE WS-CURTIME-HH-MM-SS    TO CURTIMEO OF COTRN1AO.

      *----------------------------------------------------------------*
      *                      CLEAR-CURRENT-SCREEN
      *----------------------------------------------------------------*
       CLEAR-CURRENT-SCREEN.

           PERFORM INIT-ALL-FIELDS
           PERFORM SEND-TRNVIEW-SCREEN.

      *----------------------------------------------------------------*
      *                      INIT-ALL-FIELDS
      *----------------------------------------------------------------*
       INIT-ALL-FIELDS.

           MOVE -1      TO TRNIDINL OF COTRN1AI
           MOVE SPACES  TO TRNIDINI OF COTRN1AI
                           TRNIDI   OF COTRN1AI
                           CARDNUMI OF COTRN1AI
                           TTYPCDI  OF COTRN1AI
                           TCATCDI  OF COTRN1AI
                           TRNSRCI  OF COTRN1AI
                           TRNAMTI  OF COTRN1AI
                           TDESCI   OF COTRN1AI
                           TORIGDTI OF COTRN1AI
                           TPROCDTI OF COTRN1AI
                           MIDI     OF COTRN1AI
                           MNAMEI   OF COTRN1AI
                           MCITYI   OF COTRN1AI
                           MZIPI    OF COTRN1AI
                           WS-MESSAGE.

      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:12:34 CDT
      *
