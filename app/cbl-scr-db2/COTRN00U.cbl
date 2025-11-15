000001******************************************************************
      * Program:     COTRN00U                                          *
      * Application: CardDemo                                         *
      * Type:        CICS COBOL Program                               *
      * Function:    List Transactions - Screen UI Program            *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COTRN00U.
       AUTHOR.     AWS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
      *----------------------------------------------------------------*
      *                     WORKING STORAGE SECTION
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

       01 WS-VARIABLES.
         05 WS-PGMNAME                 PIC X(08) VALUE 'COTRN00U'.
         05 WS-TRANID                  PIC X(04) VALUE 'AASE'.
         05 WS-MESSAGE                 PIC X(80) VALUE SPACES.
         05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.
           88 ERR-FLG-ON                         VALUE 'Y'.
           88 ERR-FLG-OFF                        VALUE 'N'.
         05 WS-SEND-ERASE-FLG          PIC X(01) VALUE 'Y'.
           88 SEND-ERASE-YES                     VALUE 'Y'.
           88 SEND-ERASE-NO                      VALUE 'N'.

         05 WS-RESP-CD                 PIC S9(09) COMP VALUE ZEROS.
         05 WS-REAS-CD                 PIC S9(09) COMP VALUE ZEROS.
         05 WS-IDX                     PIC S9(04) COMP VALUE ZEROS.

         05 WS-TRAN-AMT                PIC +99999999.99.
         05 WS-TRAN-DATE               PIC X(08) VALUE '00/00/00'.

      ******************************************************************
      * RPC Program name and communication area
      ******************************************************************
         05 WS-RPC-PROGRAM             PIC X(08) VALUE 'COTRN00A'.

       COPY COCOM01Y.
          05 CDEMO-CT00-INFO.
             10 CDEMO-CT00-TRNID-FIRST     PIC X(16).
             10 CDEMO-CT00-TRNID-LAST      PIC X(16).
             10 CDEMO-CT00-PAGE-NUM        PIC 9(08).
             10 CDEMO-CT00-NEXT-PAGE-FLG   PIC X(01) VALUE 'N'.
                88 NEXT-PAGE-YES                     VALUE 'Y'.
                88 NEXT-PAGE-NO                      VALUE 'N'.
             10 CDEMO-CT00-TRN-SEL-FLG     PIC X(01).
             10 CDEMO-CT00-TRN-SELECTED    PIC X(16).

       COPY COTRN00.

       COPY COTTL01Y.
       COPY CSDAT01Y.
       COPY CSMSG01Y.

       COPY CVTRA05Y.

       COPY DFHAID.
       COPY DFHBMSCA.

      ******************************************************************
      * RPC Communication Area
      ******************************************************************
       01 WS-RPC-COMMAREA.
          05 LK-INPUT-PARMS.
             10 LK-IN-OPERATION         PIC X(01).
                88 OP-READ                       VALUE 'R'.
                88 OP-LIST-FORWARD               VALUE 'F'.
                88 OP-LIST-BACKWARD              VALUE 'B'.
             10 LK-IN-TRAN-ID           PIC X(16).
             10 LK-IN-MAX-RECORDS       PIC S9(04) COMP.
          05 LK-OUTPUT-STATUS.
             10 LK-OUT-RETURN-CODE      PIC 9(02).
                88 RC-SUCCESS                    VALUE 00.
                88 RC-NOT-FOUND                  VALUE 01.
                88 RC-INPUT-ERROR                VALUE 03.
                88 RC-DATABASE-ERROR             VALUE 99.
             10 LK-OUT-MESSAGE          PIC X(80).
          05 LK-OUTPUT-DATA.
             10 LK-OUT-RECORDS-COUNT    PIC S9(04) COMP.
             10 LK-OUT-NEXT-PAGE-FLG    PIC X(01).
                88 LK-NEXT-PAGE-YES              VALUE 'Y'.
                88 LK-NEXT-PAGE-NO               VALUE 'N'.
             10 LK-OUT-TRAN-ARRAY       OCCURS 1 TO 10
                                        DEPENDING ON
                                        LK-OUT-RECORDS-COUNT.
                15 LK-OUT-TRAN-ID       PIC X(16).
                15 LK-OUT-TRAN-AMT      PIC S9(10)V99 COMP-3.
                15 LK-OUT-TRAN-DESC     PIC X(50).
                15 LK-OUT-TRAN-ORIG-TS  PIC X(26).

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

           SET ERR-FLG-OFF TO TRUE
           SET NEXT-PAGE-NO TO TRUE
           SET SEND-ERASE-YES TO TRUE

           MOVE SPACES TO WS-MESSAGE
                          ERRMSGO OF COTRN0AO

           MOVE -1       TO TRNIDINL OF COTRN0AI

           IF EIBCALEN = 0
               MOVE 'COSGN00S' TO CDEMO-TO-PROGRAM
               PERFORM RETURN-TO-PREV-SCREEN
           ELSE
               MOVE DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA
               IF NOT CDEMO-PGM-REENTER
                   SET CDEMO-PGM-REENTER    TO TRUE
                   MOVE LOW-VALUES          TO COTRN0AO
                   PERFORM PROCESS-ENTER-KEY
                   PERFORM SEND-TRNLST-SCREEN
               ELSE
                   PERFORM RECEIVE-TRNLST-SCREEN
                   EVALUATE EIBAID
                       WHEN DFHENTER
                           PERFORM PROCESS-ENTER-KEY
                       WHEN DFHPF3
                           MOVE 'COMEN01S' TO CDEMO-TO-PROGRAM
                           PERFORM RETURN-TO-PREV-SCREEN
                       WHEN DFHPF7
                           PERFORM PROCESS-PF7-KEY
                       WHEN DFHPF8
                           PERFORM PROCESS-PF8-KEY
                       WHEN OTHER
                           MOVE 'Y'                       TO WS-ERR-FLG
                           MOVE -1       TO TRNIDINL OF COTRN0AI
                           MOVE CCDA-MSG-INVALID-KEY      TO WS-MESSAGE
                           PERFORM SEND-TRNLST-SCREEN
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
               WHEN SEL0001I OF COTRN0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0001I OF COTRN0AI TO CDEMO-CT00-TRN-SEL-FLG
                   MOVE TRNID01I OF COTRN0AI TO CDEMO-CT00-TRN-SELECTED
               WHEN SEL0002I OF COTRN0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0002I OF COTRN0AI TO CDEMO-CT00-TRN-SEL-FLG
                   MOVE TRNID02I OF COTRN0AI TO CDEMO-CT00-TRN-SELECTED
               WHEN SEL0003I OF COTRN0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0003I OF COTRN0AI TO CDEMO-CT00-TRN-SEL-FLG
                   MOVE TRNID03I OF COTRN0AI TO CDEMO-CT00-TRN-SELECTED
               WHEN SEL0004I OF COTRN0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0004I OF COTRN0AI TO CDEMO-CT00-TRN-SEL-FLG
                   MOVE TRNID04I OF COTRN0AI TO CDEMO-CT00-TRN-SELECTED
               WHEN SEL0005I OF COTRN0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0005I OF COTRN0AI TO CDEMO-CT00-TRN-SEL-FLG
                   MOVE TRNID05I OF COTRN0AI TO CDEMO-CT00-TRN-SELECTED
               WHEN SEL0006I OF COTRN0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0006I OF COTRN0AI TO CDEMO-CT00-TRN-SEL-FLG
                   MOVE TRNID06I OF COTRN0AI TO CDEMO-CT00-TRN-SELECTED
               WHEN SEL0007I OF COTRN0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0007I OF COTRN0AI TO CDEMO-CT00-TRN-SEL-FLG
                   MOVE TRNID07I OF COTRN0AI TO CDEMO-CT00-TRN-SELECTED
               WHEN SEL0008I OF COTRN0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0008I OF COTRN0AI TO CDEMO-CT00-TRN-SEL-FLG
                   MOVE TRNID08I OF COTRN0AI TO CDEMO-CT00-TRN-SELECTED
               WHEN SEL0009I OF COTRN0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0009I OF COTRN0AI TO CDEMO-CT00-TRN-SEL-FLG
                   MOVE TRNID09I OF COTRN0AI TO CDEMO-CT00-TRN-SELECTED
               WHEN SEL0010I OF COTRN0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0010I OF COTRN0AI TO CDEMO-CT00-TRN-SEL-FLG
                   MOVE TRNID10I OF COTRN0AI TO CDEMO-CT00-TRN-SELECTED
               WHEN OTHER
                   MOVE SPACES   TO CDEMO-CT00-TRN-SEL-FLG
                   MOVE SPACES   TO CDEMO-CT00-TRN-SELECTED
           END-EVALUATE

           IF (CDEMO-CT00-TRN-SEL-FLG NOT = SPACES AND LOW-VALUES) AND
              (CDEMO-CT00-TRN-SELECTED NOT = SPACES AND LOW-VALUES)
               EVALUATE CDEMO-CT00-TRN-SEL-FLG
                   WHEN 'S'
                   WHEN 's'
                        MOVE 'COTRN01U'   TO CDEMO-TO-PROGRAM
                        MOVE WS-TRANID    TO CDEMO-FROM-TRANID
                        MOVE WS-PGMNAME   TO CDEMO-FROM-PROGRAM
                        MOVE 0        TO CDEMO-PGM-CONTEXT
                        EXEC CICS
                            XCTL PROGRAM(CDEMO-TO-PROGRAM)
                            COMMAREA(CARDDEMO-COMMAREA)
                        END-EXEC
                   WHEN OTHER
                       STRING 'Invalid selection. Valid value is S'
                              DELIMITED BY SIZE
                         INTO WS-MESSAGE
                       MOVE -1       TO TRNIDINL OF COTRN0AI
               END-EVALUATE
           END-IF

           IF TRNIDINI OF COTRN0AI = SPACES OR LOW-VALUES
               MOVE LOW-VALUES TO CDEMO-CT00-TRNID-FIRST
           ELSE
               IF TRNIDINI  OF COTRN0AI IS NUMERIC
                   MOVE TRNIDINI  OF COTRN0AI TO
                        CDEMO-CT00-TRNID-FIRST
               ELSE
                   MOVE 'Y' TO WS-ERR-FLG
                   STRING 'Tran ID must be Numeric'
                          DELIMITED BY SIZE
                     INTO WS-MESSAGE
                   MOVE -1 TO TRNIDINL OF COTRN0AI
                   PERFORM SEND-TRNLST-SCREEN
               END-IF
           END-IF

           MOVE -1       TO TRNIDINL OF COTRN0AI

           MOVE 0       TO CDEMO-CT00-PAGE-NUM
           PERFORM PROCESS-PAGE-FORWARD

           IF NOT ERR-FLG-ON
               MOVE SPACE   TO TRNIDINO  OF COTRN0AO
           END-IF.

      *----------------------------------------------------------------*
      *                      PROCESS-PF7-KEY
      *----------------------------------------------------------------*
       PROCESS-PF7-KEY.

           IF CDEMO-CT00-TRNID-FIRST = SPACES OR LOW-VALUES
               MOVE LOW-VALUES TO CDEMO-CT00-TRNID-FIRST
           END-IF

           SET NEXT-PAGE-YES TO TRUE
           MOVE -1       TO TRNIDINL OF COTRN0AI

           IF CDEMO-CT00-PAGE-NUM > 1
               PERFORM PROCESS-PAGE-BACKWARD
           ELSE
               STRING 'You are already at the top of the page'
                      DELIMITED BY SIZE
                 INTO WS-MESSAGE
               SET SEND-ERASE-NO TO TRUE
               PERFORM SEND-TRNLST-SCREEN
           END-IF.

      *----------------------------------------------------------------*
      *                      PROCESS-PF8-KEY
      *----------------------------------------------------------------*
       PROCESS-PF8-KEY.

           IF CDEMO-CT00-TRNID-LAST = SPACES OR LOW-VALUES
               MOVE HIGH-VALUES TO CDEMO-CT00-TRNID-LAST
           END-IF

           MOVE -1       TO TRNIDINL OF COTRN0AI

           IF NEXT-PAGE-YES
               PERFORM PROCESS-PAGE-FORWARD
           ELSE
               STRING 'You are already at the bottom of the page'
                      DELIMITED BY SIZE
                 INTO WS-MESSAGE
               SET SEND-ERASE-NO TO TRUE
               PERFORM SEND-TRNLST-SCREEN
           END-IF.

      *----------------------------------------------------------------*
      *                      PROCESS-PAGE-FORWARD
      *----------------------------------------------------------------*
       PROCESS-PAGE-FORWARD.

           MOVE SPACES TO LK-INPUT-PARMS
           MOVE ZEROS TO LK-OUT-RETURN-CODE
           MOVE SPACES TO LK-OUT-MESSAGE
           MOVE ZEROS TO LK-OUT-RECORDS-COUNT
           MOVE SPACES TO LK-OUT-NEXT-PAGE-FLG
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 10
               MOVE SPACES TO LK-OUT-TRAN-ID(WS-IDX)
               MOVE ZEROS TO LK-OUT-TRAN-AMT(WS-IDX)
               MOVE SPACES TO LK-OUT-TRAN-DESC(WS-IDX)
               MOVE SPACES TO LK-OUT-TRAN-ORIG-TS(WS-IDX)
           END-PERFORM

           IF EIBAID = DFHENTER OR CDEMO-CT00-PAGE-NUM = 0
               SET OP-READ TO TRUE
               IF TRNIDINI OF COTRN0AI = SPACES OR LOW-VALUES
                   MOVE LOW-VALUES TO LK-IN-TRAN-ID
               ELSE
                   MOVE TRNIDINI OF COTRN0AI TO LK-IN-TRAN-ID
               END-IF
           ELSE
               SET OP-LIST-FORWARD TO TRUE
               MOVE CDEMO-CT00-TRNID-LAST TO LK-IN-TRAN-ID
           END-IF

           MOVE 10 TO LK-IN-MAX-RECORDS

           PERFORM CALL-RPC-PROGRAM

           IF RC-SUCCESS OR RC-NOT-FOUND
               IF LK-OUT-RECORDS-COUNT > 0
                   PERFORM POPULATE-SCREEN-FROM-RPC
                   ADD 1 TO CDEMO-CT00-PAGE-NUM
                   IF LK-NEXT-PAGE-YES
                       SET NEXT-PAGE-YES TO TRUE
                   ELSE
                       SET NEXT-PAGE-NO TO TRUE
                   END-IF
               ELSE
                   SET NEXT-PAGE-NO TO TRUE
               END-IF
               MOVE LK-OUT-MESSAGE TO WS-MESSAGE
           ELSE
               MOVE 'Y' TO WS-ERR-FLG
               MOVE LK-OUT-MESSAGE TO WS-MESSAGE
           END-IF

           MOVE CDEMO-CT00-PAGE-NUM TO PAGENUMI OF COTRN0AI
           MOVE SPACE TO TRNIDINO OF COTRN0AO
           PERFORM SEND-TRNLST-SCREEN.

      *----------------------------------------------------------------*
      *                      PROCESS-PAGE-BACKWARD
      *----------------------------------------------------------------*
       PROCESS-PAGE-BACKWARD.

           MOVE SPACES TO LK-INPUT-PARMS
           MOVE ZEROS TO LK-OUT-RETURN-CODE
           MOVE SPACES TO LK-OUT-MESSAGE
           MOVE ZEROS TO LK-OUT-RECORDS-COUNT
           MOVE SPACES TO LK-OUT-NEXT-PAGE-FLG
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 10
               MOVE SPACES TO LK-OUT-TRAN-ID(WS-IDX)
               MOVE ZEROS TO LK-OUT-TRAN-AMT(WS-IDX)
               MOVE SPACES TO LK-OUT-TRAN-DESC(WS-IDX)
               MOVE SPACES TO LK-OUT-TRAN-ORIG-TS(WS-IDX)
           END-PERFORM

           SET OP-LIST-BACKWARD TO TRUE
           MOVE CDEMO-CT00-TRNID-FIRST TO LK-IN-TRAN-ID
           MOVE 10 TO LK-IN-MAX-RECORDS

           PERFORM CALL-RPC-PROGRAM

           IF RC-SUCCESS OR RC-NOT-FOUND
               IF LK-OUT-RECORDS-COUNT > 0
                   PERFORM POPULATE-SCREEN-FROM-RPC
                   IF CDEMO-CT00-PAGE-NUM > 1
                       SUBTRACT 1 FROM CDEMO-CT00-PAGE-NUM
                   END-IF
               END-IF
               MOVE LK-OUT-MESSAGE TO WS-MESSAGE
           ELSE
               MOVE 'Y' TO WS-ERR-FLG
               MOVE LK-OUT-MESSAGE TO WS-MESSAGE
           END-IF

           MOVE CDEMO-CT00-PAGE-NUM TO PAGENUMI OF COTRN0AI
           PERFORM SEND-TRNLST-SCREEN.

      *----------------------------------------------------------------*
      *                      CALL-RPC-PROGRAM
      *----------------------------------------------------------------*
       CALL-RPC-PROGRAM.

           EXEC CICS LINK
                PROGRAM(WS-RPC-PROGRAM)
                COMMAREA(WS-RPC-COMMAREA)
                LENGTH(LENGTH OF WS-RPC-COMMAREA)
                RESP(WS-RESP-CD)
                RESP2(WS-REAS-CD)
           END-EXEC

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(PGMIDERR)
                   MOVE 'Y' TO WS-ERR-FLG
                   STRING 'RPC program not found'
                          DELIMITED BY SIZE
                     INTO WS-MESSAGE
               WHEN OTHER
                   MOVE 'Y' TO WS-ERR-FLG
                   STRING 'Error calling RPC program'
                          DELIMITED BY SIZE
                     INTO WS-MESSAGE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      POPULATE-SCREEN-FROM-RPC
      *----------------------------------------------------------------*
       POPULATE-SCREEN-FROM-RPC.

           PERFORM VARYING WS-IDX FROM 1 BY 1
                   UNTIL WS-IDX > 10
               PERFORM INITIALIZE-TRAN-DATA
           END-PERFORM

           PERFORM VARYING WS-IDX FROM 1 BY 1
                   UNTIL WS-IDX > LK-OUT-RECORDS-COUNT
                   OR WS-IDX > 10
               PERFORM POPULATE-TRAN-DATA
           END-PERFORM

           IF LK-OUT-RECORDS-COUNT > 0
               MOVE LK-OUT-TRAN-ID(1) TO CDEMO-CT00-TRNID-FIRST
               IF LK-OUT-RECORDS-COUNT >= 10
                   MOVE LK-OUT-TRAN-ID(10) TO CDEMO-CT00-TRNID-LAST
               ELSE
                   MOVE LK-OUT-TRAN-ID(LK-OUT-RECORDS-COUNT) TO
                        CDEMO-CT00-TRNID-LAST
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      *                      POPULATE-TRAN-DATA
      *----------------------------------------------------------------*
       POPULATE-TRAN-DATA.

           MOVE LK-OUT-TRAN-AMT(WS-IDX) TO WS-TRAN-AMT
           MOVE LK-OUT-TRAN-ORIG-TS(WS-IDX) TO WS-TIMESTAMP
           MOVE WS-TIMESTAMP-DT-YYYY(3:2) TO WS-CURDATE-YY
           MOVE WS-TIMESTAMP-DT-MM TO WS-CURDATE-MM
           MOVE WS-TIMESTAMP-DT-DD TO WS-CURDATE-DD
           MOVE WS-CURDATE-MM-DD-YY TO WS-TRAN-DATE

           EVALUATE WS-IDX
               WHEN 1
                   MOVE LK-OUT-TRAN-ID(WS-IDX) TO TRNID01I OF COTRN0AI
                   MOVE WS-TRAN-DATE TO TDATE01I OF COTRN0AI
                   MOVE LK-OUT-TRAN-DESC(WS-IDX) TO TDESC01I OF
                        COTRN0AI
                   MOVE WS-TRAN-AMT TO TAMT001I OF COTRN0AI
               WHEN 2
                   MOVE LK-OUT-TRAN-ID(WS-IDX) TO TRNID02I OF COTRN0AI
                   MOVE WS-TRAN-DATE TO TDATE02I OF COTRN0AI
                   MOVE LK-OUT-TRAN-DESC(WS-IDX) TO TDESC02I OF
                        COTRN0AI
                   MOVE WS-TRAN-AMT TO TAMT002I OF COTRN0AI
               WHEN 3
                   MOVE LK-OUT-TRAN-ID(WS-IDX) TO TRNID03I OF COTRN0AI
                   MOVE WS-TRAN-DATE TO TDATE03I OF COTRN0AI
                   MOVE LK-OUT-TRAN-DESC(WS-IDX) TO TDESC03I OF
                        COTRN0AI
                   MOVE WS-TRAN-AMT TO TAMT003I OF COTRN0AI
               WHEN 4
                   MOVE LK-OUT-TRAN-ID(WS-IDX) TO TRNID04I OF COTRN0AI
                   MOVE WS-TRAN-DATE TO TDATE04I OF COTRN0AI
                   MOVE LK-OUT-TRAN-DESC(WS-IDX) TO TDESC04I OF
                        COTRN0AI
                   MOVE WS-TRAN-AMT TO TAMT004I OF COTRN0AI
               WHEN 5
                   MOVE LK-OUT-TRAN-ID(WS-IDX) TO TRNID05I OF COTRN0AI
                   MOVE WS-TRAN-DATE TO TDATE05I OF COTRN0AI
                   MOVE LK-OUT-TRAN-DESC(WS-IDX) TO TDESC05I OF
                        COTRN0AI
                   MOVE WS-TRAN-AMT TO TAMT005I OF COTRN0AI
               WHEN 6
                   MOVE LK-OUT-TRAN-ID(WS-IDX) TO TRNID06I OF COTRN0AI
                   MOVE WS-TRAN-DATE TO TDATE06I OF COTRN0AI
                   MOVE LK-OUT-TRAN-DESC(WS-IDX) TO TDESC06I OF
                        COTRN0AI
                   MOVE WS-TRAN-AMT TO TAMT006I OF COTRN0AI
               WHEN 7
                   MOVE LK-OUT-TRAN-ID(WS-IDX) TO TRNID07I OF COTRN0AI
                   MOVE WS-TRAN-DATE TO TDATE07I OF COTRN0AI
                   MOVE LK-OUT-TRAN-DESC(WS-IDX) TO TDESC07I OF
                        COTRN0AI
                   MOVE WS-TRAN-AMT TO TAMT007I OF COTRN0AI
               WHEN 8
                   MOVE LK-OUT-TRAN-ID(WS-IDX) TO TRNID08I OF COTRN0AI
                   MOVE WS-TRAN-DATE TO TDATE08I OF COTRN0AI
                   MOVE LK-OUT-TRAN-DESC(WS-IDX) TO TDESC08I OF
                        COTRN0AI
                   MOVE WS-TRAN-AMT TO TAMT008I OF COTRN0AI
               WHEN 9
                   MOVE LK-OUT-TRAN-ID(WS-IDX) TO TRNID09I OF COTRN0AI
                   MOVE WS-TRAN-DATE TO TDATE09I OF COTRN0AI
                   MOVE LK-OUT-TRAN-DESC(WS-IDX) TO TDESC09I OF
                        COTRN0AI
                   MOVE WS-TRAN-AMT TO TAMT009I OF COTRN0AI
               WHEN 10
                   MOVE LK-OUT-TRAN-ID(WS-IDX) TO TRNID10I OF COTRN0AI
                   MOVE WS-TRAN-DATE TO TDATE10I OF COTRN0AI
                   MOVE LK-OUT-TRAN-DESC(WS-IDX) TO TDESC10I OF
                        COTRN0AI
                   MOVE WS-TRAN-AMT TO TAMT010I OF COTRN0AI
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      INITIALIZE-TRAN-DATA
      *----------------------------------------------------------------*
       INITIALIZE-TRAN-DATA.

           EVALUATE WS-IDX
               WHEN 1
                   MOVE SPACES TO TRNID01I OF COTRN0AI
                   MOVE SPACES TO TDATE01I OF COTRN0AI
                   MOVE SPACES TO TDESC01I OF COTRN0AI
                   MOVE SPACES TO TAMT001I OF COTRN0AI
               WHEN 2
                   MOVE SPACES TO TRNID02I OF COTRN0AI
                   MOVE SPACES TO TDATE02I OF COTRN0AI
                   MOVE SPACES TO TDESC02I OF COTRN0AI
                   MOVE SPACES TO TAMT002I OF COTRN0AI
               WHEN 3
                   MOVE SPACES TO TRNID03I OF COTRN0AI
                   MOVE SPACES TO TDATE03I OF COTRN0AI
                   MOVE SPACES TO TDESC03I OF COTRN0AI
                   MOVE SPACES TO TAMT003I OF COTRN0AI
               WHEN 4
                   MOVE SPACES TO TRNID04I OF COTRN0AI
                   MOVE SPACES TO TDATE04I OF COTRN0AI
                   MOVE SPACES TO TDESC04I OF COTRN0AI
                   MOVE SPACES TO TAMT004I OF COTRN0AI
               WHEN 5
                   MOVE SPACES TO TRNID05I OF COTRN0AI
                   MOVE SPACES TO TDATE05I OF COTRN0AI
                   MOVE SPACES TO TDESC05I OF COTRN0AI
                   MOVE SPACES TO TAMT005I OF COTRN0AI
               WHEN 6
                   MOVE SPACES TO TRNID06I OF COTRN0AI
                   MOVE SPACES TO TDATE06I OF COTRN0AI
                   MOVE SPACES TO TDESC06I OF COTRN0AI
                   MOVE SPACES TO TAMT006I OF COTRN0AI
               WHEN 7
                   MOVE SPACES TO TRNID07I OF COTRN0AI
                   MOVE SPACES TO TDATE07I OF COTRN0AI
                   MOVE SPACES TO TDESC07I OF COTRN0AI
                   MOVE SPACES TO TAMT007I OF COTRN0AI
               WHEN 8
                   MOVE SPACES TO TRNID08I OF COTRN0AI
                   MOVE SPACES TO TDATE08I OF COTRN0AI
                   MOVE SPACES TO TDESC08I OF COTRN0AI
                   MOVE SPACES TO TAMT008I OF COTRN0AI
               WHEN 9
                   MOVE SPACES TO TRNID09I OF COTRN0AI
                   MOVE SPACES TO TDATE09I OF COTRN0AI
                   MOVE SPACES TO TDESC09I OF COTRN0AI
                   MOVE SPACES TO TAMT009I OF COTRN0AI
               WHEN 10
                   MOVE SPACES TO TRNID10I OF COTRN0AI
                   MOVE SPACES TO TDATE10I OF COTRN0AI
                   MOVE SPACES TO TDESC10I OF COTRN0AI
                   MOVE SPACES TO TAMT010I OF COTRN0AI
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

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
      *                      SEND-TRNLST-SCREEN
      *----------------------------------------------------------------*
       SEND-TRNLST-SCREEN.

           PERFORM POPULATE-HEADER-INFO

           MOVE WS-MESSAGE TO ERRMSGO OF COTRN0AO

           IF SEND-ERASE-YES
               EXEC CICS SEND
                         MAP('COTRN0A')
                         MAPSET('COTRN00')
                         FROM(COTRN0AO)
                         ERASE
                         CURSOR
               END-EXEC
           ELSE
               EXEC CICS SEND
                         MAP('COTRN0A')
                         MAPSET('COTRN00')
                         FROM(COTRN0AO)
                         CURSOR
               END-EXEC
           END-IF.

      *----------------------------------------------------------------*
      *                      RECEIVE-TRNLST-SCREEN
      *----------------------------------------------------------------*
       RECEIVE-TRNLST-SCREEN.

           EXEC CICS RECEIVE
                     MAP('COTRN0A')
                     MAPSET('COTRN00')
                     INTO(COTRN0AI)
                     RESP(WS-RESP-CD)
                     RESP2(WS-REAS-CD)
           END-EXEC.

      *----------------------------------------------------------------*
      *                      POPULATE-HEADER-INFO
      *----------------------------------------------------------------*
       POPULATE-HEADER-INFO.

           MOVE FUNCTION CURRENT-DATE  TO WS-CURDATE-DATA

           MOVE CCDA-TITLE01           TO TITLE01O OF COTRN0AO
           MOVE CCDA-TITLE02           TO TITLE02O OF COTRN0AO
           MOVE WS-TRANID              TO TRNNAMEO OF COTRN0AO
           MOVE WS-PGMNAME             TO PGMNAMEO OF COTRN0AO

           MOVE WS-CURDATE-MONTH       TO WS-CURDATE-MM
           MOVE WS-CURDATE-DAY         TO WS-CURDATE-DD
           MOVE WS-CURDATE-YEAR(3:2)   TO WS-CURDATE-YY

           MOVE WS-CURDATE-MM-DD-YY    TO CURDATEO OF COTRN0AO

           MOVE WS-CURTIME-HOURS       TO WS-CURTIME-HH
           MOVE WS-CURTIME-MINUTE      TO WS-CURTIME-MM
           MOVE WS-CURTIME-SECOND      TO WS-CURTIME-SS

           MOVE WS-CURTIME-HH-MM-SS    TO CURTIMEO OF COTRN0AO.
