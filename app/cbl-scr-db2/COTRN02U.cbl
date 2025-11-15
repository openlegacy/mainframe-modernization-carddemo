000001******************************************************************
      * Program:     COTRN02U                                          *
      * Function:    Transaction add screen interface                  *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COTRN02U.
       AUTHOR.     AWS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
      *----------------------------------------------------------------*
      *                     WORKING STORAGE SECTION
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

       01 WS-VARIABLES.
         05 WS-PGMNAME                 PIC X(08) VALUE 'COTRN02U'.
         05 WS-TRANID                  PIC X(04) VALUE 'AASG'.
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
             10 WS-RPC-ACCT-ID          PIC X(11).
             10 WS-RPC-CARD-NUM         PIC X(16).
             10 WS-RPC-TRAN-TYPE-CD     PIC X(02).
             10 WS-RPC-TRAN-CAT-CD      PIC X(04).
             10 WS-RPC-TRAN-SOURCE      PIC X(10).
             10 WS-RPC-TRAN-AMT         PIC X(12).
             10 WS-RPC-TRAN-DESC        PIC X(100).
             10 WS-RPC-TRAN-ORIG-DT     PIC X(10).
             10 WS-RPC-TRAN-PROC-DT     PIC X(10).
             10 WS-RPC-MERCH-ID         PIC X(09).
             10 WS-RPC-MERCH-NAME       PIC X(50).
             10 WS-RPC-MERCH-CITY       PIC X(50).
             10 WS-RPC-MERCH-ZIP        PIC X(10).
             10 WS-RPC-CONFIRM          PIC X(01).
          05 WS-RPC-OUTPUT-PARMS.
             10 WS-RPC-RESP-CODE        PIC S9(04) COMP.
             10 WS-RPC-RESP-MSG         PIC X(80).
             10 WS-RPC-OUT-TRAN-ID      PIC X(16).

       COPY COCOM01Y.
          05 CDEMO-CT02-INFO.
             10 CDEMO-CT02-TRNID-FIRST     PIC X(16).
             10 CDEMO-CT02-TRNID-LAST      PIC X(16).
             10 CDEMO-CT02-PAGE-NUM        PIC 9(08).
             10 CDEMO-CT02-NEXT-PAGE-FLG   PIC X(01) VALUE 'N'.
                88 NEXT-PAGE-YES                     VALUE 'Y'.
                88 NEXT-PAGE-NO                      VALUE 'N'.
             10 CDEMO-CT02-TRN-SEL-FLG     PIC X(01).
             10 CDEMO-CT02-TRN-SELECTED    PIC X(16).

       COPY COTRN02.

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
         05  LK-COMMAREA                           PIC X(01)
             OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.

      *----------------------------------------------------------------*
      *                       PROCEDURE DIVISION
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       MAIN-PARA.

           SET ERR-FLG-OFF TO TRUE

           MOVE SPACES TO WS-MESSAGE
                          ERRMSGO OF COTRN2AO

           IF EIBCALEN = 0
               MOVE 'COSGN00S' TO CDEMO-TO-PROGRAM
               PERFORM RETURN-TO-PREV-SCREEN
           ELSE
               MOVE DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA
               IF NOT CDEMO-PGM-REENTER
                   SET CDEMO-PGM-REENTER    TO TRUE
                   MOVE LOW-VALUES          TO COTRN2AO
                   MOVE -1       TO ACTIDINL OF COTRN2AI
                   IF CDEMO-CT02-TRN-SELECTED NOT =
                                              SPACES AND LOW-VALUES
                       MOVE CDEMO-CT02-TRN-SELECTED TO
                            CARDNINI OF COTRN2AI
                       PERFORM CALL-RPC-PROGRAM
                   END-IF
                   PERFORM SEND-TRNADD-SCREEN
               ELSE
                   PERFORM RECEIVE-TRNADD-SCREEN
                   EVALUATE EIBAID
                       WHEN DFHENTER
                           PERFORM CALL-RPC-PROGRAM
                       WHEN DFHPF3
                           IF CDEMO-FROM-PROGRAM = SPACES OR LOW-VALUES
                               MOVE 'COMEN01S' TO CDEMO-TO-PROGRAM
                           ELSE
                               MOVE CDEMO-FROM-PROGRAM TO
                               CDEMO-TO-PROGRAM
                           END-IF
                           PERFORM RETURN-TO-PREV-SCREEN
                       WHEN DFHPF4
                           PERFORM CLEAR-CURRENT-SCREEN
                       WHEN DFHPF5
                           PERFORM COPY-LAST-TRAN-DATA
                       WHEN OTHER
                           MOVE 'Y'                       TO WS-ERR-FLG
                           MOVE CCDA-MSG-INVALID-KEY      TO WS-MESSAGE
                           PERFORM SEND-TRNADD-SCREEN
                   END-EVALUATE
               END-IF
           END-IF

           EXEC CICS RETURN
                     TRANSID (WS-TRANID)
                     COMMAREA (CARDDEMO-COMMAREA)
           END-EXEC.

      *----------------------------------------------------------------*
      *                      CALL-RPC-PROGRAM
      *----------------------------------------------------------------*
       CALL-RPC-PROGRAM.

      *    Prepare input parameters for RPC call
           MOVE ACTIDINI OF COTRN2AI TO WS-RPC-ACCT-ID
           MOVE CARDNINI OF COTRN2AI TO WS-RPC-CARD-NUM
           MOVE TTYPCDI  OF COTRN2AI TO WS-RPC-TRAN-TYPE-CD
           MOVE TCATCDI  OF COTRN2AI TO WS-RPC-TRAN-CAT-CD
           MOVE TRNSRCI  OF COTRN2AI TO WS-RPC-TRAN-SOURCE
           MOVE TRNAMTI  OF COTRN2AI TO WS-RPC-TRAN-AMT
           MOVE TDESCI   OF COTRN2AI TO WS-RPC-TRAN-DESC
           MOVE TORIGDTI OF COTRN2AI TO WS-RPC-TRAN-ORIG-DT
           MOVE TPROCDTI OF COTRN2AI TO WS-RPC-TRAN-PROC-DT
           MOVE MIDI     OF COTRN2AI TO WS-RPC-MERCH-ID
           MOVE MNAMEI   OF COTRN2AI TO WS-RPC-MERCH-NAME
           MOVE MCITYI   OF COTRN2AI TO WS-RPC-MERCH-CITY
           MOVE MZIPI    OF COTRN2AI TO WS-RPC-MERCH-ZIP
           MOVE CONFIRMI OF COTRN2AI TO WS-RPC-CONFIRM

      *    Initialize output parameters
           MOVE ZEROS TO WS-RPC-RESP-CODE
           MOVE SPACES TO WS-RPC-RESP-MSG
           MOVE SPACES TO WS-RPC-OUT-TRAN-ID

      *    Call the RPC program
           EXEC CICS LINK
                PROGRAM('COTRN02A')
                COMMAREA(WS-RPC-COMMAREA)
                LENGTH(LENGTH OF WS-RPC-COMMAREA)
                RESP(WS-RESP-CD)
                RESP2(WS-REAS-CD)
           END-EXEC

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   MOVE WS-RPC-ACCT-ID TO ACTIDINI OF COTRN2AI
                   MOVE WS-RPC-CARD-NUM TO CARDNINI OF COTRN2AI
                   PERFORM PROCESS-RPC-RESPONSE
               WHEN OTHER
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE 'Error calling transaction add service...' TO
                                   WS-MESSAGE
                   MOVE -1 TO ACTIDINL OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
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
                   MOVE DFHGREEN TO ERRMSGC OF COTRN2AO
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   PERFORM SEND-TRNADD-SCREEN
               WHEN 1
      *            Empty Account ID
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO ACTIDINL OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
               WHEN 2
      *            Invalid Account ID
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO ACTIDINL OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
               WHEN 3
      *            Account ID Not Found
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO ACTIDINL OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
               WHEN 4
      *            Empty Card Number
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO CARDNINL OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
               WHEN 5
      *            Invalid Card Number
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO CARDNINL OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
               WHEN 6
      *            Card Number Not Found
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO CARDNINL OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
               WHEN 7
      *            Empty Type CD
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO TTYPCDL OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
               WHEN 8
      *            Invalid Type CD
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO TTYPCDL OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
               WHEN 9
      *            Empty Category CD
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO TCATCDL OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
               WHEN 10
      *            Invalid Category CD
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO TCATCDL OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
               WHEN 11
      *            Empty Source
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO TRNSRCL OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
               WHEN 12
      *            Empty Description
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO TDESCL OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
               WHEN 13
      *            Empty Amount
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO TRNAMTL OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
               WHEN 14
      *            Invalid Amount
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO TRNAMTL OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
               WHEN 15
      *            Empty Orig Date
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO TORIGDTL OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
               WHEN 16
      *            Invalid Orig Date
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO TORIGDTL OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
               WHEN 17
      *            Empty Proc Date
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO TPROCDTL OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
               WHEN 18
      *            Invalid Proc Date
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO TPROCDTL OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
               WHEN 19
      *            Empty Merchant ID
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO MIDL OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
               WHEN 20
      *            Invalid Merchant ID
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO MIDL OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
               WHEN 21
      *            Empty Merchant Name
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO MNAMEL OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
               WHEN 22
      *            Empty Merchant City
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO MCITYL OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
               WHEN 23
      *            Empty Merchant Zip
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO MZIPL OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
               WHEN 24
      *            Empty Confirm
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO CONFIRML OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
               WHEN 25
      *            Invalid Confirm
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO CONFIRML OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
               WHEN 26
      *            Write Error
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE WS-RPC-RESP-MSG TO WS-MESSAGE
                   MOVE -1 TO ACTIDINL OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
               WHEN OTHER
                   MOVE 'Y' TO WS-ERR-FLG
                   MOVE 'Unknown error occurred...' TO WS-MESSAGE
                   MOVE -1 TO ACTIDINL OF COTRN2AI
                   PERFORM SEND-TRNADD-SCREEN
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      COPY-LAST-TRAN-DATA
      *----------------------------------------------------------------*
       COPY-LAST-TRAN-DATA.

      *    This PF5 function is not converted to RPC
      *    It reads the last transaction directly
           MOVE 'PF5 - Copy Last Transaction not available' TO
             WS-MESSAGE
           MOVE 'Y' TO WS-ERR-FLG
           PERFORM SEND-TRNADD-SCREEN.

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
      *                      SEND-TRNADD-SCREEN
      *----------------------------------------------------------------*
       SEND-TRNADD-SCREEN.

           PERFORM POPULATE-HEADER-INFO

           MOVE WS-MESSAGE TO ERRMSGO OF COTRN2AO

           EXEC CICS SEND
                     MAP('COTRN2A')
                     MAPSET('COTRN02')
                     FROM(COTRN2AO)
                     ERASE
                     CURSOR
           END-EXEC.

      *----------------------------------------------------------------*
      *                      RECEIVE-TRNADD-SCREEN
      *----------------------------------------------------------------*
       RECEIVE-TRNADD-SCREEN.

           EXEC CICS RECEIVE
                     MAP('COTRN2A')
                     MAPSET('COTRN02')
                     INTO(COTRN2AI)
                     RESP(WS-RESP-CD)
                     RESP2(WS-REAS-CD)
           END-EXEC.

      *----------------------------------------------------------------*
      *                      POPULATE-HEADER-INFO
      *----------------------------------------------------------------*
       POPULATE-HEADER-INFO.

           MOVE FUNCTION CURRENT-DATE  TO WS-CURDATE-DATA

           MOVE CCDA-TITLE01           TO TITLE01O OF COTRN2AO
           MOVE CCDA-TITLE02           TO TITLE02O OF COTRN2AO
           MOVE WS-TRANID              TO TRNNAMEO OF COTRN2AO
           MOVE WS-PGMNAME             TO PGMNAMEO OF COTRN2AO

           MOVE WS-CURDATE-MONTH       TO WS-CURDATE-MM
           MOVE WS-CURDATE-DAY         TO WS-CURDATE-DD
           MOVE WS-CURDATE-YEAR(3:2)   TO WS-CURDATE-YY

           MOVE WS-CURDATE-MM-DD-YY    TO CURDATEO OF COTRN2AO

           MOVE WS-CURTIME-HOURS       TO WS-CURTIME-HH
           MOVE WS-CURTIME-MINUTE      TO WS-CURTIME-MM
           MOVE WS-CURTIME-SECOND      TO WS-CURTIME-SS

           MOVE WS-CURTIME-HH-MM-SS    TO CURTIMEO OF COTRN2AO.

      *----------------------------------------------------------------*
      *                    CLEAR-CURRENT-SCREEN
      *----------------------------------------------------------------*
       CLEAR-CURRENT-SCREEN.

           PERFORM INITIALIZE-ALL-FIELDS.
           PERFORM SEND-TRNADD-SCREEN.

      *----------------------------------------------------------------*
      *                    INITIALIZE-ALL-FIELDS
      *----------------------------------------------------------------*
       INITIALIZE-ALL-FIELDS.

           MOVE -1              TO ACTIDINL OF COTRN2AI
           MOVE SPACES          TO ACTIDINI OF COTRN2AI
                                   CARDNINI OF COTRN2AI
                                   TTYPCDI  OF COTRN2AI
                                   TCATCDI  OF COTRN2AI
                                   TRNSRCI  OF COTRN2AI
                                   TRNAMTI  OF COTRN2AI
                                   TDESCI   OF COTRN2AI
                                   TORIGDTI OF COTRN2AI
                                   TPROCDTI OF COTRN2AI
                                   MIDI     OF COTRN2AI
                                   MNAMEI   OF COTRN2AI
                                   MCITYI   OF COTRN2AI
                                   MZIPI    OF COTRN2AI
                                   CONFIRMI OF COTRN2AI
                                   WS-MESSAGE.

      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:12:34 CDT
      *
