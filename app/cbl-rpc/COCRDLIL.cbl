       IDENTIFICATION DIVISION.
       PROGRAM-ID.
              COCRDLIL.
       DATE-WRITTEN.
           April 2023.
       DATE-COMPILED.
           Today.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

       01 WS-DEBUG-MESSAGE           PIC X(79).
       01 WS-RESP-CD-DISP            PIC 9(09) VALUE ZEROS.
       01 WS-REAS-CD-DISP            PIC 9(09) VALUE ZEROS.
       01 WS-DISPLAY-LINE            PIC X(79) VALUE SPACES.
       01 WS-IN-OPERATION            PIC X(50) VALUE SPACES.
       01  WS-MISC-STORAGE.


      ******************************************************************
      * General CICS related
      ******************************************************************

         05 WS-CICS-PROCESSNG-VARS.
            07 WS-RESP-CD                          PIC S9(09) COMP
                                                   VALUE ZEROS.
            07 WS-REAS-CD                          PIC S9(09) COMP
                                                   VALUE ZEROS.
            07 WS-TRANID                           PIC X(4)
                                                   VALUE 'ALCP'.
      ******************************************************************
      * Input edits
      ******************************************************************
         05 WS-INPUT-FLAG                          PIC X(1).
           88  INPUT-OK                            VALUES '0'
                                                          ' '
                                                   LOW-VALUES.
           88  INPUT-ERROR                         VALUE '1'.
         05  WS-EDIT-ACCT-FLAG                     PIC X(1).
           88  FLG-ACCTFILTER-NOT-OK               VALUE '0'.
           88  FLG-ACCTFILTER-ISVALID              VALUE '1'.
           88  FLG-ACCTFILTER-BLANK                VALUE ' '.
         05  WS-EDIT-CARD-FLAG                     PIC X(1).
           88  FLG-CARDFILTER-NOT-OK               VALUE '0'.
           88  FLG-CARDFILTER-ISVALID              VALUE '1'.
           88  FLG-CARDFILTER-BLANK                VALUE ' '.

      ******************************************************************
      * File and data Handling
      ******************************************************************
         05 WS-FILE-HANDLING-VARS.
            10  WS-CARD-RID.
                20  WS-CARD-RID-CARDNUM            PIC X(16).
                20  WS-CARD-RID-ACCT-ID            PIC 9(11).
                20  WS-CARD-RID-ACCT-ID-X          REDEFINES
                    WS-CARD-RID-ACCT-ID            PIC X(11).

         05  WS-SCRN-COUNTER                     PIC S9(4) COMP VALUE 0.
         05  WS-IDX                              PIC S9(4) COMP VALUE 0.
         05  WS-OUT-IDX                          PIC S9(4) COMP VALUE 0.

         05  WS-FILTER-RECORD-FLAG                 PIC X(1).
           88  WS-EXCLUDE-THIS-RECORD              VALUE '0'.
           88  WS-DONOT-EXCLUDE-THIS-RECORD        VALUE '1'.
         05  WS-RECORDS-TO-PROCESS-FLAG            PIC X(1).
           88  read-LOOP-EXIT                      VALUE '0'.
           88  MORE-RECORDS-TO-READ                VALUE '1'.
         05  WS-CARD-EOF                          PIC X(1) VALUE 'N'.
           88  CARD-EOF                            VALUE 'Y'.
           88  CARD-NOT-EOF                        VALUE 'N'.
         05  WS-ERR-FLG                           PIC X(1) VALUE 'N'.
           88  ERR-FLG-ON                          VALUE 'Y'.
           88  ERR-FLG-OFF                         VALUE 'N'.

      ******************************************************************
      * BROWSE STATUS CONTROL - PREVENTS DOUBLE STARTBR
      ******************************************************************
         05  WS-BROWSE-STATUS                     PIC X(1) VALUE 'N'.
           88  BROWSE-ACTIVE                       VALUE 'Y'.
           88  BROWSE-NOT-ACTIVE                   VALUE 'N'.

         05  WS-FILE-ERROR-MESSAGE.
           10  FILLER                              PIC X(12)
                                                   VALUE 'File Error:'.
           10  ERROR-OPNAME                        PIC X(8)
                                                   VALUE SPACES.
           10  FILLER                              PIC X(4)
                                                   VALUE ' on '.
           10  ERROR-FILE                          PIC X(9)
                                                   VALUE SPACES.
           10  FILLER                              PIC X(15)
                                                   VALUE
                                                   ' returned RESP '.
           10  ERROR-RESP                          PIC X(10)
                                                   VALUE SPACES.
           10  FILLER                              PIC X(7)
                                                   VALUE ',RESP2 '.
           10  ERROR-RESP2                         PIC X(10)
                                                   VALUE SPACES.
          10  FILLER                               PIC X(5).

      ******************************************************************
      * Literals and Constants
      ******************************************************************
       01 WS-CONSTANTS.
         05  WS-MAX-RECORDS                        PIC S9(4) COMP
                                                   VALUE 7.
         05  LIT-THISPGM                           PIC X(8)
                                                   VALUE 'COCRDLIL'.
         05  LIT-CARD-FILE                         PIC X(8)
                                                   VALUE 'CARDDAT '.

      *----------------------------------------------------------------*
      * Temporary storage for backward records
      *----------------------------------------------------------------*
       01 WS-TEMP-CARD-RECORDS.
          05 WS-TEMP-CARD-COUNT            PIC 9(02) VALUE 0.
          05 WS-TEMP-CARD-DATA OCCURS 7 TIMES.
             10 WS-TEMP-CARD-NUM           PIC X(16).
             10 WS-TEMP-CARD-ACCT-ID       PIC 9(11).
             10 WS-TEMP-CARD-CVV-CD        PIC 9(03).
             10 WS-TEMP-CARD-EMBOSSED-NM   PIC X(50).
             10 WS-TEMP-CARD-EXPIR-DATE    PIC X(10).
             10 WS-TEMP-CARD-ACTIVE-ST     PIC X(01).

      ******************************************************************
      *  Card Record Layout
      ******************************************************************
       COPY CVACT02Y.

       LINKAGE SECTION.
      ******************************************************************
      * DFHCOMMAREA
      ******************************************************************
       01  DFHCOMMAREA.
      ******************************************************************
      * Input Parameters
      ******************************************************************
           05  LK-INPUT-PARAMS.
               10  LK-FILTER-ACCT-ID                   PIC X(11).
               10  LK-FILTER-CARD-NUM                  PIC X(16).
               10  LK-START-KEY                        PIC X(16).
               10  LK-MAX-RECORDS                      PIC S9(4) COMP.
               10  LK-PAGE-DIR                         PIC X(1).

      ************************************************************i******
      * Output Parameters
      ******************************************************************
           05  LK-OUTPUT-PARAMS.
               10  LK-RETURN-CODE                      PIC S9(4) COMP.
                  88  LK-RC-OK                         VALUE 0.
                  88  LK-RC-FILTER-ERROR               VALUE 1.
                  88  LK-RC-DB-ERROR                   VALUE 2.
                  88  LK-RC-NO-RECORDS                 VALUE 3.
               10  LK-RETURN-MSG                       PIC X(80).
               10  LK-RECORDS-COUNT                    PIC S9(4) COMP.
               10  LK-CARDS-DATA.
                   15  LK-CARDS-ARRAY                  OCCURS 1 TO 7
                                                       DEPENDING ON
                                                       LK-RECORDS-COUNT.
                       20  LK-CARD-NUM                 PIC X(16).
                       20  LK-CARD-ACCT-ID             PIC 9(11).
                       20  LK-CARD-CVV-CD              PIC 9(03).
                       20  LK-CARD-EMBOSSED-NAME       PIC X(50).
                       20  LK-CARD-EXPIRATION-DATE     PIC X(10).
                       20  LK-CARD-ACTIVE-STATUS       PIC X(01).

       PROCEDURE DIVISION.

       0000-MAIN.
           INITIALIZE WS-MISC-STORAGE
           SET BROWSE-NOT-ACTIVE TO TRUE

           MOVE 0 TO LK-RETURN-CODE
           MOVE SPACES TO LK-RETURN-MSG
           MOVE 0 TO LK-RECORDS-COUNT

           IF LK-MAX-RECORDS = 0
              MOVE WS-MAX-RECORDS TO LK-MAX-RECORDS
           END-IF

      *****************************************************************
      * Validate input parameters                                     *
      *****************************************************************
           PERFORM 1000-VALIDATE-INPUT
              THRU 1000-VALIDATE-INPUT-EXIT

           IF INPUT-ERROR
              SET LK-RC-FILTER-ERROR TO TRUE
              GO TO 0000-MAIN-EXIT
           END-IF

      *****************************************************************
      * Read cards from database                                      *
      *****************************************************************
           IF LK-PAGE-DIR = 'F'
               PERFORM 9000-READ-FORWARD
                  THRU 9000-READ-FORWARD-EXIT
           ELSE
               IF LK-PAGE-DIR = 'B'
                   PERFORM 9100-READ-BACKWARDS
                      THRU 9100-READ-BACKWARDS-EXIT
               ELSE
                   PERFORM PROCESS-LIST
               END-IF
           END-IF

           .
       0000-MAIN-EXIT.
      * Ensure browse is closed before exit
           IF BROWSE-ACTIVE
              PERFORM ENDBR-CARD-FILE
           END-IF
           EXIT.

           EXEC CICS RETURN
           END-EXEC.

       1000-VALIDATE-INPUT.

      *****************************************************************
      * Validate input parameters, especially filter values           *
      *****************************************************************
           SET INPUT-OK TO TRUE

      *****************************************************************
      * Validate Account ID filter if provided                        *
      *****************************************************************
           SET FLG-ACCTFILTER-BLANK TO TRUE

           IF LK-FILTER-ACCT-ID   EQUAL LOW-VALUES
           OR LK-FILTER-ACCT-ID   EQUAL SPACES
              SET FLG-ACCTFILTER-BLANK  TO TRUE
           ELSE
              IF LK-FILTER-ACCT-ID  IS NOT NUMERIC
                 SET INPUT-ERROR TO TRUE
                 SET FLG-ACCTFILTER-NOT-OK TO TRUE
                 MOVE
                 'ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER'
                                 TO LK-RETURN-MSG
              ELSE
                 SET FLG-ACCTFILTER-ISVALID TO TRUE
              END-IF
           END-IF

      *****************************************************************
      * Validate Card Number filter if provided                       *
      *****************************************************************
           IF LK-FILTER-CARD-NUM   EQUAL LOW-VALUES
           OR LK-FILTER-CARD-NUM   EQUAL SPACES
              SET FLG-CARDFILTER-BLANK  TO TRUE
           ELSE
              IF LK-FILTER-CARD-NUM  IS NUMERIC
                 SET FLG-CARDFILTER-ISVALID TO TRUE
              ELSE
                 SET INPUT-ERROR TO TRUE
                 SET FLG-CARDFILTER-NOT-OK TO TRUE
                 IF LK-RETURN-MSG EQUAL SPACES
                    MOVE
                  'CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER'
                                    TO LK-RETURN-MSG
                 END-IF
              END-IF
           END-IF

      *****************************************************************
      * Validate max records parameter                                *
      *****************************************************************
           IF LK-MAX-RECORDS EQUAL 0
              MOVE WS-MAX-RECORDS TO LK-MAX-RECORDS
           ELSE
              IF LK-MAX-RECORDS > WS-MAX-RECORDS
                 MOVE WS-MAX-RECORDS TO LK-MAX-RECORDS
              END-IF
           END-IF
           .
       1000-VALIDATE-INPUT-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *                      PROCESS-LIST (Current Records)
      *----------------------------------------------------------------*
       PROCESS-LIST.
           MOVE 0 TO LK-RECORDS-COUNT
           SET ERR-FLG-OFF TO TRUE
           SET CARD-NOT-EOF TO TRUE

      * CLEAR ALL OUTPUT ARRAY POSITIONS FIRST
           PERFORM CLEAR-OUTPUT-ARRAY

      * If we have filters, always start from beginning
           IF FLG-ACCTFILTER-ISVALID OR FLG-CARDFILTER-ISVALID
               MOVE LOW-VALUES TO WS-CARD-RID-CARDNUM
           END-IF

      * Initialize browse if not already active
           PERFORM INIT-BROWSE-IF-NEEDED

           IF ERR-FLG-OFF
      * Read records and apply filters
               PERFORM UNTIL LK-RECORDS-COUNT >= LK-MAX-RECORDS OR
                             CARD-EOF OR ERR-FLG-ON
                   PERFORM READNEXT-CARD-FILE
                   IF CARD-NOT-EOF AND ERR-FLG-OFF
                       PERFORM 9500-FILTER-RECORDS
                          THRU 9500-FILTER-RECORDS-EXIT

                       IF WS-DONOT-EXCLUDE-THIS-RECORD
                           ADD 1 TO LK-RECORDS-COUNT
                           PERFORM POPULATE-CARD-OUTPUT
                       END-IF
                   END-IF
               END-PERFORM

               IF LK-RECORDS-COUNT = 0
                  SET LK-RC-NO-RECORDS TO TRUE
                  MOVE 'NO RECORDS FOUND FOR THIS SEARCH CONDITION.'
                               TO LK-RETURN-MSG
               END-IF
           END-IF.

       PROCESS-LIST-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *                      INIT-BROWSE-IF-NEEDED
      *----------------------------------------------------------------*
       INIT-BROWSE-IF-NEEDED.
           IF BROWSE-NOT-ACTIVE
      * If filters active, always start from beginning
               IF FLG-ACCTFILTER-ISVALID OR FLG-CARDFILTER-ISVALID
                   MOVE LOW-VALUES TO WS-CARD-RID-CARDNUM
               ELSE
      * Normal positioning logic when no filters
                   IF LK-START-KEY EQUAL SPACES
                      MOVE LOW-VALUES TO WS-CARD-RID-CARDNUM
                   ELSE
                      MOVE LOW-VALUES TO WS-CARD-RID
                      MOVE LK-START-KEY TO WS-CARD-RID-CARDNUM
                   END-IF
               END-IF

               EXEC CICS STARTBR
                    DATASET(LIT-CARD-FILE)
                    RIDFLD(WS-CARD-RID-CARDNUM)
                    KEYLENGTH(LENGTH OF WS-CARD-RID-CARDNUM)
                    GTEQ
                    RESP(WS-RESP-CD)
                    RESP2(WS-REAS-CD)
               END-EXEC

               EVALUATE WS-RESP-CD
                   WHEN DFHRESP(NORMAL)
                       SET BROWSE-ACTIVE TO TRUE

                   WHEN DFHRESP(NOTFND)
                       SET CARD-EOF TO TRUE
                       SET LK-RC-NO-RECORDS TO TRUE
                      MOVE 'NO RECORDS FOUND FOR THIS SEARCH CONDITION.'
                            TO LK-RETURN-MSG
                   WHEN OTHER
                     move WS-RESP-CD to WS-RESP-CD-DISP
                       SET ERR-FLG-ON TO TRUE
                       SET LK-RC-DB-ERROR TO TRUE
                       MOVE 'STARTBR'     TO ERROR-OPNAME
                       MOVE LIT-CARD-FILE TO ERROR-FILE
                       MOVE WS-RESP-CD    TO ERROR-RESP
                       MOVE WS-REAS-CD    TO ERROR-RESP2
                       MOVE WS-FILE-ERROR-MESSAGE TO LK-RETURN-MSG
               END-EVALUATE
           END-IF.

      *----------------------------------------------------------------*
      *                      POPULATE-CARD-OUTPUT
      *----------------------------------------------------------------*
       POPULATE-CARD-OUTPUT.
           MOVE CARD-NUM TO
                LK-CARD-NUM(LK-RECORDS-COUNT)
           MOVE CARD-ACCT-ID TO
                LK-CARD-ACCT-ID(LK-RECORDS-COUNT)
           MOVE CARD-CVV-CD TO
                LK-CARD-CVV-CD(LK-RECORDS-COUNT)
           MOVE CARD-EMBOSSED-NAME TO
                LK-CARD-EMBOSSED-NAME(LK-RECORDS-COUNT)
           MOVE CARD-EXPIRAION-DATE TO
                LK-CARD-EXPIRATION-DATE(LK-RECORDS-COUNT)
           MOVE CARD-ACTIVE-STATUS TO
                LK-CARD-ACTIVE-STATUS(LK-RECORDS-COUNT).

      *----------------------------------------------------------------*
      *                      READNEXT-CARD-FILE
      *----------------------------------------------------------------*
       READNEXT-CARD-FILE.
           EXEC CICS READNEXT
                DATASET(LIT-CARD-FILE)
                INTO (CARD-RECORD)
                LENGTH(LENGTH OF CARD-RECORD)
                RIDFLD(WS-CARD-RID-CARDNUM)
                KEYLENGTH(LENGTH OF WS-CARD-RID-CARDNUM)
                RESP(WS-RESP-CD)
                RESP2(WS-REAS-CD)
           END-EXEC

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
               WHEN DFHRESP(DUPREC)
                   CONTINUE
               WHEN DFHRESP(ENDFILE)
                   SET CARD-EOF TO TRUE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   SET LK-RC-DB-ERROR TO TRUE
                   MOVE 'READNEXT'    TO ERROR-OPNAME
                   MOVE LIT-CARD-FILE TO ERROR-FILE
                   MOVE WS-RESP-CD    TO ERROR-RESP
                   MOVE WS-REAS-CD    TO ERROR-RESP2
                   MOVE WS-FILE-ERROR-MESSAGE TO LK-RETURN-MSG
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      READPREV-CARD-FILE
      *----------------------------------------------------------------*
       READPREV-CARD-FILE.
           EXEC CICS READPREV
                DATASET(LIT-CARD-FILE)
                INTO (CARD-RECORD)
                LENGTH(LENGTH OF CARD-RECORD)
                RIDFLD(WS-CARD-RID-CARDNUM)
                KEYLENGTH(LENGTH OF WS-CARD-RID-CARDNUM)
                RESP(WS-RESP-CD)
                RESP2(WS-REAS-CD)
           END-EXEC

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
               WHEN DFHRESP(DUPREC)
                   CONTINUE
               WHEN DFHRESP(ENDFILE)
                   SET CARD-EOF TO TRUE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   SET LK-RC-DB-ERROR TO TRUE
                   MOVE 'READPREV'    TO ERROR-OPNAME
                   MOVE LIT-CARD-FILE TO ERROR-FILE
                   MOVE WS-RESP-CD    TO ERROR-RESP
                   MOVE WS-REAS-CD    TO ERROR-RESP2
                   MOVE WS-FILE-ERROR-MESSAGE TO LK-RETURN-MSG
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      ENDBR-CARD-FILE
      *----------------------------------------------------------------*
       ENDBR-CARD-FILE.
           IF BROWSE-ACTIVE
               EXEC CICS ENDBR
                    DATASET(LIT-CARD-FILE)
                    RESP(WS-RESP-CD)
                    RESP2(WS-REAS-CD)
               END-EXEC
               SET BROWSE-NOT-ACTIVE TO TRUE
           END-IF.

       9000-READ-FORWARD.
      *****************************************************************
      * Read card records forward (for pagination - skip first batch)*
      *****************************************************************

           SET ERR-FLG-OFF TO TRUE
           SET CARD-NOT-EOF TO TRUE

           PERFORM INIT-BROWSE-IF-NEEDED

           IF ERR-FLG-OFF
      * Skip the first batch of records (for forward pagination)
               MOVE ZEROES TO WS-IDX
               PERFORM UNTIL WS-IDX >= LK-MAX-RECORDS OR
                             CARD-EOF OR ERR-FLG-ON
                   PERFORM READNEXT-CARD-FILE
                   IF CARD-NOT-EOF AND ERR-FLG-OFF
                       PERFORM 9500-FILTER-RECORDS
                          THRU 9500-FILTER-RECORDS-EXIT

                       IF WS-DONOT-EXCLUDE-THIS-RECORD
                           ADD 1 TO WS-IDX
                       END-IF
                   END-IF
               END-PERFORM

      * Now read the next batch
               PERFORM UNTIL LK-RECORDS-COUNT >= LK-MAX-RECORDS OR
                              CARD-EOF OR ERR-FLG-ON

                  PERFORM READNEXT-CARD-FILE

                  IF CARD-NOT-EOF AND ERR-FLG-OFF
                     PERFORM 9500-FILTER-RECORDS
                        THRU 9500-FILTER-RECORDS-EXIT

                     IF WS-DONOT-EXCLUDE-THIS-RECORD
                        ADD 1 TO LK-RECORDS-COUNT
                        PERFORM POPULATE-CARD-OUTPUT

                        IF LK-RECORDS-COUNT >= LK-MAX-RECORDS
                           CONTINUE
                        END-IF
                     END-IF
                  END-IF
               END-PERFORM

               IF LK-RECORDS-COUNT = 0
                  MOVE 'NO MORE PAGES TO DISPLAY'
                               TO LK-RETURN-MSG
               END-IF
           END-IF
           .
       9000-READ-FORWARD-EXIT.
           EXIT.

       9100-READ-BACKWARDS.
      *****************************************************************
      * Read card records backwards from database with filters       *
      * SIMPLIFIED VERSION - Just read 7 previous records            *
      *****************************************************************

           SET ERR-FLG-OFF TO TRUE
           SET CARD-NOT-EOF TO TRUE
           MOVE 0 TO LK-RECORDS-COUNT
           MOVE 0 TO WS-TEMP-CARD-COUNT

           PERFORM INIT-BROWSE-IF-NEEDED

           IF ERR-FLG-OFF
      * Read 8 records backward into temp storage (to skip first one)
               PERFORM UNTIL WS-TEMP-CARD-COUNT >= 8 OR
                             CARD-EOF OR ERR-FLG-ON
                   PERFORM READPREV-CARD-FILE
                   IF CARD-NOT-EOF AND ERR-FLG-OFF
                       PERFORM 9500-FILTER-RECORDS
                          THRU 9500-FILTER-RECORDS-EXIT

                       IF WS-DONOT-EXCLUDE-THIS-RECORD
                           ADD 1 TO WS-TEMP-CARD-COUNT
                           MOVE CARD-NUM TO
                                WS-TEMP-CARD-NUM(WS-TEMP-CARD-COUNT)
                           MOVE CARD-ACCT-ID TO
                               WS-TEMP-CARD-ACCT-ID(WS-TEMP-CARD-COUNT)
                           MOVE CARD-CVV-CD TO
                                WS-TEMP-CARD-CVV-CD(WS-TEMP-CARD-COUNT)
                           MOVE CARD-EMBOSSED-NAME TO
                            WS-TEMP-CARD-EMBOSSED-NM(WS-TEMP-CARD-COUNT)
                           MOVE CARD-EXPIRAION-DATE TO
                            WS-TEMP-CARD-EXPIR-DATE(WS-TEMP-CARD-COUNT)
                           MOVE CARD-ACTIVE-STATUS TO
                             WS-TEMP-CARD-ACTIVE-ST(WS-TEMP-CARD-COUNT)
                       END-IF
                   END-IF
               END-PERFORM

      * Check if we hit EOF or error during backward read (at top)
               IF CARD-EOF OR ERR-FLG-ON OR
                  WS-TEMP-CARD-COUNT < 8
      * We're at the top - display first 7 records from beginning
                   SET CARD-NOT-EOF TO TRUE
                   SET ERR-FLG-OFF TO TRUE
                   PERFORM ENDBR-CARD-FILE
                   MOVE LOW-VALUES TO WS-CARD-RID-CARDNUM
                   PERFORM INIT-BROWSE-IF-NEEDED
                   IF ERR-FLG-OFF
                       PERFORM PROCESS-LIST
                   END-IF
                   MOVE 'NO PREVIOUS PAGES TO DISPLAY'
                        TO LK-RETURN-MSG
               ELSE
      * Normal case - display records 2-8 in reverse order (skip record 1)
                   PERFORM VARYING WS-IDX FROM WS-TEMP-CARD-COUNT BY -1
                           UNTIL WS-IDX < 2 OR
                                 LK-RECORDS-COUNT >= 7
                       ADD 1 TO LK-RECORDS-COUNT
                       MOVE WS-TEMP-CARD-NUM(WS-IDX) TO
                            LK-CARD-NUM(LK-RECORDS-COUNT)
                       MOVE WS-TEMP-CARD-ACCT-ID(WS-IDX) TO
                            LK-CARD-ACCT-ID(LK-RECORDS-COUNT)
                       MOVE WS-TEMP-CARD-CVV-CD(WS-IDX) TO
                            LK-CARD-CVV-CD(LK-RECORDS-COUNT)
                       MOVE WS-TEMP-CARD-EMBOSSED-NM(WS-IDX) TO
                            LK-CARD-EMBOSSED-NAME(LK-RECORDS-COUNT)
                       MOVE WS-TEMP-CARD-EXPIR-DATE(WS-IDX) TO
                            LK-CARD-EXPIRATION-DATE(LK-RECORDS-COUNT)
                       MOVE WS-TEMP-CARD-ACTIVE-ST(WS-IDX) TO
                            LK-CARD-ACTIVE-STATUS(LK-RECORDS-COUNT)
                   END-PERFORM
               END-IF

               IF LK-RECORDS-COUNT = 0
                  SET LK-RC-NO-RECORDS TO TRUE
                  MOVE
                  'NO RECORDS FOUND FOR THIS SEARCH CONDITION.'
                               TO LK-RETURN-MSG
               END-IF
           END-IF
           .
       9100-READ-BACKWARDS-EXIT.
           EXIT.

       9500-FILTER-RECORDS.
      *****************************************************************
      * Filter record based on input criteria                         *
      *****************************************************************
           SET WS-DONOT-EXCLUDE-THIS-RECORD TO TRUE

      * Apply Account ID filter if provided
           IF FLG-ACCTFILTER-ISVALID
              IF CARD-ACCT-ID NOT = LK-FILTER-ACCT-ID
                 SET WS-EXCLUDE-THIS-RECORD TO TRUE
                 GO TO 9500-FILTER-RECORDS-EXIT
              END-IF
           END-IF

      * Apply Card Number filter if provided
           IF FLG-CARDFILTER-ISVALID
              IF CARD-NUM NOT = LK-FILTER-CARD-NUM
                 SET WS-EXCLUDE-THIS-RECORD TO TRUE
                 GO TO 9500-FILTER-RECORDS-EXIT
              END-IF
           END-IF
           .
       9500-FILTER-RECORDS-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *                      CLEAR-OUTPUT-ARRAY
      *----------------------------------------------------------------*
       CLEAR-OUTPUT-ARRAY.
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 7
               MOVE SPACES TO LK-CARD-NUM(WS-IDX)
               MOVE ZEROS TO LK-CARD-ACCT-ID(WS-IDX)
               MOVE ZEROS TO LK-CARD-CVV-CD(WS-IDX)
               MOVE SPACES TO LK-CARD-EMBOSSED-NAME(WS-IDX)
               MOVE SPACES TO LK-CARD-EXPIRATION-DATE(WS-IDX)
               MOVE SPACES TO LK-CARD-ACTIVE-STATUS(WS-IDX)
           END-PERFORM.