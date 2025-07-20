******************************************************************
      * Program     : COCRDLIA.CBL
      * Application : CardDemo
      * Type        : COBOL RPC Program
      * Function    : Card list operations using DB2
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID.
              COCRDLIA.
       DATE-WRITTEN.
           April 2023.
       DATE-COMPILED.
           Today.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

       01  WS-DEBUG-MESSAGE           PIC X(79).
       01  WS-RESP-CD-DISP            PIC 9(09) VALUE ZEROS.
       01  WS-REAS-CD-DISP            PIC 9(09) VALUE ZEROS.
       01  WS-DISPLAY-LINE            PIC X(79) VALUE SPACES.
       01  WS-IN-OPERATION            PIC X(50) VALUE SPACES.
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
      * DB2 related variables
      ******************************************************************
         05 WS-DB2-VARS.
            07 WS-SQLCODE                          PIC S9(09) COMP
                                                   VALUE ZEROS.
            07 WS-DB2-CURSOR-STATE                 PIC X(1)
                                                   VALUE SPACES.
               88 WS-CURSOR-CLOSED                 VALUE 'C'.
               88 WS-CURSOR-OPEN                   VALUE 'O'.
            07 WS-DB2-ROWS-FETCHED                 PIC S9(04) COMP
                                                   VALUE ZEROS.
            07 WS-DB2-END-OF-CURSOR                PIC X(1)
                                                   VALUE 'N'.
               88 WS-END-OF-CURSOR                 VALUE 'Y'.
               88 WS-MORE-ROWS                     VALUE 'N'.

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
      * DB2 Data Handling
      ******************************************************************
         05 WS-DB2-HANDLING-VARS.
            10  WS-CARD-RID.
                20  WS-CARD-RID-CARDNUM            PIC X(16).
                20  WS-CARD-RID-ACCT-ID            PIC 9(11).
                20  WS-CARD-RID-ACCT-ID-X          REDEFINES
                    WS-CARD-RID-ACCT-ID            PIC X(11).

         05  WS-SCRN-COUNTER                     PIC S9(4) COMP
                                                 VALUE 0.
         05  WS-IDX                              PIC S9(4) COMP
                                                 VALUE 0.
         05  WS-OUT-IDX                          PIC S9(4) COMP
                                                 VALUE 0.

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

         05  WS-DB2-ERROR-MESSAGE.
           10  FILLER                              PIC X(12)
                                                   VALUE 'DB2 Error: '.
           10  ERROR-OPNAME                        PIC X(8)
                                                   VALUE SPACES.
           10  FILLER                              PIC X(4)
                                                   VALUE ' on '.
           10  ERROR-TABLE                         PIC X(9)
                                                   VALUE SPACES.
           10  FILLER                              PIC X(15)
                                                   VALUE
                                                 ' return SQLCODE'.
           10  ERROR-SQLCODE                       PIC X(10)
                                                   VALUE SPACES.
           10  FILLER                              PIC X(5).

      ******************************************************************
      * Literals and Constants
      ******************************************************************
       01 WS-CONSTANTS.
         05  WS-MAX-RECORDS                        PIC S9(4) COMP
                                                   VALUE 7.
         05  LIT-THISPGM                           PIC X(8)
                                                   VALUE 'COCRDLIA'.
         05  LIT-CARD-TABLE                        PIC X(8)
                                                   VALUE 'CARDDAT'.

      ******************************************************************
      * Temporary storage for backward records
      ******************************************************************
       01 WS-TEMP-CARD-RECORDS.
          05 WS-TEMP-CARD-COUNT            PIC 9(02) VALUE 0.
          05 WS-TEMP-CARD-DATA OCCURS 7 TIMES.
             10 WS-TEMP-CARD-NUM           PIC X(16).
             10 WS-TEMP-CARD-ACCT-ID       PIC 9(11).
             10 WS-TEMP-CARD-ACTIVE-ST     PIC X(01).

      ******************************************************************
      * DB2 SQLCA
      ******************************************************************
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      ******************************************************************
      * DB2 HOST VARIABLE DECLARATIONS
      ******************************************************************
           EXEC SQL BEGIN DECLARE SECTION END-EXEC.

      * Host variables for CARDDAT table
       01  HV-CARD-VARIABLES.
           05  HV-CARD-NUM                    PIC X(16).
           05  HV-CARD-ACCT-ID                PIC S9(11) COMP-3.
           05  HV-CARD-ACTIVE-STATUS          PIC X(1).

      * Host variables for search criteria
       01  HV-SEARCH-VARIABLES.
           05  HV-SEARCH-CARD-NUM             PIC X(16).
           05  HV-SEARCH-ACCT-ID              PIC S9(11) COMP-3.

           EXEC SQL END DECLARE SECTION END-EXEC.

      ******************************************************************
      * DB2 DECLARE CURSOR
      ******************************************************************
           EXEC SQL
               DECLARE CARD_CURSOR CURSOR FOR
               SELECT CARD_NUM,
                      CARD_ACCT_ID,
                      CARD_ACTIVE_STATUS
               FROM ALAINL.CARDDAT
               WHERE CARD_NUM >= :HV-SEARCH-CARD-NUM
               ORDER BY CARD_NUM
           END-EXEC.

           EXEC SQL
               DECLARE CARD_CURSOR_FILTERED CURSOR FOR
               SELECT CARD_NUM,
                      CARD_ACCT_ID,
                      CARD_ACTIVE_STATUS
               FROM ALAINL.CARDDAT
               WHERE CARD_NUM >= :HV-SEARCH-CARD-NUM
               AND CARD_ACCT_ID = :HV-SEARCH-ACCT-ID
               ORDER BY CARD_NUM
           END-EXEC.

           EXEC SQL
               DECLARE CARD_CURSOR_PREV CURSOR FOR
               SELECT CARD_NUM,
                      CARD_ACCT_ID,
                      CARD_ACTIVE_STATUS
               FROM ALAINL.CARDDAT
               WHERE CARD_NUM < :HV-SEARCH-CARD-NUM
               ORDER BY CARD_NUM DESC
           END-EXEC.

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
               10  LK-FILTER-ACCT-ID              PIC X(11).
               10  LK-FILTER-CARD-NUM             PIC X(16).
               10  LK-START-KEY                   PIC X(16).
               10  LK-MAX-RECORDS                 PIC S9(4) COMP.
               10  LK-PAGE-DIR                    PIC X(1).

      ******************************************************************
      * Output Parameters
      ******************************************************************
           05  LK-OUTPUT-PARAMS.
               10  LK-RETURN-CODE                 PIC S9(4) COMP.
                  88  LK-RC-OK                    VALUE 0.
                  88  LK-RC-FILTER-ERROR          VALUE 1.
                  88  LK-RC-DB-ERROR              VALUE 2.
                  88  LK-RC-NO-RECORDS            VALUE 3.
               10  LK-RETURN-MSG                  PIC X(80).
               10  LK-RECORDS-COUNT               PIC S9(4) COMP.
               10  LK-CARDS-DATA.
                   15  LK-CARDS-ARRAY             OCCURS 1 TO 7
                                                  DEPENDING ON
                                                  LK-RECORDS-COUNT.
                       20  LK-CARD-NUM            PIC X(16).
                       20  LK-CARD-ACCT-ID        PIC 9(11).
                       20  LK-CARD-CVV-CD         PIC 9(03).
                       20  LK-CARD-EMBOSSED-NAME  PIC X(50).
                       20  LK-CARD-EXPIRATION-DATE PIC X(10).
                       20  LK-CARD-ACTIVE-STATUS  PIC X(01).

       PROCEDURE DIVISION.

       0000-MAIN.
           INITIALIZE WS-MISC-STORAGE
           SET WS-CURSOR-CLOSED TO TRUE

           MOVE 0 TO LK-RETURN-CODE
           MOVE SPACES TO LK-RETURN-MSG
           MOVE 0 TO LK-RECORDS-COUNT

           IF LK-MAX-RECORDS = 0
              MOVE WS-MAX-RECORDS TO LK-MAX-RECORDS
           END-IF

      *****************************************************************
      * Validate input parameters
      *****************************************************************
           PERFORM 1000-VALIDATE-INPUT
              THRU 1000-VALIDATE-INPUT-EXIT

           IF INPUT-ERROR
              SET LK-RC-FILTER-ERROR TO TRUE
              GO TO 0000-MAIN-EXIT
           END-IF

      *****************************************************************
      * Read cards from database
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
      * Ensure cursor is closed before exit
           PERFORM 9999-CLOSE-CURSOR
              THRU 9999-CLOSE-CURSOR-EXIT

           EXEC CICS RETURN
           END-EXEC.

       1000-VALIDATE-INPUT.

      *****************************************************************
      * Validate input parameters, especially filter values
      *****************************************************************
           SET INPUT-OK TO TRUE

      *****************************************************************
      * Validate Account ID filter if provided
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
                 'ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBE
      -          'R'
                                 TO LK-RETURN-MSG
              ELSE
                 SET FLG-ACCTFILTER-ISVALID TO TRUE
              END-IF
           END-IF

      *****************************************************************
      * Validate Card Number filter if provided
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
                  'CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMB
      -           'ER'
                                    TO LK-RETURN-MSG
                 END-IF
              END-IF
           END-IF

      *****************************************************************
      * Validate max records parameter
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

      ******************************************************************
      *                      PROCESS-LIST (Current Records)
      ******************************************************************
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
                   PERFORM READNEXT-CARD-DB2
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

      ******************************************************************
      *                      INIT-BROWSE-IF-NEEDED
      ******************************************************************
       INIT-BROWSE-IF-NEEDED.
           IF WS-CURSOR-CLOSED
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

               MOVE WS-CARD-RID-CARDNUM TO HV-SEARCH-CARD-NUM
               IF FLG-ACCTFILTER-ISVALID
                   MOVE LK-FILTER-ACCT-ID TO HV-SEARCH-ACCT-ID
                   EXEC SQL
                       OPEN CARD_CURSOR_FILTERED
                   END-EXEC
               ELSE
                   EXEC SQL
                       OPEN CARD_CURSOR
                   END-EXEC
               END-IF

               MOVE SQLCODE TO WS-SQLCODE

               EVALUATE WS-SQLCODE
                   WHEN 0
                       SET WS-CURSOR-OPEN TO TRUE
                   WHEN OTHER
                       SET CARD-EOF TO TRUE
                       SET ERR-FLG-ON TO TRUE
                       SET LK-RC-DB-ERROR TO TRUE
                       MOVE 'OPEN'           TO ERROR-OPNAME
                       MOVE LIT-CARD-TABLE   TO ERROR-TABLE
                       MOVE WS-SQLCODE       TO ERROR-SQLCODE
                       MOVE WS-DB2-ERROR-MESSAGE TO LK-RETURN-MSG
               END-EVALUATE
           END-IF.

      ******************************************************************
      *                      POPULATE-CARD-OUTPUT
      ******************************************************************
       POPULATE-CARD-OUTPUT.
           MOVE CARD-NUM TO
                LK-CARD-NUM(LK-RECORDS-COUNT)
           MOVE CARD-ACCT-ID TO
                LK-CARD-ACCT-ID(LK-RECORDS-COUNT)
           MOVE CARD-ACTIVE-STATUS TO
                LK-CARD-ACTIVE-STATUS(LK-RECORDS-COUNT).

      ******************************************************************
      *                      READNEXT-CARD-DB2
      ******************************************************************
       READNEXT-CARD-DB2.
           IF FLG-ACCTFILTER-ISVALID
               EXEC SQL
                   FETCH CARD_CURSOR_FILTERED
                   INTO :HV-CARD-NUM,
                        :HV-CARD-ACCT-ID,
                        :HV-CARD-ACTIVE-STATUS
               END-EXEC
           ELSE
               EXEC SQL
                   FETCH CARD_CURSOR
                   INTO :HV-CARD-NUM,
                        :HV-CARD-ACCT-ID,
                        :HV-CARD-ACTIVE-STATUS
               END-EXEC
           END-IF

           MOVE SQLCODE TO WS-SQLCODE

           EVALUATE WS-SQLCODE
               WHEN 0
                   MOVE HV-CARD-NUM          TO CARD-NUM
                   MOVE HV-CARD-ACCT-ID      TO CARD-ACCT-ID
                   MOVE HV-CARD-ACTIVE-STATUS TO CARD-ACTIVE-STATUS
               WHEN +100
                   SET CARD-EOF TO TRUE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   SET LK-RC-DB-ERROR TO TRUE
                   MOVE 'FETCH'        TO ERROR-OPNAME
                   MOVE LIT-CARD-TABLE TO ERROR-TABLE
                   MOVE WS-SQLCODE     TO ERROR-SQLCODE
                   MOVE WS-DB2-ERROR-MESSAGE TO LK-RETURN-MSG
           END-EVALUATE.

      ******************************************************************
      *                      READPREV-CARD-DB2
      ******************************************************************
       READPREV-CARD-DB2.
           EXEC SQL
               FETCH CARD_CURSOR_PREV
               INTO :HV-CARD-NUM,
                    :HV-CARD-ACCT-ID,
                    :HV-CARD-ACTIVE-STATUS
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE

           EVALUATE WS-SQLCODE
               WHEN 0
                   MOVE HV-CARD-NUM          TO CARD-NUM
                   MOVE HV-CARD-ACCT-ID      TO CARD-ACCT-ID
                   MOVE HV-CARD-ACTIVE-STATUS TO CARD-ACTIVE-STATUS
               WHEN +100
                   SET CARD-EOF TO TRUE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   SET LK-RC-DB-ERROR TO TRUE
                   MOVE 'FETCH'        TO ERROR-OPNAME
                   MOVE LIT-CARD-TABLE TO ERROR-TABLE
                   MOVE WS-SQLCODE     TO ERROR-SQLCODE
                   MOVE WS-DB2-ERROR-MESSAGE TO LK-RETURN-MSG
           END-EVALUATE.

       9000-READ-FORWARD.
      *****************************************************************
      * Read card records forward (for pagination - skip first batch)
      *****************************************************************

           SET ERR-FLG-OFF TO TRUE
           SET CARD-NOT-EOF TO TRUE

           PERFORM INIT-BROWSE-IF-NEEDED

           IF ERR-FLG-OFF
      * Skip the first batch of records (for forward pagination)
               MOVE ZEROES TO WS-IDX
               PERFORM UNTIL WS-IDX >= LK-MAX-RECORDS OR
                             CARD-EOF OR ERR-FLG-ON
                   PERFORM READNEXT-CARD-DB2
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

                  PERFORM READNEXT-CARD-DB2

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
      * Read card records backwards from database with filters
      * SIMPLIFIED VERSION - Just read 7 previous records
      *****************************************************************

           SET ERR-FLG-OFF TO TRUE
           SET CARD-NOT-EOF TO TRUE
           MOVE 0 TO LK-RECORDS-COUNT
           MOVE 0 TO WS-TEMP-CARD-COUNT

      * Set up for backward cursor
           MOVE LK-START-KEY TO HV-SEARCH-CARD-NUM

           EXEC SQL
               OPEN CARD_CURSOR_PREV
           END-EXEC

           MOVE SQLCODE TO WS-SQLCODE

           EVALUATE WS-SQLCODE
               WHEN 0
                   SET WS-CURSOR-OPEN TO TRUE
               WHEN OTHER
                   SET CARD-EOF TO TRUE
                   SET ERR-FLG-ON TO TRUE
                   SET LK-RC-DB-ERROR TO TRUE
                   MOVE 'OPEN'           TO ERROR-OPNAME
                   MOVE LIT-CARD-TABLE   TO ERROR-TABLE
                   MOVE WS-SQLCODE       TO ERROR-SQLCODE
                   MOVE WS-DB2-ERROR-MESSAGE TO LK-RETURN-MSG
                   GO TO 9100-READ-BACKWARDS-EXIT
           END-EVALUATE

           IF ERR-FLG-OFF
      * Read 8 records backward into temp storage (to skip first one)
               PERFORM UNTIL WS-TEMP-CARD-COUNT >= 8 OR
                             CARD-EOF OR ERR-FLG-ON
                   PERFORM READPREV-CARD-DB2
                   IF CARD-NOT-EOF AND ERR-FLG-OFF
                       PERFORM 9500-FILTER-RECORDS
                          THRU 9500-FILTER-RECORDS-EXIT

                       IF WS-DONOT-EXCLUDE-THIS-RECORD
                           ADD 1 TO WS-TEMP-CARD-COUNT
                           MOVE CARD-NUM TO
                                WS-TEMP-CARD-NUM(WS-TEMP-CARD-COUNT)
                           MOVE CARD-ACCT-ID TO
                               WS-TEMP-CARD-ACCT-ID(
                               WS-TEMP-CARD-COUNT)
                           MOVE CARD-ACTIVE-STATUS TO
                             WS-TEMP-CARD-ACTIVE-ST(
                             WS-TEMP-CARD-COUNT)
                       END-IF
                   END-IF
               END-PERFORM

      * Check if we hit EOF or error during backward read (at top)
               IF CARD-EOF OR ERR-FLG-ON OR
                  WS-TEMP-CARD-COUNT < 8
      * We're at the top - display first 7 records from beginning
                   SET CARD-NOT-EOF TO TRUE
                   SET ERR-FLG-OFF TO TRUE
                   PERFORM 9999-CLOSE-CURSOR
                      THRU 9999-CLOSE-CURSOR-EXIT
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
      * Filter record based on input criteria
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

      ******************************************************************
      *                      CLEAR-OUTPUT-ARRAY
      ******************************************************************
       CLEAR-OUTPUT-ARRAY.
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 7
               MOVE SPACES TO LK-CARD-NUM(WS-IDX)
               MOVE ZEROS TO LK-CARD-ACCT-ID(WS-IDX)
               MOVE SPACES TO LK-CARD-ACTIVE-STATUS(WS-IDX)
           END-PERFORM.

      ******************************************************************
      *                      9999-CLOSE-CURSOR
      ******************************************************************
       9999-CLOSE-CURSOR.
           IF WS-CURSOR-OPEN
               EXEC SQL
                   CLOSE CARD_CURSOR
               END-EXEC

               EXEC SQL
                   CLOSE CARD_CURSOR_FILTERED
               END-EXEC

               EXEC SQL
                   CLOSE CARD_CURSOR_PREV
               END-EXEC

               SET WS-CURSOR-CLOSED TO TRUE
           END-IF
           .

       9999-CLOSE-CURSOR-EXIT.
           EXIT
           .