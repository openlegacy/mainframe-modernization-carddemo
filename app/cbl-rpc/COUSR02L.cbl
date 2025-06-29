******************************************************************
      * Program     : COUSR02L.CBL
      * Application : CardDemo
      * Type        : COBOL RPC Program
      * Function    : Update/Lookup user in USRSEC file for API integration
      * Description : Receives user details and updates the user record
      *               or performs lookup if only User ID provided
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     COUSR02L.
       AUTHOR.     AWS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
      *----------------------------------------------------------------*
      *                     WORKING STORAGE SECTION
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

       01 WS-VARIABLES.
         05 WS-PGMNAME                 PIC X(08) VALUE 'COUSR02L'.
         05 WS-USRSEC-FILE             PIC X(08) VALUE 'USRSEC  '.
         05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.
           88 ERR-FLG-ON                         VALUE 'Y'.
           88 ERR-FLG-OFF                        VALUE 'N'.
         05 WS-RESP-CD                 PIC S9(09) COMP VALUE ZEROS.
         05 WS-REAS-CD                 PIC S9(09) COMP VALUE ZEROS.
         05 WS-USR-MODIFIED            PIC X(01) VALUE 'N'.
           88 USR-MODIFIED-YES                   VALUE 'Y'.
           88 USR-MODIFIED-NO                    VALUE 'N'.
         05 WS-OPERATION               PIC X(01) VALUE SPACES.
           88 OPERATION-LOOKUP                   VALUE 'L'.
           88 OPERATION-UPDATE                   VALUE 'U'.

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
       01  DFHCOMMAREA.
           05  LK-OPERATION               PIC X(01).
               88  LK-OP-LOOKUP           VALUE 'L'.
               88  LK-OP-UPDATE           VALUE 'U'.
           05  LK-INPUT-USER.
               10  LK-IN-USER-ID             PIC X(08).
               10  LK-IN-USER-FNAME          PIC X(20).
               10  LK-IN-USER-LNAME          PIC X(20).
               10  LK-IN-USER-PWD            PIC X(08).
               10  LK-IN-USER-TYPE           PIC X(01).
           05  LK-OUTPUT-STATUS.
               10  LK-OUT-RETURN-CODE        PIC 9(02).
                   88  RC-SUCCESS             VALUE 00.
                   88  RC-NOT-FOUND           VALUE 01.
                   88  RC-NO-CHANGES          VALUE 02.
                   88  RC-VALIDATION-ERROR    VALUE 10.
                   88  RC-DATABASE-ERROR      VALUE 99.
               10  LK-OUT-MESSAGE            PIC X(80).

      *----------------------------------------------------------------*
      *                       PROCEDURE DIVISION
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

       MAIN-PARA.
           INITIALIZE LK-OUTPUT-STATUS

           SET ERR-FLG-OFF TO TRUE
           SET USR-MODIFIED-NO TO TRUE
           SET RC-SUCCESS TO TRUE

           MOVE SPACES TO LK-OUT-MESSAGE

      * Process based on operation flag from calling program
           EVALUATE TRUE
               WHEN LK-OP-LOOKUP
                   PERFORM PROCESS-LOOKUP
               WHEN LK-OP-UPDATE
                   PERFORM PROCESS-UPDATE
               WHEN OTHER
                   SET RC-VALIDATION-ERROR TO TRUE
                   MOVE 'Invalid operation code' TO LK-OUT-MESSAGE
           END-EVALUATE

           GOBACK.

      *----------------------------------------------------------------*
      *                      PROCESS-LOOKUP
      *----------------------------------------------------------------*
       PROCESS-LOOKUP.

           IF LK-IN-USER-ID = SPACES OR LOW-VALUES
               SET RC-VALIDATION-ERROR TO TRUE
            MOVE 'User ID cannot be empty for lookup' TO LK-OUT-MESSAGE
           ELSE
               MOVE LK-IN-USER-ID TO SEC-USR-ID
               PERFORM READ-USER-SEC-FILE-LOOKUP

               IF NOT ERR-FLG-ON
      * Return user data in the input fields for screen display
                   MOVE SEC-USR-FNAME TO LK-IN-USER-FNAME
                   MOVE SEC-USR-LNAME TO LK-IN-USER-LNAME
                   MOVE SEC-USR-PWD   TO LK-IN-USER-PWD
                   MOVE SEC-USR-TYPE  TO LK-IN-USER-TYPE
               MOVE 'User data retrieved successfully' TO LK-OUT-MESSAGE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      *                      PROCESS-UPDATE
      *----------------------------------------------------------------*
       PROCESS-UPDATE.

      * Validate all required fields for update
           EVALUATE TRUE
               WHEN LK-IN-USER-ID = SPACES OR LOW-VALUES
                   SET RC-VALIDATION-ERROR TO TRUE
                   MOVE 'User ID cannot be empty' TO LK-OUT-MESSAGE
               WHEN LK-IN-USER-FNAME = SPACES OR LOW-VALUES
                   SET RC-VALIDATION-ERROR TO TRUE
                   MOVE 'First Name cannot be empty' TO LK-OUT-MESSAGE
               WHEN LK-IN-USER-LNAME = SPACES OR LOW-VALUES
                   SET RC-VALIDATION-ERROR TO TRUE
                   MOVE 'Last Name cannot be empty' TO LK-OUT-MESSAGE
               WHEN LK-IN-USER-PWD = SPACES OR LOW-VALUES
                   SET RC-VALIDATION-ERROR TO TRUE
                   MOVE 'Password cannot be empty' TO LK-OUT-MESSAGE
               WHEN LK-IN-USER-TYPE = SPACES OR LOW-VALUES
                   SET RC-VALIDATION-ERROR TO TRUE
                   MOVE 'User Type cannot be empty' TO LK-OUT-MESSAGE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           IF NOT ERR-FLG-ON
               MOVE LK-IN-USER-ID TO SEC-USR-ID
               PERFORM READ-USER-SEC-FILE-UPDATE

               IF NOT ERR-FLG-ON
                   IF LK-IN-USER-FNAME NOT = SEC-USR-FNAME
                       MOVE LK-IN-USER-FNAME TO SEC-USR-FNAME
                       SET USR-MODIFIED-YES TO TRUE
                   END-IF

                   IF LK-IN-USER-LNAME NOT = SEC-USR-LNAME
                       MOVE LK-IN-USER-LNAME TO SEC-USR-LNAME
                       SET USR-MODIFIED-YES TO TRUE
                   END-IF

                   IF LK-IN-USER-PWD NOT = SEC-USR-PWD
                       MOVE LK-IN-USER-PWD TO SEC-USR-PWD
                       SET USR-MODIFIED-YES TO TRUE
                   END-IF

                   IF LK-IN-USER-TYPE NOT = SEC-USR-TYPE
                       MOVE LK-IN-USER-TYPE TO SEC-USR-TYPE
                       SET USR-MODIFIED-YES TO TRUE
                   END-IF

                   IF USR-MODIFIED-YES
                       PERFORM UPDATE-USER-SEC-FILE
                   ELSE
                       SET RC-NO-CHANGES TO TRUE
                       MOVE 'No changes detected' TO LK-OUT-MESSAGE
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      *                      READ-USER-SEC-FILE-LOOKUP
      *----------------------------------------------------------------*
       READ-USER-SEC-FILE-LOOKUP.
           EXEC CICS READ
                DATASET   (WS-USRSEC-FILE)
                INTO      (SEC-USER-DATA)
                RIDFLD    (SEC-USR-ID)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   SET ERR-FLG-ON TO TRUE
                   SET RC-NOT-FOUND TO TRUE
                   MOVE 'User ID not found' TO LK-OUT-MESSAGE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   SET RC-DATABASE-ERROR TO TRUE
                   MOVE 'Unable to lookup User' TO LK-OUT-MESSAGE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      READ-USER-SEC-FILE-UPDATE
      *----------------------------------------------------------------*
       READ-USER-SEC-FILE-UPDATE.
           EXEC CICS READ
                DATASET   (WS-USRSEC-FILE)
                INTO      (SEC-USER-DATA)
                RIDFLD    (SEC-USR-ID)
                UPDATE
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   SET ERR-FLG-ON TO TRUE
                   SET RC-NOT-FOUND TO TRUE
                   MOVE 'User ID not found' TO LK-OUT-MESSAGE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   SET RC-DATABASE-ERROR TO TRUE
                   MOVE 'Unable to lookup User' TO LK-OUT-MESSAGE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      UPDATE-USER-SEC-FILE
      *----------------------------------------------------------------*
       UPDATE-USER-SEC-FILE.
           EXEC CICS REWRITE
                DATASET   (WS-USRSEC-FILE)
                FROM      (SEC-USER-DATA)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   MOVE SPACES TO LK-OUT-MESSAGE
                   STRING 'User ' DELIMITED BY SIZE
                          SEC-USR-ID DELIMITED BY SPACE
                     ' has been updated successfully' DELIMITED BY SIZE
                     INTO LK-OUT-MESSAGE
               WHEN DFHRESP(NOTFND)
                   SET ERR-FLG-ON TO TRUE
                   SET RC-NOT-FOUND TO TRUE
                   MOVE 'User ID not found' TO LK-OUT-MESSAGE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   SET RC-DATABASE-ERROR TO TRUE
                   MOVE 'Unable to update User' TO LK-OUT-MESSAGE
           END-EVALUATE.