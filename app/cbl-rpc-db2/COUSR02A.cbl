******************************************************************
      * Program     : COUSR02A.CBL
      * Application : CardDemo
      * Type        : COBOL RPC Program
      * Function    : Update/Lookup user in USERSEC DB2 table for API integratio
      * Description : Receives user details and updates the user record
      *               or performs lookup if only User ID provided
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     COUSR02A.
       AUTHOR.     AWS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
      *----------------------------------------------------------------*
      *                     WORKING STORAGE SECTION
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

       01 WS-VARIABLES.
         05 WS-PGMNAME                 PIC X(08) VALUE 'COUSR02A'.
         05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.
           88 ERR-FLG-ON                         VALUE 'Y'.
           88 ERR-FLG-OFF                        VALUE 'N'.
         05 WS-USR-MODIFIED            PIC X(01) VALUE 'N'.
           88 USR-MODIFIED-YES                   VALUE 'Y'.
           88 USR-MODIFIED-NO                    VALUE 'N'.
         05 WS-OPERATION               PIC X(01) VALUE SPACES.
           88 OPERATION-LOOKUP                   VALUE 'L'.
           88 OPERATION-UPDATE                   VALUE 'U'.

      *----------------------------------------------------------------*
      *                     DB2 SQLCA AND VARIABLES
      *----------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA END-EXEC.

       01 WS-SQL-STATUS                PIC S9(09) COMP VALUE ZEROS.

      * User Security Record Structure
       01 SEC-USER-DATA.
         05 SEC-USR-ID                 PIC X(08).
         05 SEC-USR-FNAME              PIC X(20).
         05 SEC-USR-LNAME              PIC X(20).
         05 SEC-USR-PWD                PIC X(08).
         05 SEC-USR-TYPE               PIC X(01).

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
               PERFORM READ-USER-SEC-TABLE-LOOKUP

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
               PERFORM READ-USER-SEC-TABLE-UPDATE

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
                       PERFORM UPDATE-USER-SEC-TABLE
                   ELSE
                       SET RC-NO-CHANGES TO TRUE
                       MOVE 'No changes detected' TO LK-OUT-MESSAGE
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      *                      READ-USER-SEC-TABLE-LOOKUP
      *----------------------------------------------------------------*
       READ-USER-SEC-TABLE-LOOKUP.

           EXEC SQL
               SELECT USR_ID, USR_FNAME, USR_LNAME, USR_PWD, USR_TYPE
               INTO :SEC-USR-ID, :SEC-USR-FNAME, :SEC-USR-LNAME,
                    :SEC-USR-PWD, :SEC-USR-TYPE
               FROM USERSEC
               WHERE USR_ID = :SEC-USR-ID
           END-EXEC.

           MOVE SQLCODE TO WS-SQL-STATUS

           EVALUATE WS-SQL-STATUS
               WHEN 0
                   CONTINUE
               WHEN +100
                   SET ERR-FLG-ON TO TRUE
                   SET RC-NOT-FOUND TO TRUE
                   MOVE 'User ID not found' TO LK-OUT-MESSAGE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   SET RC-DATABASE-ERROR TO TRUE
                   MOVE 'Unable to lookup User' TO LK-OUT-MESSAGE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      READ-USER-SEC-TABLE-UPDATE
      *----------------------------------------------------------------*
       READ-USER-SEC-TABLE-UPDATE.

           EXEC SQL
               SELECT USR_ID, USR_FNAME, USR_LNAME, USR_PWD, USR_TYPE
               INTO :SEC-USR-ID, :SEC-USR-FNAME, :SEC-USR-LNAME,
                    :SEC-USR-PWD, :SEC-USR-TYPE
               FROM USERSEC
               WHERE USR_ID = :SEC-USR-ID
           END-EXEC.

           MOVE SQLCODE TO WS-SQL-STATUS

           EVALUATE WS-SQL-STATUS
               WHEN 0
                   CONTINUE
               WHEN +100
                   SET ERR-FLG-ON TO TRUE
                   SET RC-NOT-FOUND TO TRUE
                   MOVE 'User ID not found' TO LK-OUT-MESSAGE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   SET RC-DATABASE-ERROR TO TRUE
                   MOVE 'Unable to lookup User' TO LK-OUT-MESSAGE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      UPDATE-USER-SEC-TABLE
      *----------------------------------------------------------------*
       UPDATE-USER-SEC-TABLE.

           EXEC SQL
               UPDATE USERSEC
               SET USR_FNAME = :SEC-USR-FNAME,
                   USR_LNAME = :SEC-USR-LNAME,
                   USR_PWD   = :SEC-USR-PWD,
                   USR_TYPE  = :SEC-USR-TYPE
               WHERE USR_ID = :SEC-USR-ID
           END-EXEC.

           MOVE SQLCODE TO WS-SQL-STATUS

           EVALUATE WS-SQL-STATUS
               WHEN 0
                   EXEC SQL COMMIT END-EXEC
                   MOVE SPACES TO LK-OUT-MESSAGE
                   STRING 'User ' DELIMITED BY SIZE
                          SEC-USR-ID DELIMITED BY SPACE
                     ' has been updated successfully' DELIMITED BY SIZE
                     INTO LK-OUT-MESSAGE
               WHEN +100
                   EXEC SQL ROLLBACK END-EXEC
                   SET ERR-FLG-ON TO TRUE
                   SET RC-NOT-FOUND TO TRUE
                   MOVE 'User ID not found' TO LK-OUT-MESSAGE
               WHEN OTHER
                   EXEC SQL ROLLBACK END-EXEC
                   SET ERR-FLG-ON TO TRUE
                   SET RC-DATABASE-ERROR TO TRUE
                   MOVE 'Unable to update User' TO LK-OUT-MESSAGE
           END-EVALUATE.