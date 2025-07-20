      *****************************************************************
      * Program     : COUSR01A.CBL
      * Application : CardDemo
      * Type        : CICS COBOL RPC Program
      * Function    : Add a new Regular/Admin user to USERSEC DB2 table
      ******************************************************************
      * Copyright Amazon.com, Inc. or its affiliates.
      * All Rights Reserved.
      *
      * Licensed under the Apache License, Version 2.0 (the "License").
      * You may not use this file except in compliance with the License.
      * You may obtain a copy of the License at
      *
      *    http://www.apache.org/licenses/LICENSE-2.0
      *
      * Unless required by applicable law or agreed to in writing,
      * software distributed under the License is distributed on an
      * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
      * either express or implied. See the License for the specific
      * language governing permissions and limitations under the License
      ******************************************************************
      * Modification History
      * 2025-05-09 - Converted from screen-based program COUSR01C
      *              to RPC program for API integration
      * 2025-07-15 - Converted from VSAM RPC program COUSR01L
      *              to DB2 RPC program for DB2 integration
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COUSR01A.
       AUTHOR.     AWS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
      *----------------------------------------------------------------*
      *                     WORKING STORAGE SECTION
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

       01 WS-VARIABLES.
         05 WS-PGMNAME                 PIC X(08) VALUE 'COUSR01A'.
         05 WS-RESP-CD                 PIC S9(09) COMP VALUE ZEROS.
         05 WS-REAS-CD                 PIC S9(09) COMP VALUE ZEROS.
         05 WS-SQLCODE-DISP            PIC 9(05) VALUE ZEROS.
         05 WS-RESP-MSG                PIC X(80) VALUE SPACES.

         05 WS-RESP-CD-DISP            PIC 9(09) VALUE ZEROS.
         05 WS-REAS-CD-DISP            PIC 9(09) VALUE ZEROS.
         05 WS-RPC-RESP-CD             PIC S9(04) COMP VALUE ZEROS.
           88 RPC-RESP-OK                        VALUE 0.
           88 RPC-RESP-EMPTY-FNAME               VALUE 1.
           88 RPC-RESP-EMPTY-LNAME               VALUE 2.
           88 RPC-RESP-EMPTY-USERID              VALUE 3.
           88 RPC-RESP-EMPTY-PASSWORD            VALUE 4.
           88 RPC-RESP-EMPTY-USERTYPE            VALUE 5.
           88 RPC-RESP-DUPLICATE-USER            VALUE 6.
           88 RPC-RESP-WRITE-ERROR               VALUE 7.

      *----------------------------------------------------------------*
      *                     DB2 SQL COMMUNICATION AREA
      *----------------------------------------------------------------*
       EXEC SQL
           INCLUDE SQLCA
       END-EXEC.

       COPY CSUSR01Y.

      *----------------------------------------------------------------*
      *                        LINKAGE SECTION
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01 DFHCOMMAREA.
          05 LK-INPUT-PARMS.
             10 LK-USER-FNAME             PIC X(20).
             10 LK-USER-LNAME             PIC X(20).
             10 LK-USER-ID                PIC X(08).
             10 LK-USER-PASSWORD          PIC X(08).
             10 LK-USER-TYPE              PIC X(01).
                88 LK-USRTYP-ADMIN        VALUE 'A'.
                88 LK-USRTYP-USER         VALUE 'U'.

          05 LK-OUTPUT-PARMS.
             10 LK-RESP-CODE              PIC S9(04) COMP.
             10 LK-RESP-MSG               PIC X(80).

      *----------------------------------------------------------------*
      *                      PROCEDURE DIVISION
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

       MAIN-PARA.

           MOVE ZEROS TO WS-RPC-RESP-CD
           MOVE SPACES TO WS-RESP-MSG
           MOVE ZEROS TO WS-RESP-CD
           MOVE ZEROS TO WS-REAS-CD

           PERFORM PROCESS-INPUT-PARMS
           MOVE WS-RPC-RESP-CD TO LK-RESP-CODE
           MOVE WS-RESP-MSG TO LK-RESP-MSG

           GOBACK.

      *----------------------------------------------------------------*
      *                      PROCESS-INPUT-PARMS -> Enter
      *----------------------------------------------------------------*
       PROCESS-INPUT-PARMS.

           EVALUATE TRUE
               WHEN LK-USER-FNAME = SPACES OR LOW-VALUES
                   SET RPC-RESP-EMPTY-FNAME TO TRUE
                   MOVE 'First Name cannot be empty' TO WS-RESP-MSG
               WHEN LK-USER-LNAME = SPACES OR LOW-VALUES
                   SET RPC-RESP-EMPTY-LNAME TO TRUE
                   MOVE 'Last Name cannot be empty' TO WS-RESP-MSG
               WHEN LK-USER-ID = SPACES OR LOW-VALUES
                   SET RPC-RESP-EMPTY-USERID TO TRUE
                   MOVE 'User ID cannot be empty' TO WS-RESP-MSG
               WHEN LK-USER-PASSWORD = SPACES OR LOW-VALUES
                   SET RPC-RESP-EMPTY-PASSWORD TO TRUE
                   MOVE 'Password cannot be empty' TO WS-RESP-MSG
               WHEN LK-USER-TYPE = SPACES OR LOW-VALUES
                   SET RPC-RESP-EMPTY-USERTYPE TO TRUE
                   MOVE 'User Type cannot be empty' TO WS-RESP-MSG
               WHEN LK-USER-TYPE NOT = 'A' AND LK-USER-TYPE NOT = 'U'
                   SET RPC-RESP-EMPTY-USERTYPE TO TRUE
                   MOVE 'User Type must be A or U' TO WS-RESP-MSG
           END-EVALUATE.

           IF RPC-RESP-OK
              MOVE LK-USER-ID TO SEC-USR-ID
              MOVE LK-USER-FNAME TO SEC-USR-FNAME
              MOVE LK-USER-LNAME TO SEC-USR-LNAME
              MOVE LK-USER-PASSWORD TO SEC-USR-PWD
              MOVE LK-USER-TYPE TO SEC-USR-TYPE
              MOVE SPACES TO SEC-USR-FILLER
              PERFORM INSERT-USER-DB2-TABLE
           END-IF.

      *----------------------------------------------------------------*
      *                      INSERT-USER-DB2-TABLE
      *----------------------------------------------------------------*
       INSERT-USER-DB2-TABLE.

           EXEC SQL
               INSERT INTO USERSEC
               (USR_ID, USR_FNAME, USR_LNAME, USR_PWD, USR_TYPE)
               VALUES
               (:SEC-USR-ID, :SEC-USR-FNAME, :SEC-USR-LNAME,
                :SEC-USR-PWD, :SEC-USR-TYPE)
           END-EXEC.


           EVALUATE SQLCODE
               WHEN 0
                   SET RPC-RESP-OK TO TRUE
                   EXEC SQL COMMIT END-EXEC
                   STRING 'User '     DELIMITED BY SIZE
                          SEC-USR-ID  DELIMITED BY SPACE
                          ' has been added successfully'
                           DELIMITED BY SIZE
                     INTO WS-RESP-MSG
               WHEN -803
               WHEN -1
                   SET RPC-RESP-DUPLICATE-USER TO TRUE
                   EXEC SQL ROLLBACK END-EXEC
                   STRING 'User ID '  DELIMITED BY SIZE
                          SEC-USR-ID  DELIMITED BY SPACE
                          ' already exists...' DELIMITED BY SIZE
                     INTO WS-RESP-MSG
               WHEN OTHER
                   MOVE SQLCODE TO WS-SQLCODE-DISP
                   SET RPC-RESP-WRITE-ERROR TO TRUE
                   EXEC SQL ROLLBACK END-EXEC
                   STRING 'Error adding user. SQLCODE: '
                   DELIMITED BY SIZE
                   WS-SQLCODE-DISP
                   DELIMITED BY SIZE
                     INTO WS-RESP-MSG
           END-EVALUATE.
