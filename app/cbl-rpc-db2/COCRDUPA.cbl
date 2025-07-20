******************************************************************
      * Program     : COCRDUPA.CBL
      * Application : CardDemo
      * Type        : COBOL RPC Program
      * Function    : Update/Lookup card in CARDDAT table for API integration
      * Description : Receives card details and updates the card record
      *               or performs lookup if only Card ID provided
      *               (DB2 Version)
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
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     COCRDUPA.
       AUTHOR.     AWS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
      *----------------------------------------------------------------*
      *                     WORKING STORAGE SECTION
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------*
      *                     DB2 SQL COMMUNICATION AREA
      *----------------------------------------------------------------*
       EXEC SQL
           INCLUDE SQLCA
       END-EXEC.

       01 WS-VARIABLES.
         05 WS-PGMNAME                 PIC X(08) VALUE 'COCRDUPA'.
         05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.
           88 ERR-FLG-ON                         VALUE 'Y'.
           88 ERR-FLG-OFF                        VALUE 'N'.
         05 WS-RESP-CD                 PIC S9(09) COMP VALUE ZEROS.
         05 WS-REAS-CD                 PIC S9(09) COMP VALUE ZEROS.
         05 WS-CARD-MODIFIED           PIC X(01) VALUE 'N'.
           88 CARD-MODIFIED-YES                  VALUE 'Y'.
           88 CARD-MODIFIED-NO                   VALUE 'N'.
         05 WS-OPERATION               PIC X(01) VALUE SPACES.
           88 OPERATION-LOOKUP                   VALUE 'L'.
           88 OPERATION-UPDATE                   VALUE 'U'.

      ******************************************************************
      * DB2 Related Variables
      ******************************************************************
         05 WS-DB2-VARS.
            07 WS-SQLCODE                          PIC S9(09) COMP
                                                   VALUE ZEROS.

      ******************************************************************
      *      DB2 Host Variables for Card Operations
      ******************************************************************
         05  WS-CARD-HOST-VARS.
           10  HV-CARD-NUM                         PIC X(16).
           10  HV-CARD-ACCT-ID                     PIC S9(11) COMP-3.
           10  HV-CARD-CVV-CD                      PIC S9(03) COMP-3.
           10  HV-CARD-EMBOSSED-NAME               PIC X(50).
           10  HV-CARD-EXPIRY-DATE                 PIC X(10).
           10  HV-CARD-ACTIVE-STATUS               PIC X(01).

      ******************************************************************
      *      Original Host Variables for Comparison
      ******************************************************************
         05  WS-ORIGINAL-CARD-HOST-VARS.
           10  HV-ORIG-CARD-EMBOSSED-NAME          PIC X(50).
           10  HV-ORIG-CARD-EXPIRY-DATE            PIC X(10).
           10  HV-ORIG-CARD-ACTIVE-STATUS          PIC X(01).

      ******************************************************************
      *      Error Message Handling
      ******************************************************************
         05  WS-DB2-ERROR-MESSAGE.
           10  FILLER                              PIC X(12)
                                                   VALUE 'DB2 Error: '.
           10  ERROR-OPNAME                        PIC X(8)
                                                   VALUE SPACES.
           10  FILLER                              PIC X(4)
                                                   VALUE ' on '.
           10  ERROR-TABLE                         PIC X(9)
                                                   VALUE SPACES.
           10  FILLER                              PIC X(9)
                                                   VALUE
                                                   ' SQLCODE '.
           10  ERROR-SQLCODE                       PIC X(10)
                                                   VALUE SPACES.
           10  FILLER                              PIC X(5)
                                                   VALUE SPACES.

      ******************************************************************
      *      Literals and Constants
      ******************************************************************
       01 WS-LITERALS.
          05 LIT-CARDTABLENAME                     PIC X(8)
                                                   VALUE 'CARDDAT '.
          05 LIT-UPPER                             PIC X(26)
                                 VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
          05 LIT-LOWER                             PIC X(26)
                                 VALUE 'abcdefghijklmnopqrstuvwxyz'.

       01 WS-DATE-STRING                           PIC X(10).

      * Card Record Structure
       01  CARD-RECORD.
           05  CARD-NUM                          PIC X(16).
           05  CARD-ACCT-ID                      PIC 9(11).
           05  CARD-CVV-CD                       PIC 9(03).
           05  CARD-EMBOSSED-NAME                PIC X(50).
           05  CARD-EXPIRAION-DATE               PIC X(10).
           05  CARD-ACTIVE-STATUS                PIC X(01).
           05  FILLER                            PIC X(59).

       01 CARD-UPDATE-RECORD.
           10 CARD-UPDATE-NUM                   PIC X(16).
           10 CARD-UPDATE-ACCT-ID               PIC 9(11).
           10 CARD-UPDATE-CVV-CD                PIC 9(03).
           10 CARD-UPDATE-EMBOSSED-NAME         PIC X(50).
           10 CARD-UPDATE-EXPIRAION-DATE        PIC X(10).
           10 CARD-UPDATE-ACTIVE-STATUS         PIC X(01).
           10 FILLER                            PIC X(59).

      *----------------------------------------------------------------*
      *                        LINKAGE SECTION
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01  DFHCOMMAREA.
           05  LK-OPERATION               PIC X(01).
               88  LK-OP-LOOKUP           VALUE 'L'.
               88  LK-OP-UPDATE           VALUE 'U'.
           05  LK-INPUT-CARD.
               10  LK-IN-CARD-NUM         PIC X(16).
               10  LK-IN-ACCT-ID          PIC X(11).
               10  LK-IN-CVV-CD           PIC X(03).
               10  LK-IN-CARD-NAME        PIC X(50).
               10  LK-IN-EXPIRY-YEAR      PIC X(04).
               10  LK-IN-EXPIRY-MONTH     PIC X(02).
               10  LK-IN-EXPIRY-DAY       PIC X(02).
               10  LK-IN-CARD-STATUS      PIC X(01).
           05  LK-OUTPUT-STATUS.
               10  LK-OUT-RETURN-CODE     PIC 9(02).
                   88  RC-SUCCESS         VALUE 00.
                   88  RC-NOT-FOUND       VALUE 01.
                   88  RC-NO-CHANGES      VALUE 02.
                   88  RC-VALIDATION-ERROR VALUE 10.
                   88  RC-LOCK-ERROR      VALUE 11.
                   88  RC-DATA-CHANGED    VALUE 12.
                   88  RC-DATABASE-ERROR  VALUE 99.
               10  LK-OUT-MESSAGE         PIC X(80).
           05  LK-OUTPUT-CARD.
               10  LK-OUT-CARD-NUM        PIC X(16).
               10  LK-OUT-ACCT-ID         PIC X(11).
               10  LK-OUT-CVV-CD          PIC X(03).
               10  LK-OUT-CARD-NAME       PIC X(50).
               10  LK-OUT-EXPIRY-YEAR     PIC X(04).
               10  LK-OUT-EXPIRY-MONTH    PIC X(02).
               10  LK-OUT-EXPIRY-DAY      PIC X(02).
               10  LK-OUT-CARD-STATUS     PIC X(01).

      *----------------------------------------------------------------*
      *                       PROCEDURE DIVISION
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

       MAIN-PARA.
           INITIALIZE LK-OUTPUT-STATUS
                      LK-OUTPUT-CARD

           SET ERR-FLG-OFF TO TRUE
           SET CARD-MODIFIED-NO TO TRUE
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

           IF LK-IN-CARD-NUM = SPACES OR LOW-VALUES
               SET RC-VALIDATION-ERROR TO TRUE
               MOVE 'Card number cannot be empty for lookup'
                    TO LK-OUT-MESSAGE
           ELSE
               MOVE LK-IN-CARD-NUM TO HV-CARD-NUM
               PERFORM READ-CARD-DB2-LOOKUP

               IF NOT ERR-FLG-ON
      * Return card data in the output fields for screen display
                   MOVE CARD-NUM           TO LK-OUT-CARD-NUM
                   MOVE CARD-ACCT-ID       TO LK-OUT-ACCT-ID
                   MOVE CARD-CVV-CD        TO LK-OUT-CVV-CD
                   MOVE CARD-EMBOSSED-NAME TO LK-OUT-CARD-NAME
                   MOVE CARD-EXPIRAION-DATE(1:4) TO LK-OUT-EXPIRY-YEAR
                   MOVE CARD-EXPIRAION-DATE(6:2) TO LK-OUT-EXPIRY-MONTH
                   MOVE CARD-EXPIRAION-DATE(9:2) TO LK-OUT-EXPIRY-DAY
                   MOVE CARD-ACTIVE-STATUS TO LK-OUT-CARD-STATUS
                   MOVE 'Card data retrieved successfully'
                        TO LK-OUT-MESSAGE

                   INSPECT LK-OUT-CARD-NAME
                   CONVERTING LIT-LOWER
                           TO LIT-UPPER
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      *                      PROCESS-UPDATE
      *----------------------------------------------------------------*
       PROCESS-UPDATE.

      * Validate all required fields for update
           EVALUATE TRUE
               WHEN LK-IN-CARD-NUM = SPACES OR LOW-VALUES
                   SET RC-VALIDATION-ERROR TO TRUE
                   MOVE 'Card number cannot be empty' TO LK-OUT-MESSAGE
               WHEN LK-IN-ACCT-ID = SPACES OR LOW-VALUES
                   SET RC-VALIDATION-ERROR TO TRUE
                   MOVE 'Account ID cannot be empty' TO LK-OUT-MESSAGE
               WHEN LK-IN-CARD-NAME = SPACES OR LOW-VALUES
                   SET RC-VALIDATION-ERROR TO TRUE
                   MOVE 'Card name cannot be empty' TO LK-OUT-MESSAGE
               WHEN LK-IN-EXPIRY-YEAR = SPACES OR LOW-VALUES
                   SET RC-VALIDATION-ERROR TO TRUE
                   MOVE 'Expiry year cannot be empty' TO LK-OUT-MESSAGE
               WHEN LK-IN-EXPIRY-MONTH = SPACES OR LOW-VALUES
                   SET RC-VALIDATION-ERROR TO TRUE
                   MOVE 'Expiry month cannot be empty' TO LK-OUT-MESSAGE
               WHEN LK-IN-CARD-STATUS = SPACES OR LOW-VALUES
                   SET RC-VALIDATION-ERROR TO TRUE
                   MOVE 'Card status cannot be empty' TO LK-OUT-MESSAGE
           END-EVALUATE

           IF NOT ERR-FLG-ON
               MOVE LK-IN-CARD-NUM TO HV-CARD-NUM
               PERFORM READ-CARD-DB2-FOR-UPDATE

               IF NOT ERR-FLG-ON
                   PERFORM CHECK-FOR-CHANGES
                   IF CARD-MODIFIED-YES
                       PERFORM UPDATE-CARD-DB2
                   ELSE
                       SET RC-NO-CHANGES TO TRUE
                       MOVE 'No changes detected' TO LK-OUT-MESSAGE
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      *                      READ-CARD-DB2-LOOKUP
      *----------------------------------------------------------------*
       READ-CARD-DB2-LOOKUP.
      *    Read the Card table using DB2
           EXEC SQL
                SELECT CARD_NUM,
                       CARD_ACCT_ID,
                       CARD_CVV_CD,
                       CARD_EMBOSSED_NAME,
                       CARD_EXPIRY_DATE,
                       CARD_ACTIVE_STATUS
                INTO   :HV-CARD-NUM,
                       :HV-CARD-ACCT-ID,
                       :HV-CARD-CVV-CD,
                       :HV-CARD-EMBOSSED-NAME,
                       :HV-CARD-EXPIRY-DATE,
                       :HV-CARD-ACTIVE-STATUS
                FROM   CARDDAT
                WHERE  CARD_NUM = :HV-CARD-NUM
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
      * Move data from host variables to CARD-RECORD structure
                   MOVE HV-CARD-NUM         TO CARD-NUM
                   MOVE HV-CARD-ACCT-ID     TO CARD-ACCT-ID
                   MOVE HV-CARD-CVV-CD      TO CARD-CVV-CD
                   MOVE HV-CARD-EMBOSSED-NAME TO CARD-EMBOSSED-NAME
                   MOVE HV-CARD-EXPIRY-DATE TO CARD-EXPIRAION-DATE
                   MOVE HV-CARD-ACTIVE-STATUS TO CARD-ACTIVE-STATUS
               WHEN +100
                   SET ERR-FLG-ON TO TRUE
                   SET RC-NOT-FOUND TO TRUE
                   MOVE 'Card not found' TO LK-OUT-MESSAGE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   SET RC-DATABASE-ERROR TO TRUE
                   MOVE 'SELECT'                      TO ERROR-OPNAME
                   MOVE LIT-CARDTABLENAME             TO ERROR-TABLE
                   MOVE SQLCODE                       TO ERROR-SQLCODE
                   MOVE WS-DB2-ERROR-MESSAGE          TO LK-OUT-MESSAGE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      READ-CARD-DB2-FOR-UPDATE
      *----------------------------------------------------------------*
       READ-CARD-DB2-FOR-UPDATE.
      *    Read the Card table using DB2 for update
           EXEC SQL
                SELECT CARD_NUM,
                       CARD_ACCT_ID,
                       CARD_CVV_CD,
                       CARD_EMBOSSED_NAME,
                       CARD_EXPIRY_DATE,
                       CARD_ACTIVE_STATUS
                INTO   :HV-CARD-NUM,
                       :HV-CARD-ACCT-ID,
                       :HV-CARD-CVV-CD,
                       :HV-CARD-EMBOSSED-NAME,
                       :HV-CARD-EXPIRY-DATE,
                       :HV-CARD-ACTIVE-STATUS
                FROM   CARDDAT
                WHERE  CARD_NUM = :HV-CARD-NUM
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
      * Move data from host variables to CARD-RECORD structure
                   MOVE HV-CARD-NUM         TO CARD-NUM
                   MOVE HV-CARD-ACCT-ID     TO CARD-ACCT-ID
                   MOVE HV-CARD-CVV-CD      TO CARD-CVV-CD
                   MOVE HV-CARD-EMBOSSED-NAME TO CARD-EMBOSSED-NAME
                   MOVE HV-CARD-EXPIRY-DATE TO CARD-EXPIRAION-DATE
                   MOVE HV-CARD-ACTIVE-STATUS TO CARD-ACTIVE-STATUS

      * Store original values for optimistic locking check
                   MOVE HV-CARD-EMBOSSED-NAME
                   TO HV-ORIG-CARD-EMBOSSED-NAME
                   MOVE HV-CARD-EXPIRY-DATE
                   TO HV-ORIG-CARD-EXPIRY-DATE
                   MOVE HV-CARD-ACTIVE-STATUS TO
                   HV-ORIG-CARD-ACTIVE-STATUS
               WHEN +100
                   SET ERR-FLG-ON TO TRUE
                   SET RC-NOT-FOUND TO TRUE
                   MOVE 'Card not found' TO LK-OUT-MESSAGE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   SET RC-LOCK-ERROR TO TRUE
                   MOVE 'Unable to lock card for update'
                   TO LK-OUT-MESSAGE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      CHECK-FOR-CHANGES
      *----------------------------------------------------------------*
       CHECK-FOR-CHANGES.

           INSPECT CARD-EMBOSSED-NAME
           CONVERTING LIT-LOWER
                   TO LIT-UPPER

           IF  CARD-EMBOSSED-NAME       NOT = LK-IN-CARD-NAME
           OR  CARD-EXPIRAION-DATE(1:4) NOT = LK-IN-EXPIRY-YEAR
           OR  CARD-EXPIRAION-DATE(6:2) NOT = LK-IN-EXPIRY-MONTH
           OR  CARD-EXPIRAION-DATE(9:2) NOT = LK-IN-EXPIRY-DAY
           OR  CARD-ACTIVE-STATUS       NOT = LK-IN-CARD-STATUS
               SET CARD-MODIFIED-YES TO TRUE
           END-IF.

      *----------------------------------------------------------------*
      *                      UPDATE-CARD-DB2
      *----------------------------------------------------------------*
       UPDATE-CARD-DB2.

      * Prepare the update data using CARD-RECORD structure
           INITIALIZE CARD-UPDATE-RECORD
           MOVE LK-IN-CARD-NUM          TO CARD-UPDATE-NUM
           MOVE LK-IN-ACCT-ID           TO CARD-UPDATE-ACCT-ID
           MOVE LK-IN-CVV-CD            TO CARD-UPDATE-CVV-CD
           MOVE LK-IN-CARD-NAME         TO CARD-UPDATE-EMBOSSED-NAME
           STRING  LK-IN-EXPIRY-YEAR
                   '-'
                   LK-IN-EXPIRY-MONTH
                   '-'
                   LK-IN-EXPIRY-DAY
                   DELIMITED BY SIZE
              INTO CARD-UPDATE-EXPIRAION-DATE
           END-STRING
           MOVE LK-IN-CARD-STATUS       TO CARD-UPDATE-ACTIVE-STATUS

      * Move CARD-RECORD to host variables for update
           MOVE CARD-UPDATE-NUM             TO HV-CARD-NUM
           MOVE CARD-UPDATE-ACCT-ID         TO HV-CARD-ACCT-ID
           MOVE CARD-UPDATE-CVV-CD          TO HV-CARD-CVV-CD
           MOVE CARD-UPDATE-EMBOSSED-NAME   TO HV-CARD-EMBOSSED-NAME
           MOVE CARD-UPDATE-EXPIRAION-DATE  TO HV-CARD-EXPIRY-DATE
           MOVE CARD-UPDATE-ACTIVE-STATUS   TO HV-CARD-ACTIVE-STATUS

      *****************************************************************
      * Perform optimistic update with original values check
      *****************************************************************
           EXEC SQL
                UPDATE CARDDAT
                SET    CARD_EMBOSSED_NAME = :HV-CARD-EMBOSSED-NAME,
                       CARD_EXPIRY_DATE = :HV-CARD-EXPIRY-DATE,
                       CARD_ACTIVE_STATUS = :HV-CARD-ACTIVE-STATUS
                WHERE  CARD_NUM = :HV-CARD-NUM
                AND    CARD_EMBOSSED_NAME = :HV-ORIG-CARD-EMBOSSED-NAME
                AND    CARD_EXPIRY_DATE = :HV-ORIG-CARD-EXPIRY-DATE
                AND    CARD_ACTIVE_STATUS = :HV-ORIG-CARD-ACTIVE-STATUS
           END-EXEC

      *****************************************************************
      * Check if update was successful
      *****************************************************************
           EVALUATE SQLCODE
               WHEN 0
                   EXEC SQL COMMIT END-EXEC
                   MOVE SPACES TO LK-OUT-MESSAGE
                   STRING 'Card ' DELIMITED BY SIZE
                          LK-IN-CARD-NUM DELIMITED BY SPACE
                     ' has been updated successfully' DELIMITED BY SIZE
                     INTO LK-OUT-MESSAGE
               WHEN +100
      *            No rows updated - record was changed by someone else
                   SET RC-DATA-CHANGED TO TRUE
                   MOVE 'Record changed by someone else. Please review'
                        TO LK-OUT-MESSAGE
      * Return current values for display - re-read the record
                   MOVE LK-IN-CARD-NUM TO HV-CARD-NUM
                   PERFORM READ-CARD-DB2-LOOKUP
                   IF NOT ERR-FLG-ON
                       MOVE CARD-NUM           TO LK-OUT-CARD-NUM
                       MOVE CARD-ACCT-ID       TO LK-OUT-ACCT-ID
                       MOVE CARD-CVV-CD        TO LK-OUT-CVV-CD
                       MOVE CARD-EMBOSSED-NAME TO LK-OUT-CARD-NAME
                       MOVE CARD-EXPIRAION-DATE(1:4)
                       TO LK-OUT-EXPIRY-YEAR
                       MOVE CARD-EXPIRAION-DATE(6:2)
                       TO LK-OUT-EXPIRY-MONTH
                       MOVE CARD-EXPIRAION-DATE(9:2)
                       TO LK-OUT-EXPIRY-DAY
                       MOVE CARD-ACTIVE-STATUS TO LK-OUT-CARD-STATUS
                   END-IF
                   EXEC SQL ROLLBACK END-EXEC
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   SET RC-DATABASE-ERROR TO TRUE
                   MOVE 'Unable to update card' TO LK-OUT-MESSAGE
                   EXEC SQL ROLLBACK END-EXEC
           END-EVALUATE.

       UPDATE-CARD-DB2-EXIT.
           EXIT.

      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:12:33 CDT
      *