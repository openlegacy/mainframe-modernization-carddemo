******************************************************************
      * Program     : COCRDUPL.CBL
      * Application : CardDemo
      * Type        : COBOL RPC Program
      * Function    : Update/Lookup card in CARDDAT file for API integration
      * Description : Receives card details and updates the card record
      *               or performs lookup if only Card ID provided
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
       PROGRAM-ID.     COCRDUPL.
       AUTHOR.     AWS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
      *----------------------------------------------------------------*
      *                     WORKING STORAGE SECTION
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

       01 WS-VARIABLES.
         05 WS-PGMNAME                 PIC X(08) VALUE 'COCRDUPL'.
         05 WS-CARDDAT-FILE            PIC X(08) VALUE 'CARDDAT '.
         05 WS-CARDDAT-FILE-AIX        PIC X(08) VALUE 'CARDAIX '.
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
      *      File and data Handling
      ******************************************************************
         05  WS-CARD-RID.
           10  WS-CARD-RID-CARDNUM                 PIC X(16).
           10  WS-CARD-RID-ACCT-ID                 PIC 9(11).
           10  WS-CARD-RID-ACCT-ID-X REDEFINES
                  WS-CARD-RID-ACCT-ID              PIC X(11).
         05  WS-FILE-ERROR-MESSAGE.
           10  FILLER                              PIC X(12)
                                                   VALUE 'File Error: '.
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
          10  FILLER                               PIC X(5)
                                                   VALUE SPACES.

      ******************************************************************
      *      Literals and Constants
      ******************************************************************
       01 WS-LITERALS.
          05 LIT-UPPER                             PIC X(26)
                                 VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
          05 LIT-LOWER                             PIC X(26)
                                 VALUE 'abcdefghijklmnopqrstuvwxyz'.

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
           05  LK-OLD-CARD.
               10  LK-OLD-CVV-CD          PIC X(03).
               10  LK-OLD-CARD-NAME       PIC X(50).
               10  LK-OLD-EXPIRY-YEAR     PIC X(04).
               10  LK-OLD-EXPIRY-MONTH    PIC X(02).
               10  LK-OLD-EXPIRY-DAY      PIC X(02).
               10  LK-OLD-CARD-STATUS     PIC X(01).
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
           END-EVALUATE.


           EXEC CICS RETURN
           END-EXEC.


      *----------------------------------------------------------------*
      *                      PROCESS-LOOKUP
      *----------------------------------------------------------------*
       PROCESS-LOOKUP.

      *    Validate input - must have EITHER account-id OR card number
           IF (LK-IN-CARD-NUM = SPACES OR LOW-VALUES)
           AND (LK-IN-ACCT-ID = SPACES OR LOW-VALUES)
               SET RC-VALIDATION-ERROR TO TRUE
               MOVE 'Account ID or Card number must be provided'
                    TO LK-OUT-MESSAGE
               GO TO PROCESS-LOOKUP-EXIT
           END-IF

      *    Determine which lookup method to use
           EVALUATE TRUE
               WHEN LK-IN-CARD-NUM NOT = SPACES
               AND  LK-IN-CARD-NUM NOT = LOW-VALUES
      *            Card number provided - use primary key
                   MOVE LK-IN-CARD-NUM TO WS-CARD-RID-CARDNUM
                   PERFORM READ-CARD-BY-CARDNUM

               WHEN LK-IN-ACCT-ID NOT = SPACES
               AND  LK-IN-ACCT-ID NOT = LOW-VALUES
      *            Account ID provided - use alternate index
                   MOVE LK-IN-ACCT-ID TO WS-CARD-RID-ACCT-ID-X
                   PERFORM READ-CARD-BY-ACCTID
           END-EVALUATE

           IF NOT ERR-FLG-ON
      * Return card data in the output fields
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
           END-IF.
       
       PROCESS-LOOKUP-EXIT.
           EXIT.

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
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           IF NOT ERR-FLG-ON
               MOVE LK-IN-CARD-NUM TO WS-CARD-RID-CARDNUM
               PERFORM READ-CARD-FILE-UPDATE

               IF NOT ERR-FLG-ON
                   PERFORM CHECK-FOR-CHANGES
                   IF CARD-MODIFIED-YES
                       PERFORM UPDATE-CARD-FILE  
                          THRU UPDATE-CARD-FILE-EXIT
                   ELSE
                       SET RC-NO-CHANGES TO TRUE
                       MOVE 'No changes detected' TO LK-OUT-MESSAGE
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      *                      READ-CARD-BY-CARDNUM
      *----------------------------------------------------------------*
       READ-CARD-BY-CARDNUM.
           EXEC CICS READ
                FILE      (WS-CARDDAT-FILE)
                RIDFLD    (WS-CARD-RID-CARDNUM)
                KEYLENGTH (LENGTH OF WS-CARD-RID-CARDNUM)
                INTO      (CARD-RECORD)
                LENGTH    (LENGTH OF CARD-RECORD)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   SET ERR-FLG-ON TO TRUE
                   SET RC-NOT-FOUND TO TRUE
                   MOVE 'Card not found' TO LK-OUT-MESSAGE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   SET RC-DATABASE-ERROR TO TRUE
                   MOVE 'Unable to lookup card' TO LK-OUT-MESSAGE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      READ-CARD-BY-ACCTID
      *----------------------------------------------------------------*
       READ-CARD-BY-ACCTID.
           EXEC CICS READ
                FILE      (WS-CARDDAT-FILE-AIX)
                RIDFLD    (WS-CARD-RID-ACCT-ID-X)
                KEYLENGTH (LENGTH OF WS-CARD-RID-ACCT-ID-X)
                INTO      (CARD-RECORD)
                LENGTH    (LENGTH OF CARD-RECORD)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   SET ERR-FLG-ON TO TRUE
                   SET RC-NOT-FOUND TO TRUE
                   MOVE 'Card not found for this account'
                        TO LK-OUT-MESSAGE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   SET RC-DATABASE-ERROR TO TRUE
                   MOVE 'Unable to lookup card by account'
                        TO LK-OUT-MESSAGE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      READ-CARD-FILE-UPDATE
      *----------------------------------------------------------------*
       READ-CARD-FILE-UPDATE.
           EXEC CICS READ
                FILE      (WS-CARDDAT-FILE)
                UPDATE
                RIDFLD    (WS-CARD-RID-CARDNUM)
                KEYLENGTH (LENGTH OF WS-CARD-RID-CARDNUM)
                INTO      (CARD-RECORD)
                LENGTH    (LENGTH OF CARD-RECORD)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
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
      *                      UPDATE-CARD-FILE
      *----------------------------------------------------------------*
       UPDATE-CARD-FILE.

      * Check if someone changed the record while we were out
           PERFORM CHECK-CHANGE-IN-REC
           IF RC-DATA-CHANGED
               GO TO UPDATE-CARD-FILE-EXIT
           END-IF

      * Prepare the update
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

           EXEC CICS
                REWRITE FILE(WS-CARDDAT-FILE)
                        FROM(CARD-UPDATE-RECORD)
                        LENGTH(LENGTH OF CARD-UPDATE-RECORD)
                        RESP      (WS-RESP-CD)
                        RESP2     (WS-REAS-CD)
           END-EXEC

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   MOVE SPACES TO LK-OUT-MESSAGE
                   STRING 'Card ' DELIMITED BY SIZE
                          LK-IN-CARD-NUM DELIMITED BY SPACE
                     ' has been updated successfully' DELIMITED BY SIZE
                     INTO LK-OUT-MESSAGE
               WHEN DFHRESP(NOTFND)
                   SET ERR-FLG-ON TO TRUE
                   SET RC-NOT-FOUND TO TRUE
                   MOVE 'Card not found' TO LK-OUT-MESSAGE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   SET RC-DATABASE-ERROR TO TRUE
                   MOVE 'Unable to update card' TO LK-OUT-MESSAGE
           END-EVALUATE.

       UPDATE-CARD-FILE-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *                      CHECK-CHANGE-IN-REC
      *----------------------------------------------------------------*
       CHECK-CHANGE-IN-REC.
           INSPECT CARD-EMBOSSED-NAME
           CONVERTING LIT-LOWER
                   TO LIT-UPPER

           IF  CARD-CVV-CD              EQUAL  TO LK-OLD-CVV-CD
           AND CARD-EMBOSSED-NAME       EQUAL  TO LK-OLD-CARD-NAME
           AND CARD-EXPIRAION-DATE(1:4) EQUAL  TO LK-OLD-EXPIRY-YEAR
           AND CARD-EXPIRAION-DATE(6:2) EQUAL  TO LK-OLD-EXPIRY-MONTH
           AND CARD-EXPIRAION-DATE(9:2) EQUAL  TO LK-OLD-EXPIRY-DAY
           AND CARD-ACTIVE-STATUS       EQUAL  TO LK-OLD-CARD-STATUS
               CONTINUE
           ELSE
               SET RC-DATA-CHANGED TO TRUE
               MOVE 'Record changed by someone else. Please review'
                    TO LK-OUT-MESSAGE
      * Return current values for display
               MOVE CARD-NUM           TO LK-OUT-CARD-NUM
               MOVE CARD-ACCT-ID       TO LK-OUT-ACCT-ID
               MOVE CARD-CVV-CD        TO LK-OUT-CVV-CD
               MOVE CARD-EMBOSSED-NAME TO LK-OUT-CARD-NAME
               MOVE CARD-EXPIRAION-DATE(1:4) TO LK-OUT-EXPIRY-YEAR
               MOVE CARD-EXPIRAION-DATE(6:2) TO LK-OUT-EXPIRY-MONTH
               MOVE CARD-EXPIRAION-DATE(9:2) TO LK-OUT-EXPIRY-DAY
               MOVE CARD-ACTIVE-STATUS TO LK-OUT-CARD-STATUS
           END-IF.
        
           
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:12:33 CDT
      *