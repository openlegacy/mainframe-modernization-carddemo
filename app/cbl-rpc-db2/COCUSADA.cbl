******************************************************************
      * Program:     COCUSADA.CBL - TARGET CUSTOMER ID SUPPORT       *
      * Layer:       Business logic                                   *
      * Function:    RPC Service for Customer Creation (DB2)          *
      * Description: CREATE CUSTOMER DURING ACCOUNT CREATION          *
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
       PROGRAM-ID.
           COCUSADA.
       DATE-WRITTEN.
           August 2025.
       DATE-COMPILED.
           Today.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01  WS-MISC-STORAGE.
      ******************************************************************
      * General CICS related
      ******************************************************************
         05 WS-CICS-PROCESSNG-VARS.
            07 WS-RESP-CD                          PIC S9(09) COMP
                                                   VALUE ZEROS.
            07 WS-REAS-CD                          PIC S9(09) COMP
                                                   VALUE ZEROS.
            07 WS-RESP-CD-DISP                   PIC 9(09) VALUE ZEROS.
            07 WS-REAS-CD-DISP                   PIC 9(09) VALUE ZEROS.

      ******************************************************************
      * Input validation flags
      ******************************************************************
         05  WS-INPUT-FLAG                         PIC X(1).
           88  INPUT-OK                            VALUE '0'.
           88  INPUT-ERROR                         VALUE '1'.

      ******************************************************************
      * Validation work fields - ADDED FROM COACTUPA
      ******************************************************************
         05 WS-VALID-WORK.
            10 WS-EDIT-VARIABLE-NAME              PIC X(30).
            10 WS-EDIT-ALPHANUM-ONLY              PIC X(256).
            10 WS-EDIT-ALPHANUM-LENGTH            PIC S9(4) COMP-3.
            10 WS-EDIT-ALPHA-ONLY-FLAGS           PIC X(1).
               88  FLG-ALPHA-ISVALID              VALUE LOW-VALUES.
               88  FLG-ALPHA-NOT-OK               VALUE '0'.
               88  FLG-ALPHA-BLANK                VALUE 'B'.
            10 WS-EDIT-ALPHANUM-ONLY-FLAGS        PIC X(1).
               88  FLG-ALPHNANUM-ISVALID          VALUE LOW-VALUES.
               88  FLG-ALPHNANUM-NOT-OK           VALUE '0'.
               88  FLG-ALPHNANUM-BLANK            VALUE 'B'.
            10 WS-EDIT-MANDATORY-FLAGS            PIC X(1).
               88  FLG-MANDATORY-ISVALID          VALUE LOW-VALUES.
               88  FLG-MANDATORY-NOT-OK           VALUE '0'.
               88  FLG-MANDATORY-BLANK            VALUE 'B'.

            10 WS-EDIT-US-PHONE-NUM               PIC X(15).
            10 WS-EDIT-US-PHONE-NUM-X REDEFINES
               WS-EDIT-US-PHONE-NUM.
               15 FILLER                          PIC X(1).
               15 WS-EDIT-US-PHONE-NUMA           PIC X(3).
               15 WS-EDIT-US-PHONE-NUMA-N REDEFINES
                  WS-EDIT-US-PHONE-NUMA           PIC 9(3).
               15 FILLER                          PIC X(1).
               15 WS-EDIT-US-PHONE-NUMB           PIC X(3).
               15 WS-EDIT-US-PHONE-NUMB-N REDEFINES
                  WS-EDIT-US-PHONE-NUMB           PIC 9(3).
               15 FILLER                          PIC X(1).
               15 WS-EDIT-US-PHONE-NUMC           PIC X(4).
               15 WS-EDIT-US-PHONE-NUMC-N REDEFINES
                  WS-EDIT-US-PHONE-NUMC           PIC 9(4).
               15 FILLER                          PIC X(2).
            10 WS-EDIT-US-PHONE-NUM-FLGS.
               88 WS-EDIT-US-PHONE-IS-INVALID    VALUE '000'.
               88 WS-EDIT-US-PHONE-IS-VALID      VALUE LOW-VALUES.
               15 WS-EDIT-US-PHONEA-FLG          PIC X(01).
                  88 FLG-EDIT-US-PHONEA-ISVALID  VALUE LOW-VALUES.
                  88 FLG-EDIT-US-PHONEA-NOT-OK   VALUE '0'.
                  88 FLG-EDIT-US-PHONEA-BLANK    VALUE 'B'.
               15 WS-EDIT-EDIT-US-PHONEB         PIC X(01).
                  88 FLG-EDIT-US-PHONEB-ISVALID  VALUE LOW-VALUES.
                  88 FLG-EDIT-US-PHONEB-NOT-OK   VALUE '0'.
                  88 FLG-EDIT-US-PHONEB-BLANK    VALUE 'B'.
               15 WS-EDIT-EDIT-PHONEC            PIC X(01).
                  88 FLG-EDIT-US-PHONEC-ISVALID  VALUE LOW-VALUES.
                  88 FLG-EDIT-US-PHONEC-NOT-OK   VALUE '0'.
                  88 FLG-EDIT-US-PHONEC-BLANK    VALUE 'B'.

      ******************************************************************
      * Date validation work fields
      ******************************************************************
         05 WS-CALCULATION-VARS.
          10 WS-DIV-BY                             PIC S9(4) COMP-3
                                                   VALUE 4.
          10 WS-DIVIDEND                           PIC S9(4) COMP-3
                                                   VALUE 0.
          10 WS-REMAINDER                          PIC S9(4) COMP-3
                                                   VALUE 0.

      ******************************************************************
      * Customer generation work fields
      ******************************************************************
         05  WS-CUSTOMER-WORK-FIELDS.
           10 WS-NEW-CUST-ID                       PIC 9(9).

      ******************************************************************
      * Error Messages
      ******************************************************************
         05  WS-RETURN-MSG                         PIC X(75).
           88  WS-RETURN-MSG-OFF                   VALUE SPACES.

      ******************************************************************
      * Literals and Constants
      ******************************************************************
       01 WS-LITERALS.
          05 LIT-THISPGM                           PIC X(8)
                                                   VALUE 'COCUSADA'.

      ******************************************************************
      * Literals for use in INSPECT statements - FROM COACTUPA
      ******************************************************************
          05 LIT-ALL-ALPHANUM-FROM-X.
             10 LIT-ALL-ALPHA-FROM-X.
                15 LIT-UPPER                       PIC X(26)
                                 VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
                15 LIT-LOWER                       PIC X(26)
                                 VALUE 'abcdefghijklmnopqrstuvwxyz'.
             10 LIT-NUMBERS                        PIC X(10)
                                 VALUE '0123456789'.

      ******************************************************************
      * Variables for use in INSPECT statements - FROM COACTUPA
      ******************************************************************
       01  LIT-ALL-ALPHA-FROM     PIC X(52) VALUE SPACES.
       01  LIT-ALL-ALPHANUM-FROM  PIC X(62) VALUE SPACES.
       01  LIT-ALL-NUM-FROM       PIC X(10) VALUE SPACES.
       77  LIT-ALPHA-SPACES-TO    PIC X(52) VALUE SPACES.
       77  LIT-ALPHANUM-SPACES-TO PIC X(62) VALUE SPACES.
       77  LIT-NUM-SPACES-TO      PIC X(10) VALUE SPACES.

      ******************************************************************
      * DB2 SQL COMMUNICATION AREA
      ******************************************************************
       EXEC SQL
           INCLUDE SQLCA
       END-EXEC.

      ******************************************************************
      * DB2 HOST VARIABLES
      ******************************************************************
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  HV-CUSTOMER-ID                       PIC S9(09) COMP.
       01  HV-SQLCODE-DISPLAY                   PIC S9(09) DISPLAY.
       01  HV-CUSTOMER-INSERT.
           05  HV-NEW-CUST-ID                   PIC S9(09) COMP.
           05  HV-CUST-FNAME                    PIC X(25).
           05  HV-CUST-MNAME                    PIC X(25).
           05  HV-CUST-LNAME                    PIC X(25).
           05  HV-CUST-ADDR1                    PIC X(50).
           05  HV-CUST-ADDR2                    PIC X(50).
           05  HV-CUST-ADDR3                    PIC X(50).
           05  HV-CUST-STATE                    PIC X(02).
           05  HV-CUST-COUNTRY                  PIC X(03).
           05  HV-CUST-ZIP                      PIC X(10).
           05  HV-CUST-PHONE1                   PIC X(15).
           05  HV-CUST-PHONE2                   PIC X(15).
           05  HV-CUST-SSN                      PIC S9(09) COMP.
           05  HV-CUST-GOVT-ID                  PIC X(20).
           05  HV-CUST-DOB                      PIC X(8).
           05  HV-CUST-EFT-ID                   PIC X(10).
           05  HV-CUST-PRI-HOLDER               PIC X(01).
           05  HV-CUST-FICO                     PIC S9(03) COMP.
       01  HV-CUSTOMER-SEQUENCE.
           05  HV-MAX-CUST-ID                   PIC S9(09) COMP.
           05  HV-MAX-CUST-ID-IND               PIC S9(04) COMP.
       01  HV-TARGET-VALID.
           05  HV-TARGET-CUST-ID                PIC S9(09) COMP.
           05  HV-TARGET-COUNT                  PIC S9(04) COMP.
       EXEC SQL END DECLARE SECTION END-EXEC.

      ******************************************************************
      * COMMON COPYBOOKS
      ******************************************************************
       COPY CVCUS01Y.
       COPY CSLKPCDY.

      ******************************************************************
      * Date validation work fields
      ******************************************************************
       01 DATE-VALID.
         05 DATE-VALID-LABEL.
           COPY CSUTLDWY.

       LINKAGE SECTION.
      ******************************************************************
      * COMMAREA Structure - ENHANCED FOR TARGET CUSTOMER ID + ERROR FIELD
      ******************************************************************
       01 DFHCOMMAREA.
          05 LK-INPUT-PARMS.
             10 LK-IN-TARGET-ID           PIC 9(09).
             10 LK-IN-FNAME               PIC X(25).
             10 LK-IN-MNAME               PIC X(25).
             10 LK-IN-LNAME               PIC X(25).
             10 LK-IN-ADDR1               PIC X(50).
             10 LK-IN-ADDR2               PIC X(50).
             10 LK-IN-CITY                PIC X(50).
             10 LK-IN-STATE               PIC X(02).
             10 LK-IN-COUNTRY             PIC X(03).
             10 LK-IN-ZIP                 PIC X(10).
             10 LK-IN-PHONE1.
                15 LK-IN-PHONE1-AREA     PIC X(03).
                15 LK-IN-PHONE1-PREFIX   PIC X(03).
                15 LK-IN-PHONE1-LINE     PIC X(04).
             10 LK-IN-PHONE2.
                15 LK-IN-PHONE2-AREA     PIC X(03).
                15 LK-IN-PHONE2-PREFIX   PIC X(03).
                15 LK-IN-PHONE2-LINE     PIC X(04).
             10 LK-IN-SSN                 PIC 9(09).
             10 LK-IN-GOVT-ID             PIC X(20).
             10 LK-IN-DOB                 PIC X(8).
             10 LK-IN-EFT-ID              PIC X(10).
             10 LK-IN-PRI-HOLDER          PIC X(01).
             10 LK-IN-FICO                PIC 9(03).
          05 LK-OUTPUT-STATUS.
             10 LK-OUT-RETURN-CODE        PIC 9(02).
                88 RC-SUCCESS             VALUE 00.
                88 RC-NOT-FOUND           VALUE 01.
                88 RC-INPUT-ERROR         VALUE 03.
                88 RC-DATABASE-ERROR      VALUE 99.
                88 RC-TARGET-ID-EXISTS    VALUE 06.
             10 LK-OUT-MESSAGE            PIC X(80).
             10 LK-OUT-ERROR-FIELD        PIC X(25).
          05 LK-OUTPUT-DATA.
             10 LK-OUT-NEW-CUST-ID        PIC 9(09).

       PROCEDURE DIVISION USING DFHCOMMAREA.

           EXEC CICS HANDLE ABEND
              LABEL(ABEND-ROUTINE)
           END-EXEC.

      ******************************************************************
      * Main processing logic - ENHANCED FOR TARGET CUSTOMER ID
      ******************************************************************
       MAIN-PARA.

           INITIALIZE LK-OUTPUT-STATUS
                      LK-OUTPUT-DATA
                      WS-MISC-STORAGE

           SET RC-SUCCESS TO TRUE
           MOVE SPACES TO LK-OUT-MESSAGE
           MOVE SPACES TO LK-OUT-ERROR-FIELD
           SET INPUT-OK TO TRUE
           SET WS-RETURN-MSG-OFF TO TRUE


      * Validate input parameters in screen order (top to bottom)
           PERFORM 1000-VALIDATE-INPUT
              THRU 1000-VALIDATE-INPUT-EXIT

           IF INPUT-ERROR
              SET RC-INPUT-ERROR TO TRUE
              MOVE WS-RETURN-MSG TO LK-OUT-MESSAGE
              PERFORM 1290-SET-ERROR-FIELD
                 THRU 1290-SET-ERROR-FIELD-EXIT
              GOBACK
           END-IF

      * Create new customer
           PERFORM 9000-CREATE-CUSTOMER
              THRU 9000-CREATE-CUSTOMER-EXIT

           IF NOT RC-SUCCESS
              MOVE WS-RETURN-MSG TO LK-OUT-MESSAGE
              GOBACK
           END-IF

           MOVE WS-NEW-CUST-ID TO LK-OUT-NEW-CUST-ID
           MOVE 'Customer created successfully.' TO LK-OUT-MESSAGE

           GOBACK.

      ******************************************************************
      * Validate Input Parameters - SCREEN ORDER TOP TO BOTTOM
      ******************************************************************
       1000-VALIDATE-INPUT.

      *    1. Names first (top of screen - lines 6-7)
           PERFORM 1100-EDIT-NAMES
              THRU 1100-EDIT-NAMES-EXIT
           IF INPUT-ERROR
              GO TO 1000-VALIDATE-INPUT-EXIT
           END-IF

      *    2. Address fields (middle of screen - lines 9-11)
           PERFORM 1200-EDIT-ADDRESS-FIELDS
              THRU 1200-EDIT-ADDRESS-FIELDS-EXIT
           IF INPUT-ERROR
              GO TO 1000-VALIDATE-INPUT-EXIT
           END-IF

      *    3. Personal data (line 13: SSN, DOB, FICO)
           PERFORM 1300-EDIT-PERSONAL-DATA
              THRU 1300-EDIT-PERSONAL-DATA-EXIT
           IF INPUT-ERROR
              GO TO 1000-VALIDATE-INPUT-EXIT
           END-IF

      *    4. Contact info (lines 15-16: phones, govt-id, etc)
           PERFORM 1400-EDIT-CONTACT-INFO
              THRU 1400-EDIT-CONTACT-INFO-EXIT
           IF INPUT-ERROR
              GO TO 1000-VALIDATE-INPUT-EXIT
           END-IF

      *    5. Cross-field validations and defaults last
           PERFORM 1500-EDIT-CROSS-VALID
              THRU 1500-EDIT-CROSS-VALID-EXIT
           .
       1000-VALIDATE-INPUT-EXIT.
           EXIT.

      ******************************************************************
      * 1. Edit Names (First, Middle, Last) - Lines 6-7
      ******************************************************************
       1100-EDIT-NAMES.

      *    First Name - required, alphabetic
           MOVE 'First Name' TO WS-EDIT-VARIABLE-NAME
           MOVE LK-IN-FNAME TO WS-EDIT-ALPHANUM-ONLY
           MOVE 25 TO WS-EDIT-ALPHANUM-LENGTH
           PERFORM 1515-EDIT-ALPHA-REQD
              THRU 1515-EDIT-ALPHA-REQD-EXIT
           IF INPUT-ERROR
              GO TO 1100-EDIT-NAMES-EXIT
           END-IF

      *    Middle Name - optional, alphabetic if provided
           MOVE 'Middle Name' TO WS-EDIT-VARIABLE-NAME
           MOVE LK-IN-MNAME TO WS-EDIT-ALPHANUM-ONLY
           MOVE 25 TO WS-EDIT-ALPHANUM-LENGTH
           PERFORM 1525-EDIT-ALPHA-OPT
              THRU 1525-EDIT-ALPHA-OPT-EXIT
           IF INPUT-ERROR
              GO TO 1100-EDIT-NAMES-EXIT
           END-IF

      *    Last Name - required, alphabetic
           MOVE 'Last Name' TO WS-EDIT-VARIABLE-NAME
           MOVE LK-IN-LNAME TO WS-EDIT-ALPHANUM-ONLY
           MOVE 25 TO WS-EDIT-ALPHANUM-LENGTH
           PERFORM 1515-EDIT-ALPHA-REQD
              THRU 1515-EDIT-ALPHA-REQD-EXIT
           .
       1100-EDIT-NAMES-EXIT.
           EXIT.

      ******************************************************************
      * 2. Edit Address Fields - Lines 9-11
      ******************************************************************
       1200-EDIT-ADDRESS-FIELDS.

      *    Address Line 1 - required
           MOVE 'Address Line 1' TO WS-EDIT-VARIABLE-NAME
           MOVE LK-IN-ADDR1 TO WS-EDIT-ALPHANUM-ONLY
           MOVE 50 TO WS-EDIT-ALPHANUM-LENGTH
           PERFORM 1535-EDIT-MANDATORY
              THRU 1535-EDIT-MANDATORY-EXIT
           IF INPUT-ERROR
              GO TO 1200-EDIT-ADDRESS-FIELDS-EXIT
           END-IF

      *    State - required, 2 char alphabetic + lookup (line 9, right side)
           MOVE 'State' TO WS-EDIT-VARIABLE-NAME
           MOVE LK-IN-STATE TO WS-EDIT-ALPHANUM-ONLY
           MOVE 2 TO WS-EDIT-ALPHANUM-LENGTH
           PERFORM 1515-EDIT-ALPHA-REQD
              THRU 1515-EDIT-ALPHA-REQD-EXIT
           IF INPUT-ERROR
              GO TO 1200-EDIT-ADDRESS-FIELDS-EXIT
           END-IF
           PERFORM 1270-EDIT-US-STATE-CD
              THRU 1270-EDIT-US-STATE-CD-EXIT
           IF INPUT-ERROR
              GO TO 1200-EDIT-ADDRESS-FIELDS-EXIT
           END-IF

      *    Address Line 2 - optional (line 10)
           MOVE 'Address Line 2' TO WS-EDIT-VARIABLE-NAME
           MOVE LK-IN-ADDR2 TO WS-EDIT-ALPHANUM-ONLY
           MOVE 50 TO WS-EDIT-ALPHANUM-LENGTH
           PERFORM 1525-EDIT-ALPHANUM-OPT
              THRU 1525-EDIT-ALPHANUM-OPT-EXIT
           IF INPUT-ERROR
              GO TO 1200-EDIT-ADDRESS-FIELDS-EXIT
           END-IF

      *    Zip - required, 5 digits (line 10, right side)
           MOVE 'Zip Code' TO WS-EDIT-VARIABLE-NAME
           MOVE LK-IN-ZIP TO WS-EDIT-ALPHANUM-ONLY
           MOVE 5 TO WS-EDIT-ALPHANUM-LENGTH
           PERFORM 1545-EDIT-NUM-REQD
              THRU 1545-EDIT-NUM-REQD-EXIT
           IF INPUT-ERROR
              GO TO 1200-EDIT-ADDRESS-FIELDS-EXIT
           END-IF

      *    City - required, alphabetic (line 11)
           MOVE 'City' TO WS-EDIT-VARIABLE-NAME
           MOVE LK-IN-CITY TO WS-EDIT-ALPHANUM-ONLY
           MOVE 50 TO WS-EDIT-ALPHANUM-LENGTH
           PERFORM 1515-EDIT-ALPHA-REQD
              THRU 1515-EDIT-ALPHA-REQD-EXIT
           IF INPUT-ERROR
              GO TO 1200-EDIT-ADDRESS-FIELDS-EXIT
           END-IF

      *    Country - required, 3 char alphabetic (line 11, right side)
           MOVE 'Country' TO WS-EDIT-VARIABLE-NAME
           MOVE LK-IN-COUNTRY TO WS-EDIT-ALPHANUM-ONLY
           MOVE 3 TO WS-EDIT-ALPHANUM-LENGTH
           PERFORM 1515-EDIT-ALPHA-REQD
              THRU 1515-EDIT-ALPHA-REQD-EXIT
           .
       1200-EDIT-ADDRESS-FIELDS-EXIT.
           EXIT.

      ******************************************************************
      * 3. Edit Personal Data (SSN, DOB, FICO) - Line 13
      ******************************************************************
       1300-EDIT-PERSONAL-DATA.

      *    SSN - required, 9 digits, range validation
           MOVE 'SSN' TO WS-EDIT-VARIABLE-NAME
           IF LK-IN-SSN EQUAL ZERO OR LK-IN-SSN EQUAL SPACES
              SET INPUT-ERROR TO TRUE
              MOVE 'SSN is required' TO WS-RETURN-MSG
              GO TO 1300-EDIT-PERSONAL-DATA-EXIT
           END-IF
           IF LK-IN-SSN < 100000000 OR LK-IN-SSN > 999999999
              SET INPUT-ERROR TO TRUE
              MOVE 'SSN must be a valid 9-digit number' TO WS-RETURN-MSG
              GO TO 1300-EDIT-PERSONAL-DATA-EXIT
           END-IF

      *    Date of Birth - required, valid format and age
           MOVE 'Date of Birth' TO WS-EDIT-VARIABLE-NAME
           IF LK-IN-DOB EQUAL SPACES OR LK-IN-DOB EQUAL LOW-VALUES
              SET INPUT-ERROR TO TRUE
              MOVE 'Date of birth is required' TO WS-RETURN-MSG
              GO TO 1300-EDIT-PERSONAL-DATA-EXIT
           END-IF
           MOVE LK-IN-DOB TO WS-EDIT-DATE-CCYYMMDD
           PERFORM EDIT-DATE-CCYYMMDD
              THRU EDIT-DATE-CCYYMMDD-EXIT
           IF WS-EDIT-DATE-IS-INVALID
              SET INPUT-ERROR TO TRUE
              GO TO 1300-EDIT-PERSONAL-DATA-EXIT
           END-IF
           PERFORM EDIT-DATE-OF-BIRTH
              THRU EDIT-DATE-OF-BIRTH-EXIT
           IF INPUT-ERROR
              GO TO 1300-EDIT-PERSONAL-DATA-EXIT
           END-IF

      *    FICO Score - validate range, set default if zero
           MOVE 'FICO Score' TO WS-EDIT-VARIABLE-NAME
           IF LK-IN-FICO = ZERO
              MOVE 650 TO LK-IN-FICO
           ELSE
              PERFORM 1275-EDIT-FICO-SCORE
                 THRU 1275-EDIT-FICO-SCORE-EXIT
           END-IF
           .
       1300-EDIT-PERSONAL-DATA-EXIT.
           EXIT.

      ******************************************************************
      * 4. Edit Contact Info (Phones, Govt ID, etc) - Lines 15-16
      ******************************************************************
       1400-EDIT-CONTACT-INFO.

      *    Phone 1 - optional but if provided must be valid
           IF LK-IN-PHONE1-AREA NOT = SPACES
           AND LK-IN-PHONE1-AREA NOT = LOW-VALUES
              MOVE 'Phone Number 1' TO WS-EDIT-VARIABLE-NAME
              STRING '('
                     LK-IN-PHONE1-AREA
                     ')'
                     LK-IN-PHONE1-PREFIX
                     '-'
                     LK-IN-PHONE1-LINE
              DELIMITED BY SIZE INTO WS-EDIT-US-PHONE-NUM
              PERFORM 1260-EDIT-US-PHONE-NUM
                 THRU 1260-EDIT-US-PHONE-NUM-EXIT
              IF INPUT-ERROR
                 GO TO 1400-EDIT-CONTACT-INFO-EXIT
              END-IF
           END-IF

      *    Government ID - optional
           MOVE 'Government ID' TO WS-EDIT-VARIABLE-NAME
           MOVE LK-IN-GOVT-ID TO WS-EDIT-ALPHANUM-ONLY
           MOVE 20 TO WS-EDIT-ALPHANUM-LENGTH
           PERFORM 1525-EDIT-ALPHANUM-OPT
              THRU 1525-EDIT-ALPHANUM-OPT-EXIT
           IF INPUT-ERROR
              GO TO 1400-EDIT-CONTACT-INFO-EXIT
           END-IF

      *    Phone 2 - optional but if provided must be valid
           IF LK-IN-PHONE2-AREA NOT = SPACES
           AND LK-IN-PHONE2-AREA NOT = LOW-VALUES
              MOVE 'Phone Number 2' TO WS-EDIT-VARIABLE-NAME
              STRING '('
                     LK-IN-PHONE2-AREA
                     ')'
                     LK-IN-PHONE2-PREFIX
                     '-'
                     LK-IN-PHONE2-LINE
              DELIMITED BY SIZE INTO WS-EDIT-US-PHONE-NUM
              PERFORM 1260-EDIT-US-PHONE-NUM
                 THRU 1260-EDIT-US-PHONE-NUM-EXIT
              IF INPUT-ERROR
                 GO TO 1400-EDIT-CONTACT-INFO-EXIT
              END-IF
           END-IF

      *    EFT Account ID - optional
           MOVE 'EFT Account ID' TO WS-EDIT-VARIABLE-NAME
           MOVE LK-IN-EFT-ID TO WS-EDIT-ALPHANUM-ONLY
           MOVE 10 TO WS-EDIT-ALPHANUM-LENGTH
           PERFORM 1525-EDIT-ALPHANUM-OPT
              THRU 1525-EDIT-ALPHANUM-OPT-EXIT
           IF INPUT-ERROR
              GO TO 1400-EDIT-CONTACT-INFO-EXIT
           END-IF

      *    Primary Holder - optional, Y/N validation
           IF LK-IN-PRI-HOLDER NOT = SPACES
           AND LK-IN-PRI-HOLDER NOT = LOW-VALUES
              IF LK-IN-PRI-HOLDER NOT = 'Y'
              AND LK-IN-PRI-HOLDER NOT = 'N'
                 SET INPUT-ERROR TO TRUE
                 MOVE 'Primary Holder must be Y or N' TO WS-RETURN-MSG
                 MOVE 'Primary Holder' TO WS-EDIT-VARIABLE-NAME
                 GO TO 1400-EDIT-CONTACT-INFO-EXIT
              END-IF
           END-IF
           .
       1400-EDIT-CONTACT-INFO-EXIT.
           EXIT.

      ******************************************************************
      * 5. Cross-field validations and defaults
      ******************************************************************
       1500-EDIT-CROSS-VALID.

      *    State-Zip validation
           IF LK-IN-STATE NOT = SPACES AND LK-IN-ZIP NOT = SPACES
              PERFORM 1280-EDIT-US-STATE-ZIP-CD
                 THRU 1280-EDIT-US-STATE-ZIP-CD-EXIT
              IF INPUT-ERROR
                 GO TO 1500-EDIT-CROSS-VALID-EXIT
              END-IF
           END-IF

      *    Set defaults for optional fields
           IF LK-IN-COUNTRY EQUAL SPACES
              MOVE 'USA' TO LK-IN-COUNTRY
           END-IF
           IF LK-IN-PRI-HOLDER EQUAL SPACES
              MOVE 'Y' TO LK-IN-PRI-HOLDER
           END-IF
           .
       1500-EDIT-CROSS-VALID-EXIT.
           EXIT.

      ******************************************************************
      * HELPER VALIDATION PARAGRAPHS - FROM COACTUPA
      ******************************************************************
       1515-EDIT-ALPHA-REQD.
      *    Initialize
           SET FLG-ALPHA-NOT-OK TO TRUE

      *    Not supplied
           IF WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH)
                                             EQUAL LOW-VALUES
           OR WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH)
               EQUAL SPACES
           OR FUNCTION LENGTH(FUNCTION TRIM(
              WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH))) = 0

              SET INPUT-ERROR                TO TRUE
              SET FLG-ALPHA-BLANK            TO TRUE
              IF WS-RETURN-MSG-OFF
                 STRING
                   FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                   ' must be supplied.'
                   DELIMITED BY SIZE
                   INTO WS-RETURN-MSG
                 END-STRING
              END-IF
              GO TO  1515-EDIT-ALPHA-REQD-EXIT
           END-IF

      *    Only Alphabets and space allowed
           MOVE LIT-ALL-ALPHA-FROM-X   TO LIT-ALL-ALPHA-FROM
           INSPECT WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH)
             CONVERTING LIT-ALL-ALPHA-FROM
                     TO LIT-ALPHA-SPACES-TO

           IF FUNCTION LENGTH(
                   FUNCTION TRIM(
                   WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH)
                                  )) = 0
              CONTINUE
           ELSE
              SET INPUT-ERROR           TO TRUE
              SET FLG-ALPHA-NOT-OK      TO TRUE
              IF WS-RETURN-MSG-OFF
                 STRING
                   FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                   ' can have alphabets only.'
                   DELIMITED BY SIZE
                   INTO WS-RETURN-MSG
                 END-STRING
              END-IF
              GO TO  1515-EDIT-ALPHA-REQD-EXIT
           END-IF

           SET FLG-ALPHA-ISVALID        TO TRUE
           .
       1515-EDIT-ALPHA-REQD-EXIT.
           EXIT
           .

       1525-EDIT-ALPHA-OPT.
      *    Initialize
           SET FLG-ALPHA-NOT-OK              TO TRUE

      *    Not supplied - OK for optional field
           IF WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH)
                                             EQUAL LOW-VALUES
           OR WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH)
               EQUAL SPACES
           OR FUNCTION LENGTH(FUNCTION TRIM(
              WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH))) = 0

              SET FLG-ALPHA-ISVALID          TO TRUE
              GO TO  1525-EDIT-ALPHA-OPT-EXIT
           END-IF

      *    Only Alphabets and space allowed
           MOVE LIT-ALL-ALPHA-FROM-X    TO LIT-ALL-ALPHA-FROM
           INSPECT WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH)
             CONVERTING LIT-ALL-ALPHA-FROM
                     TO LIT-ALPHA-SPACES-TO

           IF FUNCTION LENGTH(
                   FUNCTION TRIM(
                   WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH)
                                  )) = 0
              CONTINUE
           ELSE
              SET INPUT-ERROR           TO TRUE
              SET FLG-ALPHA-NOT-OK      TO TRUE
              IF WS-RETURN-MSG-OFF
                 STRING
                   FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                   ' can have alphabets only.'
                   DELIMITED BY SIZE
                   INTO WS-RETURN-MSG
                 END-STRING
              END-IF
              GO TO  1525-EDIT-ALPHA-OPT-EXIT
           END-IF

           SET FLG-ALPHA-ISVALID        TO TRUE
           .
       1525-EDIT-ALPHA-OPT-EXIT.
           EXIT
           .

       1525-EDIT-ALPHANUM-OPT.
      *    Optional alphanumeric field - blank is OK
           SET FLG-ALPHNANUM-NOT-OK          TO TRUE

      *    Not supplied - OK for optional field
           IF WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH)
                                             EQUAL LOW-VALUES
           OR WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH)
               EQUAL SPACES
           OR FUNCTION LENGTH(FUNCTION TRIM(
              WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH))) = 0

              SET FLG-ALPHNANUM-ISVALID      TO TRUE
              GO TO  1525-EDIT-ALPHANUM-OPT-EXIT
           END-IF

      *    If provided, any alphanumeric characters are acceptable
           SET FLG-ALPHNANUM-ISVALID    TO TRUE
           .
       1525-EDIT-ALPHANUM-OPT-EXIT.
           EXIT
           .

       1535-EDIT-MANDATORY.
      *    Initialize
           SET FLG-MANDATORY-NOT-OK    TO TRUE

      *    Not supplied
           IF WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH)
                                       EQUAL LOW-VALUES
           OR WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH)
                                       EQUAL SPACES
           OR FUNCTION LENGTH(FUNCTION TRIM(
              WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH))) = 0

              SET INPUT-ERROR          TO TRUE
              SET FLG-MANDATORY-BLANK  TO TRUE
              IF WS-RETURN-MSG-OFF
                 STRING
                   FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                   ' must be supplied.'
                   DELIMITED BY SIZE
                   INTO WS-RETURN-MSG
                 END-STRING
              END-IF
              GO TO  1535-EDIT-MANDATORY-EXIT
           END-IF

           SET FLG-MANDATORY-ISVALID   TO TRUE
           .
       1535-EDIT-MANDATORY-EXIT.
           EXIT
           .

       1545-EDIT-NUM-REQD.
      *    Initialize
           SET FLG-ALPHNANUM-NOT-OK          TO TRUE

      *    Not supplied
           IF WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH)
                                             EQUAL LOW-VALUES
           OR WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH)
               EQUAL SPACES
           OR FUNCTION LENGTH(FUNCTION TRIM(
              WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH))) = 0

              SET INPUT-ERROR                TO TRUE
              SET FLG-ALPHNANUM-BLANK        TO TRUE
              IF WS-RETURN-MSG-OFF
                 STRING
                   FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                   ' must be supplied.'
                   DELIMITED BY SIZE
                   INTO WS-RETURN-MSG
                 END-STRING
              END-IF
              GO TO  1545-EDIT-NUM-REQD-EXIT
           END-IF

      *    Only all numeric allowed
           IF WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH)
                  IS NUMERIC
              CONTINUE
           ELSE
              SET INPUT-ERROR           TO TRUE
              SET FLG-ALPHNANUM-NOT-OK  TO TRUE
              IF WS-RETURN-MSG-OFF
                 STRING
                   FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                   ' must be all numeric.'
                   DELIMITED BY SIZE
                   INTO WS-RETURN-MSG
                 END-STRING
              END-IF
              GO TO  1545-EDIT-NUM-REQD-EXIT
           END-IF

      *    Must not be zero
           IF FUNCTION NUMVAL(WS-EDIT-ALPHANUM-ONLY(1:
                              WS-EDIT-ALPHANUM-LENGTH)) = 0
              SET INPUT-ERROR           TO TRUE
              SET FLG-ALPHNANUM-NOT-OK  TO TRUE
              IF WS-RETURN-MSG-OFF
                 STRING
                   FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                   ' must not be zero.'
                   DELIMITED BY SIZE
                   INTO WS-RETURN-MSG
                 END-STRING
              END-IF
              GO TO  1545-EDIT-NUM-REQD-EXIT
           END-IF

           SET FLG-ALPHNANUM-ISVALID    TO TRUE
           .
       1545-EDIT-NUM-REQD-EXIT.
           EXIT
           .

      ******************************************************************
      * PHONE NUMBER VALIDATION - FIXED AND COMPLETE
      ******************************************************************
       1260-EDIT-US-PHONE-NUM.

      *    The database stores date in X(15) format (999)999-9999
      *                                             1234567890123
      *    So we take the X(15) input into WS-EDIT-US-PHONE-NUM
      *    and edit it

           SET WS-EDIT-US-PHONE-IS-INVALID TO TRUE
      *    Not mandatory to enter a phone number
           IF  (WS-EDIT-US-PHONE-NUMA EQUAL SPACES
           OR   WS-EDIT-US-PHONE-NUMA EQUAL LOW-VALUES)
           AND (WS-EDIT-US-PHONE-NUMB EQUAL SPACES
           OR   WS-EDIT-US-PHONE-NUMB EQUAL LOW-VALUES)
           AND (WS-EDIT-US-PHONE-NUMC EQUAL SPACES
           OR   WS-EDIT-US-PHONE-NUMC EQUAL LOW-VALUES)
                SET WS-EDIT-US-PHONE-IS-VALID TO TRUE
                GO TO 1260-EDIT-US-PHONE-NUM-EXIT
           ELSE
                CONTINUE
           END-IF

      * AREA CODE VALIDATION
           IF WS-EDIT-US-PHONE-NUMA EQUAL SPACES
           OR WS-EDIT-US-PHONE-NUMA EQUAL LOW-VALUES
              SET INPUT-ERROR              TO TRUE
              SET FLG-EDIT-US-PHONEA-BLANK    TO TRUE
              IF WS-RETURN-MSG-OFF
                 STRING
                   FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                   ': Area code must be supplied.'
                   DELIMITED BY SIZE
                   INTO WS-RETURN-MSG
                 END-STRING
              END-IF
              GO TO 1260-EDIT-US-PHONE-NUM-EXIT
           ELSE
              CONTINUE
           END-IF

           IF  WS-EDIT-US-PHONE-NUMA       IS NUMERIC
              CONTINUE
           ELSE
              SET INPUT-ERROR                 TO TRUE
              SET  FLG-EDIT-US-PHONEA-NOT-OK  TO TRUE
              IF WS-RETURN-MSG-OFF
                 STRING
                   FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                   ': Area code must be A 3 digit number.'
                   DELIMITED BY SIZE
                   INTO WS-RETURN-MSG
                 END-STRING
              END-IF
              GO TO 1260-EDIT-US-PHONE-NUM-EXIT
           END-IF

           IF  WS-EDIT-US-PHONE-NUMA-N = 0
              SET INPUT-ERROR              TO TRUE
              SET  FLG-EDIT-US-PHONEA-NOT-OK  TO TRUE
              IF WS-RETURN-MSG-OFF
                 STRING
                   FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                   ': Area code cannot be zero'
                   DELIMITED BY SIZE
                   INTO WS-RETURN-MSG
                 END-STRING
              END-IF
              GO TO 1260-EDIT-US-PHONE-NUM-EXIT
           ELSE
              CONTINUE
           END-IF

           MOVE FUNCTION TRIM (WS-EDIT-US-PHONE-NUMA)
             TO WS-US-PHONE-AREA-CODE-TO-EDIT
           IF VALID-GENERAL-PURP-CODE
               CONTINUE
           ELSE
              SET INPUT-ERROR              TO TRUE
              SET  FLG-EDIT-US-PHONEA-NOT-OK  TO TRUE
              IF WS-RETURN-MSG-OFF
                 STRING
                   FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                   ': Not valid North America general purpose area code'
                   DELIMITED BY SIZE
                   INTO WS-RETURN-MSG
                 END-STRING
              END-IF
              GO TO 1260-EDIT-US-PHONE-NUM-EXIT
           END-IF

           SET FLG-EDIT-US-PHONEA-ISVALID    TO TRUE

      * PREFIX CODE VALIDATION
           IF WS-EDIT-US-PHONE-NUMB EQUAL SPACES
           OR WS-EDIT-US-PHONE-NUMB EQUAL LOW-VALUES
              SET INPUT-ERROR              TO TRUE
              SET FLG-EDIT-US-PHONEB-BLANK    TO TRUE
              IF WS-RETURN-MSG-OFF
                 STRING
                   FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                   ': Prefix code must be supplied.'
                   DELIMITED BY SIZE
                   INTO WS-RETURN-MSG
                 END-STRING
              END-IF
              GO TO 1260-EDIT-US-PHONE-NUM-EXIT
           ELSE
              CONTINUE
           END-IF

           IF  WS-EDIT-US-PHONE-NUMB          IS NUMERIC
              CONTINUE
           ELSE
              SET INPUT-ERROR                 TO TRUE
              SET  FLG-EDIT-US-PHONEB-NOT-OK  TO TRUE
              IF WS-RETURN-MSG-OFF
                 STRING
                   FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                   ': Prefix code must be A 3 digit number.'
                   DELIMITED BY SIZE
                   INTO WS-RETURN-MSG
                 END-STRING
              END-IF
              GO TO 1260-EDIT-US-PHONE-NUM-EXIT
           END-IF

           IF  WS-EDIT-US-PHONE-NUMB-N = 0
              SET INPUT-ERROR              TO TRUE
              SET  FLG-EDIT-US-PHONEB-NOT-OK  TO TRUE
              IF WS-RETURN-MSG-OFF
                 STRING
                   FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                   ': Prefix code cannot be zero'
                   DELIMITED BY SIZE
                   INTO WS-RETURN-MSG
                 END-STRING
              END-IF
              GO TO 1260-EDIT-US-PHONE-NUM-EXIT
           ELSE
              CONTINUE
           END-IF

           SET FLG-EDIT-US-PHONEB-ISVALID    TO TRUE

      * LINE NUMBER VALIDATION
           IF WS-EDIT-US-PHONE-NUMC EQUAL SPACES
           OR WS-EDIT-US-PHONE-NUMC EQUAL LOW-VALUES
              SET INPUT-ERROR              TO TRUE
              SET FLG-EDIT-US-PHONEC-BLANK    TO TRUE
              IF WS-RETURN-MSG-OFF
                 STRING
                   FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                   ': Line number code must be supplied.'
                   DELIMITED BY SIZE
                   INTO WS-RETURN-MSG
                 END-STRING
              END-IF
              GO TO 1260-EDIT-US-PHONE-NUM-EXIT
           ELSE
              CONTINUE
           END-IF

           IF  WS-EDIT-US-PHONE-NUMC          IS NUMERIC
              CONTINUE
           ELSE
              SET INPUT-ERROR                 TO TRUE
              SET  FLG-EDIT-US-PHONEC-NOT-OK  TO TRUE
              IF WS-RETURN-MSG-OFF
                 STRING
                   FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                   ': Line number code must be A 4 digit number.'
                   DELIMITED BY SIZE
                   INTO WS-RETURN-MSG
                 END-STRING
              END-IF
              GO TO 1260-EDIT-US-PHONE-NUM-EXIT
           END-IF

           IF  WS-EDIT-US-PHONE-NUMC-N = 0
              SET INPUT-ERROR              TO TRUE
              SET  FLG-EDIT-US-PHONEC-NOT-OK  TO TRUE
              IF WS-RETURN-MSG-OFF
                 STRING
                   FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                   ': Line number code cannot be zero'
                   DELIMITED BY SIZE
                   INTO WS-RETURN-MSG
                 END-STRING
              END-IF
              GO TO 1260-EDIT-US-PHONE-NUM-EXIT
           ELSE
               CONTINUE
           END-IF

           SET FLG-EDIT-US-PHONEC-ISVALID    TO TRUE
           SET WS-EDIT-US-PHONE-IS-VALID     TO TRUE
           .
       1260-EDIT-US-PHONE-NUM-EXIT.
           EXIT
           .

       1270-EDIT-US-STATE-CD.

           MOVE LK-IN-STATE TO US-STATE-CODE-TO-EDIT
           IF VALID-US-STATE-CODE
               CONTINUE
           ELSE
              SET INPUT-ERROR              TO TRUE
              IF WS-RETURN-MSG-OFF
                 STRING
                   FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                   ': is not a valid state code'
                   DELIMITED BY SIZE
                   INTO WS-RETURN-MSG
                 END-STRING
              END-IF
              GO TO  1270-EDIT-US-STATE-CD-EXIT
           END-IF
           .
       1270-EDIT-US-STATE-CD-EXIT.
           EXIT
           .

       1275-EDIT-FICO-SCORE.
           IF LK-IN-FICO >= 300
           AND LK-IN-FICO <= 850
               CONTINUE
           ELSE
              SET INPUT-ERROR              TO TRUE
              IF WS-RETURN-MSG-OFF
                 STRING
                   FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                   ': should be between 300 and 850'
                   DELIMITED BY SIZE
                   INTO WS-RETURN-MSG
                 END-STRING
              END-IF
              GO TO  1275-EDIT-FICO-SCORE-EXIT
           END-IF
           .
       1275-EDIT-FICO-SCORE-EXIT.
           EXIT
           .

       1280-EDIT-US-STATE-ZIP-CD.

           STRING LK-IN-STATE
                  LK-IN-ZIP(1:2)
             DELIMITED BY SIZE
             INTO US-STATE-AND-FIRST-ZIP2

           IF VALID-US-STATE-ZIP-CD2-COMBO
               CONTINUE
           ELSE
              SET INPUT-ERROR              TO TRUE
              IF WS-RETURN-MSG-OFF
                 STRING
                   'Invalid zip code for state'
                   DELIMITED BY SIZE
                   INTO WS-RETURN-MSG
                 END-STRING
              END-IF
              GO TO  1280-EDIT-US-STATE-ZIP-CD-EXIT
           END-IF
           .
       1280-EDIT-US-STATE-ZIP-CD-EXIT.
           EXIT
           .

      ******************************************************************
      * Map error field - LIKE COACTADA
      ******************************************************************
       1290-SET-ERROR-FIELD.
           MOVE SPACES TO LK-OUT-ERROR-FIELD

           EVALUATE TRUE
               WHEN WS-EDIT-VARIABLE-NAME = 'First Name'
                   MOVE 'FNAME' TO LK-OUT-ERROR-FIELD
               WHEN WS-EDIT-VARIABLE-NAME = 'Middle Name'
                   MOVE 'MNAME' TO LK-OUT-ERROR-FIELD
               WHEN WS-EDIT-VARIABLE-NAME = 'Last Name'
                   MOVE 'LNAME' TO LK-OUT-ERROR-FIELD
               WHEN WS-EDIT-VARIABLE-NAME = 'Address Line 1'
                   MOVE 'ADDR1' TO LK-OUT-ERROR-FIELD
               WHEN WS-EDIT-VARIABLE-NAME = 'State'
                   MOVE 'STATE' TO LK-OUT-ERROR-FIELD
               WHEN WS-EDIT-VARIABLE-NAME = 'Address Line 2'
                   MOVE 'ADDR2' TO LK-OUT-ERROR-FIELD
               WHEN WS-EDIT-VARIABLE-NAME = 'Zip Code'
                   MOVE 'ZIP' TO LK-OUT-ERROR-FIELD
               WHEN WS-EDIT-VARIABLE-NAME = 'City'
                   MOVE 'CITY' TO LK-OUT-ERROR-FIELD
               WHEN WS-EDIT-VARIABLE-NAME = 'Country'
                   MOVE 'COUNTRY' TO LK-OUT-ERROR-FIELD
               WHEN WS-EDIT-VARIABLE-NAME = 'SSN'
                   MOVE 'SSN' TO LK-OUT-ERROR-FIELD
               WHEN WS-EDIT-VARIABLE-NAME = 'Date of Birth'
                   MOVE 'DOB' TO LK-OUT-ERROR-FIELD
               WHEN WS-EDIT-VARIABLE-NAME = 'FICO Score'
                   MOVE 'FICO' TO LK-OUT-ERROR-FIELD
               WHEN WS-EDIT-VARIABLE-NAME = 'Phone Number 1'
                   MOVE 'PHONE1' TO LK-OUT-ERROR-FIELD
               WHEN WS-EDIT-VARIABLE-NAME = 'Government ID'
                   MOVE 'GOVTID' TO LK-OUT-ERROR-FIELD
               WHEN WS-EDIT-VARIABLE-NAME = 'Phone Number 2'
                   MOVE 'PHONE2' TO LK-OUT-ERROR-FIELD
               WHEN WS-EDIT-VARIABLE-NAME = 'EFT Account ID'
                   MOVE 'EFTID' TO LK-OUT-ERROR-FIELD
               WHEN WS-EDIT-VARIABLE-NAME = 'Primary Holder'
                   MOVE 'PRIHLD' TO LK-OUT-ERROR-FIELD
               WHEN OTHER
                   MOVE 'FNAME' TO LK-OUT-ERROR-FIELD
           END-EVALUATE
           .
       1290-SET-ERROR-FIELD-EXIT.
           EXIT.

      ******************************************************************
      * Create New Customer
      ******************************************************************
       9000-CREATE-CUSTOMER.
           PERFORM 9100-GENERATE-CUSTOMER-ID
              THRU 9100-GENERATE-CUSTOMER-ID-EXIT

           IF NOT RC-SUCCESS
              GO TO 9000-CREATE-CUSTOMER-EXIT
           END-IF

           PERFORM 9200-INSERT-CUSTOMER
              THRU 9200-INSERT-CUSTOMER-EXIT

           IF RC-SUCCESS
              EXEC SQL COMMIT WORK END-EXEC
           ELSE
              EXEC SQL ROLLBACK WORK END-EXEC
           END-IF
           .
       9000-CREATE-CUSTOMER-EXIT.
           EXIT.

      ******************************************************************
      * Generate new customer ID - ENHANCED FOR TARGET ID
      ******************************************************************
       9100-GENERATE-CUSTOMER-ID.

      *    If target ID specified, use it after validation
           IF LK-IN-TARGET-ID NOT = ZEROS
              MOVE LK-IN-TARGET-ID TO WS-NEW-CUST-ID
              MOVE LK-IN-TARGET-ID TO HV-TARGET-CUST-ID


              EXEC SQL
                   SELECT COUNT(*)
                   INTO :HV-TARGET-COUNT
                   FROM CUSTDAT
                   WHERE CUST_ID = :HV-TARGET-CUST-ID
              END-EXEC

              EVALUATE SQLCODE
                  WHEN 0
                     IF HV-TARGET-COUNT > 0
                        SET RC-TARGET-ID-EXISTS TO TRUE
                        STRING
                        'Target Customer ID '
                         LK-IN-TARGET-ID
                        ' already exists.'
                        DELIMITED BY SIZE
                        INTO WS-RETURN-MSG
                        END-STRING
                     END-IF
                  WHEN OTHER
                     SET RC-DATABASE-ERROR TO TRUE
                     MOVE SQLCODE TO HV-SQLCODE-DISPLAY
                     STRING
                     'Error validating target customer ID. SQLCODE:'
                     HV-SQLCODE-DISPLAY
                     DELIMITED BY SIZE
                     INTO WS-RETURN-MSG
                     END-STRING
              END-EVALUATE

              GO TO 9100-GENERATE-CUSTOMER-ID-EXIT
           END-IF

      *    Otherwise auto-generate as before
           EXEC SQL
                SELECT MAX(CUST_ID)
                INTO :HV-MAX-CUST-ID :HV-MAX-CUST-ID-IND
                FROM CUSTDAT
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
               WHEN 100
                  IF HV-MAX-CUST-ID-IND = -1
                  OR HV-MAX-CUST-ID = 0
                     MOVE 100000000 TO HV-MAX-CUST-ID
                  END-IF
                  ADD 1 TO HV-MAX-CUST-ID
                  MOVE HV-MAX-CUST-ID TO WS-NEW-CUST-ID
                   WS-NEW-CUST-ID
               WHEN OTHER
                  SET RC-DATABASE-ERROR TO TRUE
                  MOVE SQLCODE TO HV-SQLCODE-DISPLAY
                  STRING
                  'Error generating customer ID. SQLCODE:'
                  HV-SQLCODE-DISPLAY
                  DELIMITED BY SIZE
                  INTO WS-RETURN-MSG
                  END-STRING
           END-EVALUATE
           .
       9100-GENERATE-CUSTOMER-ID-EXIT.
           EXIT.

      ******************************************************************
      * Insert new customer record
      ******************************************************************
       9200-INSERT-CUSTOMER.
           MOVE WS-NEW-CUST-ID       TO HV-NEW-CUST-ID
           MOVE LK-IN-FNAME          TO HV-CUST-FNAME
           MOVE LK-IN-MNAME          TO HV-CUST-MNAME
           MOVE LK-IN-LNAME          TO HV-CUST-LNAME
           MOVE LK-IN-ADDR1          TO HV-CUST-ADDR1
           MOVE LK-IN-ADDR2          TO HV-CUST-ADDR2
           MOVE LK-IN-CITY           TO HV-CUST-ADDR3
           MOVE LK-IN-STATE          TO HV-CUST-STATE
           MOVE LK-IN-COUNTRY        TO HV-CUST-COUNTRY
           MOVE LK-IN-ZIP            TO HV-CUST-ZIP

      *    Format phone numbers properly for database
           STRING '('
                  LK-IN-PHONE1-AREA
                  ')'
                  LK-IN-PHONE1-PREFIX
                  '-'
                  LK-IN-PHONE1-LINE
           DELIMITED BY SIZE INTO HV-CUST-PHONE1

           STRING '('
                  LK-IN-PHONE2-AREA
                  ')'
                  LK-IN-PHONE2-PREFIX
                  '-'
                  LK-IN-PHONE2-LINE
           DELIMITED BY SIZE INTO HV-CUST-PHONE2

           MOVE LK-IN-SSN            TO HV-CUST-SSN
           MOVE LK-IN-GOVT-ID        TO HV-CUST-GOVT-ID
           MOVE LK-IN-DOB            TO HV-CUST-DOB
           MOVE LK-IN-EFT-ID         TO HV-CUST-EFT-ID
           MOVE LK-IN-PRI-HOLDER     TO HV-CUST-PRI-HOLDER
           MOVE LK-IN-FICO           TO HV-CUST-FICO

           EXEC SQL
                INSERT INTO CUSTDAT (
                    CUST_ID,
                    CUST_FIRST_NAME,
                    CUST_MIDDLE_NAME,
                    CUST_LAST_NAME,
                    CUST_ADDR_LINE_1,
                    CUST_ADDR_LINE_2,
                    CUST_ADDR_LINE_3,
                    CUST_ADDR_STATE_CD,
                    CUST_ADDR_COUNTRY_CD,
                    CUST_ADDR_ZIP,
                    CUST_PHONE_NUM_1,
                    CUST_PHONE_NUM_2,
                    CUST_SSN,
                    CUST_GOVT_ISSUED_ID,
                    CUST_DOB_YYYY_MM_DD,
                    CUST_EFT_ACCOUNT_ID,
                    CUST_PRI_CARD_HOLDER_IND,
                    CUST_FICO_CREDIT_SCORE
                ) VALUES (
                    :HV-NEW-CUST-ID,
                    :HV-CUST-FNAME,
                    :HV-CUST-MNAME,
                    :HV-CUST-LNAME,
                    :HV-CUST-ADDR1,
                    :HV-CUST-ADDR2,
                    :HV-CUST-ADDR3,
                    :HV-CUST-STATE,
                    :HV-CUST-COUNTRY,
                    :HV-CUST-ZIP,
                    :HV-CUST-PHONE1,
                    :HV-CUST-PHONE2,
                    :HV-CUST-SSN,
                    :HV-CUST-GOVT-ID,
                    :HV-CUST-DOB,
                    :HV-CUST-EFT-ID,
                    :HV-CUST-PRI-HOLDER,
                    :HV-CUST-FICO
                )
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   MOVE 'CUSTOMER CREATED' TO LK-OUT-MESSAGE
               WHEN OTHER
                  SET RC-DATABASE-ERROR TO TRUE
                  MOVE SQLCODE TO HV-SQLCODE-DISPLAY
                  STRING
                  'Error inserting customer. SQLCODE:'
                  DELIMITED BY SIZE
                  INTO WS-RETURN-MSG
                  END-STRING
           END-EVALUATE
           .
       9200-INSERT-CUSTOMER-EXIT.
           EXIT.

      ******************************************************************
      * Date validation procedures
      ******************************************************************
       COPY 'CSUTLDPL'.

       ABEND-ROUTINE.
           SET RC-DATABASE-ERROR TO TRUE
           MOVE 'UNEXPECTED ABEND OCCURRED.' TO LK-OUT-MESSAGE
           GOBACK
           .