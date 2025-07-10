      ******************************************************************
      * Program     : COACTUPS.CBL
      * Application : CardDemo
      * Type        : CICS Screen Program
      * Function    : Account Update Screen - calls COACTUPL RPC
      * Transaction : ALS5
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
       PROGRAM-ID. COACTUPS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-VARIABLES.
         05 WS-TRANID                  PIC X(04) VALUE 'ALS5'.
         05 WS-MESSAGE                 PIC X(80) VALUE SPACES.
         05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.
           88 ERR-FLG-ON                         VALUE 'Y'.
           88 ERR-FLG-OFF                        VALUE 'N'.
         05 WS-RESP-CD                 PIC S9(09) COMP VALUE ZEROS.
         05 WS-IDX                     PIC S9(04) COMP VALUE ZEROS.


      * Date and time for header
       01 WS-CURRENT-DATE-TIME.
         05 WS-CURRENT-DATE            PIC X(08).
         05 WS-CURRENT-TIME            PIC X(08).

      * Work fields for numeric formatting
       01 WS-DISPLAY-FIELDS.
         05 WS-CREDIT-LIMIT-DISP       PIC  +ZZZ,ZZZ,ZZZ.99.
         05 WS-CASH-LIMIT-DISP         PIC  +ZZZ,ZZZ,ZZZ.99.
         05 WS-CURR-BAL-DISP           PIC  +ZZZ,ZZZ,ZZZ.99.
         05 WS-CYC-CREDIT-DISP         PIC  +ZZZ,ZZZ,ZZZ.99.
         05 WS-CYC-DEBIT-DISP          PIC  +ZZZ,ZZZ,ZZZ.99.

      * Work fields for numeric formatting/conversion
       01 WS-CONVERSION-FIELDS.
         05 WS-TEMP-CREDIT-LIMIT         PIC S9(10)V99.
         05 WS-TEMP-CASH-LIMIT           PIC S9(10)V99.
         05 WS-TEMP-CURR-BAL             PIC S9(10)V99.
         05 WS-TEMP-CYC-CREDIT           PIC S9(10)V99.
         05 WS-TEMP-CYC-DEBIT            PIC S9(10)V99.
         05 WS-TEMP-AMOUNT-X             PIC X(15).

      ******************************************************************
      *    Work variables
      ******************************************************************
         05 WS-CALCULATION-VARS.
          10 WS-DIV-BY                             PIC S9(4) COMP-3
                                                   VALUE 4.
          10 WS-DIVIDEND                           PIC S9(4) COMP-3
                                                   VALUE 0.
          10 WS-REMAINDER                          PIC S9(4) COMP-3
                                                   VALUE 0.
          10 WS-CURR-DATE                          PIC X(21)
                                                   VALUE SPACES.

         05  WS-DATACHANGED-FLAG                   PIC X(1).
           88  NO-CHANGES-FOUND                    VALUE '0'.
           88  CHANGE-HAS-OCCURRED                 VALUE '1'.
         05  WS-INPUT-FLAG                         PIC X(1).
           88  INPUT-OK                            VALUE '0'.
           88  INPUT-ERROR                         VALUE '1'.
           88  INPUT-PENDING                       VALUE LOW-VALUES.
         05  WS-RETURN-FLAG                        PIC X(1).
           88  WS-RETURN-FLAG-OFF                  VALUE LOW-VALUES.
           88  WS-RETURN-FLAG-ON                   VALUE '1'.
         05  WS-PFK-FLAG                           PIC X(1).
           88  PFK-VALID                           VALUE '0'.
           88  PFK-INVALID                         VALUE '1'.

      *  Program specific edits
         05  WS-EDIT-ACCT-FLAG                     PIC X(1).
           88  FLG-ACCTFILTER-ISVALID              VALUE '1'.
           88  FLG-ACCTFILTER-NOT-OK               VALUE '0'.
           88  FLG-ACCTFILTER-BLANK                VALUE ' '.

      * Field highlighting control
         05  WS-ERROR-FIELD                        PIC X(25).
         05  WS-FIELD-HIGHLIGHT-FLAGS.
           10  WS-ACCT-ID-ERROR                    PIC X(1) VALUE 'N'.
               88 ACCT-ID-HAS-ERROR                VALUE 'Y'.
           10  WS-ACCT-STATUS-ERROR                PIC X(1) VALUE 'N'.
               88 ACCT-STATUS-HAS-ERROR            VALUE 'Y'.
           10  WS-CREDIT-LIMIT-ERROR               PIC X(1) VALUE 'N'.
               88 CREDIT-LIMIT-HAS-ERROR           VALUE 'Y'.
           10  WS-CASH-LIMIT-ERROR                 PIC X(1) VALUE 'N'.
               88 CASH-LIMIT-HAS-ERROR             VALUE 'Y'.
           10  WS-CURR-BAL-ERROR                   PIC X(1) VALUE 'N'.
               88 CURR-BAL-HAS-ERROR               VALUE 'Y'.
           10  WS-OPEN-DATE-ERROR                  PIC X(1) VALUE 'N'.
               88 OPEN-DATE-HAS-ERROR              VALUE 'Y'.
           10  WS-EXP-DATE-ERROR                   PIC X(1) VALUE 'N'.
               88 EXP-DATE-HAS-ERROR               VALUE 'Y'.
           10  WS-REISSUE-DATE-ERROR               PIC X(1) VALUE 'N'.
               88 REISSUE-DATE-HAS-ERROR           VALUE 'Y'.
           10  WS-FIRST-NAME-ERROR                 PIC X(1) VALUE 'N'.
               88 FIRST-NAME-HAS-ERROR             VALUE 'Y'.
           10  WS-LAST-NAME-ERROR                  PIC X(1) VALUE 'N'.
               88 LAST-NAME-HAS-ERROR              VALUE 'Y'.
           10  WS-SSN-ERROR                        PIC X(1) VALUE 'N'.
               88 SSN-HAS-ERROR                    VALUE 'Y'.
           10  WS-DOB-ERROR                        PIC X(1) VALUE 'N'.
               88 DOB-HAS-ERROR                    VALUE 'Y'.
           10  WS-STATE-ERROR                      PIC X(1) VALUE 'N'.
               88 STATE-HAS-ERROR                  VALUE 'Y'.
           10  WS-ZIP-ERROR                        PIC X(1) VALUE 'N'.
               88 ZIP-HAS-ERROR                    VALUE 'Y'.
           10  WS-PHONE1-ERROR                     PIC X(1) VALUE 'N'.
               88 PHONE1-HAS-ERROR                 VALUE 'Y'.
           10  WS-PHONE2-ERROR                     PIC X(1) VALUE 'N'.
               88 PHONE2-HAS-ERROR                 VALUE 'Y'.
           10  WS-FICO-ERROR                       PIC X(1) VALUE 'N'.
               88 FICO-HAS-ERROR                   VALUE 'Y'.

      ******************************************************************
      *      Output Message Construction
      ******************************************************************
         05  WS-LONG-MSG                           PIC X(500).
         05  WS-INFO-MSG                           PIC X(40).
           88  WS-NO-INFO-MESSAGE                 VALUES
                                                  SPACES LOW-VALUES.
           88  FOUND-ACCOUNT-DATA             VALUE
               'Details of selected account shown above'.
           88  PROMPT-FOR-SEARCH-KEYS              VALUE
               'Enter or update id of account to update'.
           88  PROMPT-FOR-CHANGES                  VALUE
               'Update account details presented above.'.
           88  PROMPT-FOR-CONFIRMATION             VALUE
               'Changes validated.Press F5 to save'.
           88  CONFIRM-UPDATE-SUCCESS              VALUE
               'Changes committed to database'.
           88  INFORM-FAILURE                      VALUE
               'Changes unsuccessful. Please try again'.

         05  WS-RETURN-MSG                         PIC X(75).
           88  WS-RETURN-MSG-OFF                   VALUE SPACES.
           88  WS-EXIT-MESSAGE                     VALUE
               'PF03 pressed.Exiting              '.
           88  WS-PROMPT-FOR-ACCT                  VALUE
               'Account number not provided'.
           88  NO-SEARCH-CRITERIA-RECEIVED         VALUE
               'No input received'.
           88  NO-CHANGES-DETECTED                 VALUE
               'No change detected with respect to values fetched.'.
           88  SEARCHED-ACCT-ZEROES                VALUE
               'Account number must be a non zero 11 digit number'.
           88  SEARCHED-ACCT-NOT-NUMERIC           VALUE
               'Account number must be a non zero 11 digit number'.

      ******************************************************************
      *      Literals and Constants
      ******************************************************************
       01 WS-LITERALS.
          05 LIT-THISPGM                           PIC X(8)
                                                   VALUE 'COACTUPS'.
          05 LIT-THISTRANID                        PIC X(4)
                                                   VALUE 'ALS5'.
          05 LIT-THISMAPSET                        PIC X(8)
                                                   VALUE 'COACTUP '.
          05 LIT-THISMAP                           PIC X(7)
                                                   VALUE 'CACTUPA'.
          05 LIT-MENUPGM                           PIC X(8)
                                                   VALUE 'COMEN01S'.
          05 LIT-MENUTRANID                        PIC X(4)
                                                   VALUE 'ALUM'.
          05 LIT-RPC-PROGRAM                       PIC X(08)
                                                   VALUE 'COACTUPL'.

      ******************************************************************
      *Other common working storage Variables
      ******************************************************************
       COPY CVCRD01Y.

      *IBM SUPPLIED COPYBOOKS
       COPY DFHBMSCA.
       COPY DFHAID.

      *COMMON COPYBOOKS
      *Screen Titles
       COPY COTTL01Y.

      *Account Update Screen Layout
       COPY COACTUP.

      *Current Date
       COPY CSDAT01Y.

      *Common Messages
       COPY CSMSG01Y.

      *Abend Variables
       COPY CSMSG02Y.

      *Signed on user data
       COPY CSUSR01Y.

      ******************************************************************
      *Application Commmarea Copybook
       COPY COCOM01Y.

       01 WS-THIS-PROGCOMMAREA.
          05 ACCT-UPDATE-SCREEN-DATA.
             10 ACUP-CHANGE-ACTION                     PIC X(1)
                                                       VALUE LOW-VALUES.
                88 ACUP-DETAILS-NOT-FETCHED            VALUES
                                                       LOW-VALUES,
                                                       SPACES.
                88 ACUP-SHOW-DETAILS                   VALUE 'S'.
                88 ACUP-CHANGES-MADE                   VALUES 'E', 'N'
                                                            , 'C', 'L'
                                                            , 'F'.
                88 ACUP-CHANGES-NOT-OK                 VALUE 'E'.
                88 ACUP-CHANGES-OK-NOT-CONFIRMED       VALUE 'N'.
                88 ACUP-CHANGES-OKAYED-AND-DONE        VALUE 'C'.
                88 ACUP-CHANGES-FAILED                 VALUES 'L', 'F'.
                88 ACUP-CHANGES-OKAYED-LOCK-ERROR      VALUE 'L'.
                88 ACUP-CHANGES-OKAYED-BUT-FAILED      VALUE 'F'.
          05 ACUP-OLD-DETAILS.
             10 ACUP-OLD-ACCT-DATA.
                15  ACUP-OLD-ACCT-ID-X                 PIC X(11).
                15  ACUP-OLD-ACCT-ID                   REDEFINES
                    ACUP-OLD-ACCT-ID-X                 PIC 9(11).
                15  ACUP-OLD-ACTIVE-STATUS             PIC X(01).
                15  ACUP-OLD-CURR-BAL                  PIC X(12).
                15  ACUP-OLD-CURR-BAL-N REDEFINES
                    ACUP-OLD-CURR-BAL                  PIC S9(10)V99.
                15  ACUP-OLD-CREDIT-LIMIT              PIC X(12).
                15  ACUP-OLD-CREDIT-LIMIT-N            REDEFINES
                    ACUP-OLD-CREDIT-LIMIT              PIC S9(10)V99.
                15  ACUP-OLD-CASH-CREDIT-LMT          PIC X(12).
                15  ACUP-OLD-CASH-CREDIT-LMT-N         REDEFINES
                    ACUP-OLD-CASH-CREDIT-LMT           PIC S9(10)V99.
                15  ACUP-OLD-OPEN-DATE                 PIC X(08).
                15  ACUP-OLD-EXPIRAION-DATE            PIC X(08).
                15  ACUP-OLD-REISSUE-DATE              PIC X(08).
                15  ACUP-OLD-CURR-CYC-CREDIT           PIC X(12).
                15  ACUP-OLD-CURR-CYC-CREDIT-N         REDEFINES
                    ACUP-OLD-CURR-CYC-CREDIT           PIC S9(10)V99.
                15  ACUP-OLD-CURR-CYC-DEBIT            PIC X(12).
                15  ACUP-OLD-CURR-CYC-DEBIT-N          REDEFINES
                    ACUP-OLD-CURR-CYC-DEBIT            PIC S9(10)V99.
                15  ACUP-OLD-GROUP-ID                  PIC X(10).
             10 ACUP-OLD-CUST-DATA.
                15  ACUP-OLD-CUST-ID-X                 PIC X(09).
                15  ACUP-OLD-CUST-ID                   REDEFINES
                    ACUP-OLD-CUST-ID-X                 PIC 9(09).
                15  ACUP-OLD-CUST-FIRST-NAME           PIC X(25).
                15  ACUP-OLD-CUST-MIDDLE-NAME          PIC X(25).
                15  ACUP-OLD-CUST-LAST-NAME            PIC X(25).
                15  ACUP-OLD-CUST-ADDR-LINE-1          PIC X(50).
                15  ACUP-OLD-CUST-ADDR-LINE-2          PIC X(50).
                15  ACUP-OLD-CUST-ADDR-LINE-3          PIC X(50).
                15  ACUP-OLD-CUST-ADDR-STATE-CD        PIC X(02).
                15  ACUP-OLD-CUST-ADDR-COUNTRY-CD      PIC X(03).
                15  ACUP-OLD-CUST-ADDR-ZIP             PIC X(10).
                15  ACUP-OLD-CUST-PHONE-NUM-1          PIC X(15).
                15  ACUP-OLD-CUST-PHONE-NUM-2          PIC X(15).
                15  ACUP-OLD-CUST-SSN-X                PIC X(09).
                15  ACUP-OLD-CUST-SSN                  REDEFINES
                    ACUP-OLD-CUST-SSN-X                PIC 9(09).
                15  ACUP-OLD-CUST-GOVT-ISSUED-ID       PIC X(20).
                15  ACUP-OLD-CUST-DOB-YYYY-MM-DD       PIC X(08).
                15  ACUP-OLD-CUST-EFT-ACCOUNT-ID       PIC X(10).
                15  ACUP-OLD-CUST-PRI-HOLDER-IND       PIC X(01).
                15  ACUP-OLD-CUST-FICO-SCORE-X         PIC X(03).
                15  ACUP-OLD-CUST-FICO-SCORE           REDEFINES
                    ACUP-OLD-CUST-FICO-SCORE-X         PIC 9(03).
          05 ACUP-NEW-DETAILS.
             10 ACUP-NEW-ACCT-DATA.
                15  ACUP-NEW-ACCT-ID-X                 PIC X(11).
                15  ACUP-NEW-ACCT-ID                   REDEFINES
                    ACUP-NEW-ACCT-ID-X                 PIC 9(11).
                15  ACUP-NEW-ACTIVE-STATUS             PIC X(01).
                15  ACUP-NEW-CURR-BAL-X                PIC X(12).
                15  ACUP-NEW-CURR-BAL-N                REDEFINES
                    ACUP-NEW-CURR-BAL-X                PIC S9(10)V99.
                15  ACUP-NEW-CREDIT-LIMIT-X            PIC X(12).
                15  ACUP-NEW-CREDIT-LIMIT-N            REDEFINES
                    ACUP-NEW-CREDIT-LIMIT-X            PIC S9(10)V99.
                15  ACUP-NEW-CASH-CREDIT-LMT-X         PIC X(12).
                15  ACUP-NEW-CASH-CREDIT-LMT-N         REDEFINES
                    ACUP-NEW-CASH-CREDIT-LMT-X         PIC S9(10)V99.
                15  ACUP-NEW-OPEN-DATE                 PIC X(08).
                15  ACUP-NEW-EXPIRAION-DATE            PIC X(08).
                15  ACUP-NEW-REISSUE-DATE              PIC X(08).
                15  ACUP-NEW-CURR-CYC-CREDIT-X         PIC X(12).
                15  ACUP-NEW-CURR-CYC-CREDIT-N         REDEFINES
                    ACUP-NEW-CURR-CYC-CREDIT-X         PIC S9(10)V99.
                15  ACUP-NEW-CURR-CYC-DEBIT-X          PIC X(12).
                15  ACUP-NEW-CURR-CYC-DEBIT-N          REDEFINES
                    ACUP-NEW-CURR-CYC-DEBIT-X          PIC S9(10)V99.
                15  ACUP-NEW-GROUP-ID                  PIC X(10).
             10 ACUP-NEW-CUST-DATA.
                15  ACUP-NEW-CUST-ID-X                 PIC X(09).
                15  ACUP-NEW-CUST-ID                   REDEFINES
                    ACUP-NEW-CUST-ID-X                 PIC 9(09).
                15  ACUP-NEW-CUST-FIRST-NAME           PIC X(25).
                15  ACUP-NEW-CUST-MIDDLE-NAME          PIC X(25).
                15  ACUP-NEW-CUST-LAST-NAME            PIC X(25).
                15  ACUP-NEW-CUST-ADDR-LINE-1          PIC X(50).
                15  ACUP-NEW-CUST-ADDR-LINE-2          PIC X(50).
                15  ACUP-NEW-CUST-ADDR-LINE-3          PIC X(50).
                15  ACUP-NEW-CUST-ADDR-STATE-CD        PIC X(02).
                15  ACUP-NEW-CUST-ADDR-COUNTRY-CD      PIC X(03).
                15  ACUP-NEW-CUST-ADDR-ZIP             PIC X(10).
                15  ACUP-NEW-CUST-PHONE-NUM-1          PIC X(15).
                15  ACUP-NEW-CUST-PHONE-NUM-2          PIC X(15).
                15  ACUP-NEW-CUST-SSN-X.
                    20 ACUP-NEW-CUST-SSN-1             PIC X(03).
                    20 ACUP-NEW-CUST-SSN-2             PIC X(02).
                    20 ACUP-NEW-CUST-SSN-3             PIC X(04).
                15  ACUP-NEW-CUST-SSN                  REDEFINES
                    ACUP-NEW-CUST-SSN-X                PIC 9(09).
                15  ACUP-NEW-CUST-GOVT-ISSUED-ID       PIC X(20).
                15  ACUP-NEW-CUST-DOB-YYYY-MM-DD       PIC X(08).
                15  ACUP-NEW-CUST-EFT-ACCOUNT-ID       PIC X(10).
                15  ACUP-NEW-CUST-PRI-HOLDER-IND       PIC X(01).
                15  ACUP-NEW-CUST-FICO-SCORE-X         PIC X(03).
                15  ACUP-NEW-CUST-FICO-SCORE           REDEFINES
                    ACUP-NEW-CUST-FICO-SCORE-X         PIC 9(03).


       01 WS-INPUT-CONVERSION-FIELDS.
          05 WS-INPUT-CREDIT-LIMIT         PIC S9(10)V99 VALUE ZEROS.
          05 WS-INPUT-CASH-LIMIT           PIC S9(10)V99 VALUE ZEROS.
          05 WS-INPUT-CURR-BAL             PIC S9(10)V99 VALUE ZEROS.
          05 WS-INPUT-CYC-CREDIT           PIC S9(10)V99 VALUE ZEROS.
          05 WS-INPUT-CYC-DEBIT            PIC S9(10)V99 VALUE ZEROS.

       01  WS-COMMAREA                                 PIC X(2000).

      * RPC Communication Area - MUST MATCH COACTUPL EXACTLY
       01 WS-RPC-COMMAREA.
          05 LK-INPUT-PARMS.
             10 LK-IN-OPERATION           PIC X(01).
                88 OP-READ                          VALUE 'R'.
                88 OP-UPDATE                        VALUE 'U'.
                88 OP-VALIDATE                      VALUE 'V'.
             10 LK-IN-ACCT-ID             PIC X(11).
             10 LK-IN-ACCT-DATA.
                15 LK-IN-ACCT-ACTIVE-STATUS    PIC X(01).
                15 LK-IN-ACCT-CREDIT-LIMIT     PIC S9(10)V99.
                15 LK-IN-ACCT-CASH-LIMIT       PIC S9(10)V99.
                15 LK-IN-ACCT-CURR-BAL         PIC S9(10)V99.
                15 LK-IN-ACCT-CURR-CYC-CREDIT  PIC S9(10)V99.
                15 LK-IN-ACCT-CURR-CYC-DEBIT   PIC S9(10)V99.
                15 LK-IN-ACCT-OPEN-DATE.
                   20 LK-IN-ACCT-OPEN-YEAR     PIC X(4).
                   20 LK-IN-ACCT-OPEN-MON      PIC X(2).
                   20 LK-IN-ACCT-OPEN-DAY      PIC X(2).
                15 LK-IN-ACCT-EXPIRATION-DATE.
                   20 LK-IN-ACCT-EXP-YEAR      PIC X(4).
                   20 LK-IN-ACCT-EXP-MON       PIC X(2).
                   20 LK-IN-ACCT-EXP-DAY       PIC X(2).
                15 LK-IN-ACCT-REISSUE-DATE.
                   20 LK-IN-ACCT-REISSUE-YEAR  PIC X(4).
                   20 LK-IN-ACCT-REISSUE-MON   PIC X(2).
                   20 LK-IN-ACCT-REISSUE-DAY   PIC X(2).
                15 LK-IN-ACCT-GROUP-ID         PIC X(10).
             10 LK-IN-CUST-DATA.
                15 LK-IN-CUST-ID               PIC X(9).
                15 LK-IN-CUST-FIRST-NAME       PIC X(25).
                15 LK-IN-CUST-MIDDLE-NAME      PIC X(25).
                15 LK-IN-CUST-LAST-NAME        PIC X(25).
                15 LK-IN-CUST-SSN.
                   20 LK-IN-CUST-SSN-1         PIC X(3).
                   20 LK-IN-CUST-SSN-2         PIC X(2).
                   20 LK-IN-CUST-SSN-3         PIC X(4).
                15 LK-IN-CUST-DOB.
                   20 LK-IN-CUST-DOB-YEAR      PIC X(4).
                   20 LK-IN-CUST-DOB-MON       PIC X(2).
                   20 LK-IN-CUST-DOB-DAY       PIC X(2).
                15 LK-IN-CUST-ADDR-LINE-1      PIC X(50).
                15 LK-IN-CUST-ADDR-LINE-2      PIC X(50).
                15 LK-IN-CUST-ADDR-LINE-3      PIC X(50).
                15 LK-IN-CUST-ADDR-STATE-CD    PIC X(2).
                15 LK-IN-CUST-ADDR-COUNTRY-CD  PIC X(3).
                15 LK-IN-CUST-ADDR-ZIP         PIC X(10).
                15 LK-IN-CUST-PHONE-NUM-1.
                   20 LK-IN-CUST-PHONE-1A      PIC X(3).
                   20 LK-IN-CUST-PHONE-1B      PIC X(3).
                   20 LK-IN-CUST-PHONE-1C      PIC X(4).
                15 LK-IN-CUST-PHONE-NUM-2.
                   20 LK-IN-CUST-PHONE-2A      PIC X(3).
                   20 LK-IN-CUST-PHONE-2B      PIC X(3).
                   20 LK-IN-CUST-PHONE-2C      PIC X(4).
                15 LK-IN-CUST-GOVT-ISSUED-ID   PIC X(20).
                15 LK-IN-CUST-EFT-ACCOUNT-ID   PIC X(10).
                15 LK-IN-CUST-PRI-HOLDER-IND   PIC X(1).
                15 LK-IN-CUST-FICO-SCORE       PIC X(3).
          05 LK-OUTPUT-STATUS.
             10 LK-OUT-RETURN-CODE        PIC 9(02).
                88 RC-SUCCESS             VALUE 00.
                88 RC-NOT-FOUND           VALUE 01.
                88 RC-NO-CHANGES          VALUE 02.
                88 RC-INPUT-ERROR         VALUE 03.
                88 RC-UPDATE-ERROR        VALUE 98.
                88 RC-DATABASE-ERROR      VALUE 99.
             10 LK-OUT-MESSAGE            PIC X(80).
             10 LK-OUT-ERROR-FIELD        PIC X(25).
          05 LK-OUTPUT-DATA.
             10 LK-OUT-ACCT-DATA.
                15 LK-OUT-ACCT-ID              PIC X(11).
                15 LK-OUT-ACCT-ACTIVE-STATUS   PIC X(01).
                15 LK-OUT-ACCT-CREDIT-LIMIT    PIC S9(10)V99.
                15 LK-OUT-ACCT-CASH-LIMIT      PIC S9(10)V99.
                15 LK-OUT-ACCT-CURR-BAL        PIC S9(10)V99.
                15 LK-OUT-ACCT-CURR-CYC-CREDIT PIC S9(10)V99.
                15 LK-OUT-ACCT-CURR-CYC-DEBIT  PIC S9(10)V99.
                15 LK-OUT-ACCT-OPEN-DATE       PIC X(10).
                15 LK-OUT-ACCT-EXPIRATION-DATE PIC X(10).
                15 LK-OUT-ACCT-REISSUE-DATE    PIC X(10).
                15 LK-OUT-ACCT-GROUP-ID        PIC X(10).
                15 LK-OUT-ACCT-CARD-NUM        PIC X(16).
             10 LK-OUT-CUST-DATA.
                15 LK-OUT-CUST-ID              PIC X(9).
                15 LK-OUT-CUST-FIRST-NAME      PIC X(25).
                15 LK-OUT-CUST-MIDDLE-NAME     PIC X(25).
                15 LK-OUT-CUST-LAST-NAME       PIC X(25).
                15 LK-OUT-CUST-SSN             PIC X(9).
                15 LK-OUT-CUST-DOB             PIC X(10).
                15 LK-OUT-CUST-ADDR-LINE-1     PIC X(50).
                15 LK-OUT-CUST-ADDR-LINE-2     PIC X(50).
                15 LK-OUT-CUST-ADDR-LINE-3     PIC X(50).
                15 LK-OUT-CUST-ADDR-STATE-CD   PIC X(2).
                15 LK-OUT-CUST-ADDR-COUNTRY-CD PIC X(3).
                15 LK-OUT-CUST-ADDR-ZIP        PIC X(10).
                15 LK-OUT-CUST-PHONE-NUM-1     PIC X(15).
                15 LK-OUT-CUST-PHONE-NUM-2     PIC X(15).
                15 LK-OUT-CUST-GOVT-ISSUED-ID  PIC X(20).
                15 LK-OUT-CUST-EFT-ACCOUNT-ID  PIC  X(10).
                15 LK-OUT-CUST-PRI-HOLDER-IND  PIC X(1).
                15 LK-OUT-CUST-FICO-SCORE      PIC 9(3).

       LINKAGE SECTION.
       01  DFHCOMMAREA.
         05  FILLER                                PIC X(1)
             OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.

       PROCEDURE DIVISION.
       0000-MAIN.

           EXEC CICS HANDLE ABEND
                     LABEL(ABEND-ROUTINE)
           END-EXEC

           INITIALIZE WS-VARIABLES
                      WS-COMMAREA
                      WS-RPC-COMMAREA
      *****************************************************************
      * Store our context
      *****************************************************************
           MOVE LIT-THISTRANID       TO WS-TRANID
      *****************************************************************
      * Ensure error message is cleared                               *
      *****************************************************************
           SET WS-RETURN-MSG-OFF  TO TRUE
      *****************************************************************
      * Store passed data if  any                *
      *****************************************************************

           IF EIBCALEN IS EQUAL TO 0
               OR (CDEMO-FROM-PROGRAM = LIT-MENUPGM
               AND NOT CDEMO-PGM-REENTER)
              INITIALIZE CARDDEMO-COMMAREA
                         WS-THIS-PROGCOMMAREA
              SET CDEMO-PGM-ENTER TO TRUE
              SET ACUP-DETAILS-NOT-FETCHED TO TRUE
           ELSE
              MOVE DFHCOMMAREA (1:LENGTH OF CARDDEMO-COMMAREA)  TO
                                CARDDEMO-COMMAREA
              MOVE DFHCOMMAREA(LENGTH OF CARDDEMO-COMMAREA + 1:
                               LENGTH OF WS-THIS-PROGCOMMAREA ) TO
                                WS-THIS-PROGCOMMAREA
           END-IF
      *****************************************************************
      * Remap PFkeys as needed.
      * Store the Mapped PF Key
      *****************************************************************
           PERFORM YYYY-STORE-PFKEY
              THRU YYYY-STORE-PFKEY-EXIT

      *****************************************************************
      * Check the AID to see if its valid at this point               *
      * F3 - Exit
      * Enter show screen again
      *****************************************************************
           SET PFK-INVALID TO TRUE
           IF CCARD-AID-ENTER OR
              CCARD-AID-PFK03 OR
              (CCARD-AID-PFK05 AND ACUP-CHANGES-OK-NOT-CONFIRMED)
                              OR
              (CCARD-AID-PFK12 AND NOT ACUP-DETAILS-NOT-FETCHED)
              SET PFK-VALID TO TRUE
           END-IF

           IF PFK-INVALID
              SET CCARD-AID-ENTER TO TRUE
           END-IF

      *****************************************************************
      * Decide what to do based on inputs received
      *****************************************************************
           EVALUATE TRUE
      ******************************************************************
      *       USER PRESSES PF03 TO EXIT
      *  OR   USER IS DONE WITH UPDATE
      *            XCTL TO CALLING PROGRAM OR MAIN MENU
      ******************************************************************
              WHEN CCARD-AID-PFK03
                   SET CCARD-AID-PFK03     TO TRUE
                   IF CDEMO-FROM-TRANID    EQUAL LOW-VALUES
                   OR CDEMO-FROM-TRANID    EQUAL SPACES
                      MOVE LIT-MENUTRANID  TO CDEMO-TO-TRANID
                   ELSE
                      MOVE CDEMO-FROM-TRANID  TO CDEMO-TO-TRANID
                   END-IF

                   IF CDEMO-FROM-PROGRAM   EQUAL LOW-VALUES
                   OR CDEMO-FROM-PROGRAM   EQUAL SPACES
                      MOVE LIT-MENUPGM     TO CDEMO-TO-PROGRAM
                   ELSE
                      MOVE CDEMO-FROM-PROGRAM TO CDEMO-TO-PROGRAM
                   END-IF

                   MOVE LIT-THISTRANID     TO CDEMO-FROM-TRANID
                   MOVE LIT-THISPGM        TO CDEMO-FROM-PROGRAM

                   SET  CDEMO-USRTYP-USER  TO TRUE
                   SET  CDEMO-PGM-ENTER    TO TRUE
                   MOVE LIT-THISMAPSET     TO CDEMO-LAST-MAPSET
                   MOVE LIT-THISMAP        TO CDEMO-LAST-MAP

                   EXEC CICS
                        SYNCPOINT
                   END-EXEC

                   EXEC CICS XCTL
                        PROGRAM (CDEMO-TO-PROGRAM)
                        COMMAREA(CARDDEMO-COMMAREA)
                   END-EXEC
      ******************************************************************
      *       FRESH ENTRY INTO PROGRAM
      *            ASK THE USER FOR THE KEYS TO FETCH CARD TO BE UPDATED
      ******************************************************************
              WHEN ACUP-DETAILS-NOT-FETCHED
               AND CDEMO-PGM-ENTER
              WHEN CDEMO-FROM-PROGRAM   EQUAL LIT-MENUPGM
               AND NOT CDEMO-PGM-REENTER
                   INITIALIZE WS-THIS-PROGCOMMAREA
                   PERFORM 3000-SEND-MAP THRU
                           3000-SEND-MAP-EXIT
                   SET CDEMO-PGM-REENTER        TO TRUE
                   SET ACUP-DETAILS-NOT-FETCHED TO TRUE
                   GO TO COMMON-RETURN
      ******************************************************************
      *       ACCT DATA CHANGES REVIEWED, OKAYED AND DONE SUCESSFULLY
      *            RESET THE SEARCH KEYS
      *            ASK THE USER FOR FRESH SEARCH CRITERIA
      ******************************************************************
              WHEN ACUP-CHANGES-OKAYED-AND-DONE
              WHEN ACUP-CHANGES-FAILED
                   INITIALIZE WS-THIS-PROGCOMMAREA
                              CDEMO-ACCT-ID
                   SET CDEMO-PGM-ENTER            TO TRUE
                   PERFORM 3000-SEND-MAP THRU
                           3000-SEND-MAP-EXIT
                   SET CDEMO-PGM-REENTER          TO TRUE
                   SET ACUP-DETAILS-NOT-FETCHED   TO TRUE
                   GO TO COMMON-RETURN
      ******************************************************************
      *      ACCT DATA HAS BEEN PRESENTED TO USER
      *            CHECK THE USER INPUTS
      *            DECIDE WHAT TO DO
      *            PRESENT NEXT STEPS TO USER
      ******************************************************************
              WHEN OTHER
                   PERFORM 1000-PROCESS-INPUTS
                      THRU 1000-PROCESS-INPUTS-EXIT
                   PERFORM 2000-DECIDE-ACTION
                      THRU 2000-DECIDE-ACTION-EXIT
                   PERFORM 3000-SEND-MAP
                      THRU 3000-SEND-MAP-EXIT
                   GO TO COMMON-RETURN
           END-EVALUATE
           .

       COMMON-RETURN.
           MOVE WS-RETURN-MSG     TO CCARD-ERROR-MSG

           MOVE  CARDDEMO-COMMAREA    TO WS-COMMAREA
           MOVE  WS-THIS-PROGCOMMAREA TO
                  WS-COMMAREA(LENGTH OF CARDDEMO-COMMAREA + 1:
                               LENGTH OF WS-THIS-PROGCOMMAREA )

           EXEC CICS RETURN
                TRANSID (LIT-THISTRANID)
                COMMAREA (WS-COMMAREA)
                LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC
           .
       0000-MAIN-EXIT.
           EXIT
           .

       1000-PROCESS-INPUTS.
           PERFORM 1100-RECEIVE-MAP
              THRU 1100-RECEIVE-MAP-EXIT
           MOVE LIT-THISPGM    TO CCARD-NEXT-PROG
           MOVE LIT-THISMAPSET TO CCARD-NEXT-MAPSET
           MOVE LIT-THISMAP    TO CCARD-NEXT-MAP
           .

       1000-PROCESS-INPUTS-EXIT.
           EXIT
           .

       1100-RECEIVE-MAP.
           EXEC CICS RECEIVE MAP(LIT-THISMAP)
                     MAPSET(LIT-THISMAPSET)
                     INTO(CACTUPAI)
                     RESP(WS-RESP-CD)
           END-EXEC

      *    ALWAYS COLLECT ACCOUNT ID FIRST
           IF  ACCTSIDI OF CACTUPAI = '*'
           OR  ACCTSIDI OF CACTUPAI = SPACES
               MOVE ZEROS            TO CDEMO-ACCT-ID
           ELSE
               MOVE ACCTSIDI OF CACTUPAI TO CDEMO-ACCT-ID
               IF ACCTSIDI OF CACTUPAI IS NUMERIC
                   IF FUNCTION NUMVAL(ACCTSIDI OF CACTUPAI) > 0
                       SET FLG-ACCTFILTER-ISVALID TO TRUE
                   ELSE
                       SET FLG-ACCTFILTER-NOT-OK TO TRUE
                   END-IF
               ELSE
                   SET FLG-ACCTFILTER-NOT-OK TO TRUE
               END-IF
           END-IF

      *    IF NO DETAILS FETCHED YET, STOP HERE
           IF ACUP-DETAILS-NOT-FETCHED
              INITIALIZE ACUP-NEW-DETAILS
              MOVE CDEMO-ACCT-ID TO ACUP-NEW-ACCT-ID-X
              GO TO 1100-RECEIVE-MAP-EXIT
           END-IF

      *    DETAILS ARE LOADED - COLLECT ALL CURRENT SCREEN DATA
           INITIALIZE ACUP-NEW-DETAILS

      * Collect ALL fields from the screen immediately after RECEIVE
           PERFORM 1150-COLLECT-SCREEN-DATA
              THRU 1150-COLLECT-SCREEN-DATA-EXIT

           .
       1100-RECEIVE-MAP-EXIT.
           EXIT
           .

       1150-COLLECT-SCREEN-DATA.
      * IMPORTANT: This paragraph must collect ALL screen changes
      * for every field, not just the numeric amounts

      * CRITICAL: Collect Account ID first
            IF  ACCTSIDI OF CACTUPAI = '*'
            OR  ACCTSIDI OF CACTUPAI = SPACES
                MOVE LOW-VALUES           TO ACUP-NEW-ACCT-ID-X
            ELSE
                MOVE ACCTSIDI OF CACTUPAI TO ACUP-NEW-ACCT-ID-X
            END-IF.

      * Active Status
           IF  ACSTTUSI OF CACTUPAI = '*'
           OR  ACSTTUSI OF CACTUPAI = SPACES
               MOVE LOW-VALUES           TO ACUP-NEW-ACTIVE-STATUS
           ELSE
               MOVE ACSTTUSI OF CACTUPAI TO ACUP-NEW-ACTIVE-STATUS
           END-IF

      * Credit Limit - Validate and convert edited display to raw numeric
           IF  ACRDLIMI OF CACTUPAI = '*'
           OR  ACRDLIMI OF CACTUPAI = SPACES
               MOVE ZEROS                TO WS-INPUT-CREDIT-LIMIT
           ELSE
               IF FUNCTION TEST-NUMVAL-C(ACRDLIMI OF CACTUPAI) = 0
                  COMPUTE WS-INPUT-CREDIT-LIMIT =
                     FUNCTION NUMVAL-C(ACRDLIMI OF CACTUPAI)
               ELSE
                  SET INPUT-ERROR TO TRUE
                  MOVE -1 TO ACRDLIML OF CACTUPAI
                  IF WS-RETURN-MSG-OFF
                     MOVE 'Credit Limit must be a valid amount' TO
                     WS-RETURN-MSG
                  END-IF
                  MOVE ZEROS             TO WS-INPUT-CREDIT-LIMIT
               END-IF
           END-IF

      * Cash Limit - Validate and convert edited display to raw numeric
           IF  ACSHLIMI OF CACTUPAI = '*'
           OR  ACSHLIMI OF CACTUPAI = SPACES
               MOVE ZEROS                TO WS-INPUT-CASH-LIMIT
           ELSE
               IF FUNCTION TEST-NUMVAL-C(ACSHLIMI OF CACTUPAI) = 0
                  COMPUTE WS-INPUT-CASH-LIMIT =
                     FUNCTION NUMVAL-C(ACSHLIMI OF CACTUPAI)
               ELSE
                  SET INPUT-ERROR TO TRUE
                  MOVE -1 TO ACSHLIML OF CACTUPAI
                  IF WS-RETURN-MSG-OFF
                     MOVE 'Cash Limit must be a valid amount' TO
                     WS-RETURN-MSG
                  END-IF
                  MOVE ZEROS             TO WS-INPUT-CASH-LIMIT
               END-IF
           END-IF

      * Current Balance - Validate and convert edited display to raw
      * numeric
           IF  ACURBALI OF CACTUPAI = '*'
           OR  ACURBALI OF CACTUPAI = SPACES
               MOVE ZEROS                TO WS-INPUT-CURR-BAL
           ELSE
               IF FUNCTION TEST-NUMVAL-C(ACURBALI OF CACTUPAI) = 0
                  COMPUTE WS-INPUT-CURR-BAL =
                     FUNCTION NUMVAL-C(ACURBALI OF CACTUPAI)
               ELSE
                  SET INPUT-ERROR TO TRUE
                  MOVE -1 TO ACURBALL OF CACTUPAI
                  IF WS-RETURN-MSG-OFF
                     MOVE 'Current Balance must be a valid amount'
                     TO WS-RETURN-MSG
                  END-IF
                  MOVE ZEROS             TO WS-INPUT-CURR-BAL
               END-IF
           END-IF

      * Current Cycle Credit - Validate and convert edited display to
      *raw numeriC
           IF  ACRCYCRI OF CACTUPAI = '*'
           OR  ACRCYCRI OF CACTUPAI = SPACES
               MOVE ZEROS                TO WS-INPUT-CYC-CREDIT
           ELSE
               IF FUNCTION TEST-NUMVAL-C(ACRCYCRI OF CACTUPAI) = 0
                  COMPUTE WS-INPUT-CYC-CREDIT =
                     FUNCTION NUMVAL-C(ACRCYCRI OF CACTUPAI)
               ELSE
                  SET INPUT-ERROR TO TRUE
                  MOVE -1 TO ACRCYCRL OF CACTUPAI
                  IF WS-RETURN-MSG-OFF
                     MOVE 'Current Cycle Credit must be a valid amount'
                     TO WS-RETURN-MSG
                  END-IF
                   MOVE ZEROS             TO WS-INPUT-CYC-CREDIT
               END-IF
           END-IF

      * Current Cycle Debit - Validate and convert edited display to raw numeric
           IF  ACRCYDBI OF CACTUPAI = '*'
           OR  ACRCYDBI OF CACTUPAI = SPACES
               MOVE ZEROS                TO WS-INPUT-CYC-DEBIT
           ELSE
               IF FUNCTION TEST-NUMVAL-C(ACRCYDBI OF CACTUPAI) = 0
                  COMPUTE WS-INPUT-CYC-DEBIT =
                     FUNCTION NUMVAL-C(ACRCYDBI OF CACTUPAI)
               ELSE
                  SET INPUT-ERROR TO TRUE
                  MOVE -1 TO ACRCYDBL OF CACTUPAI
                  IF WS-RETURN-MSG-OFF
                     MOVE 'Current Cycle Debit must be a valid amount'
                      TO WS-RETURN-MSG
                  END-IF
                  MOVE ZEROS             TO WS-INPUT-CYC-DEBIT
               END-IF
           END-IF

      * Open date
           IF  OPNYEARI OF CACTUPAI = '*'
           OR  OPNYEARI OF CACTUPAI = SPACES
               MOVE LOW-VALUES           TO ACUP-NEW-OPEN-DATE(1:4)
           ELSE
               MOVE OPNYEARI OF CACTUPAI TO ACUP-NEW-OPEN-DATE(1:4)
           END-IF

           IF  OPNMONI OF CACTUPAI = '*'
           OR  OPNMONI OF CACTUPAI = SPACES
               MOVE LOW-VALUES           TO ACUP-NEW-OPEN-DATE(5:2)
           ELSE
               MOVE OPNMONI OF CACTUPAI TO  ACUP-NEW-OPEN-DATE(5:2)
           END-IF

           IF  OPNDAYI OF CACTUPAI = '*'
           OR  OPNDAYI OF CACTUPAI = SPACES
               MOVE LOW-VALUES           TO ACUP-NEW-OPEN-DATE(7:2)
           ELSE
               MOVE OPNDAYI OF CACTUPAI TO  ACUP-NEW-OPEN-DATE(7:2)
           END-IF

      * Expiry date
           IF  EXPYEARI OF CACTUPAI = '*'
           OR  EXPYEARI OF CACTUPAI = SPACES
               MOVE LOW-VALUES           TO ACUP-NEW-EXPIRAION-DATE(1:4)
           ELSE
               MOVE EXPYEARI OF CACTUPAI TO ACUP-NEW-EXPIRAION-DATE(1:4)
           END-IF

           IF  EXPMONI OF CACTUPAI = '*'
           OR  EXPMONI OF CACTUPAI = SPACES
               MOVE LOW-VALUES           TO ACUP-NEW-EXPIRAION-DATE(5:2)
           ELSE
               MOVE EXPMONI OF CACTUPAI TO  ACUP-NEW-EXPIRAION-DATE(5:2)
           END-IF

           IF  EXPDAYI OF CACTUPAI = '*'
           OR  EXPDAYI OF CACTUPAI = SPACES
               MOVE LOW-VALUES           TO ACUP-NEW-EXPIRAION-DATE(7:2)
           ELSE
               MOVE EXPDAYI OF CACTUPAI TO  ACUP-NEW-EXPIRAION-DATE(7:2)
           END-IF

      * Reissue date
           IF  RISYEARI OF CACTUPAI = '*'
           OR  RISYEARI OF CACTUPAI = SPACES
               MOVE LOW-VALUES           TO ACUP-NEW-REISSUE-DATE(1:4)
           ELSE
               MOVE RISYEARI OF CACTUPAI TO ACUP-NEW-REISSUE-DATE(1:4)
           END-IF

           IF  RISMONI OF CACTUPAI = '*'
           OR  RISMONI OF CACTUPAI = SPACES
               MOVE LOW-VALUES           TO ACUP-NEW-REISSUE-DATE(5:2)
           ELSE
               MOVE RISMONI OF CACTUPAI TO  ACUP-NEW-REISSUE-DATE(5:2)
           END-IF

           IF  RISDAYI OF CACTUPAI = '*'
           OR  RISDAYI OF CACTUPAI = SPACES
               MOVE LOW-VALUES           TO ACUP-NEW-REISSUE-DATE(7:2)
           ELSE
               MOVE RISDAYI OF CACTUPAI TO  ACUP-NEW-REISSUE-DATE(7:2)
           END-IF

      * Account Group
           IF  AADDGRPI OF CACTUPAI = '*'
           OR  AADDGRPI OF CACTUPAI = SPACES
               MOVE LOW-VALUES           TO ACUP-NEW-GROUP-ID
           ELSE
               MOVE AADDGRPI OF CACTUPAI TO ACUP-NEW-GROUP-ID
           END-IF

      ******************************************************************
      *    Customer Master data - COLLECT ALL CHANGES FROM SCREEN
      ******************************************************************
      * Customer Id (actually not editable)
           IF  ACSTNUMI OF CACTUPAI = '*'
           OR  ACSTNUMI OF CACTUPAI = SPACES
               MOVE LOW-VALUES           TO ACUP-NEW-CUST-ID-X
           ELSE
               MOVE ACSTNUMI OF CACTUPAI TO ACUP-NEW-CUST-ID-X
           END-IF

      * Social Security Number
           IF  ACTSSN1I OF CACTUPAI = '*'
           OR  ACTSSN1I OF CACTUPAI = SPACES
               MOVE LOW-VALUES           TO ACUP-NEW-CUST-SSN-1
           ELSE
               MOVE ACTSSN1I OF CACTUPAI TO ACUP-NEW-CUST-SSN-1
           END-IF

           IF  ACTSSN2I OF CACTUPAI = '*'
           OR  ACTSSN2I OF CACTUPAI = SPACES
               MOVE LOW-VALUES           TO ACUP-NEW-CUST-SSN-2
           ELSE
               MOVE ACTSSN2I OF CACTUPAI TO ACUP-NEW-CUST-SSN-2
           END-IF

           IF  ACTSSN3I OF CACTUPAI = '*'
           OR  ACTSSN3I OF CACTUPAI = SPACES
               MOVE LOW-VALUES           TO ACUP-NEW-CUST-SSN-3
           ELSE
               MOVE ACTSSN3I OF CACTUPAI TO ACUP-NEW-CUST-SSN-3
           END-IF

      * Date of birth
           IF  DOBYEARI OF CACTUPAI = '*'
           OR  DOBYEARI OF CACTUPAI = SPACES
               MOVE LOW-VALUES
                TO ACUP-NEW-CUST-DOB-YYYY-MM-DD(1:4)
           ELSE
               MOVE DOBYEARI OF CACTUPAI
               TO ACUP-NEW-CUST-DOB-YYYY-MM-DD(1:4)
           END-IF

           IF  DOBMONI OF CACTUPAI = '*'
           OR  DOBMONI OF CACTUPAI = SPACES
               MOVE LOW-VALUES
               TO ACUP-NEW-CUST-DOB-YYYY-MM-DD(5:2)
           ELSE
               MOVE DOBMONI OF CACTUPAI
               TO ACUP-NEW-CUST-DOB-YYYY-MM-DD(5:2)
           END-IF

           IF  DOBDAYI OF CACTUPAI = '*'
           OR  DOBDAYI OF CACTUPAI = SPACES
               MOVE LOW-VALUES
               TO ACUP-NEW-CUST-DOB-YYYY-MM-DD(7:2)
           ELSE
               MOVE DOBDAYI OF CACTUPAI
                TO ACUP-NEW-CUST-DOB-YYYY-MM-DD(7:2)
           END-IF

      * FICO
           IF  ACSTFCOI OF CACTUPAI = '*'
           OR  ACSTFCOI OF CACTUPAI = SPACES
               MOVE LOW-VALUES           TO ACUP-NEW-CUST-FICO-SCORE-X
           ELSE
               MOVE ACSTFCOI OF CACTUPAI TO ACUP-NEW-CUST-FICO-SCORE-X
           END-IF

      * First Name
           IF  ACSFNAMI OF CACTUPAI = '*'
           OR  ACSFNAMI OF CACTUPAI = SPACES
               MOVE LOW-VALUES           TO ACUP-NEW-CUST-FIRST-NAME
           ELSE
               MOVE ACSFNAMI OF CACTUPAI TO ACUP-NEW-CUST-FIRST-NAME
           END-IF

      * Middle Name
           IF  ACSMNAMI OF CACTUPAI = '*'
           OR  ACSMNAMI OF CACTUPAI = SPACES
               MOVE LOW-VALUES           TO ACUP-NEW-CUST-MIDDLE-NAME
           ELSE
               MOVE ACSMNAMI OF CACTUPAI TO ACUP-NEW-CUST-MIDDLE-NAME
           END-IF

      * Last Name
           IF  ACSLNAMI OF CACTUPAI = '*'
           OR  ACSLNAMI OF CACTUPAI = SPACES
               MOVE LOW-VALUES           TO ACUP-NEW-CUST-LAST-NAME
           ELSE
               MOVE ACSLNAMI OF CACTUPAI TO ACUP-NEW-CUST-LAST-NAME
           END-IF

      * Address
           IF  ACSADL1I OF CACTUPAI = '*'
           OR  ACSADL1I OF CACTUPAI = SPACES
               MOVE LOW-VALUES           TO ACUP-NEW-CUST-ADDR-LINE-1
           ELSE
               MOVE ACSADL1I OF CACTUPAI TO ACUP-NEW-CUST-ADDR-LINE-1
           END-IF

           IF  ACSADL2I OF CACTUPAI = '*'
           OR  ACSADL2I OF CACTUPAI = SPACES
               MOVE LOW-VALUES           TO ACUP-NEW-CUST-ADDR-LINE-2
           ELSE
               MOVE ACSADL2I OF CACTUPAI TO ACUP-NEW-CUST-ADDR-LINE-2
           END-IF

           IF  ACSCITYI OF CACTUPAI = '*'
           OR  ACSCITYI OF CACTUPAI = SPACES
               MOVE LOW-VALUES           TO ACUP-NEW-CUST-ADDR-LINE-3
           ELSE
               MOVE ACSCITYI OF CACTUPAI TO ACUP-NEW-CUST-ADDR-LINE-3
           END-IF

           IF  ACSSTTEI OF CACTUPAI = '*'
           OR  ACSSTTEI OF CACTUPAI = SPACES
               MOVE LOW-VALUES           TO ACUP-NEW-CUST-ADDR-STATE-CD
           ELSE
               MOVE ACSSTTEI OF CACTUPAI TO ACUP-NEW-CUST-ADDR-STATE-CD
           END-IF

           IF  ACSCTRYI OF CACTUPAI = '*'
           OR  ACSCTRYI OF CACTUPAI = SPACES
              MOVE LOW-VALUES           TO ACUP-NEW-CUST-ADDR-COUNTRY-CD
           ELSE
              MOVE ACSCTRYI OF CACTUPAI TO ACUP-NEW-CUST-ADDR-COUNTRY-CD
           END-IF

           IF  ACSZIPCI OF CACTUPAI = '*'
           OR  ACSZIPCI OF CACTUPAI = SPACES
               MOVE LOW-VALUES           TO ACUP-NEW-CUST-ADDR-ZIP
           ELSE
               MOVE ACSZIPCI OF CACTUPAI TO ACUP-NEW-CUST-ADDR-ZIP
           END-IF

      * Phone numbers
           IF  ACSPH1AI OF CACTUPAI = '*'
           OR  ACSPH1AI OF CACTUPAI = SPACES
               MOVE LOW-VALUES
                TO ACUP-NEW-CUST-PHONE-NUM-1(2:3)
           ELSE
               MOVE ACSPH1AI OF CACTUPAI
               TO ACUP-NEW-CUST-PHONE-NUM-1(2:3)
           END-IF

           IF  ACSPH1BI OF CACTUPAI = '*'
           OR  ACSPH1BI OF CACTUPAI = SPACES
               MOVE LOW-VALUES
                 TO ACUP-NEW-CUST-PHONE-NUM-1(6:3)
           ELSE
               MOVE ACSPH1BI OF CACTUPAI
               TO ACUP-NEW-CUST-PHONE-NUM-1(6:3)
           END-IF

           IF  ACSPH1CI OF CACTUPAI = '*'
           OR  ACSPH1CI OF CACTUPAI = SPACES
               MOVE LOW-VALUES
                  TO ACUP-NEW-CUST-PHONE-NUM-1(10:4)
           ELSE
               MOVE ACSPH1CI OF CACTUPAI
               TO ACUP-NEW-CUST-PHONE-NUM-1(10:4)
           END-IF

           IF  ACSPH2AI OF CACTUPAI = '*'
           OR  ACSPH2AI OF CACTUPAI = SPACES
               MOVE LOW-VALUES
                TO ACUP-NEW-CUST-PHONE-NUM-2(2:3)
           ELSE
               MOVE ACSPH2AI OF CACTUPAI
               TO ACUP-NEW-CUST-PHONE-NUM-2(2:3)
           END-IF

           IF  ACSPH2BI OF CACTUPAI = '*'
           OR  ACSPH2BI OF CACTUPAI = SPACES
               MOVE LOW-VALUES
                    TO ACUP-NEW-CUST-PHONE-NUM-2(6:3)
           ELSE
               MOVE ACSPH2BI OF CACTUPAI
               TO ACUP-NEW-CUST-PHONE-NUM-2(6:3)
           END-IF

           IF  ACSPH2CI OF CACTUPAI = '*'
           OR  ACSPH2CI OF CACTUPAI = SPACES
               MOVE LOW-VALUES
               TO ACUP-NEW-CUST-PHONE-NUM-2(10:4)
           ELSE
               MOVE ACSPH2CI OF CACTUPAI
               TO ACUP-NEW-CUST-PHONE-NUM-2(10:4)
           END-IF

      * Government Id
           IF  ACSGOVTI OF CACTUPAI = '*'
           OR  ACSGOVTI OF CACTUPAI = SPACES
               MOVE LOW-VALUES           TO ACUP-NEW-CUST-GOVT-ISSUED-ID
           ELSE
               MOVE ACSGOVTI OF CACTUPAI TO ACUP-NEW-CUST-GOVT-ISSUED-ID
           END-IF

      * EFT Code
           IF  ACSEFTCI OF CACTUPAI = '*'
           OR  ACSEFTCI OF CACTUPAI = SPACES
               MOVE LOW-VALUES           TO ACUP-NEW-CUST-EFT-ACCOUNT-ID
           ELSE
               MOVE ACSEFTCI OF CACTUPAI TO ACUP-NEW-CUST-EFT-ACCOUNT-ID
           END-IF

      * Primary Holder Indicator
           IF  ACSPFLGI OF CACTUPAI = '*'
           OR  ACSPFLGI OF CACTUPAI = SPACES
              MOVE LOW-VALUES            TO ACUP-NEW-CUST-PRI-HOLDER-IND
           ELSE
              MOVE ACSPFLGI OF CACTUPAI  TO ACUP-NEW-CUST-PRI-HOLDER-IND
           END-IF
           .
       1150-COLLECT-SCREEN-DATA-EXIT.
           EXIT.

       2000-DECIDE-ACTION.

              IF INPUT-ERROR
             GO TO 2000-DECIDE-ACTION-EXIT
              END-IF.
           EVALUATE TRUE
      ******************************************************************
      *       NO DETAILS SHOWN.
      *       SO GET THEM AND SETUP DETAIL EDIT SCREEN
      ******************************************************************
              WHEN ACUP-DETAILS-NOT-FETCHED
                 IF  FLG-ACCTFILTER-ISVALID
                     SET WS-RETURN-MSG-OFF       TO TRUE
                     PERFORM 9000-READ-ACCT-VIA-RPC
                        THRU 9000-READ-ACCT-VIA-RPC-EXIT
                     IF RC-SUCCESS
                        SET ACUP-SHOW-DETAILS    TO TRUE
                     END-IF
                 END-IF
      ******************************************************************
      *       CHANGES MADE. BUT USER CANCELS
      ******************************************************************
              WHEN CCARD-AID-PFK12
                 IF  FLG-ACCTFILTER-ISVALID
                     SET WS-RETURN-MSG-OFF       TO TRUE
                     PERFORM 9000-READ-ACCT-VIA-RPC
                        THRU 9000-READ-ACCT-VIA-RPC-EXIT
                     IF RC-SUCCESS
                        SET ACUP-SHOW-DETAILS    TO TRUE
                     END-IF
                 END-IF
      ******************************************************************
      *       DETAILS SHOWN - VALIDATE FIRST (ENTER KEY)
      ******************************************************************
              WHEN ACUP-SHOW-DETAILS
                 IF CCARD-AID-ENTER
                    PERFORM 9600-UPDATE-ACCT-VIA-RPC
                       THRU 9600-UPDATE-ACCT-VIA-RPC-EXIT

                    EVALUATE TRUE
                       WHEN RC-SUCCESS
                          SET ACUP-CHANGES-OK-NOT-CONFIRMED TO TRUE
                       WHEN RC-NO-CHANGES
                          CONTINUE
                       WHEN RC-INPUT-ERROR
                          SET ACUP-CHANGES-NOT-OK TO TRUE
                       WHEN OTHER
                          SET ACUP-CHANGES-OKAYED-BUT-FAILED TO TRUE
                    END-EVALUATE
                 END-IF
      ******************************************************************
      *       CRITICAL FIX: VALIDATE USER INPUT IN ERROR STATE
      ******************************************************************
              WHEN ACUP-CHANGES-NOT-OK
                 IF CCARD-AID-ENTER
                    PERFORM 9600-UPDATE-ACCT-VIA-RPC
                       THRU 9600-UPDATE-ACCT-VIA-RPC-EXIT

                    EVALUATE TRUE
                       WHEN RC-SUCCESS
                          SET ACUP-CHANGES-OK-NOT-CONFIRMED TO TRUE
                       WHEN RC-NO-CHANGES
                          CONTINUE
                       WHEN RC-INPUT-ERROR
                          CONTINUE
                       WHEN OTHER
                          SET ACUP-CHANGES-OKAYED-BUT-FAILED TO TRUE
                    END-EVALUATE
                 END-IF
      ******************************************************************
      *       DETAILS EDITED, FOUND OK, CONFIRM SAVE REQUESTED
      *       CONFIRMATION GIVEN. SO SAVE THE CHANGES
      ******************************************************************
              WHEN ACUP-CHANGES-OK-NOT-CONFIRMED
               AND CCARD-AID-PFK05
                 PERFORM 9600-UPDATE-ACCT-VIA-RPC
                    THRU 9600-UPDATE-ACCT-VIA-RPC-EXIT
                 EVALUATE TRUE
                    WHEN RC-SUCCESS
                       SET ACUP-CHANGES-OKAYED-AND-DONE   TO TRUE
                    WHEN RC-UPDATE-ERROR
                       SET ACUP-CHANGES-OKAYED-BUT-FAILED TO TRUE
                    WHEN OTHER
                       SET ACUP-CHANGES-OKAYED-BUT-FAILED TO TRUE
                 END-EVALUATE
      ******************************************************************
      *       DETAILS EDITED, FOUND OK, CONFIRM SAVE REQUESTED
      *       CONFIRMATION NOT GIVEN. SO SHOW DETAILS AGAIN
      ******************************************************************
              WHEN ACUP-CHANGES-OK-NOT-CONFIRMED
                  CONTINUE
      ******************************************************************
      *       SHOW CONFIRMATION. GO BACK TO SQUARE 1
      ******************************************************************
              WHEN ACUP-CHANGES-OKAYED-AND-DONE
                  SET ACUP-SHOW-DETAILS TO TRUE
                  IF CDEMO-FROM-TRANID    EQUAL LOW-VALUES
                  OR CDEMO-FROM-TRANID    EQUAL SPACES
                     MOVE ZEROES       TO CDEMO-ACCT-ID
                     MOVE LOW-VALUES   TO CDEMO-ACCT-STATUS
                  END-IF
              WHEN OTHER
                   MOVE LIT-THISPGM    TO ABEND-CULPRIT
                   MOVE '0001'         TO ABEND-CODE
                   MOVE SPACES         TO ABEND-REASON
                   MOVE 'UNEXPECTED DATA SCENARIO'
                                       TO ABEND-MSG
                   PERFORM ABEND-ROUTINE
                      THRU ABEND-ROUTINE-EXIT
           END-EVALUATE


           .
       2000-DECIDE-ACTION-EXIT.
           EXIT
           .


       3000-SEND-MAP.
           PERFORM 3100-SCREEN-INIT
              THRU 3100-SCREEN-INIT-EXIT
           PERFORM 3200-SETUP-SCREEN-VARS
              THRU 3200-SETUP-SCREEN-VARS-EXIT
           PERFORM 3250-SETUP-INFOMSG
              THRU 3250-SETUP-INFOMSG-EXIT
           PERFORM 3300-SETUP-SCREEN-ATTRS
              THRU 3300-SETUP-SCREEN-ATTRS-EXIT
           PERFORM 3400-SEND-SCREEN
              THRU 3400-SEND-SCREEN-EXIT
           .

       3000-SEND-MAP-EXIT.
           EXIT
           .

       3100-SCREEN-INIT.
           MOVE LOW-VALUES TO CACTUPAO

           MOVE FUNCTION CURRENT-DATE     TO WS-CURDATE-DATA

           MOVE CCDA-TITLE01              TO TITLE01O OF CACTUPAO
           MOVE CCDA-TITLE02              TO TITLE02O OF CACTUPAO
           MOVE LIT-THISTRANID            TO TRNNAMEO OF CACTUPAO
           MOVE LIT-THISPGM               TO PGMNAMEO OF CACTUPAO

           MOVE WS-CURDATE-MONTH          TO WS-CURDATE-MM
           MOVE WS-CURDATE-DAY            TO WS-CURDATE-DD
           MOVE WS-CURDATE-YEAR(3:2)      TO WS-CURDATE-YY

           MOVE WS-CURDATE-MM-DD-YY       TO CURDATEO OF CACTUPAO

           MOVE WS-CURTIME-HOURS          TO WS-CURTIME-HH
           MOVE WS-CURTIME-MINUTE         TO WS-CURTIME-MM
           MOVE WS-CURTIME-SECOND         TO WS-CURTIME-SS

           MOVE WS-CURTIME-HH-MM-SS       TO CURTIMEO OF CACTUPAO
           .

       3100-SCREEN-INIT-EXIT.
           EXIT
           .

      ******************************************************************
      * SECTION 2: Fix the 3200-SETUP-SCREEN-VARS paragraph
      ******************************************************************
       3200-SETUP-SCREEN-VARS.
      *    INITIALIZE SEARCH CRITERIA
           IF CDEMO-PGM-ENTER
              CONTINUE
           ELSE
           IF CDEMO-ACCT-ID = ZEROS
           AND FLG-ACCTFILTER-ISVALID
              MOVE SPACES                       TO ACCTSIDO OF CACTUPAO
           ELSE
              MOVE CDEMO-ACCT-ID             TO ACCTSIDO OF CACTUPAO
           END-IF

              EVALUATE TRUE
                WHEN ACUP-DETAILS-NOT-FETCHED
                WHEN CDEMO-ACCT-ID =  ZERO
                  PERFORM 3201-SHOW-INITIAL-VALUES
                     THRU 3201-SHOW-INITIAL-VALUES-EXIT
               WHEN ACUP-SHOW-DETAILS
                  PERFORM 3202-SHOW-ORIGINAL-VALUES
                     THRU 3202-SHOW-ORIGINAL-VALUES-EXIT
      *        CRITICAL: Handle ACUP-CHANGES-NOT-OK case
               WHEN ACUP-CHANGES-NOT-OK
                  PERFORM 3203-SHOW-UPDATED-VALUES
                     THRU 3203-SHOW-UPDATED-VALUES-EXIT
               WHEN ACUP-CHANGES-MADE
                  PERFORM 3203-SHOW-UPDATED-VALUES
                     THRU 3203-SHOW-UPDATED-VALUES-EXIT
               WHEN OTHER
                  PERFORM 3202-SHOW-ORIGINAL-VALUES
                     THRU 3202-SHOW-ORIGINAL-VALUES-EXIT
              END-EVALUATE
            END-IF
           .
       3200-SETUP-SCREEN-VARS-EXIT.
           EXIT
           .


       3201-SHOW-INITIAL-VALUES.
           MOVE SPACES                          TO  ACSTTUSO OF CACTUPAO
                                                   ACRDLIMO OF CACTUPAO
                                                   ACURBALO OF CACTUPAO
                                                   ACSHLIMO OF CACTUPAO
                                                   ACRCYCRO OF CACTUPAO
                                                   ACRCYDBO OF CACTUPAO
                                                   OPNYEARO OF CACTUPAO
                                                   OPNMONO  OF CACTUPAO
                                                   OPNDAYO  OF CACTUPAO
                                                   EXPYEARO OF CACTUPAO
                                                   EXPMONO  OF CACTUPAO
                                                   EXPDAYO  OF CACTUPAO
                                                   RISYEARO OF CACTUPAO
                                                   RISMONO  OF CACTUPAO
                                                   RISDAYO  OF CACTUPAO
                                                   AADDGRPO OF CACTUPAO
                                                   ACSTNUMO OF CACTUPAO
                                                   ACTSSN1O OF CACTUPAO
                                                   ACTSSN2O OF CACTUPAO
                                                   ACTSSN3O OF CACTUPAO
                                                   ACSTFCOO OF CACTUPAO
                                                   DOBYEARO OF CACTUPAO
                                                   DOBMONO  OF CACTUPAO
                                                   DOBDAYO  OF CACTUPAO
                                                   ACSFNAMO OF CACTUPAO
                                                   ACSMNAMO OF CACTUPAO
                                                   ACSLNAMO OF CACTUPAO
                                                   ACSADL1O OF CACTUPAO
                                                   ACSADL2O OF CACTUPAO
                                                   ACSCITYO OF CACTUPAO
                                                   ACSSTTEO OF CACTUPAO
                                                   ACSZIPCO OF CACTUPAO
                                                   ACSCTRYO OF CACTUPAO
                                                   ACSPH1AO OF CACTUPAO
                                                   ACSPH1BO OF CACTUPAO
                                                   ACSPH1CO OF CACTUPAO
                                                   ACSPH2AO OF CACTUPAO
                                                   ACSPH2BO OF CACTUPAO
                                                   ACSPH2CO OF CACTUPAO
                                                   ACSGOVTO OF CACTUPAO
                                                   ACSEFTCO OF CACTUPAO
                                                   ACSPFLGO OF CACTUPAO
           .

       3201-SHOW-INITIAL-VALUES-EXIT.
           EXIT
           .

       3202-SHOW-ORIGINAL-VALUES.

      * In 3202-SHOW-ORIGINAL-VALUES:
           MOVE ACUP-OLD-CREDIT-LIMIT-N     TO WS-CREDIT-LIMIT-DISP
           MOVE WS-CREDIT-LIMIT-DISP        TO ACRDLIMO OF CACTUPAO
           SET PROMPT-FOR-CHANGES              TO TRUE

      *    Format Active Status properly
           MOVE ACUP-OLD-ACTIVE-STATUS      TO ACSTTUSO OF CACTUPAO

      *    Format money fields using the proper display fields
           MOVE ACUP-OLD-CREDIT-LIMIT-N     TO WS-CREDIT-LIMIT-DISP
           MOVE WS-CREDIT-LIMIT-DISP        TO ACRDLIMO OF CACTUPAO

           MOVE ACUP-OLD-CASH-CREDIT-LMT-N  TO WS-CASH-LIMIT-DISP
           MOVE WS-CASH-LIMIT-DISP          TO ACSHLIMO OF CACTUPAO

           MOVE ACUP-OLD-CURR-BAL-N         TO WS-CURR-BAL-DISP
           MOVE WS-CURR-BAL-DISP            TO ACURBALO OF CACTUPAO

           MOVE ACUP-OLD-CURR-CYC-CREDIT-N  TO WS-CYC-CREDIT-DISP
           MOVE WS-CYC-CREDIT-DISP          TO ACRCYCRO OF CACTUPAO

           MOVE ACUP-OLD-CURR-CYC-DEBIT-N   TO WS-CYC-DEBIT-DISP
           MOVE WS-CYC-DEBIT-DISP           TO ACRCYDBO OF CACTUPAO
      *    Format date fields
           MOVE ACUP-OLD-OPEN-DATE(1:4)     TO OPNYEARO OF CACTUPAO
           MOVE ACUP-OLD-OPEN-DATE(5:2)     TO OPNMONO  OF CACTUPAO
           MOVE ACUP-OLD-OPEN-DATE(7:2)     TO OPNDAYO  OF CACTUPAO

           MOVE ACUP-OLD-EXPIRAION-DATE(1:4) TO EXPYEARO OF CACTUPAO
           MOVE ACUP-OLD-EXPIRAION-DATE(5:2) TO EXPMONO  OF CACTUPAO
           MOVE ACUP-OLD-EXPIRAION-DATE(7:2) TO EXPDAYO  OF CACTUPAO

           MOVE ACUP-OLD-REISSUE-DATE(1:4)  TO RISYEARO OF CACTUPAO
           MOVE ACUP-OLD-REISSUE-DATE(5:2)  TO RISMONO  OF CACTUPAO
           MOVE ACUP-OLD-REISSUE-DATE(7:2)  TO RISDAYO  OF CACTUPAO

           MOVE ACUP-OLD-GROUP-ID           TO AADDGRPO OF CACTUPAO

      *    Customer fields
           MOVE ACUP-OLD-CUST-ID-X          TO ACSTNUMO OF CACTUPAO
           MOVE ACUP-OLD-CUST-SSN-X(1:3)    TO ACTSSN1O OF CACTUPAO
           MOVE ACUP-OLD-CUST-SSN-X(4:2)    TO ACTSSN2O OF CACTUPAO
           MOVE ACUP-OLD-CUST-SSN-X(6:4)    TO ACTSSN3O OF CACTUPAO
           MOVE ACUP-OLD-CUST-FICO-SCORE-X  TO ACSTFCOO OF CACTUPAO

           MOVE ACUP-OLD-CUST-DOB-YYYY-MM-DD(1:4)
           TO DOBYEARO OF CACTUPAO
           MOVE ACUP-OLD-CUST-DOB-YYYY-MM-DD(5:2)
           TO DOBMONO  OF CACTUPAO
           MOVE ACUP-OLD-CUST-DOB-YYYY-MM-DD(7:2)
           TO DOBDAYO  OF CACTUPAO

           MOVE ACUP-OLD-CUST-FIRST-NAME    TO ACSFNAMO OF CACTUPAO
           MOVE ACUP-OLD-CUST-MIDDLE-NAME   TO ACSMNAMO OF CACTUPAO
           MOVE ACUP-OLD-CUST-LAST-NAME     TO ACSLNAMO OF CACTUPAO
           MOVE ACUP-OLD-CUST-ADDR-LINE-1   TO ACSADL1O OF CACTUPAO
           MOVE ACUP-OLD-CUST-ADDR-LINE-2   TO ACSADL2O OF CACTUPAO
           MOVE ACUP-OLD-CUST-ADDR-LINE-3   TO ACSCITYO OF CACTUPAO
           MOVE ACUP-OLD-CUST-ADDR-STATE-CD TO ACSSTTEO OF CACTUPAO
           MOVE ACUP-OLD-CUST-ADDR-ZIP      TO ACSZIPCO OF CACTUPAO
           MOVE ACUP-OLD-CUST-ADDR-COUNTRY-CD TO ACSCTRYO OF CACTUPAO

      *    Format phone numbers properly
           MOVE ACUP-OLD-CUST-PHONE-NUM-1(2:3) TO ACSPH1AO OF CACTUPAO
           MOVE ACUP-OLD-CUST-PHONE-NUM-1(6:3) TO ACSPH1BO OF CACTUPAO
           MOVE ACUP-OLD-CUST-PHONE-NUM-1(10:4) TO ACSPH1CO OF CACTUPAO
           MOVE ACUP-OLD-CUST-PHONE-NUM-2(2:3) TO ACSPH2AO OF CACTUPAO
           MOVE ACUP-OLD-CUST-PHONE-NUM-2(6:3) TO ACSPH2BO OF CACTUPAO
           MOVE ACUP-OLD-CUST-PHONE-NUM-2(10:4) TO ACSPH2CO OF CACTUPAO

           MOVE ACUP-OLD-CUST-GOVT-ISSUED-ID TO ACSGOVTO OF CACTUPAO
           MOVE ACUP-OLD-CUST-EFT-ACCOUNT-ID TO ACSEFTCO OF CACTUPAO
           MOVE ACUP-OLD-CUST-PRI-HOLDER-IND TO ACSPFLGO OF CACTUPAO
           .

       3202-SHOW-ORIGINAL-VALUES-EXIT.
           EXIT
           .

       3203-SHOW-UPDATED-VALUES.
           MOVE ACUP-NEW-ACTIVE-STATUS         TO ACSTTUSO OF CACTUPAO

      *    Format money fields using proper display formatting
           MOVE WS-INPUT-CREDIT-LIMIT       TO WS-CREDIT-LIMIT-DISP
           MOVE WS-CREDIT-LIMIT-DISP        TO ACRDLIMO OF CACTUPAO

           MOVE WS-INPUT-CASH-LIMIT         TO WS-CASH-LIMIT-DISP
           MOVE WS-CASH-LIMIT-DISP          TO ACSHLIMO OF CACTUPAO

           MOVE WS-INPUT-CURR-BAL           TO WS-CURR-BAL-DISP
           MOVE WS-CURR-BAL-DISP            TO ACURBALO OF CACTUPAO

           MOVE WS-INPUT-CYC-CREDIT         TO WS-CYC-CREDIT-DISP
           MOVE WS-CYC-CREDIT-DISP          TO ACRCYCRO OF CACTUPAO

           MOVE WS-INPUT-CYC-DEBIT          TO WS-CYC-DEBIT-DISP
           MOVE WS-CYC-DEBIT-DISP           TO ACRCYDBO OF CACTUPAO

      *    Date fields
           MOVE ACUP-NEW-OPEN-DATE(1:4)        TO OPNYEARO OF CACTUPAO
           MOVE ACUP-NEW-OPEN-DATE(5:2)        TO OPNMONO  OF CACTUPAO
           MOVE ACUP-NEW-OPEN-DATE(7:2)        TO OPNDAYO  OF CACTUPAO
           MOVE ACUP-NEW-EXPIRAION-DATE(1:4)   TO EXPYEARO OF CACTUPAO
           MOVE ACUP-NEW-EXPIRAION-DATE(5:2)   TO EXPMONO  OF CACTUPAO
           MOVE ACUP-NEW-EXPIRAION-DATE(7:2)   TO EXPDAYO  OF CACTUPAO
           MOVE ACUP-NEW-REISSUE-DATE(1:4)     TO RISYEARO OF CACTUPAO
           MOVE ACUP-NEW-REISSUE-DATE(5:2)     TO RISMONO  OF CACTUPAO
           MOVE ACUP-NEW-REISSUE-DATE(7:2)     TO RISDAYO  OF CACTUPAO
           MOVE ACUP-NEW-GROUP-ID              TO AADDGRPO OF CACTUPAO

      *    Customer fields
           MOVE ACUP-NEW-CUST-ID-X             TO ACSTNUMO OF CACTUPAO
           MOVE ACUP-NEW-CUST-SSN-1            TO ACTSSN1O OF CACTUPAO
           MOVE ACUP-NEW-CUST-SSN-2            TO ACTSSN2O OF CACTUPAO
           MOVE ACUP-NEW-CUST-SSN-3            TO ACTSSN3O OF CACTUPAO
           MOVE ACUP-NEW-CUST-FICO-SCORE-X     TO ACSTFCOO OF CACTUPAO
           MOVE ACUP-NEW-CUST-DOB-YYYY-MM-DD(1:4)
           TO DOBYEARO OF CACTUPAO
           MOVE ACUP-NEW-CUST-DOB-YYYY-MM-DD(5:2)
           TO DOBMONO  OF CACTUPAO
           MOVE ACUP-NEW-CUST-DOB-YYYY-MM-DD(7:2)
           TO DOBDAYO  OF CACTUPAO
           MOVE ACUP-NEW-CUST-FIRST-NAME       TO ACSFNAMO OF CACTUPAO
           MOVE ACUP-NEW-CUST-MIDDLE-NAME      TO ACSMNAMO OF CACTUPAO
           MOVE ACUP-NEW-CUST-LAST-NAME        TO ACSLNAMO OF CACTUPAO
           MOVE ACUP-NEW-CUST-ADDR-LINE-1      TO ACSADL1O OF CACTUPAO
           MOVE ACUP-NEW-CUST-ADDR-LINE-2      TO ACSADL2O OF CACTUPAO
           MOVE ACUP-NEW-CUST-ADDR-LINE-3      TO ACSCITYO OF CACTUPAO
           MOVE ACUP-NEW-CUST-ADDR-STATE-CD    TO ACSSTTEO OF CACTUPAO
           MOVE ACUP-NEW-CUST-ADDR-ZIP         TO ACSZIPCO OF CACTUPAO
           MOVE ACUP-NEW-CUST-ADDR-COUNTRY-CD  TO ACSCTRYO OF CACTUPAO
           MOVE ACUP-NEW-CUST-PHONE-NUM-1(2:3) TO ACSPH1AO OF CACTUPAO
           MOVE ACUP-NEW-CUST-PHONE-NUM-1(6:3) TO ACSPH1BO OF CACTUPAO
           MOVE ACUP-NEW-CUST-PHONE-NUM-1(10:4) TO ACSPH1CO OF CACTUPAO
           MOVE ACUP-NEW-CUST-PHONE-NUM-2(2:3) TO ACSPH2AO OF CACTUPAO
           MOVE ACUP-NEW-CUST-PHONE-NUM-2(6:3) TO ACSPH2BO OF CACTUPAO
           MOVE ACUP-NEW-CUST-PHONE-NUM-2(10:4) TO ACSPH2CO OF CACTUPAO
           MOVE ACUP-NEW-CUST-GOVT-ISSUED-ID   TO ACSGOVTO OF CACTUPAO
           MOVE ACUP-NEW-CUST-EFT-ACCOUNT-ID   TO ACSEFTCO OF CACTUPAO
           MOVE ACUP-NEW-CUST-PRI-HOLDER-IND   TO ACSPFLGO OF CACTUPAO
           .

       3203-SHOW-UPDATED-VALUES-EXIT.
           EXIT
           .

       3250-SETUP-INFOMSG.
      *    SETUP INFORMATION MESSAGE


           EVALUATE TRUE
               WHEN CDEMO-PGM-ENTER
                    SET  PROMPT-FOR-SEARCH-KEYS TO TRUE
               WHEN ACUP-DETAILS-NOT-FETCHED
                   SET PROMPT-FOR-SEARCH-KEYS      TO TRUE
               WHEN ACUP-SHOW-DETAILS
                    SET PROMPT-FOR-CHANGES         TO TRUE
               WHEN ACUP-CHANGES-NOT-OK
                    SET PROMPT-FOR-CHANGES         TO TRUE
               WHEN ACUP-CHANGES-OK-NOT-CONFIRMED
                    SET PROMPT-FOR-CONFIRMATION    TO TRUE
               WHEN ACUP-CHANGES-OKAYED-AND-DONE
                    SET CONFIRM-UPDATE-SUCCESS     TO TRUE
               WHEN ACUP-CHANGES-OKAYED-LOCK-ERROR
                    SET INFORM-FAILURE             TO TRUE
               WHEN ACUP-CHANGES-OKAYED-BUT-FAILED
                    SET INFORM-FAILURE             TO TRUE
               WHEN WS-NO-INFO-MESSAGE
                   SET PROMPT-FOR-SEARCH-KEYS      TO TRUE
           END-EVALUATE

           MOVE WS-INFO-MSG                    TO INFOMSGO OF CACTUPAO
           MOVE WS-RETURN-MSG                  TO ERRMSGO OF CACTUPAO

           .

       3250-SETUP-INFOMSG-EXIT.
           EXIT
           .

       3300-SETUP-SCREEN-ATTRS.

      *    PROTECT ALL FIELDS FIRST

           PERFORM 3310-PROTECT-ALL-ATTRS
              THRU 3310-PROTECT-ALL-ATTRS-EXIT

      *    RESET FIELD HIGHLIGHTING
           PERFORM 3305-RESET-FIELD-HIGHLIGHTING
              THRU 3305-RESET-FIELD-HL-EXIT

      *    UNPROTECT BASED ON CONTEXT
           EVALUATE TRUE
              WHEN ACUP-DETAILS-NOT-FETCHED
      *            Make Account Id editable
                   MOVE DFHBMFSE      TO ACCTSIDA OF CACTUPAI
              WHEN  ACUP-SHOW-DETAILS
              WHEN  ACUP-CHANGES-NOT-OK
                   PERFORM 3320-UNPROTECT-FEW-ATTRS
                      THRU 3320-UNPROTECT-FEW-ATTRS-EXIT
              WHEN ACUP-CHANGES-OK-NOT-CONFIRMED
              WHEN ACUP-CHANGES-OKAYED-AND-DONE
                   CONTINUE
              WHEN OTHER
                   MOVE DFHBMFSE      TO ACCTSIDA OF CACTUPAI
           END-EVALUATE

      *    POSITION CURSOR - ENHANCED LOGIC USING RPC ERROR FIELD
           EVALUATE TRUE
      *       Use error field returned from RPC call first
              WHEN LK-OUT-ERROR-FIELD = 'ACCT-ID'
                   MOVE -1 TO ACCTSIDL OF CACTUPAI

              WHEN LK-OUT-ERROR-FIELD = 'ACCT-STATUS'
                   MOVE -1 TO ACSTTUSL OF CACTUPAI

              WHEN LK-OUT-ERROR-FIELD = 'CREDIT-LIMIT'
                   MOVE -1 TO ACRDLIML OF CACTUPAI

              WHEN LK-OUT-ERROR-FIELD = 'CASH-LIMIT'
                   MOVE -1 TO ACSHLIML OF CACTUPAI

              WHEN LK-OUT-ERROR-FIELD = 'CURR-BAL'
                   MOVE -1 TO ACURBALL OF CACTUPAI

              WHEN LK-OUT-ERROR-FIELD = 'CYC-CREDIT'
                   MOVE -1 TO ACRCYCRL OF CACTUPAI

              WHEN LK-OUT-ERROR-FIELD = 'CYC-DEBIT'
                   MOVE -1 TO ACRCYDBL OF CACTUPAI

              WHEN LK-OUT-ERROR-FIELD = 'OPEN-YEAR'
                   MOVE -1 TO OPNYEARL OF CACTUPAI
              WHEN LK-OUT-ERROR-FIELD = 'OPEN-MONTH'
                   MOVE -1 TO OPNMONL OF CACTUPAI
              WHEN LK-OUT-ERROR-FIELD = 'OPEN-DAY'
                   MOVE -1 TO OPNDAYL OF CACTUPAI

              WHEN LK-OUT-ERROR-FIELD = 'EXP-YEAR'
                   MOVE -1 TO EXPYEARL OF CACTUPAI
              WHEN LK-OUT-ERROR-FIELD = 'EXP-MONTH'
                   MOVE -1 TO EXPMONL OF CACTUPAI
              WHEN LK-OUT-ERROR-FIELD = 'EXP-DAY'
                   MOVE -1 TO EXPDAYL OF CACTUPAI

              WHEN LK-OUT-ERROR-FIELD = 'REISSUE-YEAR'
                   MOVE -1 TO RISYEARL OF CACTUPAI
              WHEN LK-OUT-ERROR-FIELD = 'REISSUE-MONTH'
                   MOVE -1 TO RISMONL OF CACTUPAI
              WHEN LK-OUT-ERROR-FIELD = 'REISSUE-DAY'
                   MOVE -1 TO RISDAYL OF CACTUPAI

              WHEN LK-OUT-ERROR-FIELD = 'SSN-PART1'
                   MOVE -1 TO ACTSSN1L OF CACTUPAI
              WHEN LK-OUT-ERROR-FIELD = 'SSN-PART2'
                   MOVE -1 TO ACTSSN2L OF CACTUPAI
              WHEN LK-OUT-ERROR-FIELD = 'SSN-PART3'
                   MOVE -1 TO ACTSSN3L OF CACTUPAI

              WHEN LK-OUT-ERROR-FIELD = 'DOB-YEAR'
                   MOVE -1 TO DOBYEARL OF CACTUPAI
              WHEN LK-OUT-ERROR-FIELD = 'DOB-MONTH'
                   MOVE -1 TO DOBMONL OF CACTUPAI
              WHEN LK-OUT-ERROR-FIELD = 'DOB-DAY'
                   MOVE -1 TO DOBDAYL OF CACTUPAI

              WHEN LK-OUT-ERROR-FIELD = 'FICO-SCORE'
                   MOVE -1 TO ACSTFCOL OF CACTUPAI

              WHEN LK-OUT-ERROR-FIELD = 'FIRST-NAME'
                   MOVE -1 TO ACSFNAML OF CACTUPAI
              WHEN LK-OUT-ERROR-FIELD = 'MIDDLE-NAME'
                   MOVE -1 TO ACSMNAML OF CACTUPAI
              WHEN LK-OUT-ERROR-FIELD = 'LAST-NAME'
                   MOVE -1 TO ACSLNAML OF CACTUPAI

              WHEN LK-OUT-ERROR-FIELD = 'ADDR-LINE1'
                   MOVE -1 TO ACSADL1L OF CACTUPAI
              WHEN LK-OUT-ERROR-FIELD = 'STATE'
                   MOVE -1 TO ACSSTTEL OF CACTUPAI
              WHEN LK-OUT-ERROR-FIELD = 'ZIP-CODE'
                   MOVE -1 TO ACSZIPCL OF CACTUPAI
              WHEN LK-OUT-ERROR-FIELD = 'CITY'
                   MOVE -1 TO ACSCITYL OF CACTUPAI
              WHEN LK-OUT-ERROR-FIELD = 'COUNTRY'
                   MOVE -1 TO ACSCTRYL OF CACTUPAI

              WHEN LK-OUT-ERROR-FIELD = 'PHONE1-AREA'
                   MOVE -1 TO ACSPH1AL OF CACTUPAI
              WHEN LK-OUT-ERROR-FIELD = 'PHONE1-PREFIX'
                   MOVE -1 TO ACSPH1BL OF CACTUPAI
              WHEN LK-OUT-ERROR-FIELD = 'PHONE1-LINE'
                   MOVE -1 TO ACSPH1CL OF CACTUPAI

              WHEN LK-OUT-ERROR-FIELD = 'PHONE2-AREA'
                   MOVE -1 TO ACSPH2AL OF CACTUPAI
              WHEN LK-OUT-ERROR-FIELD = 'PHONE2-PREFIX'
                   MOVE -1 TO ACSPH2BL OF CACTUPAI
              WHEN LK-OUT-ERROR-FIELD = 'PHONE2-LINE'
                   MOVE -1 TO ACSPH2CL OF CACTUPAI

              WHEN LK-OUT-ERROR-FIELD = 'EFT-ACCOUNT'
                   MOVE -1 TO ACSEFTCL OF CACTUPAI
              WHEN LK-OUT-ERROR-FIELD = 'PRI-HOLDER'
                   MOVE -1 TO ACSPFLGL OF CACTUPAI

      *       Fall back to existing local logic if no RPC error field
              WHEN FOUND-ACCOUNT-DATA
              WHEN NO-CHANGES-DETECTED
                  MOVE -1 TO ACSTTUSL OF CACTUPAI
              WHEN FLG-ACCTFILTER-NOT-OK
              WHEN FLG-ACCTFILTER-BLANK
                   MOVE -1 TO ACCTSIDL OF CACTUPAI
              WHEN OTHER
                  MOVE -1 TO ACCTSIDL OF CACTUPAI
            END-EVALUATE

      *    SETUP COLOR FOR ACCOUNT ID
           IF FLG-ACCTFILTER-NOT-OK
              MOVE DFHRED              TO ACCTSIDC OF CACTUPAO
           END-IF

           IF  FLG-ACCTFILTER-BLANK
           AND CDEMO-PGM-REENTER
               MOVE '*'                TO ACCTSIDO OF CACTUPAO
               MOVE DFHRED             TO ACCTSIDC OF CACTUPAO
           END-IF
           .

       3300-SETUP-SCREEN-ATTRS-EXIT.
           EXIT
           .
       3305-RESET-FIELD-HIGHLIGHTING.
      *    Reset all field highlighting flags
           MOVE 'N' TO WS-ACCT-ID-ERROR
                       WS-ACCT-STATUS-ERROR
                       WS-CREDIT-LIMIT-ERROR
                       WS-CASH-LIMIT-ERROR
                       WS-CURR-BAL-ERROR
                       WS-OPEN-DATE-ERROR
                       WS-EXP-DATE-ERROR
                       WS-REISSUE-DATE-ERROR
                       WS-FIRST-NAME-ERROR
                       WS-LAST-NAME-ERROR
                       WS-SSN-ERROR
                       WS-DOB-ERROR
                       WS-STATE-ERROR
                       WS-ZIP-ERROR
                       WS-PHONE1-ERROR
                       WS-PHONE2-ERROR
                       WS-FICO-ERROR

      *    Reset all field colors to default (NOT GREEN!)
           MOVE DFHDFCOL TO ACCTSIDC OF CACTUPAO
                           ACSTTUSC OF CACTUPAO
                           ACRDLIMC OF CACTUPAO
                           ACSHLIMC OF CACTUPAO
                           ACURBALC OF CACTUPAO
                           OPNYEARC OF CACTUPAO
                           OPNMONC  OF CACTUPAO
                           OPNDAYC  OF CACTUPAO
                           EXPYEARC OF CACTUPAO
                           EXPMONC  OF CACTUPAO
                           EXPDAYC  OF CACTUPAO
                           RISYEARC OF CACTUPAO
                           RISMONC  OF CACTUPAO
                           RISDAYC  OF CACTUPAO
                           ACSFNAMC OF CACTUPAO
                           ACSLNAMC OF CACTUPAO
                           ACTSSN1C OF CACTUPAO
                           ACTSSN2C OF CACTUPAO
                           ACTSSN3C OF CACTUPAO
                           DOBYEARC OF CACTUPAO
                           DOBMONC  OF CACTUPAO
                           DOBDAYC  OF CACTUPAO
                           ACSSTTEC OF CACTUPAO
                           ACSZIPCC OF CACTUPAO
                           ACSPH1AC OF CACTUPAO
                           ACSPH1BC OF CACTUPAO
                           ACSPH1CC OF CACTUPAO
                           ACSPH2AC OF CACTUPAO
                           ACSPH2BC OF CACTUPAO
                           ACSPH2CC OF CACTUPAO
                           ACSTFCOC OF CACTUPAO
           .
       3305-RESET-FIELD-HL-EXIT.
           EXIT
           .

       3310-PROTECT-ALL-ATTRS.
           MOVE DFHBMPRF              TO ACCTSIDA OF CACTUPAI
                                         ACSTTUSA OF CACTUPAI
                                         ACRDLIMA OF CACTUPAI
                                         ACSHLIMA OF CACTUPAI
                                         ACURBALA OF CACTUPAI
                                         ACRCYCRA OF CACTUPAI
                                         ACRCYDBA OF CACTUPAI
                                         OPNYEARA OF CACTUPAI
                                         OPNMONA  OF CACTUPAI
                                         OPNDAYA  OF CACTUPAI
                                         EXPYEARA OF CACTUPAI
                                         EXPMONA  OF CACTUPAI
                                         EXPDAYA  OF CACTUPAI
                                         RISYEARA OF CACTUPAI
                                         RISMONA  OF CACTUPAI
                                         RISDAYA  OF CACTUPAI
                                         AADDGRPA OF CACTUPAI
                                         ACSTNUMA OF CACTUPAI
                                         ACTSSN1A OF CACTUPAI
                                         ACTSSN2A OF CACTUPAI
                                         ACTSSN3A OF CACTUPAI
                                         DOBYEARA OF CACTUPAI
                                         DOBMONA  OF CACTUPAI
                                         DOBDAYA  OF CACTUPAI
                                         ACSFNAMA OF CACTUPAI
                                         ACSMNAMA OF CACTUPAI
                                         ACSLNAMA OF CACTUPAI
                                         ACSADL1A OF CACTUPAI
                                         ACSADL2A OF CACTUPAI
                                         ACSCITYA OF CACTUPAI
                                         ACSSTTEA OF CACTUPAI
                                         ACSZIPCA OF CACTUPAI
                                         ACSCTRYA OF CACTUPAI
                                         ACSPH1AA OF CACTUPAI
                                         ACSPH1BA OF CACTUPAI
                                         ACSPH1CA OF CACTUPAI
                                         ACSPH2AA OF CACTUPAI
                                         ACSPH2BA OF CACTUPAI
                                         ACSPH2CA OF CACTUPAI
                                         ACSGOVTA OF CACTUPAI
                                         ACSEFTCA OF CACTUPAI
                                         ACSPFLGA OF CACTUPAI
                                         ACSTFCOA OF CACTUPAI
           .
       3310-PROTECT-ALL-ATTRS-EXIT.
           EXIT
           .

       3320-UNPROTECT-FEW-ATTRS.
           MOVE DFHBMFSE              TO ACSTTUSA OF CACTUPAI
                                         ACRDLIMA OF CACTUPAI
                                         ACSHLIMA OF CACTUPAI
                                         ACURBALA OF CACTUPAI
                                         ACRCYCRA OF CACTUPAI
                                         ACRCYDBA OF CACTUPAI
                                         OPNYEARA OF CACTUPAI
                                         OPNMONA  OF CACTUPAI
                                         OPNDAYA  OF CACTUPAI
                                         EXPYEARA OF CACTUPAI
                                         EXPMONA  OF CACTUPAI
                                         EXPDAYA  OF CACTUPAI
                                         RISYEARA OF CACTUPAI
                                         RISMONA  OF CACTUPAI
                                         RISDAYA  OF CACTUPAI
                                         AADDGRPA OF CACTUPAI
                                         ACSFNAMA OF CACTUPAI
                                         ACSMNAMA OF CACTUPAI
                                         ACSLNAMA OF CACTUPAI
                                         ACSADL1A OF CACTUPAI
                                         ACSADL2A OF CACTUPAI
                                         ACSCITYA OF CACTUPAI
                                         ACSSTTEA OF CACTUPAI
                                         ACSZIPCA OF CACTUPAI
                                         ACSCTRYA OF CACTUPAI
                                         ACSPH1AA OF CACTUPAI
                                         ACSPH1BA OF CACTUPAI
                                         ACSPH1CA OF CACTUPAI
                                         ACSPH2AA OF CACTUPAI
                                         ACSPH2BA OF CACTUPAI
                                         ACSPH2CA OF CACTUPAI
                                         ACSGOVTA OF CACTUPAI
                                         ACSEFTCA OF CACTUPAI
                                         ACSPFLGA OF CACTUPAI
                                         ACTSSN1A OF CACTUPAI
                                         ACTSSN2A OF CACTUPAI
                                         ACTSSN3A OF CACTUPAI
                                         DOBYEARA OF CACTUPAI
                                         DOBMONA  OF CACTUPAI
                                         DOBDAYA  OF CACTUPAI
                                         ACSTFCOA OF CACTUPAI
           .
       3320-UNPROTECT-FEW-ATTRS-EXIT.
           EXIT
           .


       3400-SEND-SCREEN.

           MOVE LIT-THISMAPSET         TO CCARD-NEXT-MAPSET
           MOVE LIT-THISMAP            TO CCARD-NEXT-MAP

           EXEC CICS SEND MAP(CCARD-NEXT-MAP)
                          MAPSET(CCARD-NEXT-MAPSET)
                          FROM(CACTUPAO)
                          CURSOR
                          ERASE
                          FREEKB
                          RESP(WS-RESP-CD)
           END-EXEC
           .
       3400-SEND-SCREEN-EXIT.
           EXIT
           .

      *----------------------------------------------------------------*
      *                      RPC CALL ROUTINES
      *----------------------------------------------------------------*
       9000-READ-ACCT-VIA-RPC.
           INITIALIZE ACUP-OLD-DETAILS
           INITIALIZE WS-RPC-COMMAREA
           SET  WS-NO-INFO-MESSAGE      TO TRUE

           MOVE SPACES TO LK-INPUT-PARMS LK-OUTPUT-STATUS LK-OUTPUT-DATA
           SET OP-READ TO TRUE
           MOVE CDEMO-ACCT-ID TO LK-IN-ACCT-ID

           PERFORM CALL-RPC-PROGRAM
           IF NOT ERR-FLG-ON
               IF RC-SUCCESS
                   PERFORM POPULATE-FROM-RPC
                      THRU POPULATE-FROM-RPC-EXIT
                   MOVE 'Account data loaded. F12=Update' TO WS-MESSAGE
               ELSE
                   MOVE LK-OUT-MESSAGE TO WS-RETURN-MSG
               END-IF
           ELSE
               MOVE 'Error calling account service' TO WS-RETURN-MSG
           END-IF
           .
       9000-READ-ACCT-VIA-RPC-EXIT.
           EXIT
           .

       9600-UPDATE-ACCT-VIA-RPC.
           INITIALIZE WS-RPC-COMMAREA
           MOVE SPACES TO LK-INPUT-PARMS LK-OUTPUT-STATUS
           MOVE SPACES TO LK-OUT-ERROR-FIELD

      *    Determine operation based on current state
           EVALUATE TRUE
               WHEN ACUP-SHOW-DETAILS OR ACUP-CHANGES-NOT-OK
      *            Validate only - don't update database yet
                   SET OP-VALIDATE TO TRUE
               WHEN ACUP-CHANGES-OK-NOT-CONFIRMED
      *            F5 pressed - actually update
                   SET OP-UPDATE TO TRUE
               WHEN OTHER
      *            Default to validate
                   SET OP-VALIDATE TO TRUE
           END-EVALUATE

           MOVE ACUP-NEW-ACCT-ID-X TO LK-IN-ACCT-ID

      * Collect data from screen into RPC structure
           PERFORM COLLECT-RPC-FROM-SCREEN
              THRU COLLECT-RPC-FROM-SCREEN-EXIT

           PERFORM CALL-RPC-PROGRAM

           IF NOT ERR-FLG-ON
      *        Use the message from RPC directly
               MOVE LK-OUT-MESSAGE TO WS-RETURN-MSG

      *        Handle field-specific error highlighting for input errors
               IF RC-INPUT-ERROR
                   PERFORM 9650-PROCESS-FIELD-ERRORS
                      THRU 9650-PROCESS-FIELD-ERRORS-EXIT
               END-IF
           ELSE
               MOVE 'Error calling account service' TO WS-RETURN-MSG
           END-IF
           .
       9600-UPDATE-ACCT-VIA-RPC-EXIT.
           EXIT
           .
       9650-PROCESS-FIELD-ERRORS.
      *    This paragraph processes field-specific errors returned by COACTUPL
      *    and sets highlighting flags for the appropriate fields

           MOVE SPACES TO WS-ERROR-FIELD

      *    Check if LK-OUT-MESSAGE contains field name information
      *    This is a simplified approach - in reality, COACTUPL would need
      *    to return structured error information
           EVALUATE TRUE
               WHEN LK-OUT-MESSAGE(1:12) = 'Account ID' OR
                    LK-OUT-MESSAGE(1:14) = 'Account Number'
                   SET ACCT-ID-HAS-ERROR TO TRUE
                   MOVE DFHRED TO ACCTSIDC OF CACTUPAO
                   MOVE -1 TO ACCTSIDL OF CACTUPAI

               WHEN LK-OUT-MESSAGE(1:14) = 'Account Status'
                   SET ACCT-STATUS-HAS-ERROR TO TRUE
                   MOVE DFHRED TO ACSTTUSC OF CACTUPAO
                   MOVE -1 TO ACSTTUSL OF CACTUPAI

               WHEN LK-OUT-MESSAGE(1:12) = 'Credit Limit'
                   SET CREDIT-LIMIT-HAS-ERROR TO TRUE
                   MOVE DFHRED TO ACRDLIMC OF CACTUPAO
                   MOVE -1 TO ACRDLIML OF CACTUPAI

               WHEN LK-OUT-MESSAGE(1:10) = 'Cash Limit' OR
                    LK-OUT-MESSAGE(1:18) = 'Cash Credit Limit'
                   SET CASH-LIMIT-HAS-ERROR TO TRUE
                   MOVE DFHRED TO ACSHLIMO OF CACTUPAO
                   MOVE -1 TO ACSHLIML    OF CACTUPAI

               WHEN LK-OUT-MESSAGE(1:15) = 'Current Balance'
                   SET CURR-BAL-HAS-ERROR TO TRUE
                   MOVE DFHRED TO ACURBALC OF CACTUPAO
                   MOVE -1 TO ACURBALL  OF CACTUPAI

               WHEN LK-OUT-MESSAGE(1:9) = 'Open Date'
                   SET OPEN-DATE-HAS-ERROR TO TRUE
                   MOVE DFHRED TO OPNYEARC OF CACTUPAO
                   MOVE -1 TO OPNYEARL  OF CACTUPAI

               WHEN LK-OUT-MESSAGE(1:11) = 'Expiry Date'
                   SET EXP-DATE-HAS-ERROR TO TRUE
                   MOVE DFHRED TO EXPYEARC OF CACTUPAO
                   MOVE -1 TO EXPYEARL  OF CACTUPAI

               WHEN LK-OUT-MESSAGE(1:12) = 'Reissue Date'
                   SET REISSUE-DATE-HAS-ERROR TO TRUE
                   MOVE DFHRED TO RISYEARC OF CACTUPAO
                   MOVE -1 TO RISYEARL  OF CACTUPAI

               WHEN LK-OUT-MESSAGE(1:10) = 'First Name'
                   SET FIRST-NAME-HAS-ERROR TO TRUE
                   MOVE DFHRED TO ACSFNAMC OF CACTUPAO
                   MOVE -1 TO ACSFNAML OF CACTUPAI

               WHEN LK-OUT-MESSAGE(1:9) = 'Last Name'
                   SET LAST-NAME-HAS-ERROR TO TRUE
                   MOVE DFHRED TO ACSLNAMC OF CACTUPAO
                   MOVE -1 TO ACSLNAML OF CACTUPAI

               WHEN LK-OUT-MESSAGE(1:3) = 'SSN'
                   SET SSN-HAS-ERROR TO TRUE
                   MOVE DFHRED TO ACTSSN1C OF CACTUPAO
                   MOVE -1 TO ACTSSN1L OF CACTUPAI

               WHEN LK-OUT-MESSAGE(1:13) = 'Date of Birth'
                   SET DOB-HAS-ERROR TO TRUE
                   MOVE DFHRED TO DOBYEARC OF CACTUPAO
                   MOVE -1 TO DOBYEARL OF CACTUPAI

               WHEN LK-OUT-MESSAGE(1:5) = 'State'
                   SET STATE-HAS-ERROR TO TRUE
                   MOVE DFHRED TO ACSSTTEC OF CACTUPAO
                   MOVE -1 TO ACSSTTEL OF CACTUPAI

               WHEN LK-OUT-MESSAGE(1:3) = 'Zip'
                   SET ZIP-HAS-ERROR TO TRUE
                   MOVE DFHRED TO ACSZIPCC OF CACTUPAO
                   MOVE -1 TO ACSZIPCI   OF CACTUPAI

               WHEN LK-OUT-MESSAGE(1:14) = 'Phone Number 1'
                   SET PHONE1-HAS-ERROR TO TRUE
                   MOVE DFHRED TO ACSPH1AC OF CACTUPAO
                   MOVE -1 TO ACSPH1AL  OF CACTUPAI

               WHEN LK-OUT-MESSAGE(1:14) = 'Phone Number 2'
                   SET PHONE2-HAS-ERROR TO TRUE
                   MOVE DFHRED TO ACSPH2AC OF CACTUPAO
                   MOVE -1 TO ACSPH2AL  OF CACTUPAI

               WHEN LK-OUT-MESSAGE(1:10) = 'FICO Score'
                   SET FICO-HAS-ERROR TO TRUE
                   MOVE DFHRED TO ACSTFCOC OF CACTUPAO
                   MOVE -1 TO ACSTFCOL OF CACTUPAI

               WHEN OTHER
      *            Generic error - highlight first editable field
                   MOVE DFHRED TO ACCTSIDC OF CACTUPAO
                   MOVE -1 TO ACCTSIDL OF CACTUPAI
           END-EVALUATE
           .
       9650-PROCESS-FIELD-ERRORS-EXIT.
           EXIT
           .

       COLLECT-RPC-FROM-SCREEN.
      * Account data - Use work fields to ensure proper format
           MOVE ACUP-NEW-ACTIVE-STATUS TO LK-IN-ACCT-ACTIVE-STATUS

           MOVE WS-INPUT-CREDIT-LIMIT  TO LK-IN-ACCT-CREDIT-LIMIT
           MOVE WS-INPUT-CASH-LIMIT    TO LK-IN-ACCT-CASH-LIMIT
           MOVE WS-INPUT-CURR-BAL      TO LK-IN-ACCT-CURR-BAL
           MOVE WS-INPUT-CYC-CREDIT    TO LK-IN-ACCT-CURR-CYC-CREDIT
           MOVE WS-INPUT-CYC-DEBIT     TO LK-IN-ACCT-CURR-CYC-DEBIT

      * Account dates
           MOVE ACUP-NEW-OPEN-DATE(1:4) TO LK-IN-ACCT-OPEN-YEAR
           MOVE ACUP-NEW-OPEN-DATE(5:2) TO LK-IN-ACCT-OPEN-MON
           MOVE ACUP-NEW-OPEN-DATE(7:2) TO LK-IN-ACCT-OPEN-DAY

           MOVE ACUP-NEW-EXPIRAION-DATE(1:4) TO LK-IN-ACCT-EXP-YEAR
           MOVE ACUP-NEW-EXPIRAION-DATE(5:2) TO LK-IN-ACCT-EXP-MON
           MOVE ACUP-NEW-EXPIRAION-DATE(7:2) TO LK-IN-ACCT-EXP-DAY

           MOVE ACUP-NEW-REISSUE-DATE(1:4) TO LK-IN-ACCT-REISSUE-YEAR
           MOVE ACUP-NEW-REISSUE-DATE(5:2) TO LK-IN-ACCT-REISSUE-MON
           MOVE ACUP-NEW-REISSUE-DATE(7:2) TO LK-IN-ACCT-REISSUE-DAY

           MOVE ACUP-NEW-GROUP-ID TO LK-IN-ACCT-GROUP-ID

      * Customer data
           MOVE ACUP-NEW-CUST-ID-X TO LK-IN-CUST-ID
           MOVE ACUP-NEW-CUST-FIRST-NAME TO LK-IN-CUST-FIRST-NAME
           MOVE ACUP-NEW-CUST-MIDDLE-NAME TO LK-IN-CUST-MIDDLE-NAME
           MOVE ACUP-NEW-CUST-LAST-NAME TO LK-IN-CUST-LAST-NAME

      * SSN
           MOVE ACUP-NEW-CUST-SSN-1 TO LK-IN-CUST-SSN-1
           MOVE ACUP-NEW-CUST-SSN-2 TO LK-IN-CUST-SSN-2
           MOVE ACUP-NEW-CUST-SSN-3 TO LK-IN-CUST-SSN-3

      * DOB
           MOVE ACUP-NEW-CUST-DOB-YYYY-MM-DD(1:4) TO LK-IN-CUST-DOB-YEAR
           MOVE ACUP-NEW-CUST-DOB-YYYY-MM-DD(5:2) TO LK-IN-CUST-DOB-MON
           MOVE ACUP-NEW-CUST-DOB-YYYY-MM-DD(7:2) TO LK-IN-CUST-DOB-DAY

      * Address
           MOVE ACUP-NEW-CUST-ADDR-LINE-1 TO LK-IN-CUST-ADDR-LINE-1
           MOVE ACUP-NEW-CUST-ADDR-LINE-2 TO LK-IN-CUST-ADDR-LINE-2
           MOVE ACUP-NEW-CUST-ADDR-LINE-3 TO LK-IN-CUST-ADDR-LINE-3
           MOVE ACUP-NEW-CUST-ADDR-STATE-CD TO LK-IN-CUST-ADDR-STATE-CD
           MOVE ACUP-NEW-CUST-ADDR-COUNTRY-CD
           TO LK-IN-CUST-ADDR-COUNTRY-CD
           MOVE ACUP-NEW-CUST-ADDR-ZIP TO LK-IN-CUST-ADDR-ZIP

      * Phone numbers
           MOVE ACUP-NEW-CUST-PHONE-NUM-1(2:3) TO LK-IN-CUST-PHONE-1A
           MOVE ACUP-NEW-CUST-PHONE-NUM-1(6:3) TO LK-IN-CUST-PHONE-1B
           MOVE ACUP-NEW-CUST-PHONE-NUM-1(10:4) TO LK-IN-CUST-PHONE-1C

           MOVE ACUP-NEW-CUST-PHONE-NUM-2(2:3) TO LK-IN-CUST-PHONE-2A
           MOVE ACUP-NEW-CUST-PHONE-NUM-2(6:3) TO LK-IN-CUST-PHONE-2B
           MOVE ACUP-NEW-CUST-PHONE-NUM-2(10:4) TO LK-IN-CUST-PHONE-2C

      * Other customer fields
           MOVE ACUP-NEW-CUST-GOVT-ISSUED-ID
            TO LK-IN-CUST-GOVT-ISSUED-ID
           MOVE ACUP-NEW-CUST-EFT-ACCOUNT-ID
            TO LK-IN-CUST-EFT-ACCOUNT-ID
           MOVE ACUP-NEW-CUST-PRI-HOLDER-IND
            TO LK-IN-CUST-PRI-HOLDER-IND
           MOVE ACUP-NEW-CUST-FICO-SCORE-X TO LK-IN-CUST-FICO-SCORE
           .
       COLLECT-RPC-FROM-SCREEN-EXIT.
           EXIT
           .

       CALL-RPC-PROGRAM.
           EXEC CICS LINK
                PROGRAM(LIT-RPC-PROGRAM)
                COMMAREA(WS-RPC-COMMAREA)
                LENGTH(LENGTH OF WS-RPC-COMMAREA)
                RESP(WS-RESP-CD)
           END-EXEC

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(PGMIDERR)
                   SET ERR-FLG-ON TO TRUE
                   MOVE 'COACTUPL program not found' TO WS-MESSAGE
               WHEN OTHER
                   SET ERR-FLG-ON TO TRUE
                   MOVE 'Error calling RPC program' TO WS-MESSAGE
           END-EVALUATE
           .
       CALL-RPC-PROGRAM-EXIT.
           EXIT
           .

       POPULATE-FROM-RPC.

      * Store Context in Commarea
           MOVE LK-OUT-ACCT-ID TO CDEMO-ACCT-ID
           MOVE LK-OUT-CUST-ID TO CDEMO-CUST-ID
           MOVE LK-OUT-CUST-FIRST-NAME TO CDEMO-CUST-FNAME
           MOVE LK-OUT-CUST-MIDDLE-NAME TO CDEMO-CUST-MNAME
           MOVE LK-OUT-CUST-LAST-NAME TO CDEMO-CUST-LNAME
           MOVE LK-OUT-ACCT-ACTIVE-STATUS TO CDEMO-ACCT-STATUS

      * Account data
           MOVE LK-OUT-ACCT-ID TO ACUP-OLD-ACCT-ID-X
           MOVE LK-OUT-ACCT-ACTIVE-STATUS TO ACUP-OLD-ACTIVE-STATUS

      * Format numeric fields for storage
           MOVE LK-OUT-ACCT-CREDIT-LIMIT TO ACUP-OLD-CREDIT-LIMIT-N
           MOVE LK-OUT-ACCT-CASH-LIMIT TO ACUP-OLD-CASH-CREDIT-LMT-N
           MOVE LK-OUT-ACCT-CURR-BAL TO ACUP-OLD-CURR-BAL-N
           MOVE LK-OUT-ACCT-CURR-CYC-CREDIT
           TO ACUP-OLD-CURR-CYC-CREDIT-N
           MOVE LK-OUT-ACCT-CURR-CYC-DEBIT TO ACUP-OLD-CURR-CYC-DEBIT-N

      * Account dates - parse from YYYY-MM-DD format
           IF LK-OUT-ACCT-OPEN-DATE NOT = SPACES
               MOVE LK-OUT-ACCT-OPEN-DATE(1:4)
               TO ACUP-OLD-OPEN-DATE(1:4)
               MOVE LK-OUT-ACCT-OPEN-DATE(6:2)
                TO ACUP-OLD-OPEN-DATE(5:2)
               MOVE LK-OUT-ACCT-OPEN-DATE(9:2)
                TO ACUP-OLD-OPEN-DATE(7:2)
           END-IF

           IF LK-OUT-ACCT-EXPIRATION-DATE NOT = SPACES
               MOVE LK-OUT-ACCT-EXPIRATION-DATE(1:4) TO
                    ACUP-OLD-EXPIRAION-DATE(1:4)
               MOVE LK-OUT-ACCT-EXPIRATION-DATE(6:2) TO
                    ACUP-OLD-EXPIRAION-DATE(5:2)
               MOVE LK-OUT-ACCT-EXPIRATION-DATE(9:2) TO
                    ACUP-OLD-EXPIRAION-DATE(7:2)
           END-IF

           IF LK-OUT-ACCT-REISSUE-DATE NOT = SPACES
               MOVE LK-OUT-ACCT-REISSUE-DATE(1:4) TO
                    ACUP-OLD-REISSUE-DATE(1:4)
               MOVE LK-OUT-ACCT-REISSUE-DATE(6:2) TO
                    ACUP-OLD-REISSUE-DATE(5:2)
               MOVE LK-OUT-ACCT-REISSUE-DATE(9:2) TO
                    ACUP-OLD-REISSUE-DATE(7:2)
           END-IF

           MOVE LK-OUT-ACCT-GROUP-ID TO ACUP-OLD-GROUP-ID

      * Customer data
           MOVE LK-OUT-CUST-ID TO ACUP-OLD-CUST-ID-X
           MOVE LK-OUT-CUST-FIRST-NAME TO ACUP-OLD-CUST-FIRST-NAME
           MOVE LK-OUT-CUST-MIDDLE-NAME TO ACUP-OLD-CUST-MIDDLE-NAME
           MOVE LK-OUT-CUST-LAST-NAME TO ACUP-OLD-CUST-LAST-NAME

      * SSN
           IF LK-OUT-CUST-SSN NOT = SPACES
               MOVE LK-OUT-CUST-SSN(1:3) TO ACUP-OLD-CUST-SSN-X(1:3)
               MOVE LK-OUT-CUST-SSN(4:2) TO ACUP-OLD-CUST-SSN-X(4:2)
               MOVE LK-OUT-CUST-SSN(6:4) TO ACUP-OLD-CUST-SSN-X(6:4)
           END-IF

      * DOB - parse from YYYY-MM-DD format
           IF LK-OUT-CUST-DOB NOT = SPACES
               MOVE LK-OUT-CUST-DOB(1:4)
               TO ACUP-OLD-CUST-DOB-YYYY-MM-DD(1:4)
               MOVE LK-OUT-CUST-DOB(6:2)
               TO ACUP-OLD-CUST-DOB-YYYY-MM-DD(5:2)
               MOVE LK-OUT-CUST-DOB(9:2)
               TO ACUP-OLD-CUST-DOB-YYYY-MM-DD(7:2)
           END-IF

      * Address
           MOVE LK-OUT-CUST-ADDR-LINE-1 TO ACUP-OLD-CUST-ADDR-LINE-1
           MOVE LK-OUT-CUST-ADDR-LINE-2 TO ACUP-OLD-CUST-ADDR-LINE-2
           MOVE LK-OUT-CUST-ADDR-LINE-3 TO ACUP-OLD-CUST-ADDR-LINE-3
           MOVE LK-OUT-CUST-ADDR-STATE-CD TO ACUP-OLD-CUST-ADDR-STATE-CD
           MOVE LK-OUT-CUST-ADDR-COUNTRY-CD
           TO ACUP-OLD-CUST-ADDR-COUNTRY-CD
           MOVE LK-OUT-CUST-ADDR-ZIP TO ACUP-OLD-CUST-ADDR-ZIP

      * Phone numbers - parse from formatted strings
           IF LK-OUT-CUST-PHONE-NUM-1 NOT = SPACES
               MOVE LK-OUT-CUST-PHONE-NUM-1(2:3)
               TO ACUP-OLD-CUST-PHONE-NUM-1(2:3)
               MOVE LK-OUT-CUST-PHONE-NUM-1(6:3)
               TO ACUP-OLD-CUST-PHONE-NUM-1(6:3)
               MOVE LK-OUT-CUST-PHONE-NUM-1(10:4)
               TO ACUP-OLD-CUST-PHONE-NUM-1(10:4)
           END-IF

           IF LK-OUT-CUST-PHONE-NUM-2 NOT = SPACES
               MOVE LK-OUT-CUST-PHONE-NUM-2(2:3)
                TO ACUP-OLD-CUST-PHONE-NUM-2(2:3)
               MOVE LK-OUT-CUST-PHONE-NUM-2(6:3)
               TO ACUP-OLD-CUST-PHONE-NUM-2(6:3)
               MOVE LK-OUT-CUST-PHONE-NUM-2(10:4)
               TO ACUP-OLD-CUST-PHONE-NUM-2(10:4)
           END-IF

      * Other customer fields
           MOVE LK-OUT-CUST-GOVT-ISSUED-ID
            TO ACUP-OLD-CUST-GOVT-ISSUED-ID
           MOVE LK-OUT-CUST-EFT-ACCOUNT-ID
           TO ACUP-OLD-CUST-EFT-ACCOUNT-ID
           MOVE LK-OUT-CUST-PRI-HOLDER-IND
           TO ACUP-OLD-CUST-PRI-HOLDER-IND
           MOVE LK-OUT-CUST-FICO-SCORE TO ACUP-OLD-CUST-FICO-SCORE
           .
       POPULATE-FROM-RPC-EXIT.
           EXIT
           .

      ******************************************************************
      *Common code to store PFKey
      ******************************************************************
       COPY 'CSSTRPFY'
           .

       ABEND-ROUTINE.
           IF ABEND-MSG EQUAL LOW-VALUES
              MOVE 'UNEXPECTED ABEND OCCURRED.' TO ABEND-MSG
           END-IF


           MOVE LIT-THISPGM       TO ABEND-CULPRIT

           EXEC CICS SEND
                            FROM (ABEND-DATA)
                            LENGTH(LENGTH OF ABEND-DATA)
                            NOHANDLE
                            ERASE
           END-EXEC

           EXEC CICS HANDLE ABEND
                CANCEL
           END-EXEC

           EXEC CICS ABEND
                ABCODE('9999')
           END-EXEC
           .
       ABEND-ROUTINE-EXIT.
           EXIT
           .

      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:12:32 CDT
      * ACCTSIDA OF CACTUPAI
