**************************************** *************************
      * Program:     COACTUPL                                         *
      * Layer:       Business logic                                   *
      * Function:    RPC Service for Account Management               *
      * Description: API for read/update account and customer data    *
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
           COACTUPL.
       DATE-WRITTEN.
           May 2025.
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
            07 WS-TRANID                           PIC X(4)
                                                   VALUE SPACES.
            07 WS-UCTRANS                          PIC X(4)
                                                   VALUE SPACES.
      ******************************************************************
      *      Input edits
      ******************************************************************
      *  Generic Input Edits
         05  WS-GENERIC-EDITS.
           10 WS-EDIT-VARIABLE-NAME                PIC X(25).


           10 WS-FLG-SIGNED-NUMBER-EDIT            PIC X(1).
              88  FLG-SIGNED-NUMBER-ISVALID        VALUE LOW-VALUES.
              88  FLG-SIGNED-NUMBER-NOT-OK         VALUE '0'.
              88  FLG-SIGNED-NUMBER-BLANK          VALUE 'B'.

           10 WS-EDIT-ALPHANUM-ONLY                PIC X(256).
           10 WS-EDIT-ALPHANUM-LENGTH              PIC S9(4) COMP-3.

           10 WS-EDIT-ALPHA-ONLY-FLAGS             PIC X(1).
              88  FLG-ALPHA-ISVALID                VALUE LOW-VALUES.
              88  FLG-ALPHA-NOT-OK                 VALUE '0'.
              88  FLG-ALPHA-BLANK                  VALUE 'B'.
           10 WS-EDIT-ALPHANUM-ONLY-FLAGS          PIC X(1).
              88  FLG-ALPHNANUM-ISVALID            VALUE LOW-VALUES.
              88  FLG-ALPHNANUM-NOT-OK             VALUE '0'.
              88  FLG-ALPHNANUM-BLANK              VALUE 'B'.
           10 WS-EDIT-MANDATORY-FLAGS              PIC X(1).
              88  FLG-MANDATORY-ISVALID            VALUE LOW-VALUES.
              88  FLG-MANDATORY-NOT-OK             VALUE '0'.
              88  FLG-MANDATORY-BLANK              VALUE 'B'.
           10 WS-EDIT-YES-NO                       PIC X(1)
                                                   VALUE 'N'.
              88 FLG-YES-NO-ISVALID                VALUES 'Y', 'N'.
              88 FLG-YES-NO-NOT-OK                 VALUE '0'.
              88 FLG-YES-NO-BLANK                  VALUE 'B'.

           10 WS-EDIT-US-PHONE-NUM                 PIC X(15).
           10 WS-EDIT-US-PHONE-NUM-X REDEFINES
              WS-EDIT-US-PHONE-NUM.
              20 FILLER                            PIC X(1).
      *                                            VALUE '('
              20 WS-EDIT-US-PHONE-NUMA             PIC X(3).
              20 WS-EDIT-US-PHONE-NUMA-N REDEFINES
                 WS-EDIT-US-PHONE-NUMA             PIC 9(3).
              20 FILLER                            PIC X(1).
      *                                            VALUE ')'
              20 WS-EDIT-US-PHONE-NUMB             PIC X(3).
              20 WS-EDIT-US-PHONE-NUMB-N REDEFINES
                 WS-EDIT-US-PHONE-NUMB             PIC 9(3).
              20 FILLER                            PIC X(1).
      *                                            VALUE '-'
              20 WS-EDIT-US-PHONE-NUMC             PIC X(4).
              20 WS-EDIT-US-PHONE-NUMC-N REDEFINES
                 WS-EDIT-US-PHONE-NUMC             PIC 9(4).
              20 FILLER                            PIC X(2).
           10 WS-EDIT-US-PHONE-NUM-FLGS.
               88 WS-EDIT-US-PHONE-IS-INVALID      VALUE '000'.
               88 WS-EDIT-US-PHONE-IS-VALID        VALUE LOW-VALUES.
               20 WS-EDIT-US-PHONEA-FLG            PIC X(01).
                  88 FLG-EDIT-US-PHONEA-ISVALID    VALUE LOW-VALUES.
                  88 FLG-EDIT-US-PHONEA-NOT-OK     VALUE '0'.
                  88 FLG-EDIT-US-PHONEA-BLANK      VALUE 'B'.
               20 WS-EDIT-EDIT-US-PHONEB           PIC X(01).
                  88 FLG-EDIT-US-PHONEB-ISVALID    VALUE LOW-VALUES.
                  88 FLG-EDIT-US-PHONEB-NOT-OK     VALUE '0'.
                  88 FLG-EDIT-US-PHONEB-BLANK      VALUE 'B'.
               20 WS-EDIT-EDIT-PHONEC              PIC X(01).
                  88 FLG-EDIT-US-PHONEC-ISVALID    VALUE LOW-VALUES.
                  88 FLG-EDIT-US-PHONEC-NOT-OK     VALUE '0'.
                  88 FLG-EDIT-US-PHONEC-BLANK      VALUE 'B'.

           10 WS-EDIT-US-SSN.
               20 WS-EDIT-US-SSN-PART1              PIC X(3).
               20 WS-EDIT-US-SSN-PART1-N REDEFINES
                  WS-EDIT-US-SSN-PART1              PIC 9(3).
                  88 INVALID-SSN-PART1  VALUES      0,
                                                    666,
                                                    900 THRU 999.
               20 WS-EDIT-US-SSN-PART2              PIC X(2).
               20 WS-EDIT-US-SSN-PART2-N REDEFINES
                  WS-EDIT-US-SSN-PART2              PIC 9(2).
               20 WS-EDIT-US-SSN-PART3              PIC X(4).
               20 WS-EDIT-US-SSN-PART3-N REDEFINES
                  WS-EDIT-US-SSN-PART3              PIC 9(4).
           10 WS-EDIT-US-SSN-N REDEFINES
              WS-EDIT-US-SSN                        PIC 9(09).
           10 WS-EDIT-US-SSN-FLGS.
               88 WS-EDIT-US-SSN-IS-INVALID         VALUE '000'.
               88 WS-EDIT-US-SSN-IS-VALID           VALUE LOW-VALUES.
               20 WS-EDIT-US-SSN-PART1-FLGS         PIC X(01).
                  88 FLG-EDIT-US-SSN-PART1-ISVALID  VALUE LOW-VALUES.
                  88 FLG-EDIT-US-SSN-PART1-NOT-OK   VALUE '0'.
                  88 FLG-EDIT-US-SSN-PART1-BLANK    VALUE 'B'.
               20 WS-EDIT-US-SSN-PART2-FLGS         PIC X(01).
                  88 FLG-EDIT-US-SSN-PART2-ISVALID  VALUE LOW-VALUES.
                  88 FLG-EDIT-US-SSN-PART2-NOT-OK   VALUE '0'.
                  88 FLG-EDIT-US-SSN-PART2-BLANK    VALUE 'B'.
               20 WS-EDIT-US-SSN-PART3-FLGS         PIC X(01).
                  88 FLG-EDIT-US-SSN-PART3-ISVALID  VALUE LOW-VALUES.
                  88 FLG-EDIT-US-SSN-PART3-NOT-OK   VALUE '0'.
                  88 FLG-EDIT-US-SSN-PART3-BLANK    VALUE 'B'.

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



      ******************************************************************
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

      *  Program specific edits
         05  WS-EDIT-ACCT-FLAG                     PIC X(1).
           88  FLG-ACCTFILTER-ISVALID              VALUE '1'.
           88  FLG-ACCTFILTER-NOT-OK               VALUE '0'.
           88  FLG-ACCTFILTER-BLANK                VALUE ' '.
         05  WS-EDIT-CUST-FLAG                     PIC X(1).
           88  FLG-CUSTFILTER-ISVALID              VALUE '1'.
           88  FLG-CUSTFILTER-NOT-OK               VALUE '0'.
           88  FLG-CUSTFILTER-BLANK                VALUE ' '.
         05 WS-NON-KEY-FLAGS.
           10  WS-EDIT-ACCT-STATUS                 PIC  X(1).
               88  FLG-ACCT-STATUS-ISVALID         VALUES 'Y', 'N'.
               88  FLG-ACCT-STATUS-NOT-OK          VALUE '0'.
               88  FLG-ACCT-STATUS-BLANK           VALUE 'B'.
           10  WS-EDIT-CREDIT-LIMIT                PIC  X(1).
               88  FLG-CRED-LIMIT-ISVALID          VALUE LOW-VALUES.
               88  FLG-CRED-LIMIT-NOT-OK           VALUE '0'.
               88  FLG-CRED-LIMIT-BLANK            VALUE 'B'.
           10  WS-EDIT-CASH-CREDIT-LIMIT           PIC  X(1).
               88  FLG-CASH-CREDIT-LIMIT-ISVALID   VALUE LOW-VALUES.
               88  FLG-CASH-CREDIT-LIMIT-NOT-OK    VALUE '0'.
               88  FLG-CASH-CREDIT-LIMIT-BLANK     VALUE 'B'.
           10  WS-EDIT-CURR-BAL                    PIC  X(1).
               88  FLG-CURR-BAL-ISVALID            VALUE LOW-VALUES.
               88  FLG-CURR-BAL-NOT-OK             VALUE '0'.
               88  FLG-CURR-BAL-BLANK              VALUE 'B'.
           10  WS-EDIT-CURR-CYC-CREDIT             PIC  X(1).
               88  FLG-CURR-CYC-CREDIT-ISVALID     VALUE LOW-VALUES.
               88  FLG-CURR-CYC-CREDIT-NOT-OK      VALUE '0'.
               88  FLG-CURR-CYC-CREDIT-BLANK       VALUE 'B'.
           10  WS-EDIT-CURR-CYC-DEBIT              PIC  X(1).
               88  FLG-CURR-CYC-DEBIT-ISVALID      VALUE LOW-VALUES.
               88  FLG-CURR-CYC-DEBIT-NOT-OK       VALUE '0'.
               88  FLG-CURR-CYC-DEBIT-BLANK        VALUE 'B'.
           10 WS-EDIT-DT-OF-BIRTH-FLGS.
               88 WS-EDIT-DT-OF-BIRTH-INVALID      VALUE '000'.
               88 WS-EDIT-DT-OF-BIRTH-ISVALID      VALUE LOW-VALUES.
               20 WS-EDIT-DT-OF-BIRTH-YEAR-FLG     PIC X(01).
                  88 FLG-DT-OF-BIRTH-YEAR-ISVALID  VALUE LOW-VALUES.
                  88 FLG-DT-OF-BIRTH-YEAR-NOT-OK   VALUE '0'.
                  88 FLG-DT-OF-BIRTH-YEAR-BLANK    VALUE 'B'.
               20 WS-EDIT-DT-OF-BIRTH-MONTH        PIC X(01).
                  88 FLG-DT-OF-BIRTH-MONTH-ISVALID VALUE LOW-VALUES.
                  88 FLG-DT-OF-BIRTH-MONTH-NOT-OK  VALUE '0'.
                  88 FLG-DT-OF-BIRTH-MONTH-BLANK   VALUE 'B'.
               20 WS-EDIT-DT-OF-BIRTH-DAY          PIC X(01).
                  88 FLG-DT-OF-BIRTH-DAY-ISVALID   VALUE LOW-VALUES.
                  88 FLG-DT-OF-BIRTH-DAY-NOT-OK    VALUE '0'.
                  88 FLG-DT-OF-BIRTH-DAY-BLANK     VALUE 'B'.
           10  WS-EDIT-FICO-SCORE-FLGS             PIC  X(1).
               88  FLG-FICO-SCORE-ISVALID          VALUE LOW-VALUES.
               88  FLG-FICO-SCORE-NOT-OK           VALUE '0'.
               88  FLG-FICO-SCORE-BLANK            VALUE 'B'.
           10 WS-EDIT-OPEN-DATE-FLGS.
               88 WS-EDIT-OPEN-DATE-IS-INVALID     VALUE '000'.
               20 WS-EDIT-OPEN-YEAR-FLG            PIC X(01).
                  88 FLG-OPEN-YEAR-ISVALID         VALUE LOW-VALUES.
                  88 FLG-OPEN-YEAR-NOT-OK          VALUE '0'.
                  88 FLG-OPEN-YEAR-BLANK           VALUE 'B'.
               20 WS-EDIT-OPEN-MONTH               PIC X(01).
                  88 FLG-OPEN-MONTH-ISVALID        VALUE LOW-VALUES.
                  88 FLG-OPEN-MONTH-NOT-OK         VALUE '0'.
                  88 FLG-OPEN-MONTH-BLANK          VALUE 'B'.
               20 WS-EDIT-OPEN-DAY                 PIC X(01).
                  88 FLG-OPEN-DAY-ISVALID          VALUE LOW-VALUES.
                  88 FLG-OPEN-DAY-NOT-OK           VALUE '0'.
                  88 FLG-OPEN-DAY-BLANK            VALUE 'B'.
           10 WS-EXPIRY-DATE-FLGS.
               88 WS-EDIT-EXPIRY-IS-INVALID        VALUE '000'.
               20 WS-EDIT-EXPIRY-YEAR-FLG          PIC X(01).
                  88 FLG-EXPIRY-YEAR-ISVALID       VALUE LOW-VALUES.
                  88 FLG-EXPIRY-YEAR-NOT-OK        VALUE '0'.
                  88 FLG-EXPIRY-YEAR-BLANK         VALUE 'B'.
               20 WS-EDIT-EXPIRY-MONTH             PIC X(01).
                  88 FLG-EXPIRY-MONTH-ISVALID      VALUE LOW-VALUES.
                  88 FLG-EXPIRY-MONTH-NOT-OK       VALUE '0'.
                  88 FLG-EXPIRY-MONTH-BLANK        VALUE 'B'.
               20 WS-EDIT-EXPIRY-DAY               PIC X(01).
                  88 FLG-EXPIRY-DAY-ISVALID        VALUE LOW-VALUES.
                  88 FLG-EXPIRY-DAY-NOT-OK         VALUE '0'.
                  88 FLG-EXPIRY-DAY-BLANK          VALUE 'B'.
           10 WS-EDIT-REISSUE-DATE-FLGS.
               88 WS-EDIT-REISSUE-DATE-INVALID     VALUE '000'.
               20 WS-EDIT-REISSUE-YEAR-FLG         PIC X(01).
                  88 FLG-REISSUE-YEAR-ISVALID      VALUE LOW-VALUES.
                  88 FLG-REISSUE-YEAR-NOT-OK       VALUE '0'.
                  88 FLG-REISSUE-YEAR-BLANK        VALUE 'B'.
               20 WS-EDIT-REISSUE-MONTH            PIC X(01).
                  88 FLG-REISSUE-MONTH-ISVALID     VALUE LOW-VALUES.
                  88 FLG-REISSUE-MONTH-NOT-OK      VALUE '0'.
                  88 FLG-REISSUE-MONTH-BLANK       VALUE 'B'.
               20 WS-EDIT-REISSUE-DAY              PIC X(01).
                  88 FLG-REISSUE-DAY-ISVALID       VALUE LOW-VALUES.
                  88 FLG-REISSUE-DAY-NOT-OK        VALUE '0'.
                  88 FLG-REISSUE-DAY-BLANK         VALUE 'B'.
           10 WS-EDIT-NAME-FLAGS.
               20 WS-EDIT-FIRST-NAME-FLGS          PIC X(01).
                  88 FLG-FIRST-NAME-ISVALID        VALUE LOW-VALUES.
                  88 FLG-FIRST-NAME-NOT-OK         VALUE '0'.
                  88 FLG-FIRST-NAME-BLANK          VALUE 'B'.
               20 WS-EDIT-MIDDLE-NAME-FLGS         PIC X(01).
                  88 FLG-MIDDLE-NAME-ISVALID       VALUE LOW-VALUES.
                  88 FLG-MIDDLE-NAME-NOT-OK        VALUE '0'.
                  88 FLG-MIDDLE-NAME-BLANK         VALUE 'B'.
               20 WS-EDIT-LAST-NAME-FLGS           PIC X(01).
                  88 FLG-LAST-NAME-ISVALID         VALUE LOW-VALUES.
                  88 FLG-LAST-NAME-NOT-OK          VALUE '0'.
                  88 FLG-LAST-NAME-BLANK           VALUE 'B'.
           10 WS-EDIT-ADDRESS-FLAGS.
               20 WS-EDIT-ADDRESS-LINE-1-FLGS      PIC X(01).
                  88 FLG-ADDRESS-LINE-1-ISVALID    VALUE LOW-VALUES.
                  88 FLG-ADDRESS-LINE-1-NOT-OK     VALUE '0'.
                  88 FLG-ADDRESS-LINE-1-BLANK      VALUE 'B'.
               20 WS-EDIT-ADDRESS-LINE-2-FLGS      PIC X(01).
                  88 FLG-ADDRESS-LINE-2-ISVALID    VALUE LOW-VALUES.
                  88 FLG-ADDRESS-LINE-2-NOT-OK     VALUE '0'.
                  88 FLG-ADDRESS-LINE-2-BLANK      VALUE 'B'.
               20 WS-EDIT-CITY-FLGS                PIC X(01).
                  88 FLG-CITY-ISVALID              VALUE LOW-VALUES.
                  88 FLG-CITY-NOT-OK               VALUE '0'.
                  88 FLG-CITY-BLANK                VALUE 'B'.
               20 WS-EDIT-STATE-FLGS               PIC X(01).
                  88 FLG-STATE-ISVALID             VALUE LOW-VALUES.
                  88 FLG-STATE-NOT-OK              VALUE '0'.
                  88 FLG-STATE-BLANK               VALUE 'B'.
               20 WS-EDIT-ZIPCODE-FLGS             PIC X(01).
                  88 FLG-ZIPCODE-ISVALID           VALUE LOW-VALUES.
                  88 FLG-ZIPCODE-NOT-OK            VALUE '0'.
                  88 FLG-ZIPCODE-BLANK             VALUE 'B'.
               20 WS-EDIT-COUNTRY-FLGS             PIC X(01).
                  88 FLG-COUNTRY-ISVALID           VALUE LOW-VALUES.
                  88 FLG-COUNTRY-NOT-OK            VALUE '0'.
                  88 FLG-COUNTRY-BLANK             VALUE 'B'.
               20 WS-EDIT-PHONE-NUM-1-FLGS.
                  88 WS-EDIT-PHONE-NUM-1-IS-INVALID
                                                   VALUE '000'.
                  30 WS-EDIT-PHONE-NUM-1A-FLG      PIC X(01).
                     88 FLG-PHONE-NUM-1A-ISVALID   VALUE LOW-VALUES.
                     88 FLG-PHONE-NUM-1A-NOT-OK    VALUE '0'.
                     88 FLG-PHONE-NUM-1A-BLANK     VALUE 'B'.
                  30 WS-EDIT-PHONE-NUM-1B          PIC X(01).
                     88 FLG-PHONE-NUM-1B-ISVALID   VALUE LOW-VALUES.
                     88 FLG-PHONE-NUM-1B-NOT-OK    VALUE '0'.
                     88 FLG-PHONE-NUM-1B-BLANK     VALUE 'B'.
                  30 WS-EDIT-PHONE-NUM-1C          PIC X(01).
                     88 FLG-PHONE-NUM-1C-ISVALID   VALUE LOW-VALUES.
                     88 FLG-PHONE-NUM-1C-NOT-OK    VALUE '0'.
                     88 FLG-PHONE-NUM-1C-BLANK     VALUE 'B'.
               20 WS-EDIT-PHONE-NUM-2-FLGS.
                  88 WS-EDIT-PHONE-NUM-2-IS-INVALID
                                                   VALUE '000'.
                  30 WS-EDIT-PHONE-NUM-2A-FLG      PIC X(01).
                     88 FLG-PHONE-NUM-2A-ISVALID   VALUE LOW-VALUES.
                     88 FLG-PHONE-NUM-2A-NOT-OK    VALUE '0'.
                     88 FLG-PHONE-NUM-2A-BLANK     VALUE 'B'.
                  30 WS-EDIT-PHONE-NUM-2B          PIC X(01).
                     88 FLG-PHONE-NUM-2B-ISVALID   VALUE LOW-VALUES.
                     88 FLG-PHONE-NUM-2B-NOT-OK    VALUE '0'.
                     88 FLG-PHONE-NUM-2B-BLANK     VALUE 'B'.
                  30 WS-EDIT-PHONE-NUM-2C          PIC X(01).
                     88 FLG-PHONE-NUM-2C-ISVALID   VALUE LOW-VALUES.
                     88 FLG-PHONE-NUM-2C-NOT-OK    VALUE '0'.
                     88 FLG-PHONE-NUM-2C-BLANK     VALUE 'B'.
           10  WS-EFT-ACCOUNT-ID-FLGS              PIC X(01).
               88 FLG-EFT-ACCOUNT-ID-ISVALID       VALUE LOW-VALUES.
               88 FLG-EFT-ACCOUNT-ID-NOT-OK        VALUE '0'.
               88 FLG-EFT-ACCOUNT-ID-BLANK         VALUE 'B'.
           10  WS-EDIT-PRI-CARDHOLDER              PIC  X(1).
               88  FLG-PRI-CARDHOLDER-ISVALID      VALUES 'Y', 'N'.
               88  FLG-PRI-CARDHOLDER-NOT-OK       VALUE '0'.
               88  FLG-PRI-CARDHOLDER-BLANK        VALUE 'B'.

      ******************************************************************
      *      File and data Handling
      ******************************************************************
         05 WS-XREF-RID.
           10  WS-CARD-RID-CARDNUM                 PIC X(16).
           10  WS-CARD-RID-CUST-ID                 PIC 9(09).
           10  WS-CARD-RID-CUST-ID-X REDEFINES
                  WS-CARD-RID-CUST-ID              PIC X(09).
           10  WS-CARD-RID-ACCT-ID                 PIC 9(11).
           10  WS-CARD-RID-ACCT-ID-X REDEFINES
                  WS-CARD-RID-ACCT-ID              PIC X(11).
         05  WS-FILE-READ-FLAGS.
           10 WS-ACCOUNT-MASTER-READ-FLAG          PIC X(1).
              88 FOUND-ACCT-IN-MASTER              VALUE '1'.
           10 WS-CUST-MASTER-READ-FLAG             PIC X(1).
              88 FOUND-CUST-IN-MASTER              VALUE '1'.

         05  WS-FILE-ERROR-MESSAGE.
           10  FILLER                         PIC X(12)
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
           88  WS-PROMPT-FOR-LASTNAME              VALUE
               'Last name not provided'.
           88  WS-NAME-MUST-BE-ALPHA               VALUE
               'Name can only contain alphabets and spaces'.
           88  NO-SEARCH-CRITERIA-RECEIVED         VALUE
               'No input received'.
           88  NO-CHANGES-DETECTED                 VALUE
               'No change detected with respect to values fetched.'.
           88  SEARCHED-ACCT-ZEROES                VALUE
               'Account number must be a non zero 11 digit number'.
           88  SEARCHED-ACCT-NOT-NUMERIC           VALUE
               'Account number must be a non zero 11 digit number'.
           88  DID-NOT-FIND-ACCT-IN-CARDXREF       VALUE
               'Did not find this account in account card xref file'.
           88  DID-NOT-FIND-ACCT-IN-ACCTDAT        VALUE
               'Did not find this account in account master file'.
           88  DID-NOT-FIND-CUST-IN-CUSTDAT        VALUE
               'Did not find associated customer in master file'.
           88  ACCT-STATUS-MUST-BE-YES-NO          VALUE
               'Account Active Status must be Y or N'.
           88  CRED-LIMIT-IS-BLANK                 VALUE
               'Credit Limit must be supplied'.
           88  CRED-LIMIT-IS-NOT-VALID             VALUE
               'Credit Limit is not valid'.
           88  THIS-MONTH-NOT-VALID                VALUE
               'Card expiry month must be between 1 and 12'.
           88  THIS-YEAR-NOT-VALID                 VALUE
               'Invalid card expiry year'.
           88  DID-NOT-FIND-ACCTCARD-COMBO         VALUE
               'Did not find cards for this search condition'.
           88  COULD-NOT-LOCK-ACCT-FOR-UPDATE      VALUE
               'Could not lock account record for update'.
           88  COULD-NOT-LOCK-CUST-FOR-UPDATE      VALUE
               'Could not lock customer record for update'.
           88  DATA-WAS-CHANGED-BEFORE-UPDATE      VALUE
               'Record changed by some one else. Please review'.
           88  LOCKED-BUT-UPDATE-FAILED            VALUE
               'Update of record failed'.
           88  XREF-READ-ERROR                     VALUE
               'Error reading Card Data File'.
           88  CODING-TO-BE-DONE                   VALUE
               'Looks Good.... so far'.

      ******************************************************************
      *      Literals and Constants
      ******************************************************************
       01 WS-LITERALS.
          05 LIT-THISPGM                           PIC X(8)
                                                   VALUE 'COACTUPL'.
          05 LIT-ACCTFILENAME                      PIC X(8)
                                                   VALUE 'ACCTDAT '.
          05 LIT-CUSTFILENAME                      PIC X(8)
                                                   VALUE 'CUSTDAT '.
          05 LIT-CARDFILENAME                      PIC X(8)
                                                   VALUE 'CARDDAT '.
          05 LIT-CARDFILENAME-ACCT-PATH            PIC X(8)
                                                   VALUE 'CARDAIX '.
          05 LIT-CARDXREFNAME-ACCT-PATH            PIC X(8)
                                                   VALUE 'CXACAIX '.
      ******************************************************************
      * Literals for use in INSPECT statements
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
      * Variables for use in INSPECT statements
      ******************************************************************
       01  LIT-ALL-ALPHA-FROM     PIC X(52) VALUE SPACES.
       01  LIT-ALL-ALPHANUM-FROM  PIC X(62) VALUE SPACES.
       01  LIT-ALL-NUM-FROM       PIC X(10) VALUE SPACES.
       77  LIT-ALPHA-SPACES-TO    PIC X(52) VALUE SPACES.
       77  LIT-ALPHANUM-SPACES-TO PIC X(62) VALUE SPACES.
       77  LIT-NUM-SPACES-TO      PIC X(10) VALUE SPACES.

      *COMMON COPYBOOKS
      *Lookups
      *North America Phone Area codes
       COPY CSLKPCDY.

      *Dataset layouts
      *ACCT RECORD LAYOUT
       COPY CVACT01Y.

      *CARD XREF LAYOUT
       COPY CVACT03Y.

      *CUSTOMER LAYOUT
       COPY CVCUS01Y.


      * Account Update Records
       01 ACCT-UPDATE-RECORD.
      *****************************************************************
      *    Data-structure for  account entity (RECLN 300)
      *****************************************************************
               15  ACCT-UPDATE-ID                      PIC 9(11).
               15  ACCT-UPDATE-ACTIVE-STATUS           PIC X(01).
               15  ACCT-UPDATE-CURR-BAL                PIC S9(10)V99.
               15  ACCT-UPDATE-CREDIT-LIMIT            PIC S9(10)V99.
               15  ACCT-UPDATE-CASH-CREDIT-LIMIT       PIC S9(10)V99.
               15  ACCT-UPDATE-OPEN-DATE               PIC X(10).
               15  ACCT-UPDATE-EXPIRAION-DATE          PIC X(10).
               15  ACCT-UPDATE-REISSUE-DATE            PIC X(10).
               15  ACCT-UPDATE-CURR-CYC-CREDIT         PIC S9(10)V99.
               15  ACCT-UPDATE-CURR-CYC-DEBIT          PIC S9(10)V99.
               15  ACCT-UPDATE-GROUP-ID                PIC X(10).
               15  FILLER                              PIC X(188).

       01 CUST-UPDATE-RECORD.
      *****************************************************************
      *    Data-structure for  CUSTOMER entity (RECLN 300)
      *****************************************************************
               15  CUST-UPDATE-ID                      PIC 9(09).
               15  CUST-UPDATE-FIRST-NAME              PIC X(25).
               15  CUST-UPDATE-MIDDLE-NAME             PIC X(25).
               15  CUST-UPDATE-LAST-NAME               PIC X(25).
               15  CUST-UPDATE-ADDR-LINE-1             PIC X(50).
               15  CUST-UPDATE-ADDR-LINE-2             PIC X(50).
               15  CUST-UPDATE-ADDR-LINE-3             PIC X(50).
               15  CUST-UPDATE-ADDR-STATE-CD           PIC X(02).
               15  CUST-UPDATE-ADDR-COUNTRY-CD         PIC X(03).
               15  CUST-UPDATE-ADDR-ZIP                PIC X(10).
               15  CUST-UPDATE-PHONE-NUM-1             PIC X(15).
               15  CUST-UPDATE-PHONE-NUM-2             PIC X(15).
               15  CUST-UPDATE-SSN                     PIC 9(09).
               15  CUST-UPDATE-GOVT-ISSUED-ID          PIC X(20).
               15  CUST-UPDATE-DOB-YYYY-MM-DD          PIC X(10).
               15  CUST-UPDATE-EFT-ACCOUNT-ID          PIC X(10).
               15  CUST-UPDATE-PRI-CARD-IND            PIC X(01).
               15  CUST-UPDATE-FICO-CREDIT-SCORE       PIC 9(03).
               15  FILLER                              PIC X(168).

      * Work areas for change comparison
       01 WS-OLD-DETAILS.
          05 WS-OLD-ACCT-DATA.
             15  WS-OLD-ACCT-ID-X                    PIC X(11).
             15  WS-OLD-ACCT-ID                      REDEFINES
                 WS-OLD-ACCT-ID-X                    PIC 9(11).
             15  WS-OLD-ACTIVE-STATUS                PIC X(01).
             15  WS-OLD-CURR-BAL                     PIC X(12).
             15  WS-OLD-CURR-BAL-N REDEFINES
                 WS-OLD-CURR-BAL                     PIC S9(10)V99.
             15  WS-OLD-CREDIT-LIMIT                 PIC X(12).
             15  WS-OLD-CREDIT-LIMIT-N               REDEFINES
                 WS-OLD-CREDIT-LIMIT                 PIC S9(10)V99.
             15  WS-OLD-CASH-CREDIT-LIMIT            PIC X(12).
             15  WS-OLD-CASH-CREDIT-LIMIT-N          REDEFINES
                 WS-OLD-CASH-CREDIT-LIMIT            PIC S9(10)V99.
             15  WS-OLD-OPEN-DATE                    PIC X(08).
             15  WS-OLD-OPEN-DATE-PARTS              REDEFINES
                 WS-OLD-OPEN-DATE.
                 20 WS-OLD-OPEN-YEAR                 PIC X(4).
                 20 WS-OLD-OPEN-MON                  PIC X(2).
                 20 WS-OLD-OPEN-DAY                  PIC X(2).
             15  WS-OLD-EXPIRAION-DATE               PIC X(08).
             15  WS-OLD-EXPIRAION-DATE-PARTS         REDEFINES
                 WS-OLD-EXPIRAION-DATE.
                 20 WS-OLD-EXP-YEAR                  PIC X(4).
                 20 WS-OLD-EXP-MON                   PIC X(2).
                 20 WS-OLD-EXP-DAY                   PIC X(2).
             15  WS-OLD-REISSUE-DATE                 PIC X(08).
             15  WS-OLD-REISSUE-DATE-PARTS           REDEFINES
                 WS-OLD-REISSUE-DATE.
                 20 WS-OLD-REISSUE-YEAR              PIC X(4).
                 20 WS-OLD-REISSUE-MON               PIC X(2).
                 20 WS-OLD-REISSUE-DAY               PIC X(2).
             15  WS-OLD-CURR-CYC-CREDIT              PIC X(12).
             15  WS-OLD-CURR-CYC-CREDIT-N            REDEFINES
                 WS-OLD-CURR-CYC-CREDIT              PIC S9(10)V99.
             15  WS-OLD-CURR-CYC-DEBIT               PIC X(12).
             15  WS-OLD-CURR-CYC-DEBIT-N             REDEFINES
                 WS-OLD-CURR-CYC-DEBIT               PIC S9(10)V99.
             15  WS-OLD-GROUP-ID                     PIC X(10).
          05 WS-OLD-CUST-DATA.
             15  WS-OLD-CUST-ID-X                    PIC X(09).
             15  WS-OLD-CUST-ID                      REDEFINES
                 WS-OLD-CUST-ID-X                    PIC 9(09).
             15  WS-OLD-CUST-FIRST-NAME              PIC X(25).
             15  WS-OLD-CUST-MIDDLE-NAME             PIC X(25).
             15  WS-OLD-CUST-LAST-NAME               PIC X(25).
             15  WS-OLD-CUST-ADDR-LINE-1             PIC X(50).
             15  WS-OLD-CUST-ADDR-LINE-2             PIC X(50).
             15  WS-OLD-CUST-ADDR-LINE-3             PIC X(50).
             15  WS-OLD-CUST-ADDR-STATE-CD           PIC X(02).
             15  WS-OLD-CUST-ADDR-COUNTRY-CD         PIC X(03).
             15  WS-OLD-CUST-ADDR-ZIP                PIC X(10).
             15  WS-OLD-CUST-PHONE-NUM-1             PIC X(15).
             15  WS-OLD-CUST-PHONE-NUM-1-X REDEFINES
                 WS-OLD-CUST-PHONE-NUM-1.
                 20 FILLER                           PIC X(1).
                 20 WS-OLD-CUST-PHONE-NUM-1A         PIC X(3).
                 20 FILLER                           PIC X(1).
                 20 WS-OLD-CUST-PHONE-NUM-1B         PIC X(3).
                 20 FILLER                           PIC X(1).
                 20 WS-OLD-CUST-PHONE-NUM-1C         PIC X(4).
                 20 FILLER                           PIC X(2).
             15  WS-OLD-CUST-PHONE-NUM-2             PIC X(15).
             15  WS-OLD-CUST-PHONE-NUM-2-X REDEFINES
                 WS-OLD-CUST-PHONE-NUM-2.
                 20 FILLER                           PIC X(1).
                 20 WS-OLD-CUST-PHONE-NUM-2A         PIC X(3).
                 20 FILLER                           PIC X(1).
                 20 WS-OLD-CUST-PHONE-NUM-2B         PIC X(3).
                 20 FILLER                           PIC X(1).
                 20 WS-OLD-CUST-PHONE-NUM-2C         PIC X(4).
                 20 FILLER                           PIC X(2).
             15  WS-OLD-CUST-SSN-X                   PIC X(09).
             15  WS-OLD-CUST-SSN                     REDEFINES
                 WS-OLD-CUST-SSN-X                   PIC 9(09).
             15  WS-OLD-CUST-GOVT-ISSUED-ID          PIC X(20).
             15  WS-OLD-CUST-DOB-YYYY-MM-DD          PIC X(08).
             15  WS-OLD-CUST-DOB-PARTS               REDEFINES
                 WS-OLD-CUST-DOB-YYYY-MM-DD.
                 20 WS-OLD-CUST-DOB-YEAR             PIC X(4).
                 20 WS-OLD-CUST-DOB-MON              PIC X(2).
                 20 WS-OLD-CUST-DOB-DAY              PIC X(2).
             15  WS-OLD-CUST-EFT-ACCOUNT-ID          PIC X(10).
             15  WS-OLD-CUST-PRI-HOLDER-IND          PIC X(01).
             15  WS-OLD-CUST-FICO-SCORE-X            PIC X(03).
             15  WS-OLD-CUST-FICO-SCORE              REDEFINES
                 WS-OLD-CUST-FICO-SCORE-X            PIC 9(03).

      *****************************************************************
      *    Generic date edit variables CCYYMMDD
      ******************************************************************
       01 DATE-VALID.
         05 DATE-VALID-LABEL.
           COPY CSUTLDWY.



       LINKAGE SECTION.
      ******************************************************************
      * COMMAREA Structure for Account Management RPC Program
      ******************************************************************
       01 DFHCOMMAREA.
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

       PROCEDURE DIVISION USING DFHCOMMAREA.

           EXEC CICS HANDLE ABEND
              LABEL(ABEND-ROUTINE)
           END-EXEC.

      ******************************************************************
      * Main processing logic - mirror COACTUPC structure
      ******************************************************************
       MAIN-PARA.

           INITIALIZE LK-OUTPUT-STATUS
                      LK-OUTPUT-DATA
                      WS-MISC-STORAGE

           SET RC-SUCCESS TO TRUE
           MOVE SPACES TO LK-OUT-MESSAGE
           MOVE SPACES TO LK-OUT-ERROR-FIELD

           SET INPUT-PENDING TO TRUE
           SET WS-RETURN-MSG-OFF TO TRUE
      * Determine the operation to perform
      * Modify the main EVALUATE in COACTUPL:
           EVALUATE TRUE
               WHEN OP-READ
                   PERFORM 1000-PROCESS-READ
                      THRU 1000-PROCESS-READ-EXIT

               WHEN OP-UPDATE OR OP-VALIDATE
      *         Both operations use the same logic with different endings
                   PERFORM 2000-PROCESS-UPDATE
                      THRU 2000-PROCESS-UPDATE-EXIT
                   PERFORM 1290-SET-ERROR-FIELD

               WHEN OTHER
                   SET RC-INPUT-ERROR TO TRUE
                   MOVE 'Invalid operation code. Use R, U, or V.'
                     TO LK-OUT-MESSAGE
           END-EVALUATE
           GOBACK.

      ******************************************************************
      * Process Read operation - mirror COACTUPC 1000-PROCESS-INPUTS
      ******************************************************************
       1000-PROCESS-READ.
           PERFORM 1100-VALIDATE-READ-INPUT
              THRU 1100-VALIDATE-READ-INPUT-EXIT

           IF INPUT-ERROR
              SET RC-INPUT-ERROR TO TRUE
              MOVE WS-RETURN-MSG TO LK-OUT-MESSAGE
              GO TO 1000-PROCESS-READ-EXIT
           END-IF

           PERFORM 9000-READ-ACCT
              THRU 9000-READ-ACCT-EXIT

           IF NOT RC-SUCCESS
              GO TO 1000-PROCESS-READ-EXIT
           END-IF

           PERFORM 1300-MAP-OUTPUT-DATA
              THRU 1300-MAP-OUTPUT-DATA-EXIT
              .

       1000-PROCESS-READ-EXIT.
           EXIT.

      ******************************************************************
      * Validate Read Input Parameters - mirror COACTUPC 1210-EDIT-ACCOUNT
      ******************************************************************
       1100-VALIDATE-READ-INPUT.
           PERFORM 1210-EDIT-ACCOUNT
              THRU 1210-EDIT-ACCOUNT-EXIT.

       1100-VALIDATE-READ-INPUT-EXIT.
           EXIT.

      ******************************************************************
      * Edit Account - exact copy from COACTUPC
      ******************************************************************
       1210-EDIT-ACCOUNT.
           SET FLG-ACCTFILTER-NOT-OK    TO TRUE

      *    Not supplied
           IF LK-IN-ACCT-ID   EQUAL LOW-VALUES
           OR LK-IN-ACCT-ID   EQUAL SPACES
              SET INPUT-ERROR           TO TRUE
              SET FLG-ACCTFILTER-BLANK  TO TRUE
              IF WS-RETURN-MSG-OFF
                 SET WS-PROMPT-FOR-ACCT TO TRUE
              END-IF
              MOVE ZEROES               TO WS-CARD-RID-ACCT-ID
              GO TO  1210-EDIT-ACCOUNT-EXIT
           END-IF

      *    Not numeric
      *    Not 11 characters
           MOVE LK-IN-ACCT-ID           TO WS-CARD-RID-ACCT-ID-X
           IF LK-IN-ACCT-ID   IS NOT NUMERIC
           OR WS-CARD-RID-ACCT-ID EQUAL ZEROS
              SET INPUT-ERROR TO TRUE
              IF WS-RETURN-MSG-OFF
                STRING
                 'Account Number if supplied must be a 11 digit'
                 ' Non-Zero Number'
                DELIMITED BY SIZE
                INTO WS-RETURN-MSG
              END-IF
              MOVE ZEROES               TO WS-CARD-RID-ACCT-ID
              GO TO 1210-EDIT-ACCOUNT-EXIT
           ELSE
              SET FLG-ACCTFILTER-ISVALID TO TRUE
           END-IF
           .

       1210-EDIT-ACCOUNT-EXIT.
           EXIT
           .

      ******************************************************************
      * Map Output Data from Database Records to Output Fields
      ******************************************************************
       1300-MAP-OUTPUT-DATA.
      * Account data
           MOVE ACCT-ID                   TO LK-OUT-ACCT-ID
           MOVE ACCT-ACTIVE-STATUS        TO LK-OUT-ACCT-ACTIVE-STATUS
           MOVE ACCT-CREDIT-LIMIT         TO LK-OUT-ACCT-CREDIT-LIMIT
           MOVE ACCT-CASH-CREDIT-LIMIT    TO LK-OUT-ACCT-CASH-LIMIT
           MOVE ACCT-CURR-BAL             TO LK-OUT-ACCT-CURR-BAL
           MOVE ACCT-CURR-CYC-CREDIT      TO LK-OUT-ACCT-CURR-CYC-CREDIT
           MOVE ACCT-CURR-CYC-DEBIT       TO LK-OUT-ACCT-CURR-CYC-DEBIT
           MOVE ACCT-OPEN-DATE            TO LK-OUT-ACCT-OPEN-DATE
           MOVE ACCT-EXPIRAION-DATE       TO LK-OUT-ACCT-EXPIRATION-DATE
           MOVE ACCT-REISSUE-DATE         TO LK-OUT-ACCT-REISSUE-DATE
           MOVE ACCT-GROUP-ID             TO LK-OUT-ACCT-GROUP-ID
           MOVE XREF-CARD-NUM             TO LK-OUT-ACCT-CARD-NUM

      * Customer data
           MOVE CUST-ID                   TO LK-OUT-CUST-ID
           MOVE CUST-FIRST-NAME           TO LK-OUT-CUST-FIRST-NAME
           MOVE CUST-MIDDLE-NAME          TO LK-OUT-CUST-MIDDLE-NAME
           MOVE CUST-LAST-NAME            TO LK-OUT-CUST-LAST-NAME
           MOVE CUST-SSN                  TO LK-OUT-CUST-SSN
           MOVE CUST-DOB-YYYY-MM-DD       TO LK-OUT-CUST-DOB
           MOVE CUST-ADDR-LINE-1          TO LK-OUT-CUST-ADDR-LINE-1
           MOVE CUST-ADDR-LINE-2          TO LK-OUT-CUST-ADDR-LINE-2
           MOVE CUST-ADDR-LINE-3          TO LK-OUT-CUST-ADDR-LINE-3
           MOVE CUST-ADDR-STATE-CD        TO LK-OUT-CUST-ADDR-STATE-CD
           MOVE CUST-ADDR-COUNTRY-CD      TO LK-OUT-CUST-ADDR-COUNTRY-CD
           MOVE CUST-ADDR-ZIP             TO LK-OUT-CUST-ADDR-ZIP
           MOVE CUST-PHONE-NUM-1          TO LK-OUT-CUST-PHONE-NUM-1
           MOVE CUST-PHONE-NUM-2          TO LK-OUT-CUST-PHONE-NUM-2
           MOVE CUST-GOVT-ISSUED-ID       TO LK-OUT-CUST-GOVT-ISSUED-ID
           MOVE CUST-EFT-ACCOUNT-ID       TO LK-OUT-CUST-EFT-ACCOUNT-ID
           MOVE CUST-PRI-CARD-HOLDER-IND  TO LK-OUT-CUST-PRI-HOLDER-IND
           MOVE CUST-FICO-CREDIT-SCORE    TO LK-OUT-CUST-FICO-SCORE

           MOVE 'Account and customer data retrieved successfully.'
                TO LK-OUT-MESSAGE.

       1300-MAP-OUTPUT-DATA-EXIT.
           EXIT.

      ******************************************************************
      * Process Update operation - mirror COACTUPC structure
      ******************************************************************
        2000-PROCESS-UPDATE.

           MOVE SPACES TO LK-OUT-ERROR-FIELD
           SET FLG-ACCTFILTER-ISVALID TO TRUE

           PERFORM 1100-RECEIVE-MAP
              THRU 1100-RECEIVE-MAP-EXIT

           PERFORM 1200-EDIT-MAP-INPUTS
              THRU 1200-EDIT-MAP-INPUTS-EXIT

           IF INPUT-ERROR
              SET RC-INPUT-ERROR TO TRUE
              MOVE WS-RETURN-MSG TO LK-OUT-MESSAGE
              GO TO 2000-PROCESS-UPDATE-EXIT
           END-IF

           PERFORM 2150-VERIFY-ACCOUNT-EXISTS
              THRU 2150-VERIFY-ACCT-EXISTS-EXIT

           IF NOT RC-SUCCESS
              GO TO 2000-PROCESS-UPDATE-EXIT
           END-IF

           PERFORM 1205-COMPARE-OLD-NEW
              THRU 1205-COMPARE-OLD-NEW-EXIT

           IF NO-CHANGES-FOUND
              SET RC-NO-CHANGES TO TRUE
              SET NO-CHANGES-DETECTED TO TRUE
              MOVE WS-RETURN-MSG TO LK-OUT-MESSAGE
              GO TO 2000-PROCESS-UPDATE-EXIT
           END-IF

           IF OP-VALIDATE
               SET RC-SUCCESS TO TRUE
               MOVE 'Changes validated.Press F5 to save.'
                 TO LK-OUT-MESSAGE
           ELSE
               PERFORM 9600-WRITE-PROCESSING
                  THRU 9600-WRITE-PROCESSING-EXIT

               IF RC-SUCCESS
                  PERFORM 1300-MAP-OUTPUT-DATA
                     THRU 1300-MAP-OUTPUT-DATA-EXIT
                  MOVE 'Changes committed to database'
                    TO LK-OUT-MESSAGE
               END-IF
           END-IF

           .
       2000-PROCESS-UPDATE-EXIT.
           EXIT.
      ******************************************************************
      * Receive Map - mirror COACTUPC 1100-RECEIVE-MAP structure
      ******************************************************************
       1100-RECEIVE-MAP.
      * Store account ID

           IF  LK-IN-ACCT-ID = SPACES
           OR  LK-IN-ACCT-ID = LOW-VALUES
               MOVE LOW-VALUES           TO WS-CARD-RID-ACCT-ID-X
           ELSE
               MOVE LK-IN-ACCT-ID        TO WS-CARD-RID-ACCT-ID-X
           END-IF

           IF FLG-ACCTFILTER-NOT-OK
              GO TO 1100-RECEIVE-MAP-EXIT
           END-IF

           .
       1100-RECEIVE-MAP-EXIT.
           EXIT
           .

      ******************************************************************
      * FIXED: Date validation section in 1200-EDIT-MAP-INPUTS with error
      * field detection
      ******************************************************************
       1200-EDIT-MAP-INPUTS.
           SET INPUT-OK                  TO TRUE
           MOVE SPACES TO WS-RETURN-MSG
           MOVE SPACES TO LK-OUT-ERROR-FIELD
           IF OP-UPDATE
               SET FLG-ACCTFILTER-ISVALID TO TRUE
           ELSE
               PERFORM 1210-EDIT-ACCOUNT
                  THRU 1210-EDIT-ACCOUNT-EXIT
           END-IF
      *       IF THE SEARCH CONDITIONS HAVE PROBLEMS FLAG THEM
           IF  FLG-ACCTFILTER-BLANK
               SET NO-SEARCH-CRITERIA-RECEIVED TO TRUE
               SET INPUT-ERROR TO TRUE
               MOVE 'ACCT-ID' TO LK-OUT-ERROR-FIELD
               GO TO 1200-EDIT-MAP-INPUTS-EXIT
           END-IF
           IF FLG-ACCTFILTER-NOT-OK
               SET INPUT-ERROR TO TRUE
               MOVE 'ACCT-ID' TO LK-OUT-ERROR-FIELD
               GO TO 1200-EDIT-MAP-INPUTS-EXIT
           END-IF
      *    SEARCH KEYS ALREADY VALIDATED
           SET FOUND-ACCOUNT-DATA        TO TRUE
           SET FOUND-ACCT-IN-MASTER      TO TRUE
           SET FLG-ACCTFILTER-ISVALID    TO TRUE

           SET FOUND-CUST-IN-MASTER      TO TRUE
           SET FLG-CUSTFILTER-ISVALID    TO TRUE
           MOVE 'Account Status'          TO WS-EDIT-VARIABLE-NAME
           MOVE LK-IN-ACCT-ACTIVE-STATUS TO WS-EDIT-YES-NO
           PERFORM 1220-EDIT-YESNO
              THRU 1220-EDIT-YESNO-EXIT
           MOVE WS-EDIT-YES-NO            TO WS-EDIT-ACCT-STATUS
           IF INPUT-ERROR AND LK-OUT-ERROR-FIELD = SPACES
               MOVE 'ACCT-STATUS' TO LK-OUT-ERROR-FIELD
           END-IF
      *    FIXED: Proper date validation with actual calls to
      *    EDIT-DATE-CCYYMMDD
           MOVE 'Open Date'              TO WS-EDIT-VARIABLE-NAME
           STRING LK-IN-ACCT-OPEN-YEAR
                  LK-IN-ACCT-OPEN-MON
                  LK-IN-ACCT-OPEN-DAY
           DELIMITED BY SIZE INTO WS-EDIT-DATE-CCYYMMDD
           PERFORM EDIT-DATE-CCYYMMDD
              THRU EDIT-DATE-CCYYMMDD-EXIT
           MOVE WS-EDIT-DATE-FLGS        TO WS-EDIT-OPEN-DATE-FLGS
           IF INPUT-ERROR AND LK-OUT-ERROR-FIELD = SPACES
               MOVE 'OPEN-YEAR' TO LK-OUT-ERROR-FIELD
           END-IF
           MOVE 'Expiry Date'            TO WS-EDIT-VARIABLE-NAME
           STRING LK-IN-ACCT-EXP-YEAR
                  LK-IN-ACCT-EXP-MON
                  LK-IN-ACCT-EXP-DAY
           DELIMITED BY SIZE INTO WS-EDIT-DATE-CCYYMMDD
           PERFORM EDIT-DATE-CCYYMMDD
              THRU EDIT-DATE-CCYYMMDD-EXIT
           MOVE WS-EDIT-DATE-FLGS        TO WS-EXPIRY-DATE-FLGS
           IF INPUT-ERROR AND LK-OUT-ERROR-FIELD = SPACES
               MOVE 'EXP-YEAR' TO LK-OUT-ERROR-FIELD
           END-IF
           MOVE 'Reissue Date'           TO WS-EDIT-VARIABLE-NAME
           STRING LK-IN-ACCT-REISSUE-YEAR
                  LK-IN-ACCT-REISSUE-MON
                  LK-IN-ACCT-REISSUE-DAY
           DELIMITED BY SIZE INTO WS-EDIT-DATE-CCYYMMDD
           PERFORM EDIT-DATE-CCYYMMDD
              THRU EDIT-DATE-CCYYMMDD-EXIT
           MOVE WS-EDIT-DATE-FLGS        TO WS-EDIT-REISSUE-DATE-FLGS
           IF INPUT-ERROR AND LK-OUT-ERROR-FIELD = SPACES
               MOVE 'REISSUE-YEAR' TO LK-OUT-ERROR-FIELD
           END-IF
           MOVE 'Date of Birth'          TO WS-EDIT-VARIABLE-NAME
           STRING LK-IN-CUST-DOB-YEAR
                  LK-IN-CUST-DOB-MON
                  LK-IN-CUST-DOB-DAY
           DELIMITED BY SIZE INTO WS-EDIT-DATE-CCYYMMDD
           PERFORM EDIT-DATE-CCYYMMDD
              THRU EDIT-DATE-CCYYMMDD-EXIT
           MOVE WS-EDIT-DATE-FLGS        TO WS-EDIT-DT-OF-BIRTH-FLGS
           IF WS-EDIT-DT-OF-BIRTH-ISVALID
              PERFORM  EDIT-DATE-OF-BIRTH
                 THRU  EDIT-DATE-OF-BIRTH-EXIT
              MOVE WS-EDIT-DATE-FLGS    TO WS-EDIT-DT-OF-BIRTH-FLGS
           END-IF
           IF INPUT-ERROR AND LK-OUT-ERROR-FIELD = SPACES
               MOVE 'DOB-YEAR' TO LK-OUT-ERROR-FIELD
           END-IF
           MOVE 'SSN'                    TO WS-EDIT-VARIABLE-NAME
           PERFORM 1265-EDIT-US-SSN
              THRU 1265-EDIT-US-SSN-EXIT
           IF INPUT-ERROR AND LK-OUT-ERROR-FIELD = SPACES
               MOVE 'SSN-PART1' TO LK-OUT-ERROR-FIELD
           END-IF
      * Credit Limit - UI already validated, just accept
           SET FLG-CRED-LIMIT-ISVALID TO TRUE

      * Cash Credit Limit - UI already validated, just accept
           SET FLG-CASH-CREDIT-LIMIT-ISVALID TO TRUE

      * Current Balance - UI already validated, just accept
           SET FLG-CURR-BAL-ISVALID TO TRUE

      * Current Cycle Credit - UI already validated, just accept
           SET FLG-CURR-CYC-CREDIT-ISVALID TO TRUE
      * Current Cycle Debit - UI already validated, just accept
           SET FLG-CURR-CYC-DEBIT-ISVALID TO TRUE
           MOVE 'FICO Score'             TO WS-EDIT-VARIABLE-NAME
           MOVE LK-IN-CUST-FICO-SCORE
                                         TO WS-EDIT-ALPHANUM-ONLY
           MOVE 3                        TO WS-EDIT-ALPHANUM-LENGTH
           PERFORM 1245-EDIT-NUM-REQD
              THRU 1245-EDIT-NUM-REQD-EXIT
           MOVE WS-EDIT-ALPHANUM-ONLY-FLAGS
                                         TO WS-EDIT-FICO-SCORE-FLGS
           IF FLG-FICO-SCORE-ISVALID
              PERFORM  1275-EDIT-FICO-SCORE
                 THRU  1275-EDIT-FICO-SCORE-EXIT
           END-IF
           IF INPUT-ERROR AND LK-OUT-ERROR-FIELD = SPACES
               MOVE 'FICO-SCORE' TO LK-OUT-ERROR-FIELD
           END-IF
      ******************************************************************
      *    Edit names
      ******************************************************************
           MOVE 'First Name'             TO WS-EDIT-VARIABLE-NAME
           MOVE LK-IN-CUST-FIRST-NAME TO WS-EDIT-ALPHANUM-ONLY
           MOVE 25                       TO WS-EDIT-ALPHANUM-LENGTH
           PERFORM 1225-EDIT-ALPHA-REQD
              THRU 1225-EDIT-ALPHA-REQD-EXIT
           MOVE WS-EDIT-ALPHA-ONLY-FLAGS
                                         TO WS-EDIT-FIRST-NAME-FLGS
           IF INPUT-ERROR AND LK-OUT-ERROR-FIELD = SPACES
               MOVE 'FIRST-NAME' TO LK-OUT-ERROR-FIELD
           END-IF
           MOVE 'Middle Name'            TO WS-EDIT-VARIABLE-NAME
           MOVE LK-IN-CUST-MIDDLE-NAME TO WS-EDIT-ALPHANUM-ONLY
           MOVE 25                       TO WS-EDIT-ALPHANUM-LENGTH
           PERFORM 1235-EDIT-ALPHA-OPT
              THRU 1235-EDIT-ALPHA-OPT-EXIT
           MOVE WS-EDIT-ALPHA-ONLY-FLAGS
                                         TO WS-EDIT-MIDDLE-NAME-FLGS
           IF INPUT-ERROR AND LK-OUT-ERROR-FIELD = SPACES
               MOVE 'MIDDLE-NAME' TO LK-OUT-ERROR-FIELD
           END-IF
           MOVE 'Last Name'              TO WS-EDIT-VARIABLE-NAME
           MOVE LK-IN-CUST-LAST-NAME  TO WS-EDIT-ALPHANUM-ONLY
           MOVE 25                       TO WS-EDIT-ALPHANUM-LENGTH
           PERFORM 1225-EDIT-ALPHA-REQD
              THRU 1225-EDIT-ALPHA-REQD-EXIT
           MOVE WS-EDIT-ALPHA-ONLY-FLAGS
                                        TO WS-EDIT-LAST-NAME-FLGS
           IF INPUT-ERROR AND LK-OUT-ERROR-FIELD = SPACES
               MOVE 'LAST-NAME' TO LK-OUT-ERROR-FIELD
           END-IF
           MOVE 'Address Line 1'         TO WS-EDIT-VARIABLE-NAME
           MOVE LK-IN-CUST-ADDR-LINE-1 TO WS-EDIT-ALPHANUM-ONLY
           MOVE 50                       TO WS-EDIT-ALPHANUM-LENGTH
           PERFORM 1215-EDIT-MANDATORY
              THRU 1215-EDIT-MANDATORY-EXIT
           MOVE WS-EDIT-MANDATORY-FLAGS
                                         TO WS-EDIT-ADDRESS-LINE-1-FLGS
           IF INPUT-ERROR AND LK-OUT-ERROR-FIELD = SPACES
               MOVE 'ADDR-LINE1' TO LK-OUT-ERROR-FIELD
           END-IF
           MOVE 'State'                  TO WS-EDIT-VARIABLE-NAME
           MOVE LK-IN-CUST-ADDR-STATE-CD TO WS-EDIT-ALPHANUM-ONLY
           MOVE 2                       TO WS-EDIT-ALPHANUM-LENGTH
           PERFORM 1225-EDIT-ALPHA-REQD
              THRU 1225-EDIT-ALPHA-REQD-EXIT
           MOVE WS-EDIT-ALPHA-ONLY-FLAGS
                                         TO WS-EDIT-STATE-FLGS

           IF FLG-ALPHA-ISVALID
           PERFORM 1270-EDIT-US-STATE-CD
              THRU 1270-EDIT-US-STATE-CD-EXIT
           END-IF
           IF INPUT-ERROR AND LK-OUT-ERROR-FIELD = SPACES
               MOVE 'STATE' TO LK-OUT-ERROR-FIELD
           END-IF
           MOVE 'Zip'                    TO WS-EDIT-VARIABLE-NAME
           MOVE LK-IN-CUST-ADDR-ZIP   TO WS-EDIT-ALPHANUM-ONLY
           MOVE 5                        TO WS-EDIT-ALPHANUM-LENGTH
           PERFORM 1245-EDIT-NUM-REQD
              THRU 1245-EDIT-NUM-REQD-EXIT
           MOVE WS-EDIT-ALPHANUM-ONLY-FLAGS
                                         TO WS-EDIT-ZIPCODE-FLGS
           IF INPUT-ERROR AND LK-OUT-ERROR-FIELD = SPACES
               MOVE 'ZIP-CODE' TO LK-OUT-ERROR-FIELD
           END-IF
      *    Address Line 2 is optional
           MOVE 'City'                   TO WS-EDIT-VARIABLE-NAME
           MOVE LK-IN-CUST-ADDR-LINE-3 TO WS-EDIT-ALPHANUM-ONLY
           MOVE 50                       TO WS-EDIT-ALPHANUM-LENGTH
           PERFORM 1225-EDIT-ALPHA-REQD
              THRU 1225-EDIT-ALPHA-REQD-EXIT
           MOVE WS-EDIT-ALPHA-ONLY-FLAGS
                                         TO WS-EDIT-CITY-FLGS
           IF INPUT-ERROR AND LK-OUT-ERROR-FIELD = SPACES
               MOVE 'CITY' TO LK-OUT-ERROR-FIELD
           END-IF
           MOVE 'Country'                TO WS-EDIT-VARIABLE-NAME
           MOVE LK-IN-CUST-ADDR-COUNTRY-CD
                                        TO WS-EDIT-ALPHANUM-ONLY
           MOVE 3                       TO WS-EDIT-ALPHANUM-LENGTH
           PERFORM 1225-EDIT-ALPHA-REQD
              THRU 1225-EDIT-ALPHA-REQD-EXIT
           MOVE WS-EDIT-ALPHA-ONLY-FLAGS
                                         TO WS-EDIT-COUNTRY-FLGS
           IF INPUT-ERROR AND LK-OUT-ERROR-FIELD = SPACES
               MOVE 'COUNTRY' TO LK-OUT-ERROR-FIELD
           END-IF
           MOVE 'Phone Number 1'         TO WS-EDIT-VARIABLE-NAME
           STRING '('
                  LK-IN-CUST-PHONE-1A
                  ')'
                  LK-IN-CUST-PHONE-1B
                  '-'
                  LK-IN-CUST-PHONE-1C
           DELIMITED BY SIZE INTO WS-EDIT-US-PHONE-NUM
           PERFORM 1260-EDIT-US-PHONE-NUM
              THRU 1260-EDIT-US-PHONE-NUM-EXIT
           MOVE WS-EDIT-US-PHONE-NUM-FLGS
                                         TO  WS-EDIT-PHONE-NUM-1-FLGS
           IF INPUT-ERROR AND LK-OUT-ERROR-FIELD = SPACES
               MOVE 'PHONE1-AREA' TO LK-OUT-ERROR-FIELD
           END-IF
           MOVE 'Phone Number 2'         TO WS-EDIT-VARIABLE-NAME
           STRING '('
                  LK-IN-CUST-PHONE-2A
                  ')'
                  LK-IN-CUST-PHONE-2B
                  '-'
                  LK-IN-CUST-PHONE-2C
           DELIMITED BY SIZE INTO WS-EDIT-US-PHONE-NUM
           PERFORM 1260-EDIT-US-PHONE-NUM
              THRU 1260-EDIT-US-PHONE-NUM-EXIT
           MOVE WS-EDIT-US-PHONE-NUM-FLGS
                                         TO WS-EDIT-PHONE-NUM-2-FLGS
           IF INPUT-ERROR AND LK-OUT-ERROR-FIELD = SPACES
               MOVE 'PHONE2-AREA' TO LK-OUT-ERROR-FIELD
           END-IF
           MOVE 'EFT Account Id'         TO WS-EDIT-VARIABLE-NAME
           MOVE LK-IN-CUST-EFT-ACCOUNT-ID
                                         TO WS-EDIT-ALPHANUM-ONLY
           MOVE 10                       TO WS-EDIT-ALPHANUM-LENGTH
           PERFORM 1245-EDIT-NUM-REQD
              THRU 1245-EDIT-NUM-REQD-EXIT
           MOVE WS-EDIT-ALPHANUM-ONLY-FLAGS
                                         TO WS-EFT-ACCOUNT-ID-FLGS
           IF INPUT-ERROR AND LK-OUT-ERROR-FIELD = SPACES
               MOVE 'EFT-ACCOUNT' TO LK-OUT-ERROR-FIELD
           END-IF
           MOVE 'Primary Card Holder'    TO WS-EDIT-VARIABLE-NAME
           MOVE LK-IN-CUST-PRI-HOLDER-IND
                                         TO WS-EDIT-YES-NO
           PERFORM 1220-EDIT-YESNO
              THRU 1220-EDIT-YESNO-EXIT
           MOVE WS-EDIT-YES-NO           TO WS-EDIT-PRI-CARDHOLDER
           IF INPUT-ERROR AND LK-OUT-ERROR-FIELD = SPACES
               MOVE 'PRI-HOLDER' TO LK-OUT-ERROR-FIELD
           END-IF
      *    Cross field edits begin here
           IF  FLG-STATE-ISVALID
           AND FLG-ZIPCODE-ISVALID
               PERFORM 1280-EDIT-US-STATE-ZIP-CD
                THRU 1280-EDIT-US-STATE-ZIP-CD-EXIT
               IF INPUT-ERROR AND LK-OUT-ERROR-FIELD = SPACES
                   MOVE 'ZIP-CODE' TO LK-OUT-ERROR-FIELD
               END-IF
           END-IF
           .
       1200-EDIT-MAP-INPUTS-EXIT.
           EXIT
           .

      ******************************************************************
      * Compare Old New - mirror COACTUPC 1205-COMPARE-OLD-NEW
      ******************************************************************
       1205-COMPARE-OLD-NEW.
           SET NO-CHANGES-FOUND           TO TRUE

      * Compare Account data fields
           IF  LK-IN-ACCT-ID NOT = WS-OLD-ACCT-ID-X
               SET CHANGE-HAS-OCCURRED   TO TRUE
               GO TO 1205-COMPARE-OLD-NEW-EXIT
           END-IF

           IF FUNCTION UPPER-CASE(LK-IN-ACCT-ACTIVE-STATUS) NOT =
              FUNCTION UPPER-CASE(WS-OLD-ACTIVE-STATUS)
               SET CHANGE-HAS-OCCURRED   TO TRUE
               GO TO 1205-COMPARE-OLD-NEW-EXIT
           END-IF

           IF LK-IN-ACCT-CURR-BAL NOT = WS-OLD-CURR-BAL-N
               SET CHANGE-HAS-OCCURRED   TO TRUE
               GO TO 1205-COMPARE-OLD-NEW-EXIT
           END-IF

           IF LK-IN-ACCT-CREDIT-LIMIT NOT = WS-OLD-CREDIT-LIMIT-N
               SET CHANGE-HAS-OCCURRED   TO TRUE
               GO TO 1205-COMPARE-OLD-NEW-EXIT
           END-IF

           IF LK-IN-ACCT-CASH-LIMIT NOT = WS-OLD-CASH-CREDIT-LIMIT-N
               SET CHANGE-HAS-OCCURRED   TO TRUE
               GO TO 1205-COMPARE-OLD-NEW-EXIT
           END-IF

      * Compare dates by concatenating and comparing
           STRING LK-IN-ACCT-OPEN-YEAR
                  LK-IN-ACCT-OPEN-MON
                  LK-IN-ACCT-OPEN-DAY
           DELIMITED BY SIZE INTO WS-EDIT-VARIABLE-NAME(1:8)

           IF WS-EDIT-VARIABLE-NAME(1:8) NOT = WS-OLD-OPEN-DATE
               SET CHANGE-HAS-OCCURRED   TO TRUE
               GO TO 1205-COMPARE-OLD-NEW-EXIT
           END-IF

           STRING LK-IN-ACCT-EXP-YEAR
                  LK-IN-ACCT-EXP-MON
                  LK-IN-ACCT-EXP-DAY
           DELIMITED BY SIZE INTO WS-EDIT-VARIABLE-NAME(1:8)

           IF WS-EDIT-VARIABLE-NAME(1:8) NOT = WS-OLD-EXPIRAION-DATE
               SET CHANGE-HAS-OCCURRED   TO TRUE
               GO TO 1205-COMPARE-OLD-NEW-EXIT
           END-IF

           STRING LK-IN-ACCT-REISSUE-YEAR
                  LK-IN-ACCT-REISSUE-MON
                  LK-IN-ACCT-REISSUE-DAY
           DELIMITED BY SIZE INTO WS-EDIT-VARIABLE-NAME(1:8)

           IF WS-EDIT-VARIABLE-NAME(1:8) NOT = WS-OLD-REISSUE-DATE
               SET CHANGE-HAS-OCCURRED   TO TRUE
               GO TO 1205-COMPARE-OLD-NEW-EXIT
           END-IF

           IF LK-IN-ACCT-CURR-CYC-CREDIT NOT = WS-OLD-CURR-CYC-CREDIT-N
               SET CHANGE-HAS-OCCURRED   TO TRUE
               GO TO 1205-COMPARE-OLD-NEW-EXIT
           END-IF

           IF LK-IN-ACCT-CURR-CYC-DEBIT NOT = WS-OLD-CURR-CYC-DEBIT-N
               SET CHANGE-HAS-OCCURRED   TO TRUE
               GO TO 1205-COMPARE-OLD-NEW-EXIT
           END-IF

           IF FUNCTION UPPER-CASE(FUNCTION TRIM(LK-IN-ACCT-GROUP-ID))
              NOT = FUNCTION UPPER-CASE(FUNCTION TRIM(WS-OLD-GROUP-ID))
               SET CHANGE-HAS-OCCURRED   TO TRUE
               GO TO 1205-COMPARE-OLD-NEW-EXIT
           END-IF

      * Compare Customer data fields
           IF FUNCTION UPPER-CASE(FUNCTION TRIM(LK-IN-CUST-ID))
              NOT = FUNCTION UPPER-CASE(FUNCTION TRIM(WS-OLD-CUST-ID-X))
               SET CHANGE-HAS-OCCURRED   TO TRUE
               GO TO 1205-COMPARE-OLD-NEW-EXIT
           END-IF

           IF FUNCTION
           UPPER-CASE(FUNCTION TRIM(LK-IN-CUST-FIRST-NAME))
              NOT = FUNCTION
              UPPER-CASE(FUNCTION TRIM(WS-OLD-CUST-FIRST-NAME))
               SET CHANGE-HAS-OCCURRED   TO TRUE
               GO TO 1205-COMPARE-OLD-NEW-EXIT
           END-IF

           IF FUNCTION
           UPPER-CASE(FUNCTION TRIM(LK-IN-CUST-MIDDLE-NAME))
              NOT = FUNCTION
              UPPER-CASE(FUNCTION TRIM(WS-OLD-CUST-MIDDLE-NAME))
               SET CHANGE-HAS-OCCURRED   TO TRUE
               GO TO 1205-COMPARE-OLD-NEW-EXIT
           END-IF

           IF FUNCTION
           UPPER-CASE(FUNCTION TRIM(LK-IN-CUST-LAST-NAME))
              NOT = FUNCTION
              UPPER-CASE(FUNCTION TRIM(WS-OLD-CUST-LAST-NAME))
               SET CHANGE-HAS-OCCURRED   TO TRUE
               GO TO 1205-COMPARE-OLD-NEW-EXIT
           END-IF

           IF FUNCTION
           UPPER-CASE(FUNCTION TRIM(LK-IN-CUST-ADDR-LINE-1))
              NOT = FUNCTION
              UPPER-CASE(FUNCTION TRIM(WS-OLD-CUST-ADDR-LINE-1))
               SET CHANGE-HAS-OCCURRED   TO TRUE
               GO TO 1205-COMPARE-OLD-NEW-EXIT
           END-IF

           IF FUNCTION
           UPPER-CASE(FUNCTION TRIM(LK-IN-CUST-ADDR-LINE-2))
              NOT = FUNCTION
              UPPER-CASE(FUNCTION TRIM(WS-OLD-CUST-ADDR-LINE-2))
               SET CHANGE-HAS-OCCURRED   TO TRUE
               GO TO 1205-COMPARE-OLD-NEW-EXIT
           END-IF

           IF FUNCTION
           UPPER-CASE(FUNCTION TRIM(LK-IN-CUST-ADDR-LINE-3))
              NOT = FUNCTION
              UPPER-CASE(FUNCTION TRIM(WS-OLD-CUST-ADDR-LINE-3))
               SET CHANGE-HAS-OCCURRED   TO TRUE
               GO TO 1205-COMPARE-OLD-NEW-EXIT
           END-IF

           IF FUNCTION
           UPPER-CASE(FUNCTION TRIM(LK-IN-CUST-ADDR-STATE-CD))
              NOT = FUNCTION
              UPPER-CASE(FUNCTION TRIM(WS-OLD-CUST-ADDR-STATE-CD))
               SET CHANGE-HAS-OCCURRED   TO TRUE
               GO TO 1205-COMPARE-OLD-NEW-EXIT
           END-IF

           IF FUNCTION
           UPPER-CASE(FUNCTION TRIM(LK-IN-CUST-ADDR-COUNTRY-CD))
              NOT = FUNCTION
              UPPER-CASE(FUNCTION TRIM(WS-OLD-CUST-ADDR-COUNTRY-CD))
               SET CHANGE-HAS-OCCURRED   TO TRUE
               GO TO 1205-COMPARE-OLD-NEW-EXIT
           END-IF

           IF FUNCTION
           UPPER-CASE(FUNCTION TRIM(LK-IN-CUST-ADDR-ZIP))
              NOT = FUNCTION UPPER-CASE(
               FUNCTION TRIM(WS-OLD-CUST-ADDR-ZIP))
               SET CHANGE-HAS-OCCURRED   TO TRUE
               GO TO 1205-COMPARE-OLD-NEW-EXIT
           END-IF

      * Compare phone numbers
           IF LK-IN-CUST-PHONE-1A NOT = WS-OLD-CUST-PHONE-NUM-1A
           OR LK-IN-CUST-PHONE-1B NOT = WS-OLD-CUST-PHONE-NUM-1B
           OR LK-IN-CUST-PHONE-1C NOT = WS-OLD-CUST-PHONE-NUM-1C
               SET CHANGE-HAS-OCCURRED   TO TRUE
               GO TO 1205-COMPARE-OLD-NEW-EXIT
           END-IF

           IF LK-IN-CUST-PHONE-2A NOT = WS-OLD-CUST-PHONE-NUM-2A
           OR LK-IN-CUST-PHONE-2B NOT = WS-OLD-CUST-PHONE-NUM-2B
           OR LK-IN-CUST-PHONE-2C NOT = WS-OLD-CUST-PHONE-NUM-2C
               SET CHANGE-HAS-OCCURRED   TO TRUE
               GO TO 1205-COMPARE-OLD-NEW-EXIT
           END-IF

      * Compare SSN by concatenating
           STRING LK-IN-CUST-SSN-1
                  LK-IN-CUST-SSN-2
                  LK-IN-CUST-SSN-3
           DELIMITED BY SIZE INTO WS-EDIT-VARIABLE-NAME(1:9)

           IF WS-EDIT-VARIABLE-NAME(1:9) NOT = WS-OLD-CUST-SSN-X
               SET CHANGE-HAS-OCCURRED   TO TRUE
               GO TO 1205-COMPARE-OLD-NEW-EXIT
           END-IF

           IF FUNCTION
           UPPER-CASE(FUNCTION TRIM(LK-IN-CUST-GOVT-ISSUED-ID))
              NOT = FUNCTION
              UPPER-CASE(FUNCTION TRIM(WS-OLD-CUST-GOVT-ISSUED-ID))
               SET CHANGE-HAS-OCCURRED   TO TRUE
               GO TO 1205-COMPARE-OLD-NEW-EXIT
           END-IF

      * Compare DOB by concatenating
           STRING LK-IN-CUST-DOB-YEAR
                  LK-IN-CUST-DOB-MON
                  LK-IN-CUST-DOB-DAY
           DELIMITED BY SIZE INTO WS-EDIT-VARIABLE-NAME(1:8)

           IF WS-EDIT-VARIABLE-NAME(1:8)
           NOT = WS-OLD-CUST-DOB-YYYY-MM-DD
               SET CHANGE-HAS-OCCURRED   TO TRUE
               GO TO 1205-COMPARE-OLD-NEW-EXIT
           END-IF

           IF LK-IN-CUST-EFT-ACCOUNT-ID NOT = WS-OLD-CUST-EFT-ACCOUNT-ID
               SET CHANGE-HAS-OCCURRED   TO TRUE
               GO TO 1205-COMPARE-OLD-NEW-EXIT
           END-IF

           IF FUNCTION UPPER-CASE
           (FUNCTION TRIM(LK-IN-CUST-PRI-HOLDER-IND))
              NOT = FUNCTION UPPER-CASE
              (FUNCTION TRIM(WS-OLD-CUST-PRI-HOLDER-IND))
               SET CHANGE-HAS-OCCURRED   TO TRUE
               GO TO 1205-COMPARE-OLD-NEW-EXIT
           END-IF

           IF LK-IN-CUST-FICO-SCORE NOT = WS-OLD-CUST-FICO-SCORE-X
               SET CHANGE-HAS-OCCURRED   TO TRUE
               GO TO 1205-COMPARE-OLD-NEW-EXIT
           END-IF

      * If we get here, no changes were detected
           SET NO-CHANGES-DETECTED TO TRUE

           .

       1205-COMPARE-OLD-NEW-EXIT.
           EXIT
           .

      ******************************************************************
      * Verify Account Exists Before Update
      ******************************************************************
       2150-VERIFY-ACCOUNT-EXISTS.
           PERFORM 9000-READ-ACCT
              THRU 9000-READ-ACCT-EXIT

           IF DID-NOT-FIND-ACCT-IN-CARDXREF
              SET RC-NOT-FOUND TO TRUE
              MOVE WS-RETURN-MSG TO LK-OUT-MESSAGE
              GO TO 2150-VERIFY-ACCT-EXISTS-EXIT
           END-IF

           IF DID-NOT-FIND-ACCT-IN-ACCTDAT
              SET RC-NOT-FOUND TO TRUE
              MOVE WS-RETURN-MSG TO LK-OUT-MESSAGE
              GO TO 2150-VERIFY-ACCT-EXISTS-EXIT
           END-IF

           IF DID-NOT-FIND-CUST-IN-CUSTDAT
              SET RC-NOT-FOUND TO TRUE
              MOVE WS-RETURN-MSG TO LK-OUT-MESSAGE
              GO TO 2150-VERIFY-ACCT-EXISTS-EXIT
           END-IF

           PERFORM 9500-STORE-FETCHED-DATA
              THRU 9500-STORE-FETCHED-DATA-EXIT
              .

       2150-VERIFY-ACCT-EXISTS-EXIT.
           EXIT.

      ******************************************************************
      * All validation paragraphs from COACTUPC - exact copies
      ******************************************************************
       1215-EDIT-MANDATORY.
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

              GO TO  1215-EDIT-MANDATORY-EXIT
           END-IF

           SET FLG-MANDATORY-ISVALID   TO TRUE
           .
       1215-EDIT-MANDATORY-EXIT.
           EXIT
           .

       1220-EDIT-YESNO.
      *    Must be Y or N
      *    SET FLG-YES-NO-NOT-OK         TO TRUE
      *
      *    Not supplied
           IF WS-EDIT-YES-NO             EQUAL LOW-VALUES
           OR WS-EDIT-YES-NO             EQUAL SPACES
           OR WS-EDIT-YES-NO             EQUAL ZEROS
              SET INPUT-ERROR            TO TRUE
              SET FLG-YES-NO-BLANK       TO TRUE
              IF WS-RETURN-MSG-OFF
                 STRING
                   FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                   ' must be supplied.'
                   DELIMITED BY SIZE
                   INTO WS-RETURN-MSG
                 END-STRING
              END-IF
              GO TO  1220-EDIT-YESNO-EXIT
           END-IF


           IF FLG-YES-NO-ISVALID
              CONTINUE
           ELSE
              SET INPUT-ERROR             TO TRUE
              SET FLG-YES-NO-NOT-OK       TO TRUE
              IF WS-RETURN-MSG-OFF
                 STRING
                   FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                   ' must be Y or N.'
                   DELIMITED BY SIZE
                   INTO WS-RETURN-MSG
                 END-STRING
              END-IF
              GO TO  1220-EDIT-YESNO-EXIT
           END-IF
           .
       1220-EDIT-YESNO-EXIT.
           EXIT
           .

       1225-EDIT-ALPHA-REQD.
      *    Initialize
           SET FLG-ALPHA-NOT-OK              TO TRUE

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

              GO TO  1225-EDIT-ALPHA-REQD-EXIT
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
              GO TO  1225-EDIT-ALPHA-REQD-EXIT
           END-IF

           SET FLG-ALPHA-ISVALID        TO TRUE
           .
       1225-EDIT-ALPHA-REQD-EXIT.
           EXIT
           .

       1235-EDIT-ALPHA-OPT.
      *    Initialize
           SET FLG-ALPHA-NOT-OK              TO TRUE

      *    Not supplied
           IF WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH)
                                             EQUAL LOW-VALUES
           OR WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH)
               EQUAL SPACES
           OR FUNCTION LENGTH(FUNCTION TRIM(
              WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH))) = 0

              SET FLG-ALPHA-ISVALID          TO TRUE
              GO TO  1235-EDIT-ALPHA-OPT-EXIT
           ELSE
              CONTINUE
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
              GO TO  1235-EDIT-ALPHA-OPT-EXIT
           END-IF

           SET FLG-ALPHA-ISVALID        TO TRUE
           .
       1235-EDIT-ALPHA-OPT-EXIT.
           EXIT
           .

       1245-EDIT-NUM-REQD.
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

              GO TO  1245-EDIT-NUM-REQD-EXIT
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
              GO TO  1245-EDIT-NUM-REQD-EXIT
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
              GO TO  1245-EDIT-NUM-REQD-EXIT
           ELSE
              CONTINUE
           END-IF


           SET FLG-ALPHNANUM-ISVALID    TO TRUE
           .
       1245-EDIT-NUM-REQD-EXIT.
           EXIT
           .


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
           AND (WS-EDIT-US-PHONE-NUMA EQUAL SPACES
           OR   WS-EDIT-US-PHONE-NUMC EQUAL LOW-VALUES)
                SET WS-EDIT-US-PHONE-IS-VALID TO TRUE
                GO TO EDIT-US-PHONE-EXIT
           ELSE
                CONTINUE
           END-IF
           .
       EDIT-AREA-CODE.
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
              GO TO  EDIT-US-PHONE-PREFIX
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
              GO TO  EDIT-US-PHONE-PREFIX
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
              GO TO  EDIT-US-PHONE-PREFIX
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
              GO TO  EDIT-US-PHONE-PREFIX
           END-IF

           SET FLG-EDIT-US-PHONEA-ISVALID    TO TRUE
           .
       EDIT-US-PHONE-PREFIX.

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
              GO TO  EDIT-US-PHONE-LINENUM
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
              GO TO  EDIT-US-PHONE-LINENUM
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
              GO TO  EDIT-US-PHONE-LINENUM
           ELSE
              CONTINUE
           END-IF

           SET FLG-EDIT-US-PHONEB-ISVALID    TO TRUE
           .

       EDIT-US-PHONE-LINENUM.
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
              GO TO EDIT-US-PHONE-EXIT
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
              GO TO  EDIT-US-PHONE-EXIT
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
              GO TO  EDIT-US-PHONE-EXIT
           ELSE
               CONTINUE
           END-IF


           SET FLG-EDIT-US-PHONEC-ISVALID    TO TRUE
           .

       EDIT-US-PHONE-EXIT.
           EXIT
           .
       1260-EDIT-US-PHONE-NUM-EXIT.
           EXIT
           .

       1265-EDIT-US-SSN.
      *Format xxx-xx-xxxx
      *Part1 :should have 3 digits
      *Part2 :should have 2 digits and it should be from 01 to 99
      *Part3 should have 4 digits from 0001 to 9999.
      ******************************************************************
      *    Edit SSN Part 1
      ******************************************************************
           MOVE 'SSN: First 3 chars'     TO WS-EDIT-VARIABLE-NAME
           MOVE LK-IN-CUST-SSN-1      TO WS-EDIT-ALPHANUM-ONLY
           MOVE 3                        TO WS-EDIT-ALPHANUM-LENGTH
           PERFORM 1245-EDIT-NUM-REQD
              THRU 1245-EDIT-NUM-REQD-EXIT
           MOVE WS-EDIT-ALPHANUM-ONLY-FLAGS
                                         TO WS-EDIT-US-SSN-PART1-FLGS

      *Part1 :should not be 000, 666, or between 900 and 999
           IF FLG-EDIT-US-SSN-PART1-ISVALID
              MOVE LK-IN-CUST-SSN-1   TO WS-EDIT-US-SSN-PART1
              IF INVALID-SSN-PART1
              SET INPUT-ERROR            TO TRUE
              SET FLG-EDIT-US-SSN-PART1-NOT-OK
                                 TO TRUE
              IF WS-RETURN-MSG-OFF
                 STRING
                   FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
                   ': should not be 000, 666, or between 900 and 999'
                   DELIMITED BY SIZE
                   INTO WS-RETURN-MSG
                 END-STRING
              ELSE
                CONTINUE
              END-IF
           END-IF

      ******************************************************************
      *    Edit SSN Part 2
      ******************************************************************
           MOVE 'SSN 4th & 5th chars'    TO WS-EDIT-VARIABLE-NAME
           MOVE LK-IN-CUST-SSN-2      TO WS-EDIT-ALPHANUM-ONLY
           MOVE 2                        TO WS-EDIT-ALPHANUM-LENGTH
           PERFORM 1245-EDIT-NUM-REQD
              THRU 1245-EDIT-NUM-REQD-EXIT
           MOVE WS-EDIT-ALPHANUM-ONLY-FLAGS
                                         TO WS-EDIT-US-SSN-PART2-FLGS


      ******************************************************************
      *    Edit SSN Part 3
      ******************************************************************
           MOVE 'SSN Last 4 chars'       TO WS-EDIT-VARIABLE-NAME
           MOVE LK-IN-CUST-SSN-3      TO WS-EDIT-ALPHANUM-ONLY
           MOVE 4                        TO WS-EDIT-ALPHANUM-LENGTH
           PERFORM 1245-EDIT-NUM-REQD
              THRU 1245-EDIT-NUM-REQD-EXIT
           MOVE WS-EDIT-ALPHANUM-ONLY-FLAGS
                                         TO WS-EDIT-US-SSN-PART3-FLGS
           .
       1265-EDIT-US-SSN-EXIT.
           EXIT
           .

       1270-EDIT-US-STATE-CD.
           MOVE LK-IN-CUST-ADDR-STATE-CD TO US-STATE-CODE-TO-EDIT
           IF VALID-US-STATE-CODE
               CONTINUE
           ELSE
              SET INPUT-ERROR              TO TRUE
              SET FLG-STATE-NOT-OK         TO TRUE
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
           IF FUNCTION NUMVAL(LK-IN-CUST-FICO-SCORE) >= 300
           AND FUNCTION NUMVAL(LK-IN-CUST-FICO-SCORE) <= 850
               CONTINUE
           ELSE
              SET INPUT-ERROR              TO TRUE
              SET FLG-FICO-SCORE-NOT-OK    TO TRUE
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

      *A crude zip code edit based on data from USPS web site
       1280-EDIT-US-STATE-ZIP-CD.
           STRING LK-IN-CUST-ADDR-STATE-CD
                  LK-IN-CUST-ADDR-ZIP(1:2)
             DELIMITED BY SIZE
             INTO US-STATE-AND-FIRST-ZIP2

           IF VALID-US-STATE-ZIP-CD2-COMBO
               CONTINUE
           ELSE
              SET INPUT-ERROR              TO TRUE
              SET FLG-STATE-NOT-OK         TO TRUE
              SET FLG-ZIPCODE-NOT-OK       TO TRUE
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

      * Add this new paragraph to COACTUPL
       1290-SET-ERROR-FIELD.

      *      IF NOT RC-INPUT-ERROR
      *         GO TO 1290-SET-ERROR-FIELD-EXIT
      *     END-IF

      *    This paragraph sets the error field name based on validation flags
           MOVE SPACES TO LK-OUT-ERROR-FIELD
           EVALUATE TRUE

               WHEN FLG-ACCTFILTER-NOT-OK OR FLG-ACCTFILTER-BLANK
                   MOVE 'ACCT-ID' TO LK-OUT-ERROR-FIELD

               WHEN FLG-ACCT-STATUS-NOT-OK OR FLG-ACCT-STATUS-BLANK
                   MOVE 'ACCT-STATUS' TO LK-OUT-ERROR-FIELD

               WHEN FLG-CRED-LIMIT-NOT-OK OR FLG-CRED-LIMIT-BLANK
                   MOVE 'CREDIT-LIMIT' TO LK-OUT-ERROR-FIELD

               WHEN FLG-CASH-CREDIT-LIMIT-NOT-OK OR
               FLG-CASH-CREDIT-LIMIT-BLANK
                   MOVE 'CASH-LIMIT' TO LK-OUT-ERROR-FIELD

               WHEN FLG-CURR-BAL-NOT-OK OR FLG-CURR-BAL-BLANK
                   MOVE 'CURR-BAL' TO LK-OUT-ERROR-FIELD

               WHEN FLG-CURR-CYC-CREDIT-NOT-OK OR
                FLG-CURR-CYC-CREDIT-BLANK
                   MOVE 'CYC-CREDIT' TO LK-OUT-ERROR-FIELD

               WHEN FLG-CURR-CYC-DEBIT-NOT-OK OR
                FLG-CURR-CYC-DEBIT-BLANK
                   MOVE 'CYC-DEBIT' TO LK-OUT-ERROR-FIELD

               WHEN FLG-OPEN-YEAR-NOT-OK OR FLG-OPEN-YEAR-BLANK
                   MOVE 'OPEN-YEAR' TO LK-OUT-ERROR-FIELD
               WHEN FLG-OPEN-MONTH-NOT-OK OR FLG-OPEN-MONTH-BLANK
                   MOVE 'OPEN-MONTH' TO LK-OUT-ERROR-FIELD
               WHEN FLG-OPEN-DAY-NOT-OK OR FLG-OPEN-DAY-BLANK
                   MOVE 'OPEN-DAY' TO LK-OUT-ERROR-FIELD

               WHEN FLG-EXPIRY-YEAR-NOT-OK OR FLG-EXPIRY-YEAR-BLANK
                   MOVE 'EXP-YEAR' TO LK-OUT-ERROR-FIELD
               WHEN FLG-EXPIRY-MONTH-NOT-OK OR FLG-EXPIRY-MONTH-BLANK
                   MOVE 'EXP-MONTH' TO LK-OUT-ERROR-FIELD
               WHEN FLG-EXPIRY-DAY-NOT-OK OR FLG-EXPIRY-DAY-BLANK
                   MOVE 'EXP-DAY' TO LK-OUT-ERROR-FIELD

               WHEN FLG-REISSUE-YEAR-NOT-OK OR FLG-REISSUE-YEAR-BLANK
                   MOVE 'REISSUE-YEAR' TO LK-OUT-ERROR-FIELD
               WHEN FLG-REISSUE-MONTH-NOT-OK OR FLG-REISSUE-MONTH-BLANK
                   MOVE 'REISSUE-MONTH' TO LK-OUT-ERROR-FIELD
               WHEN FLG-REISSUE-DAY-NOT-OK OR FLG-REISSUE-DAY-BLANK
                   MOVE 'REISSUE-DAY' TO LK-OUT-ERROR-FIELD

               WHEN FLG-FIRST-NAME-NOT-OK OR FLG-FIRST-NAME-BLANK
                   MOVE 'FIRST-NAME' TO LK-OUT-ERROR-FIELD

               WHEN FLG-MIDDLE-NAME-NOT-OK
                   MOVE 'MIDDLE-NAME' TO LK-OUT-ERROR-FIELD

               WHEN FLG-LAST-NAME-NOT-OK OR FLG-LAST-NAME-BLANK
                   MOVE 'LAST-NAME' TO LK-OUT-ERROR-FIELD

               WHEN FLG-EDIT-US-SSN-PART1-NOT-OK OR
               FLG-EDIT-US-SSN-PART1-BLANK
                   MOVE 'SSN-PART1' TO LK-OUT-ERROR-FIELD
               WHEN FLG-EDIT-US-SSN-PART2-NOT-OK OR
                FLG-EDIT-US-SSN-PART2-BLANK
                   MOVE 'SSN-PART2' TO LK-OUT-ERROR-FIELD
               WHEN FLG-EDIT-US-SSN-PART3-NOT-OK OR
               FLG-EDIT-US-SSN-PART3-BLANK
                   MOVE 'SSN-PART3' TO LK-OUT-ERROR-FIELD

               WHEN FLG-DT-OF-BIRTH-YEAR-NOT-OK OR
               FLG-DT-OF-BIRTH-YEAR-BLANK
                   MOVE 'DOB-YEAR' TO LK-OUT-ERROR-FIELD
               WHEN FLG-DT-OF-BIRTH-MONTH-NOT-OK OR
                FLG-DT-OF-BIRTH-MONTH-BLANK
                   MOVE 'DOB-MONTH' TO LK-OUT-ERROR-FIELD
               WHEN FLG-DT-OF-BIRTH-DAY-NOT-OK OR
               FLG-DT-OF-BIRTH-DAY-BLANK
                   MOVE 'DOB-DAY' TO LK-OUT-ERROR-FIELD

               WHEN FLG-FICO-SCORE-NOT-OK OR FLG-FICO-SCORE-BLANK
                   MOVE 'FICO-SCORE' TO LK-OUT-ERROR-FIELD

               WHEN FLG-ADDRESS-LINE-1-NOT-OK OR
                    FLG-ADDRESS-LINE-1-BLANK
                   MOVE 'ADDR-LINE1' TO LK-OUT-ERROR-FIELD

               WHEN FLG-STATE-NOT-OK OR FLG-STATE-BLANK
                   MOVE 'STATE' TO LK-OUT-ERROR-FIELD

               WHEN FLG-ZIPCODE-NOT-OK OR FLG-ZIPCODE-BLANK
                   MOVE 'ZIP-CODE' TO LK-OUT-ERROR-FIELD

               WHEN FLG-CITY-NOT-OK OR FLG-CITY-BLANK
                   MOVE 'CITY' TO LK-OUT-ERROR-FIELD

               WHEN FLG-COUNTRY-NOT-OK OR FLG-COUNTRY-BLANK
                   MOVE 'COUNTRY' TO LK-OUT-ERROR-FIELD

               WHEN FLG-PHONE-NUM-1A-NOT-OK OR FLG-PHONE-NUM-1A-BLANK
                   MOVE 'PHONE1-AREA' TO LK-OUT-ERROR-FIELD
               WHEN FLG-PHONE-NUM-1B-NOT-OK OR FLG-PHONE-NUM-1B-BLANK
                   MOVE 'PHONE1-PREFIX' TO LK-OUT-ERROR-FIELD
               WHEN FLG-PHONE-NUM-1C-NOT-OK OR FLG-PHONE-NUM-1C-BLANK
                   MOVE 'PHONE1-LINE' TO LK-OUT-ERROR-FIELD

               WHEN FLG-PHONE-NUM-2A-NOT-OK OR FLG-PHONE-NUM-2A-BLANK
                   MOVE 'PHONE2-AREA' TO LK-OUT-ERROR-FIELD
               WHEN FLG-PHONE-NUM-2B-NOT-OK OR FLG-PHONE-NUM-2B-BLANK
                   MOVE 'PHONE2-PREFIX' TO LK-OUT-ERROR-FIELD
               WHEN FLG-PHONE-NUM-2C-NOT-OK OR FLG-PHONE-NUM-2C-BLANK
                   MOVE 'PHONE2-LINE' TO LK-OUT-ERROR-FIELD

               WHEN FLG-EFT-ACCOUNT-ID-NOT-OK OR
                    FLG-EFT-ACCOUNT-ID-BLANK
                   MOVE 'EFT-ACCOUNT' TO LK-OUT-ERROR-FIELD

               WHEN FLG-PRI-CARDHOLDER-NOT-OK OR
                    FLG-PRI-CARDHOLDER-BLANK
                   MOVE 'PRI-HOLDER' TO LK-OUT-ERROR-FIELD

               WHEN OTHER
                   MOVE 'ACCT-ID' TO LK-OUT-ERROR-FIELD
           END-EVALUATE
           .
       1290-SET-ERROR-FIELD-EXIT.
           EXIT
           .
      ******************************************************************
      * FILE OPERATIONS SECTION - IDENTICAL TO COACTUPC
      ******************************************************************
       9000-READ-ACCT.

           INITIALIZE LK-OUTPUT-DATA

           SET  WS-NO-INFO-MESSAGE      TO TRUE

           PERFORM 9200-GETCARDXREF-BYACCT
              THRU 9200-GETCARDXREF-BYACCT-EXIT

           IF NOT RC-SUCCESS
              GO TO 9000-READ-ACCT-EXIT
           END-IF

           PERFORM 9300-GETACCTDATA-BYACCT
              THRU 9300-GETACCTDATA-BYACCT-EXIT

           IF NOT RC-SUCCESS
              GO TO 9000-READ-ACCT-EXIT
           END-IF

           MOVE XREF-CUST-ID TO WS-CARD-RID-CUST-ID

           PERFORM 9400-GETCUSTDATA-BYCUST
              THRU 9400-GETCUSTDATA-BYCUST-EXIT

           IF NOT RC-SUCCESS
              GO TO 9000-READ-ACCT-EXIT
           END-IF

           .

       9000-READ-ACCT-EXIT.
           EXIT
           .
       9200-GETCARDXREF-BYACCT.

      *    Read the Card file. Access via alternate index ACCTID
      *


           EXEC CICS READ
                DATASET   (LIT-CARDXREFNAME-ACCT-PATH)
                RIDFLD    (WS-CARD-RID-ACCT-ID-X)
                KEYLENGTH (LENGTH OF WS-CARD-RID-ACCT-ID-X)
                INTO      (CARD-XREF-RECORD)
                LENGTH    (LENGTH OF CARD-XREF-RECORD)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC

           MOVE WS-RESP-CD TO ERROR-RESP.
           MOVE WS-REAS-CD TO ERROR-RESP2.

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                  CONTINUE
               WHEN DFHRESP(NOTFND)
                  SET RC-NOT-FOUND TO TRUE
                  SET DID-NOT-FIND-ACCT-IN-CARDXREF TO TRUE
                  MOVE WS-RETURN-MSG TO LK-OUT-MESSAGE
               WHEN OTHER
                  SET RC-DATABASE-ERROR TO TRUE
                  STRING
                  'Account:'
                   WS-CARD-RID-ACCT-ID-X
                  ' not found in'
                  ' Cross ref file.  Resp:'
                  ERROR-RESP
                  ' Reas:'
                  ERROR-RESP2
                  DELIMITED BY SIZE
                  INTO LK-OUT-MESSAGE
                  END-STRING
           END-EVALUATE
           .
       9200-GETCARDXREF-BYACCT-EXIT.
           EXIT
           .
       9300-GETACCTDATA-BYACCT.

           EXEC CICS READ
                DATASET   (LIT-ACCTFILENAME)
                RIDFLD    (WS-CARD-RID-ACCT-ID-X)
                KEYLENGTH (LENGTH OF WS-CARD-RID-ACCT-ID-X)
                INTO      (ACCOUNT-RECORD)
                LENGTH    (LENGTH OF ACCOUNT-RECORD)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC

           MOVE WS-RESP-CD TO ERROR-RESP.
           MOVE WS-REAS-CD TO ERROR-RESP2.


           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                  SET FOUND-ACCT-IN-MASTER        TO TRUE
               WHEN DFHRESP(NOTFND)
                  SET RC-NOT-FOUND TO TRUE
                  SET DID-NOT-FIND-ACCT-IN-ACCTDAT TO TRUE
                  MOVE WS-RETURN-MSG TO LK-OUT-MESSAGE
               WHEN OTHER
                  SET RC-DATABASE-ERROR TO TRUE
                  STRING
                  'Account:'
                   WS-CARD-RID-ACCT-ID-X
                  ' not found in'
                  ' Acct Master file.Resp:'
                  ERROR-RESP
                  ' Reas:'
                  ERROR-RESP2
                  DELIMITED BY SIZE
                  INTO LK-OUT-MESSAGE
                  END-STRING
      *
           END-EVALUATE
           .
       9300-GETACCTDATA-BYACCT-EXIT.
           EXIT
           .

       9400-GETCUSTDATA-BYCUST.
           EXEC CICS READ
                DATASET   (LIT-CUSTFILENAME)
                RIDFLD    (WS-CARD-RID-CUST-ID-X)
                KEYLENGTH (LENGTH OF WS-CARD-RID-CUST-ID-X)
                INTO      (CUSTOMER-RECORD)
                LENGTH    (LENGTH OF CUSTOMER-RECORD)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                  SET FOUND-CUST-IN-MASTER        TO TRUE
               WHEN DFHRESP(NOTFND)
                  SET RC-NOT-FOUND TO TRUE
                  SET DID-NOT-FIND-CUST-IN-CUSTDAT TO TRUE
                  MOVE WS-RETURN-MSG TO LK-OUT-MESSAGE
               WHEN OTHER
                  SET RC-DATABASE-ERROR TO TRUE
                  STRING 'Error reading customer file: RESP='
                         ERROR-RESP
                         ' RESP2='
                         ERROR-RESP2
                    DELIMITED BY SIZE
                    INTO LK-OUT-MESSAGE
           END-EVALUATE
           .
       9400-GETCUSTDATA-BYCUST-EXIT.
           EXIT
           .

       9500-STORE-FETCHED-DATA.

      ******************************************************************
      *    Account Master data
      ******************************************************************
           MOVE ACCT-ID                  TO WS-OLD-ACCT-ID
      * Active Status
           MOVE ACCT-ACTIVE-STATUS       TO WS-OLD-ACTIVE-STATUS
      * Current Balance
           MOVE ACCT-CURR-BAL            TO WS-OLD-CURR-BAL-N
      * Credit Limit
           MOVE ACCT-CREDIT-LIMIT        TO WS-OLD-CREDIT-LIMIT-N
      * Cash Limit
           MOVE ACCT-CASH-CREDIT-LIMIT   TO WS-OLD-CASH-CREDIT-LIMIT-N
      * Current Cycle Credit
           MOVE ACCT-CURR-CYC-CREDIT     TO WS-OLD-CURR-CYC-CREDIT-N
      * Current Cycle Debit
           MOVE ACCT-CURR-CYC-DEBIT      TO WS-OLD-CURR-CYC-DEBIT-N
      * Open date
           MOVE ACCT-OPEN-DATE(1:4)      TO WS-OLD-OPEN-YEAR
           MOVE ACCT-OPEN-DATE(6:2)      TO WS-OLD-OPEN-MON
           MOVE ACCT-OPEN-DATE(9:2)      TO WS-OLD-OPEN-DAY
      * Expiry date
           MOVE ACCT-EXPIRAION-DATE(1:4) TO WS-OLD-EXP-YEAR
           MOVE ACCT-EXPIRAION-DATE(6:2) TO WS-OLD-EXP-MON
           MOVE ACCT-EXPIRAION-DATE(9:2) TO WS-OLD-EXP-DAY

      * Reissue date
           MOVE ACCT-REISSUE-DATE(1:4)   TO WS-OLD-REISSUE-YEAR
           MOVE ACCT-REISSUE-DATE(6:2)   TO WS-OLD-REISSUE-MON
           MOVE ACCT-REISSUE-DATE(9:2)   TO WS-OLD-REISSUE-DAY
      * Account Group
           MOVE ACCT-GROUP-ID            TO WS-OLD-GROUP-ID
      ******************************************************************
      *    Customer Master data
      ******************************************************************
      *Customer Id (actually not editable)
           MOVE CUST-ID                  TO WS-OLD-CUST-ID
      *Social Security Number
           MOVE CUST-SSN                 TO WS-OLD-CUST-SSN
      *Date of birth
           MOVE CUST-DOB-YYYY-MM-DD(1:4) TO WS-OLD-CUST-DOB-YEAR
           MOVE CUST-DOB-YYYY-MM-DD(6:2) TO WS-OLD-CUST-DOB-MON
           MOVE CUST-DOB-YYYY-MM-DD(9:2) TO WS-OLD-CUST-DOB-DAY
      *FICO
           MOVE CUST-FICO-CREDIT-SCORE   TO WS-OLD-CUST-FICO-SCORE
      *First Name
           MOVE CUST-FIRST-NAME          TO WS-OLD-CUST-FIRST-NAME
      *Middle Name
           MOVE CUST-MIDDLE-NAME         TO WS-OLD-CUST-MIDDLE-NAME
      *Last Name
           MOVE CUST-LAST-NAME           TO WS-OLD-CUST-LAST-NAME
      *Address
           MOVE CUST-ADDR-LINE-1         TO WS-OLD-CUST-ADDR-LINE-1
           MOVE CUST-ADDR-LINE-2         TO WS-OLD-CUST-ADDR-LINE-2
           MOVE CUST-ADDR-LINE-3         TO WS-OLD-CUST-ADDR-LINE-3
           MOVE CUST-ADDR-STATE-CD       TO WS-OLD-CUST-ADDR-STATE-CD
           MOVE CUST-ADDR-COUNTRY-CD     TO
                                          WS-OLD-CUST-ADDR-COUNTRY-CD
           MOVE CUST-ADDR-ZIP            TO WS-OLD-CUST-ADDR-ZIP
           MOVE CUST-PHONE-NUM-1         TO WS-OLD-CUST-PHONE-NUM-1
           MOVE CUST-PHONE-NUM-2         TO WS-OLD-CUST-PHONE-NUM-2
      *Government Id
           MOVE CUST-GOVT-ISSUED-ID      TO WS-OLD-CUST-GOVT-ISSUED-ID
      *EFT Code
           MOVE CUST-EFT-ACCOUNT-ID      TO WS-OLD-CUST-EFT-ACCOUNT-ID
      *Primary Holder Indicator
           MOVE CUST-PRI-CARD-HOLDER-IND TO WS-OLD-CUST-PRI-HOLDER-IND
           .
       9500-STORE-FETCHED-DATA-EXIT.
           EXIT
           .

       9600-WRITE-PROCESSING.

      *    Read the account file for update
           EXEC CICS READ
                FILE      (LIT-ACCTFILENAME)
                UPDATE
                RIDFLD    (WS-CARD-RID-ACCT-ID-X)
                KEYLENGTH (LENGTH OF WS-CARD-RID-ACCT-ID-X)
                INTO      (ACCOUNT-RECORD)
                LENGTH    (LENGTH OF ACCOUNT-RECORD)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC

           MOVE WS-RESP-CD TO ERROR-RESP.
           MOVE WS-REAS-CD TO ERROR-RESP2.


      *****************************************************************
      *    Could we lock the account record ?
      *****************************************************************
           IF WS-RESP-CD EQUAL TO DFHRESP(NORMAL)
              CONTINUE
           ELSE
              SET RC-UPDATE-ERROR                TO TRUE
              SET COULD-NOT-LOCK-ACCT-FOR-UPDATE TO TRUE
              MOVE WS-RETURN-MSG TO LK-OUT-MESSAGE
              GO TO 9600-WRITE-PROCESSING-EXIT
           END-IF

      *    Read the customer file for update
           EXEC CICS READ
                FILE      (LIT-CUSTFILENAME)
                UPDATE
                RIDFLD    (WS-CARD-RID-CUST-ID-X)
                KEYLENGTH (LENGTH OF WS-CARD-RID-CUST-ID-X)
                INTO      (CUSTOMER-RECORD)
                LENGTH    (LENGTH OF CUSTOMER-RECORD)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC
      *****************************************************************
      *    Could we lock the customer record ?
      *****************************************************************
           IF WS-RESP-CD EQUAL TO DFHRESP(NORMAL)
              CONTINUE
           ELSE
              SET RC-UPDATE-ERROR                  TO TRUE
              SET COULD-NOT-LOCK-CUST-FOR-UPDATE  TO TRUE
              MOVE WS-RETURN-MSG TO LK-OUT-MESSAGE
              GO TO 9600-WRITE-PROCESSING-EXIT
           END-IF

      *****************************************************************
      *    Did someone change the record while we were out ?
      *****************************************************************
           PERFORM 9700-CHECK-CHANGE-IN-REC
              THRU 9700-CHECK-CHANGE-IN-REC-EXIT

           IF DATA-WAS-CHANGED-BEFORE-UPDATE
              SET RC-UPDATE-ERROR TO TRUE
              MOVE WS-RETURN-MSG TO LK-OUT-MESSAGE
              GO TO 9600-WRITE-PROCESSING-EXIT
           END-IF

      *****************************************************************
      * Prepare the update
      *****************************************************************
           INITIALIZE ACCT-UPDATE-RECORD
      ******************************************************************
      *    Account Master data
      ******************************************************************
           MOVE ACCT-ID TO ACCT-UPDATE-ID
      * Active Status
           MOVE LK-IN-ACCT-ACTIVE-STATUS TO ACCT-UPDATE-ACTIVE-STATUS

           MOVE LK-IN-ACCT-CREDIT-LIMIT    TO ACCT-UPDATE-CREDIT-LIMIT
           MOVE LK-IN-ACCT-CASH-LIMIT
               TO ACCT-UPDATE-CASH-CREDIT-LIMIT
           MOVE LK-IN-ACCT-CURR-BAL        TO ACCT-UPDATE-CURR-BAL
           MOVE LK-IN-ACCT-CURR-CYC-CREDIT
            TO ACCT-UPDATE-CURR-CYC-CREDIT
           MOVE LK-IN-ACCT-CURR-CYC-DEBIT  TO ACCT-UPDATE-CURR-CYC-DEBIT

      * Open date
           STRING LK-IN-ACCT-OPEN-YEAR
                  '-'
                  LK-IN-ACCT-OPEN-MON
                  '-'
                  LK-IN-ACCT-OPEN-DAY
           DELIMITED BY SIZE
                                       INTO ACCT-UPDATE-OPEN-DATE
      * Expiry date
           STRING LK-IN-ACCT-EXP-YEAR
                  '-'
                  LK-IN-ACCT-EXP-MON
                  '-'
                  LK-IN-ACCT-EXP-DAY
           DELIMITED BY SIZE
                                       INTO ACCT-UPDATE-EXPIRAION-DATE

      * Reissue date
           STRING LK-IN-ACCT-REISSUE-YEAR
                  '-'
                  LK-IN-ACCT-REISSUE-MON
                  '-'
                  LK-IN-ACCT-REISSUE-DAY
           DELIMITED BY SIZE
                                       INTO ACCT-UPDATE-REISSUE-DATE
      * Account Group
           MOVE LK-IN-ACCT-GROUP-ID        TO ACCT-UPDATE-GROUP-ID

      ******************************************************************
      *    Customer data
      ******************************************************************
           INITIALIZE CUST-UPDATE-RECORD

           MOVE  CUST-ID                   TO CUST-UPDATE-ID
           MOVE  LK-IN-CUST-FIRST-NAME     TO CUST-UPDATE-FIRST-NAME
           MOVE  LK-IN-CUST-MIDDLE-NAME    TO CUST-UPDATE-MIDDLE-NAME
           MOVE  LK-IN-CUST-LAST-NAME      TO CUST-UPDATE-LAST-NAME
           MOVE  LK-IN-CUST-ADDR-LINE-1    TO CUST-UPDATE-ADDR-LINE-1
           MOVE  LK-IN-CUST-ADDR-LINE-2    TO CUST-UPDATE-ADDR-LINE-2
           MOVE  LK-IN-CUST-ADDR-LINE-3    TO CUST-UPDATE-ADDR-LINE-3
           MOVE  LK-IN-CUST-ADDR-STATE-CD  TO CUST-UPDATE-ADDR-STATE-CD
           MOVE  LK-IN-CUST-ADDR-COUNTRY-CD
                                        TO CUST-UPDATE-ADDR-COUNTRY-CD
           MOVE  LK-IN-CUST-ADDR-ZIP       TO CUST-UPDATE-ADDR-ZIP

           STRING '(',
                  LK-IN-CUST-PHONE-1A,
                  ')',
                  LK-IN-CUST-PHONE-1B,
                  '-',
                  LK-IN-CUST-PHONE-1C
           DELIMITED BY SIZE    INTO CUST-UPDATE-PHONE-NUM-1

           STRING '(',
                  LK-IN-CUST-PHONE-2A,
                  ')',
                  LK-IN-CUST-PHONE-2B,
                  '-',
                  LK-IN-CUST-PHONE-2C
           DELIMITED BY SIZE    INTO CUST-UPDATE-PHONE-NUM-2

           STRING LK-IN-CUST-SSN-1
                  LK-IN-CUST-SSN-2
                  LK-IN-CUST-SSN-3
           DELIMITED BY SIZE INTO CUST-UPDATE-SSN
           MOVE  LK-IN-CUST-GOVT-ISSUED-ID TO CUST-UPDATE-GOVT-ISSUED-ID

           STRING LK-IN-CUST-DOB-YEAR
                  '-'
                  LK-IN-CUST-DOB-MON
                  '-'
                  LK-IN-CUST-DOB-DAY
           DELIMITED BY SIZE           INTO CUST-UPDATE-DOB-YYYY-MM-DD

           MOVE LK-IN-CUST-EFT-ACCOUNT-ID
                                         TO CUST-UPDATE-EFT-ACCOUNT-ID
           MOVE LK-IN-CUST-PRI-HOLDER-IND
                                         TO CUST-UPDATE-PRI-CARD-IND
           MOVE LK-IN-CUST-FICO-SCORE TO
                                   CUST-UPDATE-FICO-CREDIT-SCORE

      *****************************************************************
      * Update account *
      *****************************************************************
           EXEC CICS
                REWRITE FILE(LIT-ACCTFILENAME)
                        FROM(ACCT-UPDATE-RECORD)
                        LENGTH(LENGTH OF ACCT-UPDATE-RECORD)
                        RESP      (WS-RESP-CD)
                        RESP2     (WS-REAS-CD)
           END-EXEC.
      *
      *****************************************************************
      * Did account update succeed ?  *
      *****************************************************************
           IF WS-RESP-CD EQUAL TO DFHRESP(NORMAL)
             CONTINUE
           ELSE
             SET RC-UPDATE-ERROR TO TRUE
             SET LOCKED-BUT-UPDATE-FAILED TO TRUE
             MOVE WS-RETURN-MSG TO LK-OUT-MESSAGE
             GO TO 9600-WRITE-PROCESSING-EXIT
           END-IF
      *****************************************************************
      * Update customer *
      *****************************************************************
           EXEC CICS
                        REWRITE FILE(LIT-CUSTFILENAME)
                        FROM(CUST-UPDATE-RECORD)
                        LENGTH(LENGTH OF CUST-UPDATE-RECORD)
                        RESP      (WS-RESP-CD)
                        RESP2     (WS-REAS-CD)
           END-EXEC.
      *****************************************************************
      * Did customer update succeed ? *
      *****************************************************************
           IF WS-RESP-CD EQUAL TO DFHRESP(NORMAL)
             CONTINUE
           ELSE
             SET RC-UPDATE-ERROR TO TRUE
             SET LOCKED-BUT-UPDATE-FAILED TO TRUE
             MOVE WS-RETURN-MSG TO LK-OUT-MESSAGE
             EXEC CICS
                SYNCPOINT ROLLBACK
             END-EXEC
             GO TO 9600-WRITE-PROCESSING-EXIT
           END-IF
           .
       9600-WRITE-PROCESSING-EXIT.
           EXIT
           .

       9700-CHECK-CHANGE-IN-REC.

      ******************************************************************
      *    Account Master data
      ******************************************************************
           IF  ACCT-ACTIVE-STATUS      EQUAL WS-OLD-ACTIVE-STATUS
      * Current Balance
           AND ACCT-CURR-BAL           EQUAL WS-OLD-CURR-BAL-N
      * Credit Limit
           AND ACCT-CREDIT-LIMIT       EQUAL WS-OLD-CREDIT-LIMIT-N
      * Cash Limit
           AND ACCT-CASH-CREDIT-LIMIT EQUAL WS-OLD-CASH-CREDIT-LIMIT-N
      * Current Cycle Credit
           AND ACCT-CURR-CYC-CREDIT    EQUAL WS-OLD-CURR-CYC-CREDIT-N
      * Current Cycle Debit
           AND ACCT-CURR-CYC-DEBIT     EQUAL WS-OLD-CURR-CYC-DEBIT-N
      * Open date
           AND ACCT-OPEN-DATE(1:4)     EQUAL WS-OLD-OPEN-YEAR
           AND ACCT-OPEN-DATE(6:2)     EQUAL WS-OLD-OPEN-MON
           AND ACCT-OPEN-DATE(9:2)     EQUAL WS-OLD-OPEN-DAY
      * Expiry date
           AND ACCT-EXPIRAION-DATE(1:4)EQUAL WS-OLD-EXP-YEAR
           AND ACCT-EXPIRAION-DATE(6:2)EQUAL WS-OLD-EXP-MON
           AND ACCT-EXPIRAION-DATE(9:2)EQUAL WS-OLD-EXP-DAY
      * Reissue date
           AND ACCT-REISSUE-DATE(1:4)  EQUAL WS-OLD-REISSUE-YEAR
           AND ACCT-REISSUE-DATE(6:2)  EQUAL WS-OLD-REISSUE-MON
           AND ACCT-REISSUE-DATE(9:2)  EQUAL WS-OLD-REISSUE-DAY
      * Account Group
           AND FUNCTION LOWER-CASE (ACCT-GROUP-ID)           EQUAL
               FUNCTION LOWER-CASE (WS-OLD-GROUP-ID)
               CONTINUE
           ELSE
              SET DATA-WAS-CHANGED-BEFORE-UPDATE TO TRUE
              GO TO 9700-CHECK-CHANGE-IN-REC-EXIT
           END-IF

      ******************************************************************
      *    Customer  data - Split into 2 IFs for easier reading
      ******************************************************************
           IF  FUNCTION UPPER-CASE (CUST-FIRST-NAME          ) EQUAL
               FUNCTION UPPER-CASE (WS-OLD-CUST-FIRST-NAME )
           AND FUNCTION UPPER-CASE (CUST-MIDDLE-NAME         ) EQUAL
               FUNCTION UPPER-CASE (WS-OLD-CUST-MIDDLE-NAME)
           AND FUNCTION UPPER-CASE (CUST-LAST-NAME           ) EQUAL
               FUNCTION UPPER-CASE (WS-OLD-CUST-LAST-NAME  )
           AND FUNCTION UPPER-CASE (CUST-ADDR-LINE-1         ) EQUAL
               FUNCTION UPPER-CASE (WS-OLD-CUST-ADDR-LINE-1)
           AND FUNCTION UPPER-CASE (CUST-ADDR-LINE-2         ) EQUAL
               FUNCTION UPPER-CASE (WS-OLD-CUST-ADDR-LINE-2)
           AND FUNCTION UPPER-CASE (CUST-ADDR-LINE-3         ) EQUAL
               FUNCTION UPPER-CASE (WS-OLD-CUST-ADDR-LINE-3)
           AND FUNCTION UPPER-CASE (CUST-ADDR-STATE-CD       ) EQUAL
               FUNCTION UPPER-CASE (WS-OLD-CUST-ADDR-STATE-CD)
           AND FUNCTION UPPER-CASE (CUST-ADDR-COUNTRY-CD     ) EQUAL
               FUNCTION UPPER-CASE (WS-OLD-CUST-ADDR-COUNTRY-CD )
           AND CUST-ADDR-ZIP           EQUAL WS-OLD-CUST-ADDR-ZIP
           AND CUST-PHONE-NUM-1        EQUAL WS-OLD-CUST-PHONE-NUM-1
           AND CUST-PHONE-NUM-2        EQUAL WS-OLD-CUST-PHONE-NUM-2
           AND CUST-SSN                EQUAL WS-OLD-CUST-SSN
           AND FUNCTION UPPER-CASE (CUST-GOVT-ISSUED-ID      ) EQUAL
               FUNCTION UPPER-CASE (WS-OLD-CUST-GOVT-ISSUED-ID )
           AND CUST-DOB-YYYY-MM-DD (1:4)                       EQUAL
               WS-OLD-CUST-DOB-YYYY-MM-DD (1:4)
           AND CUST-DOB-YYYY-MM-DD (6:2)                       EQUAL
               WS-OLD-CUST-DOB-YYYY-MM-DD (5:2)
           AND CUST-DOB-YYYY-MM-DD (9:2)                       EQUAL
               WS-OLD-CUST-DOB-YYYY-MM-DD (7:2)

           AND CUST-EFT-ACCOUNT-ID     EQUAL
                                            WS-OLD-CUST-EFT-ACCOUNT-ID
           AND CUST-PRI-CARD-HOLDER-IND
                                       EQUAL
                                            WS-OLD-CUST-PRI-HOLDER-IND
           AND CUST-FICO-CREDIT-SCORE  EQUAL WS-OLD-CUST-FICO-SCORE
               CONTINUE
           ELSE
              SET DATA-WAS-CHANGED-BEFORE-UPDATE TO TRUE
              GO TO 9700-CHECK-CHANGE-IN-REC-EXIT
           END-IF
           .
       9700-CHECK-CHANGE-IN-REC-EXIT.
           EXIT
           .

      ******************************************************************
      * Common Date Routines
      ******************************************************************
       COPY CSUTLDPL
           .

       ABEND-ROUTINE.
           SET RC-DATABASE-ERROR TO TRUE
           MOVE 'UNEXPECTED ABEND OCCURRED.' TO LK-OUT-MESSAGE
           GOBACK
           .
