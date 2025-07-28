      ******************************************************************
      * Copybook: COACTOLD
      * Purpose:  Commarea structure for COACTVWL (View Layer) calls
      * Used by: COACTUPL to call COACTVWL for read operations
      ******************************************************************
       01 OLD-ACCOUNT-COMMAREA.
          05 OLD-INPUT-PARMS.
             10 OLD-IN-ACCT-ID                PIC X(11).
          05 OLD-OUTPUT-STATUS.
             10 OLD-OUT-RETURN-CODE           PIC 9(02).
                88 OLD-RC-SUCCESS             VALUE 00.
                88 OLD-RC-NOT-FOUND           VALUE 01.
                88 OLD-RC-INPUT-ERROR         VALUE 03.
                88 OLD-RC-DATABASE-ERROR      VALUE 99.
             10 OLD-OUT-MESSAGE               PIC X(80).
          05 OLD-OUTPUT-DATA.
             10 OLD-OUT-ACCT-DATA.
                15 OLD-OUT-ACCT-ID              PIC X(11).
                15 OLD-OUT-ACCT-ACTIVE-STATUS   PIC X(01).
                15 OLD-OUT-ACCT-CREDIT-LIMIT    PIC S9(10)V99.
                15 OLD-OUT-ACCT-CASH-LIMIT      PIC S9(10)V99.
                15 OLD-OUT-ACCT-CURR-BAL        PIC S9(10)V99.
                15 OLD-OUT-ACCT-CURR-CYC-CREDIT PIC S9(10)V99.
                15 OLD-OUT-ACCT-CURR-CYC-DEBIT  PIC S9(10)V99.
                15 OLD-OUT-ACCT-OPEN-DATE       PIC X(10).
                15 OLD-OUT-ACCT-EXPIRATION-DATE PIC X(10).
                15 OLD-OUT-ACCT-REISSUE-DATE    PIC X(10).
                15 OLD-OUT-ACCT-GROUP-ID        PIC X(10).
                15 OLD-OUT-ACCT-CARD-NUM        PIC X(16).
             10 OLD-OUT-CUST-DATA.
                15 OLD-OUT-CUST-ID              PIC X(9).
                15 OLD-OUT-CUST-FIRST-NAME      PIC X(25).
                15 OLD-OUT-CUST-MIDDLE-NAME     PIC X(25).
                15 OLD-OUT-CUST-LAST-NAME       PIC X(25).
                15 OLD-OUT-CUST-SSN             PIC X(9).
                15 OLD-OUT-CUST-DOB             PIC X(10).
                15 OLD-OUT-CUST-ADDR-LINE-1     PIC X(50).
                15 OLD-OUT-CUST-ADDR-LINE-2     PIC X(50).
                15 OLD-OUT-CUST-ADDR-LINE-3     PIC X(50).
                15 OLD-OUT-CUST-ADDR-STATE-CD   PIC X(2).
                15 OLD-OUT-CUST-ADDR-COUNTRY-CD PIC X(3).
                15 OLD-OUT-CUST-ADDR-ZIP        PIC X(10).
                15 OLD-OUT-CUST-PHONE-NUM-1     PIC X(15).
                15 OLD-OUT-CUST-PHONE-NUM-2     PIC X(15).
                15 OLD-OUT-CUST-GOVT-ISSUED-ID  PIC X(20).
                15 OLD-OUT-CUST-EFT-ACCOUNT-ID  PIC X(10).
                15 OLD-OUT-CUST-PRI-HOLDER-IND  PIC X(1).
                15 OLD-OUT-CUST-FICO-SCORE      PIC 9(3).