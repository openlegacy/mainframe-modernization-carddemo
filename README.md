# CardDemo -- Mainframe CardDemo Application
> **Note**: This is a forked version of the original [AWS Mainframe Modernization CardDemo](https://github.com/aws-samples/aws-mainframe-modernization-carddemo/blob/main/README.md). For overall application structure, installation instructions, and general usage details, please refer to the original repository. For technical support or feature requests, open an issue in this repository.
This fork adapts and extends the application to explore multiple architectural patterns:
1. **Screen to RPC (VSAM)** – AL00  
2. **Screen to DB2** – AD00  
3. **Screen to RPC with DB2** – AA00
## Overview of Variants
| Variant | Flow Type              | Initial Transaction | Subsequent Transactions Prefix | Screen Prefix | Logic Prefix | Backend       | Notes                                                                 |
|---------|------------------------|----------------------|--------------------------------|----------------|---------------|----------------|------------------------------------------------------------------------|
| AL00    | Screen → RPC           | `AL00`               | `ALSX`                         | `COxxxxxS`     | `COxxxxxL`    | VSAM (RPC)    | Standard CICS-to-RPC flow using VSAM. RPC logic handles persistence.  |
| AD00    | Screen → DB2           | `AD00`               | `ADSX`                         | `COxxxxxD`     | (None)        | DB2 (Direct)  | CICS programs directly access DB2, no RPC layer.                      |
| AA00    | Screen → RPC (with DB2)| `AA00`               | `AASX`                         | `COxxxxxU`     | `COxxxxxA`    | DB2 (via RPC) | RPC layer integrates with DB2.                           |
- ✅ **Admin Menu**: All options functional  
- ✅ **User Menu**: Options 01–05 functional  
- ⚠️ `COACTUPS` / `COACTUPU` implement View/Update via RPC-to-RPC
## Program & Dataset Structure
| Program Type     | Naming Convention | Folder/Location     | Description                                      |
|------------------|-------------------|----------------------|--------------------------------------------------|
| Screens          | `COxxxxxS`        | `cbl-scr`            | CICS screen logic                                |
| RPC (VSAM)       | `COxxxxxL`        | `cbl-rpc`            | Business logic using VSAM                        |
| DB2 Direct       | `COxxxxxD`        | `cbl-db2`            | Programs that access DB2 directly                |
| Screens (DB2)    | `COxxxxxU`        | `cbl-scr-db2`        | CICS screens that call DB2-enabled RPC programs  |
| RPC (DB2)        | `COxxxxxA`        | `cbl-rpc-db2`        | RPC programs using DB2                           |
| Copybooks        | -                 | `app/cpy`            | Copybooks remain under `app/cpy`             |
## DB2 Schema
* All DB2 tables are defined under the schema: `ALAINL` in the programs, these must be changed to `<your-schema>`
## Source Location
- COBOL source code (screen, RPC, DB2) is organized by function in:
  - `cbl-scr`  
  - `cbl-rpc`  
  - `cbl-db2`  
  - `cbl-scr-db2`  
  - `cbl-rpc-db2`  
- Copybooks remain under: `app/cpy`

## DB2 Environment Setup

### Prerequisites
To run the DB2 variants (AD00 and AA00), you need to establish a proper DB2 environment. The following setup is required:

### DB2 Configuration Steps

1. **Create DB2 Entry**: Create a DB2ENTRY in your CICS region with the same name as your schema (`ALAINL`)

2. **Thread Limit**: Set the thread limit to at least 50 to ensure adequate connection pooling

3. **CICS Resource Definitions**: Use CEDA to define and install:
   - All PROGRAMS for the respective variants
   - All TRANSACTIONS 
   - DB2TRANSACTION pointing to your DB2ENTRY
   - **Important**: Due to the pseudoconversational nature of the application, previous transactions (such as Signon, UserMenu, or AdminMenu) may retain control even after transferring control to subsequent programs. This can persist until a transaction change occurs. To prevent DB2 authorization issues, create DB2TRANSACTION definitions for all transactions in the flow, not just the DB2-accessing programs.
   - Ensure resources are installed under your CICS group

### Database Schema Creation

The following SQL script creates the complete database schema for the CardDemo application:

```sql
-- =================================================================
-- CUSTOMIZED CREDIT CARD DATABASE SCHEMA
-- Replace <your-schema> with your actual schema name
-- Replace <your-DB-name> with your actual database name
-- =================================================================

-- =================================================================
-- 1. CARDDAT - Credit Card Master Table
-- =================================================================
CREATE TABLE <your-schema>.CARDDAT (
    CARD_NUM               CHAR(16) NOT NULL,
    CARD_ACCT_ID           DECIMAL(11,0) NOT NULL,
    CARD_ACTIVE_STATUS     CHAR(1) NOT NULL,
    CARD_CVV_CD            DECIMAL(3,0),
    CARD_EMBOSSED_NAME     CHAR(50),
    CARD_EXPIRY_DATE       DATE,
    CARD_CREAT_DATE        DATE,
    CARD_CREAT_TIME        TIME,
    CARD_CREAT_USER        CHAR(10),
    CARD_UPDATE_DATE       DATE,
    CARD_UPDATE_TIME       TIME,
    CARD_UPDATE_USER       CHAR(10),
    
    CONSTRAINT PK_CARDDAT PRIMARY KEY (CARD_NUM)
) IN DATABASE <your-DB-name>;
GRANT SELECT ON <your-schema>.CARDDAT TO PUBLIC;

-- =================================================================
-- 2. ACCTDAT - Account Master Table (RECLN 300)
-- Based on ACCOUNT-RECORD copybook
-- =================================================================
CREATE TABLE <your-schema>.ACCTDAT (
    ACCT_ID                DECIMAL(11,0) NOT NULL,        -- PIC 9(11)
    ACCT_ACTIVE_STATUS     CHAR(1) NOT NULL,              -- PIC X(01)
    ACCT_CURR_BAL          DECIMAL(12,2),                 -- PIC S9(10)V99
    ACCT_CREDIT_LIMIT      DECIMAL(12,2),                 -- PIC S9(10)V99
    ACCT_CASH_CREDIT_LIMIT DECIMAL(12,2),                 -- PIC S9(10)V99
    ACCT_OPEN_DATE         CHAR(10),                      -- PIC X(10)
    ACCT_EXPIRAION_DATE    CHAR(10),                      -- PIC X(10)
    ACCT_REISSUE_DATE      CHAR(10),                      -- PIC X(10)
    ACCT_CURR_CYC_CREDIT   DECIMAL(12,2),                 -- PIC S9(10)V99
    ACCT_CURR_CYC_DEBIT    DECIMAL(12,2),                 -- PIC S9(10)V99
    ACCT_ADDR_ZIP          CHAR(10),                      -- PIC X(10)
    ACCT_GROUP_ID          CHAR(10),                      -- PIC X(10)
    
    CONSTRAINT PK_ACCTDAT PRIMARY KEY (ACCT_ID)
) IN DATABASE <your-DB-name>;

-- =================================================================
-- 3. CUSTDAT - Customer Master Table (RECLN 500)
-- Based on CUSTOMER-RECORD copybook
-- =================================================================
CREATE TABLE <your-schema>.CUSTDAT (
    CUST_ID                     DECIMAL(9,0) NOT NULL,    -- PIC 9(09)
    CUST_FIRST_NAME             CHAR(25),                 -- PIC X(25)
    CUST_MIDDLE_NAME            CHAR(25),                 -- PIC X(25)
    CUST_LAST_NAME              CHAR(25),                 -- PIC X(25)
    CUST_ADDR_LINE_1            CHAR(50),                 -- PIC X(50)
    CUST_ADDR_LINE_2            CHAR(50),                 -- PIC X(50)
    CUST_ADDR_LINE_3            CHAR(50),                 -- PIC X(50)
    CUST_ADDR_STATE_CD          CHAR(2),                  -- PIC X(02)
    CUST_ADDR_COUNTRY_CD        CHAR(3),                  -- PIC X(03)
    CUST_ADDR_ZIP               CHAR(10),                 -- PIC X(10)
    CUST_PHONE_NUM_1            CHAR(15),                 -- PIC X(15)
    CUST_PHONE_NUM_2            CHAR(15),                 -- PIC X(15)
    CUST_SSN                    DECIMAL(9,0),             -- PIC 9(09)
    CUST_GOVT_ISSUED_ID         CHAR(20),                 -- PIC X(20)
    CUST_DOB_YYYY_MM_DD         CHAR(10),                 -- PIC X(10)
    CUST_EFT_ACCOUNT_ID         CHAR(10),                 -- PIC X(10)
    CUST_PRI_CARD_HOLDER_IND    CHAR(1),                  -- PIC X(01)
    CUST_FICO_CREDIT_SCORE      DECIMAL(3,0),             -- PIC 9(03)
    
    CONSTRAINT PK_CUSTDAT PRIMARY KEY (CUST_ID)
) IN DATABASE <your-DB-name>;

-- =================================================================
-- 4. CXACAIX - Card Cross Reference Table (Account to Customer)
-- =================================================================
CREATE TABLE <your-schema>.CXACAIX (
    XREF_ACCT_ID           DECIMAL(11,0) NOT NULL,
    XREF_CARD_NUM          CHAR(16) NOT NULL,
    XREF_CUST_ID           DECIMAL(9,0) NOT NULL,
    
    CONSTRAINT PK_CXACAIX PRIMARY KEY (XREF_ACCT_ID, XREF_CARD_NUM)
) IN DATABASE <your-DB-name>;

-- =================================================================
-- Create Indexes for Performance
-- =================================================================

-- CARDDAT indexes
CREATE INDEX <your-schema>.IDX_CARDDAT_ACCT 
    ON <your-schema>.CARDDAT (CARD_ACCT_ID);

CREATE INDEX <your-schema>.IDX_CARDDAT_STATUS 
    ON <your-schema>.CARDDAT (CARD_ACTIVE_STATUS);

-- ACCTDAT indexes  
CREATE INDEX <your-schema>.IDX_ACCTDAT_STATUS 
    ON <your-schema>.ACCTDAT (ACCT_ACTIVE_STATUS);

-- CUSTDAT indexes
CREATE INDEX <your-schema>.IDX_CUSTDAT_SSN 
    ON <your-schema>.CUSTDAT (CUST_SSN);

CREATE INDEX <your-schema>.IDX_CUSTDAT_LASTNAME 
    ON <your-schema>.CUSTDAT (CUST_LAST_NAME);

-- CXACAIX indexes
CREATE INDEX <your-schema>.IDX_CXACAIX_CUST 
    ON <your-schema>.CXACAIX (XREF_CUST_ID);

CREATE INDEX <your-schema>.IDX_CXACAIX_CARD 
    ON <your-schema>.CXACAIX (XREF_CARD_NUM);

-- =================================================================
-- Add Table Comments
-- =================================================================
COMMENT ON TABLE <your-schema>.CARDDAT IS 'Credit Card Master Data';
COMMENT ON TABLE <your-schema>.ACCTDAT IS 'Account Master Data - RECLN 300';  
COMMENT ON TABLE <your-schema>.CUSTDAT IS 'Customer Master Data - RECLN 500';
COMMENT ON TABLE <your-schema>.CXACAIX IS 'Card/Account/Customer Cross Reference';

-- =================================================================
-- Grant Privileges
-- =================================================================
GRANT SELECT, INSERT, UPDATE, DELETE ON <your-schema>.CARDDAT TO PUBLIC;
GRANT SELECT, INSERT, UPDATE, DELETE ON <your-schema>.ACCTDAT TO PUBLIC;
GRANT SELECT, INSERT, UPDATE, DELETE ON <your-schema>.CUSTDAT TO PUBLIC;
GRANT SELECT, INSERT, UPDATE, DELETE ON <your-schema>.CXACAIX TO PUBLIC;

-- =================================================================
-- Sample Test Data - Individual INSERT statements
-- Based on actual copybook field definitions
-- =================================================================

-- Customer data - individual inserts
INSERT INTO <your-schema>.CUSTDAT VALUES 
    (000000001, 'JOHN', 'Q', 'DOE', '123 MAIN ST', '', 'ANYTOWN', 'NY', 'USA', '12345', '(555)123-4567', '', 123456789, 'DL123456', '1980-01-15', 'EFT001', 'Y', 750);

INSERT INTO <your-schema>.CUSTDAT VALUES 
    (000000002, 'JANE', '', 'SMITH', '456 OAK AVE', 'APT 2B', 'SOMEWHERE', 'CA', 'USA', '54321', '(555)987-6543', '', 234567890, 'DL234567', '1975-06-20', 'EFT002', 'Y', 680);

INSERT INTO <your-schema>.CUSTDAT VALUES 
    (000000003, 'BOB', 'R', 'JOHNSON', '789 PINE RD', '', 'ELSEWHERE', 'TX', 'USA', '67890', '(555)456-7890', '', 345678901, 'DL345678', '1990-12-05', 'EFT003', 'Y', 720);

-- Account data - individual inserts
INSERT INTO <your-schema>.ACCTDAT VALUES 
    (00000000001, 'Y', 1500.00, 5000.00, 1000.00, '2020-01-15', '2025-01-15', '2023-01-15', 500.00, 300.00, '12345', 'GOLD');

INSERT INTO <your-schema>.ACCTDAT VALUES 
    (00000000002, 'Y', -250.75, 3000.00, 500.00, '2019-06-20', '2024-06-20', '2022-06-20', 200.00, 450.75, '54321', 'SILVER');

INSERT INTO <your-schema>.ACCTDAT VALUES 
    (00000000003, 'N', 0.00, 2000.00, 200.00, '2021-12-05', '2026-12-05', '2024-12-05', 0.00, 0.00, '67890', 'BRONZE');

-- Card data - individual inserts
INSERT INTO <your-schema>.CARDDAT VALUES 
    ('4444000000000001', 00000000001, 'Y', 123, 'JOHN Q DOE', '2025-12-31', CURRENT_DATE, CURRENT_TIME, 'ADMIN', NULL, NULL, NULL);

INSERT INTO <your-schema>.CARDDAT VALUES 
    ('4444000000000002', 00000000002, 'Y', 456, 'JANE SMITH', '2026-06-30', CURRENT_DATE, CURRENT_TIME, 'ADMIN', NULL, NULL, NULL);

INSERT INTO <your-schema>.CARDDAT VALUES 
    ('4444000000000003', 00000000003, 'N', 789, 'BOB R JOHNSON', '2024-03-31', CURRENT_DATE, CURRENT_TIME, 'ADMIN', NULL, NULL, NULL);

-- Cross-reference data - individual inserts
INSERT INTO <your-schema>.CXACAIX VALUES 
    (00000000001, '4444000000000001', 000000001);

INSERT INTO <your-schema>.CXACAIX VALUES 
    (00000000002, '4444000000000002', 000000002);

INSERT INTO <your-schema>.CXACAIX VALUES 
    (00000000003, '4444000000000003', 000000003);

GRANT ALL ON <your-schema>.CARDDAT TO PUBLIC;
GRANT ALL ON <your-schema>.ACCTDAT TO PUBLIC;
GRANT ALL ON <your-schema>.CUSTDAT TO PUBLIC;
GRANT ALL ON <your-schema>.CXACAIX TO PUBLIC;
GRANT EXECUTE ON PLAN <your-schema> TO <your-schema>;
```

---
