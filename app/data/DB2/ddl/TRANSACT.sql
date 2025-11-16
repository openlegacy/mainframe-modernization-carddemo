-- =================================================================
-- TRANSACT - Transaction Master Table
-- =================================================================
CREATE TABLE <your-schema>.TRANSACT (
    TRAN_ID                CHAR(16) NOT NULL,
    TRAN_TYPE_CD           CHAR(2) NOT NULL,
    TRAN_CAT_CD            DECIMAL(4,0),
    TRAN_SOURCE            CHAR(10),
    TRAN_DESC              CHAR(50),
    TRAN_AMT               DECIMAL(12,2),
    TRAN_MERCHANT_ID       DECIMAL(9,0),
    TRAN_MERCHANT_NAME     CHAR(50),
    TRAN_MERCHANT_CITY     CHAR(50),
    TRAN_MERCHANT_ZIP      CHAR(10),
    TRAN_CARD_NUM          CHAR(16),
    TRAN_ORIG_TS           CHAR(26),
    TRAN_PROC_TS           CHAR(26),

    CONSTRAINT PK_TRANSACT PRIMARY KEY (TRAN_ID)
) IN DATABASE <your-DB-name>;

GRANT SELECT, INSERT, UPDATE, DELETE ON <your-schema>.TRANSACT TO PUBLIC;