# CardDemo -- Mainframe CardDemo Application

**Note:** This is a forked version of the original AWS Mainframe Modernization CardDemo. For overall application structure, installation instructions, and general usage details, please refer to the original repository. For technical support or feature requests, open an issue in this repository.

This fork adapts the application to the **AL00 architectural pattern** with a hybrid DB2/VSAM backend.

## Overview of Architecture

### AL00 Pattern: Screen → RPC (Hybrid DB2/VSAM)

| Variant | Flow Type | Initial Transaction | Subsequent Transactions Prefix | Screen Prefix | Logic Prefix | Backend | Notes |
|---------|-----------|---------------------|--------------------------------|---------------|--------------|---------|-------|
| AL00 | Screen → RPC | `AL00` | `ALSX` | `COxxxxxS` / `COxxxxxU` | `COxxxxxL` / `COxxxxxA` | VSAM + DB2 (RPC) | Standard CICS-to-RPC flow. Most operations use VSAM via RPC. Transaction-related screens (`COTRN00U`, `COTRN01U`, `COTRN02U`) and their RPCs (`COTRN00A`, `COTRN01A`, `COTRN02A`) use DB2 via RPC. |

**Key Points:**
- **VSAM**: Used for Account, Card, and Customer data (via RPC programs `COxxxxxL`)
- **DB2**: Used for Transaction data (via RPC programs `COxxxxxA`)
- Screens ending in `S` call VSAM-based RPCs (`COxxxxxL`)
- Screens ending in `U` call DB2-based RPCs (`COxxxxxA`)

## Current Status

### Admin Menu
✅ All options functional

### User Menu
✅ **Options 01–05**: Functional using VSAM backend

✅ **Transaction Screens (COTRN00U/A, COTRN01U/A, COTRN02U/A)**: Use DB2 backend via RPC as shown in the architecture diagram

⚠️ **RPC-to-RPC Chain**: `COCRDSLL` → `COACTVWL` implements reverse API lookup (Card → Account via RPC-to-RPC)

## Program & Dataset Structure

| Program Type | Naming Convention | Folder/Location | Description |
|--------------|-------------------|-----------------|-------------|
| Screens (VSAM) | `COxxxxxS` | `cbl-src` | CICS screen logic calling VSAM RPCs |
| RPC (VSAM) | `COxxxxxL` | `cbl-rpc` | Business logic using VSAM |
| Screens (DB2) | `COxxxxxU` | `cbl-src-db2` | CICS screens that call DB2-enabled RPC programs |
| RPC (DB2) | `COxxxxxA` | `cbl-rpc-db2` | RPC programs using DB2 |
| Copybooks | - | `app/cpy` | All copybooks |
| Copy BMS | - | `app/cpy-bms` | BMS copybook definitions (mapset copybooks) |
| BMS Mapsets | - | `app/bms` | BMS mapset macros |

## DB2 Schema

All DB2 tables are defined under the schema: **ALAINL**

**Important:** In the programs, these must be changed to `<your-schema>`

## Source Location

COBOL source code is organized by function in:
- `cbl-src` (VSAM screen programs)
- `cbl-rpc` (VSAM RPC programs)
- `cbl-src-db2` (DB2 screen programs)
- `cbl-rpc-db2` (DB2 RPC programs)

Copybooks remain under:
- `app/cpy`
- `app/cpy-bms` (BMS copybook definitions)
- `app/bms` (BMS mapsets)

## DB2 Environment Setup

### Prerequisites

To run the DB2 components (transaction-related screens), you need to establish a proper DB2 environment.

### DB2 Configuration Steps

1. **Create DB2 Entry**: Create a `DB2ENTRY` in your CICS region with the same name as your schema (`ALAINL`)

2. **Thread Limit**: Set the thread limit to at least **50** to ensure adequate connection pooling

3. **CICS Resource Definitions**: Use `CEDA` to define and install:
   - All `PROGRAMS` for AL00
   - Compile the `CSUTLDPL` date utility
   - All `TRANSACTIONS`
   - All `MAPSETs` related to the application
   - `DB2TRANSACTION` pointing to your `DB2ENTRY`

   **Important:** Due to the pseudoconversational nature of the application, previous transactions (such as Signon, UserMenu, or AdminMenu) may retain control even after transferring control to subsequent programs. This can persist until a transaction change occurs. To prevent DB2 authorization issues, create `DB2TRANSACTION` definitions for all transactions in the flow, not just the DB2-accessing programs.

4. Ensure resources are installed under your CICS group

5. After compiling the programs, **REBIND** the associated DB2 packages

6. **NEWCOPY** all modified program objects in CICS
