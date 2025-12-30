*&---------------------------------------------------------------------*
*& Database Table Definition: ZPALTRFHDR
*&---------------------------------------------------------------------*
*& Purpose: Transaction header for HU operations
*& Author: [Author Name]
*& Date: [Creation Date]
*& Table Category: Transparent Table
*& Delivery Class: A (Application Table)
*&---------------------------------------------------------------------*

" BEGIN: Cursor Generated Code

*----------------------------------------------------------------------*
* Table: ZPALTRFHDR
* Description: Solar Activity Transaction Header
*----------------------------------------------------------------------*

* Field Definitions:
*
* MANDT        CLNT    3   Client (Primary Key)
* TRANS_ID     CHAR    20  Transaction ID (Primary Key)
* PALLET_COUNT INT4    10  Number of Pallets
* ACTIVITY     CHAR    2   Activity Code (P1/P2/A2)
* STATUS       CHAR    10  Status (RUNNING/COMPLETED/ERROR)
* CREATED_BY   CHAR    12  Created By User
* CREATED_ON   DATS    8   Creation Date
* CREATED_AT   TIMS    6   Creation Time
* ERROR_MSG    STRG    -   Error Message (String)

*----------------------------------------------------------------------*
* Primary Key: MANDT + TRANS_ID
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Technical Settings
*----------------------------------------------------------------------*
* Data Class: APPL1 (Master data and Transaction data)
* Size Category: 2 (Expected entries: 1,000 - 10,000)
* Buffering: Not allowed (frequent updates)
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* SE11 Table Maintenance
*----------------------------------------------------------------------*
* 1. Transaction SE11
* 2. Enter table name: ZPALTRFHDR
* 3. Click "Create"
* 4. Enter short description: "Solar Activity Transaction Header"
* 5. Delivery Class: A
* 6. Tab page: Delivery and Maintenance → Data Browser/Table View Maint.: Display/Maintenance Allowed
*
* Field Definitions:
* ------------------
* Field Name    Key  Data Element    Data Type  Length  Decimals  Short Description
* -----------   ---  --------------  ---------  ------  --------  ------------------
* MANDT         X    MANDT           CLNT       3       0         Client
* TRANS_ID      X    CHAR20          CHAR       20      0         Transaction ID
* PALLET_COUNT       INT4            INT4       10      0         Number of Pallets
* ACTIVITY           CHAR2           CHAR       2       0         Activity Code
* STATUS             CHAR10          CHAR       10      0         Status
* CREATED_BY         SYUNAME         CHAR       12      0         Created By
* CREATED_ON         SYDATUM         DATS       8       0         Creation Date
* CREATED_AT         SYUZEIT         TIMS       6       0         Creation Time
* ERROR_MSG          STRING          STRG       0       0         Error Message
*
* 7. Click "Technical Settings" button
*    - Data class: APPL1
*    - Size category: 2
*    - Buffering: Not allowed
*
* 8. Save and activate
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Create Table Maintenance Generator (SM30)
*----------------------------------------------------------------------*
* 1. Transaction SE11 → Table ZPALTRFHDR
* 2. Menu: Utilities → Table Maintenance Generator
* 3. Authorization Group: &NC&
* 4. Function group: ZPALTRFHDR
* 5. Maintenance type: One step
* 6. Maintenance screen: 1
* 7. Check: Include change master data flag
* 8. Create
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Sample Data for Testing
*----------------------------------------------------------------------*
* TRANS_ID           PALLET_COUNT  ACTIVITY  STATUS      CREATED_BY
* SOLAR20231215001   10            P1        COMPLETED   TESTUSER
* SOLAR20231215002   25            A2        RUNNING     TESTUSER
* SOLAR20231215003   5             P2        ERROR       TESTUSER
*----------------------------------------------------------------------*

" END: Cursor Generated Code

