*&---------------------------------------------------------------------*
*& Database Table Definition: ZPALTRFITM
*&---------------------------------------------------------------------*
*& Purpose: Transaction items for HU operations
*& Author: [Author Name]
*& Date: [Creation Date]
*& Table Category: Transparent Table
*& Delivery Class: A (Application Table)
*&---------------------------------------------------------------------*

" BEGIN: Cursor Generated Code

*----------------------------------------------------------------------*
* Table: ZPALTRFITM
* Description: Solar Activity Transaction Items
*----------------------------------------------------------------------*

* Field Definitions:
*
* MANDT        CLNT    3   Client (Primary Key)
* TRANS_ID     CHAR    20  Transaction ID (Primary Key)
* PALLET_NO    CHAR    20  Pallet Number (Primary Key)
* CARTON_NO    CHAR    20  Carton Number (Lower Level HU)
* MATERIAL     MATNR   18  Material Number
* STOCK_CAT    CHAR    1   Stock Category

*----------------------------------------------------------------------*
* Primary Key: MANDT + TRANS_ID + PALLET_NO
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Foreign Keys
*----------------------------------------------------------------------*
* TRANS_ID references ZPALTRFHDR-TRANS_ID
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Technical Settings
*----------------------------------------------------------------------*
* Data Class: APPL1 (Master data and Transaction data)
* Size Category: 3 (Expected entries: 10,000 - 100,000)
* Buffering: Not allowed (frequent updates)
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* SE11 Table Maintenance
*----------------------------------------------------------------------*
* 1. Transaction SE11
* 2. Enter table name: ZPALTRFITM
* 3. Click "Create"
* 4. Enter short description: "Solar Activity Transaction Items"
* 5. Delivery Class: A
* 6. Tab page: Delivery and Maintenance → Data Browser/Table View Maint.: Display/Maintenance Allowed
*
* Field Definitions:
* ------------------
* Field Name    Key  Data Element    Data Type  Length  Decimals  Short Description
* -----------   ---  --------------  ---------  ------  --------  ------------------
* MANDT         X    MANDT           CLNT       3       0         Client
* TRANS_ID      X    CHAR20          CHAR       20      0         Transaction ID
* PALLET_NO     X    CHAR20          CHAR       20      0         Pallet Number
* CARTON_NO          CHAR20          CHAR       20      0         Carton Number
* MATERIAL           MATNR           CHAR       18      0         Material Number
* STOCK_CAT          CHAR1           CHAR       1       0         Stock Category
*
* 7. Create Foreign Key:
*    - Field: TRANS_ID
*    - Check table: ZPALTRFHDR
*    - Foreign key fields: TRANS_ID → TRANS_ID
*    - Cardinality: n:1 (many to one)
*    - Foreign key field type: Non-key fields/candidates
*
* 8. Click "Technical Settings" button
*    - Data class: APPL1
*    - Size category: 3
*    - Buffering: Not allowed
*
* 9. Save and activate
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Create Table Maintenance Generator (SM30)
*----------------------------------------------------------------------*
* 1. Transaction SE11 → Table ZPALTRFITM
* 2. Menu: Utilities → Table Maintenance Generator
* 3. Authorization Group: &NC&
* 4. Function group: ZPALTRFITM
* 5. Maintenance type: One step
* 6. Maintenance screen: 1
* 7. Check: Include change master data flag
* 8. Create
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Sample Data for Testing
*----------------------------------------------------------------------*
* TRANS_ID           PALLET_NO    CARTON_NO    MATERIAL      STOCK_CAT
* SOLAR20231215001   HU001        HU001A       MAT001        F
* SOLAR20231215001   HU001        HU001B       MAT001        F
* SOLAR20231215001   HU002        HU002A       MAT002        F
* SOLAR20231215002   HU003        HU003A       MAT003        S
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Indexes (Optional - Create if performance issues)
*----------------------------------------------------------------------*
* Secondary Index 1:
* - Index name: ZPALTRFITM~001
* - Fields: MANDT, MATERIAL
* - Purpose: Search by material
*
* Secondary Index 2:
* - Index name: ZPALTRFITM~002
* - Fields: MANDT, CARTON_NO
* - Purpose: Search by carton number
*----------------------------------------------------------------------*

" END: Cursor Generated Code

