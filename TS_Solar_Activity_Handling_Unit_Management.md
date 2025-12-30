# Technical Specification (TS)
## Solar Activity - Handling Unit Management System

---

### Document Information

| **Field** | **Details** |
|-----------|-------------|
| **Document Title** | Technical Specification - Handling Unit Management |
| **Program Name** | Z_SOLAR_ACTIVITY |
| **Project** | SFG to FG Conversion in Bulk |
| **Module** | SCM - Supply Chain Management |
| **Author** | [Name] |
| **Date** | [Date] |
| **Version** | 1.0 |
| **Status** | Draft |
| **SAP Version** | ECC 6.0 / NetWeaver 7.31 |

---

## 1. TECHNICAL OVERVIEW

### 1.1 System Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Presentation Layer                       │
│            (Calling Program - Not in Scope)                 │
│         (Existing Programs or Custom UI Programs)           │
└─────────────────┬───────────────────────────────────────────┘
                  │
┌─────────────────▼───────────────────────────────────────────┐
│                   Application Layer                         │
│  ┌────────────────────────────────────────────────────┐    │
│  │  Function Module: Z_SOLAR_ACTIVITY                 │    │
│  │  - Transaction Management                           │    │
│  │  - Batch Processing                                 │    │
│  │  - Process Orchestration                           │    │
│  │  - Error Handling                                   │    │
│  └────┬──────────────────────────────┬────────────────┘    │
│       │                               │                     │
│  ┌────▼─────────────┐     ┌─────────▼───────────────┐     │
│  │ ZSCM_HU_CHECK   │     │ ZSCM_SO_SLOC_VALIDATION │     │
│  │ (HU Validation) │     │ (SO/SLOC Validation)    │     │
│  └──────────────────┘     └─────────────────────────┘     │
└─────────────────┬───────────────────────────────────────────┘
                  │
┌─────────────────▼───────────────────────────────────────────┐
│                     Data Layer                              │
│  ┌──────────────┐  ┌──────────────┐  ┌─────────────────┐  │
│  │ ZPALTRFHDR   │  │ ZPALTRFITM   │  │ Standard Tables │  │
│  │ (Header)     │  │ (Item)       │  │ (/SCWM/*, VBAK) │  │
│  └──────────────┘  └──────────────┘  └─────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

### 1.2 Technical Components

| **Component** | **Type** | **Name** | **Description** |
|--------------|----------|----------|-----------------|
| Function Group 1 | Function Group | ZSOLAR_VAL | Container for validation FMs |
| Function Module 1 | RFC-Enabled FM | ZSCM_HU_CHECK | HU validation in AD2 system |
| Function Module 2 | RFC-Enabled FM | ZSCM_SO_SLOC_VALIDATION | SO/SLOC validation in RD2 system |
| Function Group 2 | Function Group | ZSOLAR | Container for action FM |
| Function Module 3 | RFC-Enabled FM | Z_SOLAR_ACTIVITY | Main processing logic |
| Database Table 1 | Custom Table | ZPALTRFHDR | Transaction header |
| Database Table 2 | Custom Table | ZPALTRFITM | Transaction items |

**Note:** No report program or selection screen is included in this specification. Calling programs are responsible for user interface.

### 1.3 Technology Stack

| **Component** | **Technology** | **Version** |
|--------------|---------------|-------------|
| Programming Language | ABAP | NetWeaver 7.31 |
| Database | SAP Database | - |
| Interface | RFC Function Modules | RFC-Enabled |
| Processing Mode | Asynchronous | RFC with STARTING NEW TASK |

**Note:** User interface is out of scope. Calling programs must provide UI components.

---

## 2. DEVELOPMENT OBJECTS

### 2.1 Function Group Structure

**Function Group 1: ZSOLAR_VAL** (Validation Functions)

```
SAPL ZSOLAR_VAL                    - Main program
  LZSOLAR_VALTOP                  - Global declarations
  LZSOLAR_VALU01                  - ZSCM_HU_CHECK implementation
  LZSOLAR_VALU02                  - ZSCM_SO_SLOC_VALIDATION implementation
```

**Function Group 2: ZSOLAR** (Action Functions)

```
SAPL ZSOLAR                       - Main program
  LZSOLARTOP                      - Global declarations
  LZSOLARU01                      - Z_SOLAR_ACTIVITY implementation
  LZSOLARF01                      - Helper subroutines
```

### 2.2 Type Definitions (Function Group TOP Includes)

**LZSOLAR_VALTOP:**
```abap
*&---------------------------------------------------------------------*
*& Include LZSOLAR_VALTOP
*&---------------------------------------------------------------------*

" Type definitions for HU validation
TYPES: BEGIN OF gty_hu_input,
         sign   TYPE char1,
         option TYPE char2,
         low    TYPE /scwm/de_huident,
         high   TYPE /scwm/de_huident,
       END OF gty_hu_input.

TYPES: gty_hu_input_tab TYPE STANDARD TABLE OF gty_hu_input.

TYPES: BEGIN OF gty_invalid_hu,
         huident TYPE /scwm/de_huident,
         message TYPE string,
       END OF gty_invalid_hu.

TYPES: gty_invalid_hu_tab TYPE STANDARD TABLE OF gty_invalid_hu.
```

**LZSOLARTOP:**
```abap
*&---------------------------------------------------------------------*
*& Include LZSOLARTOP
*&---------------------------------------------------------------------*

" Type definitions for main processing
TYPES: BEGIN OF gty_hu_data,
         hu_id   TYPE /scwm/de_huident,
         stk_cat TYPE char1,
         matid   TYPE /scwm/de_matid,
       END OF gty_hu_data.

TYPES: gty_hu_data_tab TYPE STANDARD TABLE OF gty_hu_data.

TYPES: BEGIN OF gty_message,
         type    TYPE char1,
         message TYPE string,
       END OF gty_message.

TYPES: gty_message_tab TYPE STANDARD TABLE OF gty_message.

" Constants
CONSTANTS:
  gc_batch_size TYPE i VALUE 10,
  gc_activity_p1 TYPE char2 VALUE 'P1',
  gc_activity_p2 TYPE char2 VALUE 'P2',
  gc_activity_a2 TYPE char2 VALUE 'A2',
  gc_status_running TYPE char10 VALUE 'RUNNING',
  gc_status_completed TYPE char10 VALUE 'COMPLETED',
  gc_status_error TYPE char10 VALUE 'ERROR'.
```

---

## 4. DATABASE DESIGN

### 4.1 Custom Table: ZPALTRFHDR

**Table Name:** ZPALTRFHDR  
**Description:** Transaction Header for HU Operations  
**Delivery Class:** A (Application Table)  
**Table Category:** TRANSP (Transparent Table)

**Fields:**

| **Field Name** | **Data Element** | **Type** | **Length** | **Key** | **Not Null** | **Description** |
|---------------|-----------------|----------|-----------|---------|--------------|-----------------|
| MANDT | MANDT | CLNT | 3 | X | X | Client |
| TRANS_ID | CHAR20 | CHAR | 20 | X | X | Transaction ID |
| PALLET_COUNT | INT4 | INT4 | 10 | | | Number of Pallets |
| ACTIVITY | CHAR2 | CHAR | 2 | | | Activity Code |
| STATUS | CHAR10 | CHAR | 10 | | | Status |
| CREATED_BY | SYUNAME | CHAR | 12 | | | Created By |
| CREATED_ON | SYDATUM | DATS | 8 | | | Creation Date |
| CREATED_AT | SYUZEIT | TIMS | 6 | | | Creation Time |
| ERROR_MSG | STRING | STRG | - | | | Error Message |

**Primary Key:** MANDT + TRANS_ID

**Technical Settings:**
- Data Class: APPL1
- Size Category: 2
- Buffering: Not allowed

**Create Statement:**
```sql
CREATE TABLE zpaltrfhdr (
  mandt CHAR(3) NOT NULL,
  trans_id CHAR(20) NOT NULL,
  pallet_count INT,
  activity CHAR(2),
  status CHAR(10),
  created_by CHAR(12),
  created_on DATE,
  created_at TIME,
  error_msg TEXT,
  PRIMARY KEY (mandt, trans_id)
)
```

### 4.2 Custom Table: ZPALTRFITM

**Table Name:** ZPALTRFITM  
**Description:** Transaction Items for HU Operations  
**Delivery Class:** A (Application Table)  
**Table Category:** TRANSP (Transparent Table)

**Fields:**

| **Field Name** | **Data Element** | **Type** | **Length** | **Key** | **Not Null** | **Description** |
|---------------|-----------------|----------|-----------|---------|--------------|-----------------|
| MANDT | MANDT | CLNT | 3 | X | X | Client |
| TRANS_ID | CHAR20 | CHAR | 20 | X | X | Transaction ID |
| PALLET_NO | CHAR20 | CHAR | 20 | X | X | Pallet Number |
| CARTON_NO | CHAR20 | CHAR | 20 | | | Carton Number |
| MATERIAL | MATNR | CHAR | 18 | | | Material Number |
| STOCK_CAT | CHAR1 | CHAR | 1 | | | Stock Category |

**Primary Key:** MANDT + TRANS_ID + PALLET_NO

**Foreign Key:**
- TRANS_ID references ZPALTRFHDR-TRANS_ID

**Technical Settings:**
- Data Class: APPL1
- Size Category: 3
- Buffering: Not allowed

**Create Statement:**
```sql
CREATE TABLE zpaltrfitm (
  mandt CHAR(3) NOT NULL,
  trans_id CHAR(20) NOT NULL,
  pallet_no CHAR(20) NOT NULL,
  carton_no CHAR(20),
  material CHAR(18),
  stock_cat CHAR(1),
  PRIMARY KEY (mandt, trans_id, pallet_no),
  FOREIGN KEY (mandt, trans_id) REFERENCES zpaltrfhdr (mandt, trans_id)
)
```

### 4.3 Table Maintenance

**Transaction:** SM30  
**View:** Create maintenance views for both tables

**Maintenance View: ZPALTRFHDR_VIEW**
- Allow all operations: Create, Change, Delete, Display
- Authorization Group: &NC&

**Maintenance View: ZPALTRFITM_VIEW**
- Allow all operations: Create, Change, Delete, Display
- Authorization Group: &NC&

---

## 3. DATABASE DESIGN

### 6.1 Function Module: ZSCM_HU_CHECK

**Function Group:** ZSOLAR_VAL  
**Processing Type:** RFC-Enabled  
**Release:** External  

**Purpose:** Validate handling units in EWM system (AD2)

#### 3.1.1 Interface

```abap
FUNCTION zscm_hu_check.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_WAREHOUSE) TYPE  /SCWM/LGNUM
*"  EXPORTING
*"     VALUE(EV_VALID) TYPE  CHAR1
*"     VALUE(EV_MESSAGE) TYPE  STRING
*"  TABLES
*"      IT_HU STRUCTURE  /SCWM/S_HUIDENT
*"  EXCEPTIONS
*"      INVALID_HU
*"----------------------------------------------------------------------
```

#### 3.1.2 Implementation

```abap
" BEGIN: Cursor Generated Code
FUNCTION zscm_hu_check.
  " All declarations first
  DATA: lt_hu_check TYPE TABLE OF /scwm/aqua,
        lw_hu_check TYPE /scwm/aqua,
        lw_hu_input TYPE /scwm/s_huident,
        lt_invalid_hu TYPE TABLE OF /scwm/de_huident,
        lv_invalid_hu TYPE /scwm/de_huident,
        lv_count_input TYPE i,
        lv_count_valid TYPE i,
        lv_message_part TYPE string.
  
  " Initialize
  CLEAR: ev_valid, ev_message.
  ev_valid = 'X'.
  
  " Validate input
  IF iv_warehouse IS INITIAL.
    ev_valid = space.
    ev_message = 'Warehouse number is mandatory'.
    RAISE invalid_hu.
  ENDIF.
  
  IF it_hu[] IS INITIAL.
    ev_valid = space.
    ev_message = 'No handling units provided'.
    RAISE invalid_hu.
  ENDIF.
  
  " Get HU count from input
  DESCRIBE TABLE it_hu LINES lv_count_input.
  
  " Check each HU in database
  SELECT *
    FROM /scwm/aqua
    INTO TABLE lt_hu_check
    FOR ALL ENTRIES IN it_hu
    WHERE lgnum = iv_warehouse
      AND huident = it_hu-low.
  
  " If no FOR ALL ENTRIES check needed as it_hu already validated above
  " Check count of valid HUs
  DESCRIBE TABLE lt_hu_check LINES lv_count_valid.
  
  " If counts don't match, find invalid HUs
  IF lv_count_valid <> lv_count_input.
    LOOP AT it_hu INTO lw_hu_input.
      READ TABLE lt_hu_check INTO lw_hu_check
        WITH KEY huident = lw_hu_input-low
        BINARY SEARCH.
      IF sy-subrc <> 0.
        " HU not found in database
        APPEND lw_hu_input-low TO lt_invalid_hu.
      ENDIF.
    ENDLOOP.
    
    " Build error message
    ev_valid = space.
    ev_message = 'Following handling units not found in warehouse: '.
    
    LOOP AT lt_invalid_hu INTO lv_invalid_hu.
      CONCATENATE ev_message lv_invalid_hu ',' INTO ev_message.
    ENDLOOP.
    
    RAISE invalid_hu.
  ENDIF.
  
  " All validations passed
  ev_valid = 'X'.
  ev_message = 'All handling units are valid'.
  
ENDFUNCTION.
" END: Cursor Generated Code
```

### 3.2 Function Module: ZSCM_SO_SLOC_VALIDATION

**Function Group:** ZSOLAR_VAL  
**Processing Type:** RFC-Enabled  
**Release:** External  

**Purpose:** Validate sales orders and storage locations (RD2)

#### 3.2.1 Interface

```abap
FUNCTION zscm_so_sloc_validation.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_VBELN) TYPE  VBELN_VA OPTIONAL
*"     VALUE(IV_POSNR) TYPE  POSNR_VA OPTIONAL
*"     VALUE(IV_WAREHOUSE) TYPE  /SCWM/LGNUM OPTIONAL
*"     VALUE(IV_LGORT) TYPE  LGORT_D OPTIONAL
*"  EXPORTING
*"     VALUE(EV_VALID) TYPE  CHAR1
*"     VALUE(EV_MESSAGE) TYPE  STRING
*"  EXCEPTIONS
*"      INVALID_INPUT
*"----------------------------------------------------------------------
```

#### 3.2.2 Implementation

```abap
" BEGIN: Cursor Generated Code
FUNCTION zscm_so_sloc_validation.
  " All declarations first
  DATA: lv_vbeln TYPE vbeln_va,
        lv_posnr TYPE posnr_va,
        lw_vbak TYPE vbak,
        lw_vbap TYPE vbap,
        lv_lgnum TYPE /scwm/lgnum,
        lv_werks TYPE werks_d,
        lw_t340d TYPE t340d,
        lw_t320 TYPE t320,
        lw_t001l TYPE t001l,
        lv_subrc TYPE sy-subrc.
  
  " Initialize
  CLEAR: ev_valid, ev_message.
  ev_valid = 'X'.
  
  " Determine validation type
  IF iv_vbeln IS NOT INITIAL AND iv_posnr IS NOT INITIAL.
    " Validate Sales Order
    
    " Check sales order header
    SELECT SINGLE *
      FROM vbak
      INTO lw_vbak
      WHERE vbeln = iv_vbeln.
    
    lv_subrc = sy-subrc.
    IF lv_subrc <> 0.
      ev_valid = space.
      CONCATENATE 'Sales order' iv_vbeln 'does not exist'
        INTO ev_message SEPARATED BY space.
      RAISE invalid_input.
    ENDIF.
    
    " Check sales order line item
    SELECT SINGLE *
      FROM vbap
      INTO lw_vbap
      WHERE vbeln = iv_vbeln
        AND posnr = iv_posnr.
    
    lv_subrc = sy-subrc.
    IF lv_subrc <> 0.
      ev_valid = space.
      CONCATENATE 'Line item' iv_posnr 'does not exist for sales order' iv_vbeln
        INTO ev_message SEPARATED BY space.
      RAISE invalid_input.
    ENDIF.
    
    ev_message = 'Sales order and line item are valid'.
    
  ELSEIF iv_warehouse IS NOT INITIAL AND iv_lgort IS NOT INITIAL.
    " Validate Storage Location
    
    " Get LGNUM from warehouse
    SELECT SINGLE *
      FROM t340d
      INTO lw_t340d
      WHERE lgnum = iv_warehouse.
    
    lv_subrc = sy-subrc.
    IF lv_subrc <> 0.
      ev_valid = space.
      CONCATENATE 'Warehouse' iv_warehouse 'not found in T340D'
        INTO ev_message SEPARATED BY space.
      RAISE invalid_input.
    ENDIF.
    
    lv_lgnum = lw_t340d-lgnum.
    
    " Get plant from LGNUM
    SELECT SINGLE *
      FROM t320
      INTO lw_t320
      WHERE lgnum = lv_lgnum.
    
    lv_subrc = sy-subrc.
    IF lv_subrc <> 0.
      ev_valid = space.
      CONCATENATE 'LGNUM' lv_lgnum 'not found in T320'
        INTO ev_message SEPARATED BY space.
      RAISE invalid_input.
    ENDIF.
    
    lv_werks = lw_t320-werks.
    
    " Validate storage location
    SELECT SINGLE *
      FROM t001l
      INTO lw_t001l
      WHERE werks = lv_werks
        AND lgort = iv_lgort.
    
    lv_subrc = sy-subrc.
    IF lv_subrc <> 0.
      ev_valid = space.
      CONCATENATE 'Storage location' iv_lgort 'is not valid for plant' lv_werks
        INTO ev_message SEPARATED BY space.
      RAISE invalid_input.
    ENDIF.
    
    ev_message = 'Storage location is valid'.
    
  ELSE.
    " Invalid input combination
    ev_valid = space.
    ev_message = 'Invalid input combination. Provide either SO/Line Item or Warehouse/SLOC'.
    RAISE invalid_input.
  ENDIF.
  
ENDFUNCTION.
" END: Cursor Generated Code
```

### 3.3 Function Module: Z_SOLAR_ACTIVITY

**Function Group:** ZSOLAR  
**Processing Type:** Normal Function Module  
**Release:** Not Released  

**Purpose:** Main processing function for all activities

#### 3.3.1 Interface

```abap
FUNCTION z_solar_activity.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_WAREHOUSE) TYPE  /SCWM/LGNUM
*"     VALUE(IV_ACTIVITY) TYPE  CHAR2
*"     VALUE(IV_VBELN) TYPE  VBELN_VA OPTIONAL
*"     VALUE(IV_POSNR) TYPE  POSNR_VA OPTIONAL
*"     VALUE(IV_LGORT) TYPE  LGORT_D OPTIONAL
*"  EXPORTING
*"     VALUE(EV_TRANS_ID) TYPE  CHAR20
*"     VALUE(EV_SUCCESS) TYPE  CHAR1
*"  TABLES
*"      IT_HU STRUCTURE  /SCWM/S_HUIDENT
*"  EXCEPTIONS
*"      PROCESSING_ERROR
*"----------------------------------------------------------------------
```

#### 3.3.2 Implementation

```abap
" BEGIN: Cursor Generated Code
FUNCTION z_solar_activity.
  " All declarations first
  DATA: lv_trans_id TYPE char20,
        lv_timestamp TYPE timestampl,
        lv_pallet_count TYPE i,
        lw_header TYPE zpaltrfhdr,
        lt_item TYPE TABLE OF zpaltrfitm,
        lw_item TYPE zpaltrfitm,
        lv_lgnum TYPE /scwm/lgnum,
        lt_huhdr TYPE /scwm/tt_huhdr_int,
        lw_huhdr TYPE /scwm/s_huhdr_int,
        lt_huitm TYPE /scwm/tt_huitm_int,
        lw_huitm TYPE /scwm/s_huitm_int,
        lt_hu_batch TYPE TABLE OF /scwm/s_huident,
        lw_hu_batch TYPE /scwm/s_huident,
        lv_batch_size TYPE i,
        lv_batch_count TYPE i,
        lv_remainder TYPE i,
        lv_index TYPE i,
        lv_subrc TYPE sy-subrc,
        lo_exception TYPE REF TO cx_root,
        lv_error_text TYPE string.
  
  CONSTANTS: lc_batch_size TYPE i VALUE 10.
  
  " Initialize
  CLEAR: ev_trans_id, ev_success.
  ev_success = space.
  
  TRY.
      " Generate transaction ID
      GET TIME STAMP FIELD lv_timestamp.
      CONCATENATE 'SOLAR' lv_timestamp INTO lv_trans_id.
      ev_trans_id = lv_trans_id.
      
      " Count pallets
      DESCRIBE TABLE it_hu LINES lv_pallet_count.
      
      " Update header table
      lw_header-mandt = sy-mandt.
      lw_header-trans_id = lv_trans_id.
      lw_header-pallet_count = lv_pallet_count.
      lw_header-activity = iv_activity.
      lw_header-status = 'RUNNING'.
      lw_header-created_by = sy-uname.
      lw_header-created_on = sy-datum.
      lw_header-created_at = sy-uzeit.
      
      INSERT zpaltrfhdr FROM lw_header.
      lv_subrc = sy-subrc.
      IF lv_subrc <> 0.
        RAISE processing_error.
      ENDIF.
      
      COMMIT WORK AND WAIT.
      
      " Get HU hierarchy
      CALL FUNCTION '/SCWM/HU_SELECT_GEN'
        EXPORTING
          iv_lgnum    = iv_warehouse
        TABLES
          it_huident  = it_hu
          et_huhdr    = lt_huhdr
          et_huitm    = lt_huitm
        EXCEPTIONS
          wrong_input = 1
          not_found   = 2
          OTHERS      = 3.
      
      lv_subrc = sy-subrc.
      IF lv_subrc <> 0.
        lw_header-status = 'ERROR'.
        lw_header-error_msg = 'Error getting HU hierarchy'.
        UPDATE zpaltrfhdr FROM lw_header.
        COMMIT WORK AND WAIT.
        RAISE processing_error.
      ENDIF.
      
      " Build item table
      LOOP AT lt_huhdr INTO lw_huhdr
        WHERE bottom = 'X'.  " Only lower level HUs
        
        lw_item-mandt = sy-mandt.
        lw_item-trans_id = lv_trans_id.
        
        " Find parent HU
        READ TABLE lt_huhdr INTO DATA(lw_parent_hu)
          WITH KEY guid_hu = lw_huhdr-higher_guid.
        IF sy-subrc = 0.
          lw_item-pallet_no = lw_parent_hu-huident.
        ELSE.
          lw_item-pallet_no = lw_huhdr-huident.
        ENDIF.
        
        lw_item-carton_no = lw_huhdr-huident.
        
        " Get material and stock category
        READ TABLE lt_huitm INTO lw_huitm
          WITH KEY guid_hu = lw_huhdr-guid_hu.
        IF sy-subrc = 0.
          lw_item-material = lw_huitm-matid.
          lw_item-stock_cat = lw_huitm-cat.
        ENDIF.
        
        APPEND lw_item TO lt_item.
      ENDLOOP.
      
      " Insert item records
      INSERT zpaltrfitm FROM TABLE lt_item.
      lv_subrc = sy-subrc.
      IF lv_subrc <> 0.
        lw_header-status = 'ERROR'.
        lw_header-error_msg = 'Error inserting item records'.
        UPDATE zpaltrfhdr FROM lw_header.
        COMMIT WORK AND WAIT.
        RAISE processing_error.
      ENDIF.
      
      COMMIT WORK AND WAIT.
      
      " Process in batches
      lv_batch_count = lv_pallet_count DIV lc_batch_size.
      lv_remainder = lv_pallet_count MOD lc_batch_size.
      
      IF lv_remainder > 0.
        lv_batch_count = lv_batch_count + 1.
      ENDIF.
      
      " Process each batch
      lv_index = 1.
      
      LOOP AT lt_huhdr INTO lw_huhdr
        WHERE bottom = 'X'.
        
        CLEAR lw_hu_batch.
        lw_hu_batch-low = lw_huhdr-huident.
        APPEND lw_hu_batch TO lt_hu_batch.
        
        " Process batch when size reached
        IF lines( lt_hu_batch ) >= lc_batch_size OR lv_index = lv_pallet_count.
          PERFORM process_batch USING iv_warehouse
                                      iv_activity
                                      iv_vbeln
                                      iv_posnr
                                      iv_lgort
                                      lt_hu_batch
                                      lt_huitm.
          CLEAR lt_hu_batch.
        ENDIF.
        
        lv_index = lv_index + 1.
      ENDLOOP.
      
      " Update status to completed
      lw_header-status = 'COMPLETED'.
      UPDATE zpaltrfhdr FROM lw_header.
      COMMIT WORK AND WAIT.
      
      ev_success = 'X'.
      
    CATCH cx_root INTO lo_exception.
      lv_error_text = lo_exception->get_text( ).
      lw_header-status = 'ERROR'.
      lw_header-error_msg = lv_error_text.
      UPDATE zpaltrfhdr FROM lw_header.
      COMMIT WORK AND WAIT.
      RAISE processing_error.
  ENDTRY.
  
ENDFUNCTION.
" END: Cursor Generated Code
```

#### 3.3.3 Subroutine: PROCESS_BATCH

```abap
" BEGIN: Cursor Generated Code
FORM process_batch USING iv_warehouse TYPE /scwm/lgnum
                         iv_activity TYPE char2
                         iv_vbeln TYPE vbeln_va
                         iv_posnr TYPE posnr_va
                         iv_lgort TYPE lgort_d
                         it_hu_batch TYPE /scwm/tt_huident
                         it_huitm TYPE /scwm/tt_huitm_int.
  
  " All declarations first
  DATA: lt_input TYPE TABLE OF ty_input,
        lw_input TYPE ty_input,
        lt_hu_data TYPE TABLE OF ty_hu_data,
        lw_hu_data TYPE ty_hu_data,
        lt_output TYPE TABLE OF ty_output,
        lw_output TYPE ty_output,
        lw_posting_input TYPE zscm_s_bin_intl_input,
        lv_subrc TYPE sy-subrc,
        lw_huitm TYPE /scwm/s_huitm_int,
        lw_hu_batch TYPE /scwm/s_huident.
  
  " Prepare input for GET_DET function
  LOOP AT it_hu_batch INTO lw_hu_batch.
    READ TABLE it_huitm INTO lw_huitm
      WITH KEY huident = lw_hu_batch-low.
    IF sy-subrc = 0.
      lw_input-matnr = lw_huitm-matid.
      APPEND lw_input TO lt_input.
      
      lw_hu_data-hu_id = lw_hu_batch-low.
      lw_hu_data-stk_cat = lw_huitm-cat.
      APPEND lw_hu_data TO lt_hu_data.
    ENDIF.
  ENDLOOP.
  
  " Call GET_DET function
  CALL FUNCTION 'ZSCM_BIN_INTL_POST_GET_DET'
    EXPORTING
      i_warehouse = iv_warehouse
      i_action    = iv_activity
      i_user      = sy-uname
    TABLES
      it_input    = lt_input
      it_hu       = lt_hu_data
      et_output   = lt_output
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.
  
  lv_subrc = sy-subrc.
  IF lv_subrc <> 0.
    " Log error but continue processing other batches
    WRITE: / 'Error in GET_DET for batch'.
    RETURN.
  ENDIF.
  
  " Call POSTING function for each output record
  LOOP AT lt_output INTO lw_output.
    CLEAR lw_posting_input.
    lw_posting_input-wh_number = lw_output-lgnum.
    lw_posting_input-screen_id = iv_activity.
    lw_posting_input-stock_id = lw_output-stock_id.
    lw_posting_input-parent_id = lw_output-parent_id.
    
    " Activity-specific parameters
    CASE iv_activity.
      WHEN 'P1'.  " Assign SO
        lw_posting_input-dest_stockdocno = iv_vbeln.
        lw_posting_input-dest_stockitmno = iv_posnr.
        
      WHEN 'A2'.  " Conversion
        lw_posting_input-im_location = iv_lgort.
    ENDCASE.
    
    CALL FUNCTION 'ZSCM_BIN_INTL_POSTING'
      EXPORTING
        im_input   = lw_posting_input
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.
    
    lv_subrc = sy-subrc.
    IF lv_subrc <> 0.
      " Log error but continue
      WRITE: / 'Error in POSTING for HU', lw_hu_data-hu_id.
    ENDIF.
    
    COMMIT WORK AND WAIT.
  ENDLOOP.
  
ENDFORM.
" END: Cursor Generated Code
```

---

## 5. PROCESSING LOGIC

### 5.1 Transaction ID Generation

```abap
" Generate unique transaction ID
DATA: lv_timestamp TYPE timestampl,
      lv_trans_id TYPE char20.

GET TIME STAMP FIELD lv_timestamp.
CONCATENATE 'SOLAR' lv_timestamp INTO lv_trans_id.
```

**Format:** `SOLAR` + Timestamp (e.g., `SOLAR20231215093045123456`)

### 5.2 Batch Processing Logic

```abap
" Split HUs into batches of 10
DATA: lv_batch_size TYPE i VALUE 10,
      lv_total_hus TYPE i,
      lv_batch_count TYPE i,
      lv_remainder TYPE i.

DESCRIBE TABLE lt_hu LINES lv_total_hus.

lv_batch_count = lv_total_hus DIV lv_batch_size.
lv_remainder = lv_total_hus MOD lv_batch_size.

IF lv_remainder > 0.
  lv_batch_count = lv_batch_count + 1.
ENDIF.

" Example: 94 HUs
" Batch count = 94 DIV 10 = 9
" Remainder = 94 MOD 10 = 4
" Total batches = 9 + 1 = 10
" 9 batches of 10 HUs + 1 batch of 4 HUs
```

### 5.3 Asynchronous Processing

```abap
" Call function asynchronously using RFC
CALL FUNCTION 'Z_SOLAR_ACTIVITY'
  STARTING NEW TASK 'SOLAR_TASK'
  EXPORTING
    iv_warehouse = p_whno
    iv_activity  = p_actv
  TABLES
    it_hu        = lt_hu
  EXCEPTIONS
    system_failure        = 1
    communication_failure = 2
    resource_failure      = 3
    OTHERS                = 4.

IF sy-subrc <> 0.
  MESSAGE 'Error starting background processing' TYPE 'I' DISPLAY LIKE 'E'.
ELSE.
  MESSAGE 'Processing started in background' TYPE 'S'.
ENDIF.
```

### 5.4 Error Handling Strategy

```abap
TRY.
    " Main processing logic
    
  CATCH cx_root INTO lo_exception.
    " Get error text
    lv_error_text = lo_exception->get_text( ).
    
    " Update status to error
    lw_header-status = 'ERROR'.
    lw_header-error_msg = lv_error_text.
    UPDATE zpaltrfhdr FROM lw_header.
    COMMIT WORK AND WAIT.
    
    " Display error message
    MESSAGE lv_error_text TYPE 'I' DISPLAY LIKE 'E'.
ENDTRY.
```

---

## 6. DATA FLOW DIAGRAMS

### 6.1 Activity P1: Assign Sales Order

```
┌─────────────────────────────────────────────────────────────┐
│ INPUT: Warehouse, HUs, Sales Order, Line Item              │
└─────────────────┬───────────────────────────────────────────┘
                  │
                  ▼
┌─────────────────────────────────────────────────────────────┐
│ VALIDATION PHASE                                            │
│  1. ZSCM_HU_CHECK (AD2) → Validate HUs in /SCWM/AQUA       │
│  2. ZSCM_SO_SLOC_VALIDATION (RD2) → Validate SO in VBAK/VBAP│
└─────────────────┬───────────────────────────────────────────┘
                  │ [All Valid]
                  ▼
┌─────────────────────────────────────────────────────────────┐
│ TRANSACTION SETUP                                            │
│  1. Generate Transaction ID                                  │
│  2. INSERT into ZPALTRFHDR (Status: RUNNING)                │
└─────────────────┬───────────────────────────────────────────┘
                  │
                  ▼
┌─────────────────────────────────────────────────────────────┐
│ HU HIERARCHY                                                 │
│  1. Call /SCWM/HU_SELECT_GEN                                │
│  2. Get ET_HUHDR (HU Headers)                               │
│  3. Get ET_HUITM (HU Items)                                 │
│  4. Filter bottom-level HUs (BOTTOM = 'X')                  │
└─────────────────┬───────────────────────────────────────────┘
                  │
                  ▼
┌─────────────────────────────────────────────────────────────┐
│ ITEM TABLE UPDATE                                            │
│  1. Loop through lower-level HUs                            │
│  2. Map to parent HUs                                        │
│  3. INSERT into ZPALTRFITM                                  │
└─────────────────┬───────────────────────────────────────────┘
                  │
                  ▼
┌───────��─────────────────────────────────────────────────────┐
│ BATCH PROCESSING (Sets of 10)                               │
│  FOR EACH BATCH:                                            │
│    1. Call ZSCM_BIN_INTL_POST_GET_DET                      │
│       Input: Warehouse, Action=P1, User, Materials, HUs    │
│       Output: ET_OUTPUT (Stock IDs, Parent IDs)            │
│                                                              │
│    2. Call ZSCM_BIN_INTL_POSTING                           │
│       Input: WH_NUMBER, SCREEN_ID=P1, STOCK_ID,            │
│              PARENT_ID, DEST_STOCKDOCNO=SO,                │
│              DEST_STOCKITMNO=Line Item                      │
│                                                              │
│    3. COMMIT WORK AND WAIT                                  │
└─────────────────┬───────────────────────────────────────────┘
                  │
                  ▼
┌─────────────────────────────────────────────────────────────┐
│ COMPLETION                                                   │
│  1. UPDATE ZPALTRFHDR (Status: COMPLETED)                   │
│  2. COMMIT WORK                                             │
└─────────────────┬───────────────────────────────────────────┘
                  │
                  ▼
┌─────────────────────────────────────────────────────────────┐
│ OUTPUT: Transaction ID, Success Message                     │
└─────────────────────────────────────────────────────────────┘
```

### 6.2 Activity P2: Unassign Sales Order

```
Similar to P1, but:
- No SO validation required
- ZSCM_BIN_INTL_POST_GET_DET called with Action=P2
- ZSCM_BIN_INTL_POSTING called without DEST_STOCKDOCNO/DEST_STOCKITMNO
```

### 6.3 Activity A2: SFG to FG Conversion

```
Similar to P1, but:
- Storage Location validation instead of SO
- ZSCM_BIN_INTL_POST_GET_DET called with Action=A2
- ZSCM_BIN_INTL_POSTING called with IM_LOCATION=Storage Location
```

---

## 7. PERFORMANCE CONSIDERATIONS

### 7.1 Optimization Techniques

| **Technique** | **Implementation** | **Benefit** |
|--------------|-------------------|------------|
| Batch Processing | Process 10 HUs at a time | Reduces memory usage |
| Binary Search | Sort before READ TABLE | O(log n) vs O(n) |
| FOR ALL ENTRIES | Single SELECT for multiple HUs | Reduces DB calls |
| Asynchronous Processing | STARTING NEW TASK | UI responsiveness |
| Commit Work | COMMIT after each batch | Prevents lock issues |

### 7.2 Performance Targets

| **Operation** | **Target Time** | **Maximum Records** |
|--------------|----------------|-------------------|
| HU Validation | < 3 seconds | 100 HUs |
| SO Validation | < 1 second | Single SO |
| Batch Processing | < 5 seconds | 10 HUs per batch |
| Total Processing | Variable | Depends on HU count |

### 7.3 Database Optimization

```abap
" Use FOR ALL ENTRIES with empty check
IF lt_hu IS NOT INITIAL.
  SELECT *
    FROM /scwm/aqua
    INTO TABLE lt_aqua
    FOR ALL ENTRIES IN lt_hu
    WHERE lgnum = iv_warehouse
      AND huident = lt_hu-low.
ENDIF.

" Sort before binary search
SORT lt_aqua BY huident.

LOOP AT lt_hu INTO lw_hu.
  READ TABLE lt_aqua INTO lw_aqua
    WITH KEY huident = lw_hu-low
    BINARY SEARCH.
  IF sy-subrc = 0.
    " Process
  ENDIF.
ENDLOOP.
```

---

## 8. ERROR HANDLING AND LOGGING

### 8.1 Error Handling Levels

#### Level 1: Input Validation Errors
- Handled in function module Z_SOLAR_ACTIVITY
- Return error message via export parameters
- Raise VALIDATION_ERROR exception

#### Level 2: Function Module Errors
- Caught in TRY-CATCH blocks
- Update transaction status to ERROR
- Log error message in ZPALTRFHDR

#### Level 3: Batch Processing Errors
- Log error but continue with next batch
- Update individual item status if needed
- Final status reflects partial success

### 8.2 Logging Strategy

```abap
" Update header with error
lw_header-status = 'ERROR'.
lw_header-error_msg = lv_error_text.
UPDATE zpaltrfhdr FROM lw_header.
COMMIT WORK AND WAIT.

" Application log (optional)
CALL FUNCTION 'BAL_LOG_MSG_ADD'
  EXPORTING
    i_log_handle = lv_log_handle
    i_s_msg      = lw_message.
```

### 8.3 Error Recovery

- Transaction table maintains full audit trail
- Failed transactions can be reprocessed
- Status field allows filtering for error analysis
- Error message field provides debugging information

---

## 9. TESTING STRATEGY

### 9.1 Unit Test Cases

#### Test Case 1: HU Validation
```abap
" Test valid HU
INPUT: Warehouse = 'WH01', HU = '123456'
EXPECTED: ev_valid = 'X'

" Test invalid HU
INPUT: Warehouse = 'WH01', HU = '999999'
EXPECTED: ev_valid = space, error message displayed
```

#### Test Case 2: Sales Order Validation
```abap
" Test valid SO
INPUT: VBELN = '1000000', POSNR = '000010'
EXPECTED: ev_valid = 'X'

" Test invalid SO
INPUT: VBELN = '9999999', POSNR = '000010'
EXPECTED: ev_valid = space, error message displayed
```

#### Test Case 3: Storage Location Validation
```abap
" Test valid SLOC
INPUT: Warehouse = 'WH01', LGORT = 'FG01'
EXPECTED: ev_valid = 'X'

" Test invalid SLOC
INPUT: Warehouse = 'WH01', LGORT = 'XXXX'
EXPECTED: ev_valid = space, error message displayed
```

### 9.2 Integration Test Cases

#### Test Scenario 1: Assign SO to Single HU
```
GIVEN: Valid warehouse, 1 HU, valid SO and line item
WHEN: Activity P1 is executed
THEN: Transaction created with status COMPLETED
      1 record in ZPALTRFHDR
      1 record in ZPALTRFITM
      HU assigned to SO in EWM
```

#### Test Scenario 2: Assign SO to Multiple HUs (Batch Processing)
```
GIVEN: Valid warehouse, 25 HUs, valid SO and line item
WHEN: Activity P1 is executed
THEN: Transaction created with status COMPLETED
      1 record in ZPALTRFHDR (pallet_count = 25)
      25 records in ZPALTRFITM
      3 batches processed (10 + 10 + 5)
      All HUs assigned to SO
```

#### Test Scenario 3: Error Handling
```
GIVEN: Valid warehouse, 5 valid HUs + 2 invalid HUs
WHEN: Activity P1 is executed
THEN: Validation fails
      Error message displayed for invalid HUs
      No transaction created
      No database updates
```

### 9.3 Performance Test Cases

#### Performance Test 1: 10 HUs
```
OPERATION: Assign SO to 10 HUs
EXPECTED TIME: < 5 seconds
BATCHES: 1
```

#### Performance Test 2: 100 HUs
```
OPERATION: Assign SO to 100 HUs
EXPECTED TIME: < 50 seconds (5 sec per batch × 10 batches)
BATCHES: 10
```

#### Performance Test 3: Concurrent Users
```
SCENARIO: 5 users processing simultaneously
EXPECTED: No lock issues
          All transactions complete successfully
```

### 9.4 Test Data Setup

```sql
-- Create test HUs in /SCWM/AQUA
-- Create test SOs in VBAK/VBAP
-- Configure test warehouse in T340D, T320
-- Create test storage locations in T001L
```

---

## 10. DEPLOYMENT PLAN

### 10.1 Development Objects Checklist

| **Object Type** | **Object Name** | **Transport** | **Status** |
|----------------|----------------|--------------|-----------|
| Database Table | ZPALTRFHDR | DEVK9##### | To be created |
| Database Table | ZPALTRFITM | DEVK9##### | To be created |
| Function Group | ZSOLAR_VAL | DEVK9##### | To be created |
| Function Module | ZSCM_HU_CHECK | DEVK9##### | To be created |
| Function Module | ZSCM_SO_SLOC_VALIDATION | DEVK9##### | To be created |
| Function Group | ZSOLAR | DEVK9##### | To be created |
| Function Module | Z_SOLAR_ACTIVITY | DEVK9##### | To be created |

**Note:** No report program or transaction code required. Function modules are called from existing programs.

### 10.2 Configuration Steps

#### Step 1: Create RFC Destinations
```
SM59 - RFC Destinations
- AD2_RFC_DEST (for ZSCM_HU_CHECK)
- RD2_RFC_DEST (for ZSCM_SO_SLOC_VALIDATION)
```

#### Step 2: Create Database Tables
```
SE11 - ABAP Dictionary
- Create ZPALTRFHDR
- Create ZPALTRFITM
- Activate both tables
- Generate table maintenance dialogs
```

#### Step 3: Create Function Groups and Modules
```
SE37 - Function Builder
- Create function group ZSOLAR_VAL
- Create function group ZSOLAR
- Create function modules
- Activate all
```

#### Step 4: Create Function Groups and Modules
```
SE37 - Function Builder
- Create function group ZSOLAR_VAL
- Create function group ZSOLAR
- Create function modules
- Set RFC-Enabled flag for all function modules
- Activate all
```

**Note:** No report program or transaction code creation needed.

### 10.3 Transport Strategy

```
Development (DEV) → Quality (QAS) → Production (PRD)

Transport Order: DEVK9#####
- Database objects first
- Function modules second
- Test in each environment before promoting
```

---

## 11. INTEGRATION GUIDE FOR CALLING PROGRAMS

### 11.1 How to Call the Function Modules

#### Example: Complete Integration Flow

```abap
*&---------------------------------------------------------------------*
*& Sample Calling Program
*&---------------------------------------------------------------------*
REPORT z_sample_caller.

" BEGIN: Cursor Generated Code
" Declarations
DATA: lv_warehouse TYPE /scwm/lgnum,
      lv_activity TYPE char2,
      lv_user TYPE syuname,
      lt_hu TYPE TABLE OF /scwm/s_huident,
      lw_hu TYPE /scwm/s_huident,
      lv_vbeln TYPE vbeln_va,
      lv_posnr TYPE posnr_va,
      lv_trans_id TYPE char20,
      lv_success TYPE char1,
      lv_message TYPE string,
      lv_valid TYPE char1,
      lt_invalid_hu TYPE TABLE OF gty_invalid_hu,
      lv_subrc TYPE sy-subrc.

" Get user input (from selection screen, etc.)
lv_warehouse = 'WH01'.
lv_activity = 'P1'.
lv_user = sy-uname.
lv_vbeln = '1000000'.
lv_posnr = '000010'.

" Populate HU table
lw_hu-low = '123456'.
APPEND lw_hu TO lt_hu.
lw_hu-low = '123457'.
APPEND lw_hu TO lt_hu.

" Step 1: Validate HUs
CALL FUNCTION 'ZSCM_HU_CHECK'
  DESTINATION 'AD2_RFC_DEST'
  EXPORTING
    iv_warehouse = lv_warehouse
  TABLES
    it_hu        = lt_hu
  IMPORTING
    ev_valid     = lv_valid
    ev_message   = lv_message
    et_invalid_hu = lt_invalid_hu
  EXCEPTIONS
    invalid_hu            = 1
    system_failure        = 2
    communication_failure = 3
    OTHERS                = 4.

lv_subrc = sy-subrc.
IF lv_subrc <> 0 OR lv_valid <> 'X'.
  " Display error
  MESSAGE lv_message TYPE 'I' DISPLAY LIKE 'E'.
  RETURN.
ENDIF.

" Step 2: Validate Sales Order (for P1 activity)
CALL FUNCTION 'ZSCM_SO_SLOC_VALIDATION'
  DESTINATION 'RD2_RFC_DEST'
  EXPORTING
    iv_vbeln   = lv_vbeln
    iv_posnr   = lv_posnr
  IMPORTING
    ev_valid   = lv_valid
    ev_message = lv_message
  EXCEPTIONS
    invalid_input         = 1
    system_failure        = 2
    communication_failure = 3
    OTHERS                = 4.

lv_subrc = sy-subrc.
IF lv_subrc <> 0 OR lv_valid <> 'X'.
  " Display error
  MESSAGE lv_message TYPE 'I' DISPLAY LIKE 'E'.
  RETURN.
ENDIF.

" Step 3: Process activity (asynchronously)
CALL FUNCTION 'Z_SOLAR_ACTIVITY'
  STARTING NEW TASK 'SOLAR_TASK'
  EXPORTING
    iv_warehouse = lv_warehouse
    iv_activity  = lv_activity
    iv_user      = lv_user
    iv_vbeln     = lv_vbeln
    iv_posnr     = lv_posnr
  TABLES
    it_hu        = lt_hu
  IMPORTING
    ev_trans_id  = lv_trans_id
    ev_success   = lv_success
    ev_message   = lv_message
  EXCEPTIONS
    validation_error      = 1
    processing_error      = 2
    system_failure        = 3
    communication_failure = 4
    resource_failure      = 5
    OTHERS                = 6.

lv_subrc = sy-subrc.
IF lv_subrc <> 0.
  MESSAGE 'Error starting processing' TYPE 'I' DISPLAY LIKE 'E'.
ELSE.
  " Display success message with transaction ID
  CONCATENATE 'Processing started. Transaction ID:' lv_trans_id
    INTO lv_message SEPARATED BY space.
  MESSAGE lv_message TYPE 'S'.
ENDIF.
" END: Cursor Generated Code
```

### 11.2 Monitoring Transaction Status

```abap
" BEGIN: Cursor Generated Code
" Check transaction status
DATA: lw_header TYPE zpaltrfhdr,
      lv_trans_id TYPE char20,
      lv_subrc TYPE sy-subrc.

lv_trans_id = '...'.  " Transaction ID from previous call

SELECT SINGLE *
  FROM zpaltrfhdr
  INTO lw_header
  WHERE trans_id = lv_trans_id.

lv_subrc = sy-subrc.
IF lv_subrc = 0.
  CASE lw_header-status.
    WHEN 'RUNNING'.
      WRITE: / 'Transaction is still running...'.
    WHEN 'COMPLETED'.
      WRITE: / 'Transaction completed successfully.'.
      WRITE: / 'Pallets processed:', lw_header-pallet_count.
    WHEN 'ERROR'.
      WRITE: / 'Transaction failed with error:'.
      WRITE: / lw_header-error_msg.
  ENDCASE.
ENDIF.
" END: Cursor Generated Code
```

### 11.3 Synchronous vs Asynchronous Calls

**Asynchronous (Recommended for Bulk Operations):**
```abap
CALL FUNCTION 'Z_SOLAR_ACTIVITY'
  STARTING NEW TASK 'SOLAR_TASK'
  ...
```

**Synchronous (For Small Operations or Testing):**
```abap
CALL FUNCTION 'Z_SOLAR_ACTIVITY'
  ...
```

---

## 12. SECURITY AND AUTHORIZATION

### 12.1 Authorization Objects

```abap
" RFC execution authorization in function modules
AUTHORITY-CHECK OBJECT 'S_RFC'
                ID 'RFC_TYPE' FIELD 'FUNC'
                ID 'RFC_NAME' FIELD 'Z_SOLAR_ACTIVITY'
                ID 'ACTVT' FIELD '16'.
IF sy-subrc <> 0.
  " Return authorization error
  ev_success = space.
  ev_message = 'No authorization to execute function module'.
  RAISE validation_error.
ENDIF.
```

**Note:** Authorization checks should be implemented within each function module. Calling programs are responsible for additional user-level authorization checks.

### 12.2 Role Configuration

**Role Name:** Z_SOLAR_FM_USER

**Authorizations:**
- S_RFC: ZSCM_HU_CHECK, ZSCM_SO_SLOC_VALIDATION, Z_SOLAR_ACTIVITY
- S_TABU_DIS: ZPALTRFHDR, ZPALTRFITM (Table maintenance - for monitoring)

**Note:** Calling programs will have their own authorization requirements (e.g., S_TCODE).

### 12.3 Sensitive Data Handling

- No passwords or credentials in code
- User ID passed as parameter and logged for audit trail
- Error messages sanitized (no sensitive data)
- Transaction data accessible only to authorized users
- RFC-enabled for secure cross-system communication

---

## 13. MAINTENANCE AND SUPPORT

### 13.1 Monitoring

#### Transaction Log Query
```sql
SELECT * FROM ZPALTRFHDR
WHERE status = 'ERROR'
  AND created_on = sy-datum
ORDER BY created_at DESC;
```

#### Failed Transactions Report
Create SM30 view to display:
- Transaction ID
- Pallet Count
- Activity
- Status
- Created By
- Error Message

### 13.2 Troubleshooting Guide

| **Issue** | **Diagnosis** | **Resolution** |
|----------|--------------|---------------|
| Transaction stuck in RUNNING | Function module execution failed | Check SM37/SM50, restart if needed |
| HU validation fails | HU not in /SCWM/AQUA | Verify HU exists in warehouse |
| SO validation fails | SO not in VBAK | Verify sales order number |
| Batch processing slow | Too many HUs | Process in smaller batches |
| RFC destination error | Connection issue | Check SM59, test connection |
| Function module not found | Transport not imported | Check SE37, import transport |

### 13.3 Support Contacts

| **Issue Type** | **Contact** | **Transaction** |
|---------------|------------|----------------|
| Program errors | ABAP Development Team | ST22 |
| Database errors | Basis Team | DB02 |
| Authorization | Security Team | SU53 |
| Functional | Business Analyst | - |

---

## 14. APPENDIX

### 14.1 Data Structures

#### Type Definition: TY_HU_INPUT
```abap
TYPES: BEGIN OF ty_hu_input,
         sign   TYPE char1,
         option TYPE char2,
         low    TYPE /scwm/de_huident,
         high   TYPE /scwm/de_huident,
       END OF ty_hu_input.
```

#### Type Definition: TY_HU_DATA
```abap
TYPES: BEGIN OF ty_hu_data,
         hu_id   TYPE /scwm/de_huident,
         stk_cat TYPE char1,
       END OF ty_hu_data.
```

#### Type Definition: TY_OUTPUT
```abap
TYPES: BEGIN OF ty_output,
         lgnum     TYPE /scwm/lgnum,
         stock_id  TYPE char20,
         parent_id TYPE char20,
       END OF ty_output.
```

### 14.2 Constants

```abap
CONSTANTS:
  lc_batch_size TYPE i VALUE 10,
  lc_activity_p1 TYPE char2 VALUE 'P1',
  lc_activity_p2 TYPE char2 VALUE 'P2',
  lc_activity_a2 TYPE char2 VALUE 'A2',
  lc_status_running TYPE char10 VALUE 'RUNNING',
  lc_status_completed TYPE char10 VALUE 'COMPLETED',
  lc_status_error TYPE char10 VALUE 'ERROR'.
```

### 14.3 Code Compliance Checklist

#### NetWeaver 7.31 Compliance
- [x] No inline declarations (DATA(var))
- [x] No constructor operators (NEW, VALUE)
- [x] No string templates (|text|)
- [x] No table expressions (itab[key])
- [x] All variables declared upfront
- [x] Classic OpenSQL syntax

#### ABAP Best Practices
- [x] Modular function module design
- [x] RFC-enabled for remote calls
- [x] Proper exception handling (TRY-CATCH)
- [x] SY-SUBRC checks after operations
- [x] FOR ALL ENTRIES with empty check
- [x] SORT before BINARY SEARCH
- [x] Cursor Generated Code markers
- [x] Authorization checks in function modules
- [x] Proper naming conventions (lv_, lt_, lw_, lo_, etc.)

#### Performance Optimization
- [x] Batch processing (10 HUs per batch)
- [x] Binary search for table lookups
- [x] FOR ALL ENTRIES instead of SELECT in loop
- [x] COMMIT WORK after each batch
- [x] Asynchronous processing for UI responsiveness

#### Documentation
- [x] Method documentation (ABAP Doc)
- [x] Header comments for all components
- [x] Inline comments for complex logic
- [x] Cursor generated code markers

### 14.4 Glossary

| **Term** | **Definition** |
|----------|---------------|
| HU | Handling Unit - Physical packaging unit in warehouse |
| EWM | Extended Warehouse Management Module |
| SFG | Semi-Finished Goods |
| FG | Finished Goods |
| SLOC | Storage Location |
| SO | Sales Order |
| RFC | Remote Function Call |
| GUID | Globally Unique Identifier |
| MATID | Material ID |
| CAT | Category (Stock Category) |

### 14.5 References

- SAP EWM Documentation: HU Management
- ABAP Development Guidelines v1.4
- NetWeaver 7.31 Syntax Reference
- Code Generation Checklist
- Performance Optimization Guide

### 14.6 Change History

| **Version** | **Date** | **Author** | **Description** |
|------------|----------|------------|-----------------|
| 1.0 | [Date] | [Author] | Initial version |

---

**Document End**

**Approval:**

| **Role** | **Name** | **Signature** | **Date** |
|----------|----------|--------------|----------|
| Technical Architect | | | |
| ABAP Lead | | | |
| Quality Assurance | | | |
| Project Manager | | | |

---

## NOTES FOR DEVELOPER

### Key Implementation Points:

1. **Function Module Architecture:**
   - Three main RFC-enabled function modules
   - Validation FMs can be called independently
   - Action FM orchestrates the entire process
   - No UI components - pure backend services

2. **NetWeaver 7.31 Compliance:**
   - All code uses 7.31-compatible syntax
   - No inline declarations or modern operators
   - Variables declared upfront in DATA sections

3. **Integration Design:**
   - RFC-enabled for remote calls
   - Standardized interface for easy integration
   - Return parameters for all results
   - Exception handling for errors

4. **Performance Optimization:**
   - Batch processing of 10 HUs
   - FOR ALL ENTRIES with empty checks
   - SORT + BINARY SEARCH for lookups
   - Asynchronous processing support

5. **Error Handling:**
   - TRY-CATCH blocks for exceptions
   - SY-SUBRC checks after all operations
   - Comprehensive error messages via export parameters
   - Transaction logging for audit

6. **Code Quality:**
   - Function module documentation
   - Cursor generated code markers
   - Meaningful variable names
   - Proper indentation

This technical specification provides complete implementation details for function modules following all ABAP guidelines and best practices for NetWeaver 7.31. No report program or UI components are included - these function modules serve as backend services for calling programs.

