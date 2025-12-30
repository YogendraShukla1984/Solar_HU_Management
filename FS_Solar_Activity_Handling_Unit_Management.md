# Functional Specification (FS)
## Solar Activity - Handling Unit Management System

---

### Document Information

| **Field** | **Details** |
|-----------|-------------|
| **Document Title** | Functional Specification - Handling Unit Management |
| **Program Name** | Z_SOLAR_ACTIVITY |
| **Project** | SFG to FG Conversion in Bulk |
| **Module** | SCM - Supply Chain Management |
| **Author** | [Name] |
| **Date** | [Date] |
| **Version** | 1.0 |
| **Status** | Draft |

---

## 1. EXECUTIVE SUMMARY

### 1.1 Purpose
This functional specification describes a set of function modules to manage Handling Units (HU) in the warehouse management system. These function modules will provide backend services for three primary activities:
1. Assign Sales Order to Handling Units
2. Unassign Sales Order from Handling Units
3. Convert Semi-Finished Goods (SFG) to Finished Goods (FG)

The function modules can be called from existing programs, web services, or via RFC for integration with other systems.

### 1.2 Scope
- Provide validation function modules for HU, Sales Order, and Storage Location
- Provide action function module for processing bulk operations (assign/unassign/convert)
- Track transaction status and history in custom database tables
- Support asynchronous processing for bulk operations
- Enable integration with existing SAP programs and external systems

### 1.3 Business Benefits
- Modular function modules for flexible integration
- Reusable validation services across multiple programs
- Streamline bulk handling unit operations
- Reduce manual effort in HU management
- Improve data accuracy through robust validation
- Enable tracking of all HU transactions
- Support integration with existing SAP applications and external systems
- Facilitate future enhancements without impacting calling programs

---

## 2. BUSINESS REQUIREMENTS

### 2.1 Functional Requirements

#### FR-001: Handling Unit Validation
**Priority:** High  
**Description:** System must validate that Handling Units exist in the warehouse before processing.

**Business Rules:**
- All HUs must exist in table `/SCWM/AQUA`
- HUs must belong to the specified warehouse
- System displays error for invalid HUs
- Invalid HUs prevent processing of entire batch

#### FR-002: Sales Order Validation
**Priority:** High  
**Description:** System must validate sales orders and line items before assignment.

**Business Rules:**
- Sales order must exist in table `VBAK`
- Sales order line item must exist in table `VBAP`
- Both order and line item required for assignment operations
- Display appropriate error messages for non-existent orders

#### FR-003: Storage Location Validation
**Priority:** High  
**Description:** System must validate storage locations for conversion operations.

**Business Rules:**
- Warehouse number maps to EWM warehouse via `T340D`
- EWM warehouse maps to plant via `T320`
- Storage location must exist in `T001L` for the plant
- Invalid storage location prevents conversion operation

#### FR-004: Assign Sales Order to HU
**Priority:** High  
**Description:** Link sales orders to handling units for order fulfillment.

**Business Rules:**
- Process HUs in batches of 10 for performance
- Only lower-level HUs are assigned to sales orders
- Both sales order number and line item required
- Transaction tracked in header and item tables
- Process runs asynchronously

#### FR-005: Unassign Sales Order from HU
**Priority:** High  
**Description:** Remove sales order assignment from handling units.

**Business Rules:**
- Process HUs in batches of 10 for performance
- Only lower-level HUs are unassigned
- Transaction tracked in header and item tables
- Process runs asynchronously

#### FR-006: SFG to FG Conversion
**Priority:** High  
**Description:** Convert semi-finished goods to finished goods in specified storage location.

**Business Rules:**
- Process HUs in batches of 10 for performance
- Conversion posts to specified storage location
- Material category updated in conversion
- Transaction tracked in header and item tables
- Process runs asynchronously

#### FR-007: Transaction Tracking
**Priority:** High  
**Description:** Track all transactions in custom database tables.

**Business Rules:**
- Each transaction assigned unique transaction ID
- Header table tracks: transaction ID, pallet count, activity, user, timestamp, status
- Item table tracks: transaction ID, pallet number, carton numbers
- Status values: 'RUNNING', 'COMPLETED', 'ERROR'
- Transaction history maintained for audit purposes

### 2.2 Non-Functional Requirements

#### NFR-001: Performance
- Process batches of 10 HUs at a time
- Asynchronous processing for UI responsiveness
- Maximum response time: 3 seconds for validation
- Background processing for bulk operations

#### NFR-002: Usability
- Clear error messages for validation failures returned to calling programs
- Status updates during processing via transaction tables
- Transaction history accessible for review via table queries
- Standardized function module interfaces for easy integration

#### NFR-003: Reliability
- Data validation before processing
- Error handling for all operations
- Transaction rollback on failure
- Audit trail for all activities

#### NFR-004: Security
- Authorization checks within function modules
- User ID passed as parameter and tracked in transaction tables
- RFC-enabled for secure remote calls
- Restricted access to warehouse operations via authorization objects

---

## 3. PROCESS FLOWS

### 3.1 Assign Sales Order to HU Process

```
START
  ↓
User selects Activity: "Assign Sales Order"
  ↓
User enters: Warehouse, HUs, Sales Order, Line Item
  ↓
VALIDATION PHASE
  ↓
Validate HUs in /SCWM/AQUA → [Error] → Display Error & STOP
  ↓ [Valid]
Validate Sales Order in VBAK → [Error] → Display Error & STOP
  ↓ [Valid]
Validate Line Item in VBAP → [Error] → Display Error & STOP
  ↓ [Valid]
PROCESSING PHASE
  ↓
Generate Transaction ID
  ↓
Update ZPALTRFHDR (Header Table)
  ↓
Get Lower Level HUs (via /SCWM/HU_SELECT_GEN)
  ↓
Update ZPALTRFITM (Item Table)
  ↓
Split HUs into batches of 10
  ↓
FOR EACH BATCH:
  ↓
  Call ZSCM_BIN_INTL_POST_GET_DET (Action: P1)
  ↓
  Call ZSCM_BIN_INTL_POSTING
  ↓
NEXT BATCH
  ↓
Update Status to "COMPLETED"
  ↓
Display Success Message
  ↓
END
```

### 3.2 Unassign Sales Order from HU Process

```
START
  ↓
User selects Activity: "Unassign Sales Order"
  ↓
User enters: Warehouse, HUs
  ↓
VALIDATION PHASE
  ↓
Validate HUs in /SCWM/AQUA → [Error] → Display Error & STOP
  ↓ [Valid]
PROCESSING PHASE
  ↓
Generate Transaction ID
  ↓
Update ZPALTRFHDR (Header Table)
  ↓
Get Lower Level HUs (via /SCWM/HU_SELECT_GEN)
  ↓
Update ZPALTRFITM (Item Table)
  ↓
Split HUs into batches of 10
  ↓
FOR EACH BATCH:
  ↓
  Call ZSCM_BIN_INTL_POST_GET_DET (Action: P2)
  ↓
  Call ZSCM_BIN_INTL_POSTING
  ↓
NEXT BATCH
  ↓
Update Status to "COMPLETED"
  ↓
Display Success Message
  ↓
END
```

### 3.3 SFG to FG Conversion Process

```
START
  ↓
User selects Activity: "SFG to FG Conversion"
  ↓
User enters: Warehouse, HUs, Storage Location
  ↓
VALIDATION PHASE
  ↓
Validate HUs in /SCWM/AQUA → [Error] → Display Error & STOP
  ↓ [Valid]
Validate Storage Location (T340D→T320→T001L) → [Error] → Display Error & STOP
  ↓ [Valid]
PROCESSING PHASE
  ↓
Generate Transaction ID
  ↓
Update ZPALTRFHDR (Header Table)
  ↓
Get Lower Level HUs (via /SCWM/HU_SELECT_GEN)
  ↓
Update ZPALTRFITM (Item Table)
  ↓
Split HUs into batches of 10
  ↓
FOR EACH BATCH:
  ↓
  Call ZSCM_BIN_INTL_POST_GET_DET (Action: A2)
  ↓
  Call ZSCM_BIN_INTL_POSTING (with Storage Location)
  ↓
NEXT BATCH
  ↓
Update Status to "COMPLETED"
  ↓
Display Success Message
  ↓
END
```

---

## 4. DATA REQUIREMENTS

### 4.1 Input Data

#### Function Module Import Parameters

| **Field** | **Technical Name** | **Type** | **Length** | **Mandatory** | **Description** |
|-----------|-------------------|----------|-----------|---------------|-----------------|
| Warehouse | IV_WAREHOUSE | CHAR | 4 | Yes | Warehouse Number |
| Activity | IV_ACTIVITY | CHAR | 2 | Yes | Activity Type (P1/P2/A2) |
| User ID | IV_USER | CHAR | 12 | Yes | Executing User |
| Handling Units | IT_HU | Table | - | Yes | Table of Handling Units |
| Sales Order | IV_VBELN | CHAR | 10 | Conditional | Required for P1 |
| Line Item | IV_POSNR | NUMC | 6 | Conditional | Required for P1 |
| Storage Location | IV_LGORT | CHAR | 4 | Conditional | Required for A2 |

**Activity Codes:**
- P1: Assign Sales Order to HU
- P2: Unassign Sales Order from HU
- A2: SFG to FG Conversion

**Note:** Calling programs are responsible for providing user interface and input validation before calling these function modules.

### 4.2 Output Data

#### Function Module Export Parameters

| **Field** | **Technical Name** | **Type** | **Length** | **Description** |
|-----------|-------------------|----------|-----------|-----------------|
| Transaction ID | EV_TRANS_ID | CHAR | 20 | Generated Transaction ID |
| Success Flag | EV_SUCCESS | CHAR | 1 | X = Success, blank = Error |
| Message Text | EV_MESSAGE | STRING | - | Success or Error Message |
| Message Table | ET_MESSAGES | Table | - | Detailed Messages (optional) |

#### Transaction Log (Database Tables)
- Transaction ID (in ZPALTRFHDR)
- Number of Pallets Processed
- Processing Status (RUNNING/COMPLETED/ERROR)
- Error Messages (if any)
- Timestamp and User ID

### 4.3 Database Tables

#### 4.3.1 Custom Tables

**ZPALTRFHDR - Transaction Header Table**

| **Field** | **Data Element** | **Type** | **Length** | **Description** |
|-----------|-----------------|----------|-----------|-----------------|
| TRANS_ID | CHAR | 20 | Transaction ID (Primary Key) |
| PALLET_COUNT | INT4 | - | Number of Pallets |
| ACTIVITY | CHAR | 2 | Activity Code (P1/P2/A2) |
| STATUS | CHAR | 10 | Status (RUNNING/COMPLETED/ERROR) |
| CREATED_BY | UNAME | 12 | Created By User |
| CREATED_ON | DATUM | 8 | Creation Date |
| CREATED_AT | UZEIT | 6 | Creation Time |

**ZPALTRFITM - Transaction Item Table**

| **Field** | **Data Element** | **Type** | **Length** | **Description** |
|-----------|-----------------|----------|-----------|-----------------|
| TRANS_ID | CHAR | 20 | Transaction ID (Primary Key) |
| PALLET_NO | CHAR | 20 | Pallet Number (Primary Key) |
| CARTON_NO | CHAR | 20 | Carton/Lower Level HU |

#### 4.3.2 Standard SAP Tables (Read Only)

| **Table** | **Description** | **Usage** |
|-----------|----------------|-----------|
| /SCWM/AQUA | EWM HU Stock | HU Validation |
| VBAK | Sales Order Header | Sales Order Validation |
| VBAP | Sales Order Item | Line Item Validation |
| T340D | Warehouse to EWM Mapping | Storage Location Validation |
| T320 | EWM to Plant Mapping | Storage Location Validation |
| T001L | Storage Locations | Storage Location Validation |

---

## 5. INTERFACE SPECIFICATIONS

### 5.1 Function Modules to be Created

#### 5.1.1 ZSCM_HU_CHECK (Validation Function Module)
**Purpose:** Validate Handling Units in EWM  
**System:** AD2  
**Type:** RFC-Enabled  
**Release:** External

**Import Parameters:**
- IV_WAREHOUSE (CHAR4) - Warehouse Number [Mandatory]
- IT_HU (TABLE) - Handling Units [Mandatory]

**Export Parameters:**
- EV_VALID (CHAR1) - Validation Result: X=Valid, blank=Invalid
- EV_MESSAGE (STRING) - Success or Error Message
- ET_INVALID_HU (TABLE) - List of Invalid HUs with reasons

**Exceptions:**
- INVALID_HU - One or more HUs are invalid

**Business Logic:**
- Check each HU exists in table /SCWM/AQUA for given warehouse
- Return list of invalid HUs if any found
- Provide detailed error message for each invalid HU

#### 5.1.2 ZSCM_SO_SLOC_VALIDATION (Validation Function Module)
**Purpose:** Validate Sales Orders and Storage Locations  
**System:** RD2  
**Type:** RFC-Enabled  
**Release:** External

**Import Parameters:**
- IV_VBELN (CHAR10) - Sales Order [Optional]
- IV_POSNR (NUMC6) - Line Item [Optional]
- IV_WAREHOUSE (CHAR4) - Warehouse [Optional]
- IV_LGORT (CHAR4) - Storage Location [Optional]

**Export Parameters:**
- EV_VALID (CHAR1) - Validation Result: X=Valid, blank=Invalid
- EV_MESSAGE (STRING) - Success or Error Message

**Exceptions:**
- INVALID_INPUT - Invalid input combination or data not found

**Business Logic:**
- If SO and Line Item provided: Validate in VBAK and VBAP
- If Warehouse and SLOC provided: Validate via T340D → T320 → T001L
- Return appropriate error message if validation fails

#### 5.1.3 Z_SOLAR_ACTIVITY (Action Function Module)
**Purpose:** Main processing function for all HU activities  
**Type:** RFC-Enabled (for async processing)  
**Release:** External

**Import Parameters:**
- IV_WAREHOUSE (CHAR4) - Warehouse Number [Mandatory]
- IV_ACTIVITY (CHAR2) - Activity Code: P1/P2/A2 [Mandatory]
- IV_USER (CHAR12) - User ID [Mandatory]
- IT_HU (TABLE) - Handling Units [Mandatory]
- IV_VBELN (CHAR10) - Sales Order [Conditional: Required for P1]
- IV_POSNR (NUMC6) - Line Item [Conditional: Required for P1]
- IV_LGORT (CHAR4) - Storage Location [Conditional: Required for A2]

**Export Parameters:**
- EV_TRANS_ID (CHAR20) - Generated Transaction ID
- EV_SUCCESS (CHAR1) - Success Flag: X=Success, blank=Error
- EV_MESSAGE (STRING) - Success or Error Message
- ET_MESSAGES (TABLE) - Detailed message log [Optional]

**Exceptions:**
- VALIDATION_ERROR - Input validation failed
- PROCESSING_ERROR - Error during processing

**Business Logic:**
1. Generate unique transaction ID
2. Create header record in ZPALTRFHDR
3. Get HU hierarchy using /SCWM/HU_SELECT_GEN
4. Create item records in ZPALTRFITM
5. Process HUs in batches of 10
6. Update transaction status upon completion
7. Handle errors and update error status

### 5.2 Integration Points

#### Calling the Function Modules

**Example 1: Validate HU**
```abap
CALL FUNCTION 'ZSCM_HU_CHECK'
  DESTINATION 'AD2_RFC_DEST'
  EXPORTING
    iv_warehouse = 'WH01'
  TABLES
    it_hu        = lt_hu_list
  IMPORTING
    ev_valid     = lv_valid
    ev_message   = lv_message
    et_invalid_hu = lt_invalid_hu
  EXCEPTIONS
    invalid_hu    = 1
    OTHERS        = 2.
```

**Example 2: Process Activity**
```abap
CALL FUNCTION 'Z_SOLAR_ACTIVITY'
  STARTING NEW TASK 'SOLAR_TASK'
  EXPORTING
    iv_warehouse = 'WH01'
    iv_activity  = 'P1'
    iv_user      = sy-uname
    iv_vbeln     = '1000000'
    iv_posnr     = '000010'
  TABLES
    it_hu        = lt_hu_list
  IMPORTING
    ev_trans_id  = lv_trans_id
    ev_success   = lv_success
    ev_message   = lv_message
  EXCEPTIONS
    validation_error = 1
    processing_error = 2
    OTHERS           = 3.
```

### 5.3 Existing Function Modules (To be Called)

#### 5.3.1 /SCWM/HU_SELECT_GEN
**Purpose:** Get HU hierarchy and details

**Import:**
- IV_LGNUM - Warehouse Number
- IR_HUIDENT - HU Numbers

**Export:**
- ET_HUHDR - HU Header Data
- ET_HUITM - HU Item Data

#### 5.3.2 ZSCM_BIN_INTL_POST_GET_DET
**Purpose:** Get posting details for HU operations

**Import:**
- I_WAREHOUSE - Warehouse
- I_ACTION - Action Code (P1/P2/A2)
- I_USER - User ID
- IT_INPUT - Material Data
- IT_HU - HU Data

**Export:**
- ET_OUTPUT - Posting Details

#### 5.3.3 ZSCM_BIN_INTL_POSTING
**Purpose:** Execute HU posting

**Import:**
- IM_INPUT - Posting Data Structure

---

## 6. INTEGRATION REQUIREMENTS

### 6.1 Calling Program Responsibilities

Programs that call these function modules must:

1. **Provide User Interface:**
   - Input screen for warehouse, activity, HUs, SO, SLOC
   - Field validations and F4 help
   - Activity-specific field enabling/disabling

2. **Pre-Validation:**
   - Mandatory field checks
   - Input format validation
   - Activity-specific parameter validation

3. **Call Validation Function Modules:**
   - Call ZSCM_HU_CHECK before processing
   - Call ZSCM_SO_SLOC_VALIDATION as needed
   - Handle validation errors appropriately

4. **Call Action Function Module:**
   - Pass all required parameters
   - Handle synchronous or asynchronous calls
   - Process return parameters

5. **Display Results:**
   - Show transaction ID to user
   - Display success/error messages
   - Provide link to transaction log tables

### 6.2 Asynchronous Processing

For bulk operations, calling programs should use asynchronous RFC:

```abap
" Asynchronous call for better UI responsiveness
CALL FUNCTION 'Z_SOLAR_ACTIVITY'
  STARTING NEW TASK 'SOLAR_TASK'
  EXPORTING
    ...
  EXCEPTIONS
    ...
    
" Display message to user
MESSAGE 'Processing started in background. Transaction ID: ' && lv_trans_id TYPE 'S'.
```

### 6.3 Transaction Status Monitoring

Calling programs can monitor transaction status by querying ZPALTRFHDR:

```sql
SELECT SINGLE status error_msg
  FROM zpaltrfhdr
  INTO (lv_status, lv_error)
  WHERE trans_id = lv_trans_id.
```

**Status Values:**
- RUNNING: Processing in progress
- COMPLETED: Successfully completed
- ERROR: Failed (see error_msg field)

---

## 7. ERROR HANDLING

### 7.1 Validation Errors

| **Error Code** | **Message** | **Action** |
|---------------|-------------|------------|
| E001 | Warehouse number is mandatory | Display error, stop processing |
| E002 | Activity selection is mandatory | Display error, stop processing |
| E003 | No handling units selected | Display error, stop processing |
| E004 | Sales order required for activity P1 | Display error, stop processing |
| E005 | Storage location required for activity A2 | Display error, stop processing |
| E006 | HU {HU_NUMBER} does not exist in warehouse | Display error, stop processing |
| E007 | Sales order {VBELN} does not exist | Display error, stop processing |
| E008 | Line item {POSNR} does not exist for order {VBELN} | Display error, stop processing |
| E009 | Storage location {LGORT} is not valid | Display error, stop processing |

### 7.2 Processing Errors

| **Error Code** | **Message** | **Action** |
|---------------|-------------|------------|
| E010 | Error calling /SCWM/HU_SELECT_GEN | Log error, rollback transaction |
| E011 | Error calling ZSCM_BIN_INTL_POST_GET_DET | Log error, rollback batch |
| E012 | Error calling ZSCM_BIN_INTL_POSTING | Log error, rollback batch |
| E013 | Database update failed | Log error, rollback transaction |

---

## 8. SECURITY AND AUTHORIZATION

### 8.1 Authorization Objects

| **Object** | **Field** | **Value** | **Description** |
|-----------|----------|-----------|-----------------|
| S_RFC | RFC_TYPE | FUNC | Function module authorization |
| S_RFC | RFC_NAME | ZSCM_HU_CHECK | HU validation FM |
| S_RFC | RFC_NAME | ZSCM_SO_SLOC_VALIDATION | SO/SLOC validation FM |
| S_RFC | RFC_NAME | Z_SOLAR_ACTIVITY | Action FM |
| S_RFC | ACTVT | 16 | Execute authorization |

### 8.2 Authorization Checks
- Function modules check RFC execution authorization
- User ID passed as parameter from calling program
- Calling programs responsible for user authorization checks
- All user activities logged with user ID in ZPALTRFHDR

---

## 9. TESTING REQUIREMENTS

### 9.1 Unit Testing

| **Test Case** | **Description** | **Expected Result** |
|--------------|-----------------|---------------------|
| TC-001 | Validate HU exists | Valid HU returns success |
| TC-002 | Validate invalid HU | Invalid HU returns error |
| TC-003 | Validate sales order | Valid order returns success |
| TC-004 | Validate invalid sales order | Invalid order returns error |
| TC-005 | Validate storage location | Valid location returns success |

### 9.2 Integration Testing

| **Test Case** | **Description** | **Expected Result** |
|--------------|-----------------|---------------------|
| TC-101 | Assign SO to single HU | Transaction created, status completed |
| TC-102 | Assign SO to multiple HUs | All HUs processed in batches |
| TC-103 | Unassign SO from HU | HU unassigned successfully |
| TC-104 | SFG to FG conversion | Material converted, posted to SLOC |
| TC-105 | Process 100 HUs | All processed in 10 batches |

### 9.3 Performance Testing

| **Test Case** | **Description** | **Expected Result** |
|--------------|-----------------|---------------------|
| TC-201 | Process 10 HUs | < 5 seconds |
| TC-202 | Process 100 HUs | < 50 seconds |
| TC-203 | Validation of 50 HUs | < 3 seconds |

---

## 10. ASSUMPTIONS AND DEPENDENCIES

### 10.1 Assumptions
1. Function modules ZSCM_BIN_INTL_POST_GET_DET and ZSCM_BIN_INTL_POSTING already exist
2. EWM system is properly configured
3. Table /SCWM/AQUA is maintained with current HU data
4. RFC destinations AD2_RFC_DEST and RD2_RFC_DEST are configured
5. Calling programs will provide user interface
6. Calling programs will handle user authorization

### 10.2 Dependencies
1. EWM module must be active
2. Sales order processing must be configured
3. Storage location master data must be maintained
4. Warehouse mapping tables must be configured
5. RFC connections between systems properly configured

### 10.3 Constraints
1. Processing limited to 10 HUs per batch for performance
2. Function modules are stateless (no session data stored)
3. Calling programs responsible for user interaction
4. NetWeaver 7.31 compatibility required

---

## 11. APPENDIX

### 11.1 Glossary

| **Term** | **Definition** |
|----------|---------------|
| HU | Handling Unit - Physical unit for warehouse management |
| EWM | Extended Warehouse Management |
| SFG | Semi-Finished Goods |
| FG | Finished Goods |
| SLOC | Storage Location |
| SO | Sales Order |

### 11.2 References
- SAP EWM Documentation
- ABAP Development Guidelines v1.4
- Warehouse Management Best Practices

### 11.3 Change History

| **Version** | **Date** | **Author** | **Description** |
|------------|----------|------------|-----------------|
| 1.0 | [Date] | [Author] | Initial version |

---

**Document End**

**Approval:**

| **Role** | **Name** | **Signature** | **Date** |
|----------|----------|--------------|----------|
| Business Analyst | | | |
| Functional Lead | | | |
| Technical Lead | | | |
| Project Manager | | | |

