# Functional Specification: Solar Activity Report

## Document Information
- **Document Type:** Functional Specification
- **Module:** Warehouse Management (EWM)
- **Component:** SFG to FG Conversion Tracking
- **Created:** 2025-12-31
- **Version:** 1.0

---

## 1. Overview

### 1.1 Description
This specification defines a function module to track and report the status of Semi-Finished Goods (SFG) to Finished Goods (FG) conversion activities. The system will provide comprehensive visibility into the conversion process, including Sales Order (SO) assignment to pallets, SO unassignment from pallets, and the complete conversion lifecycle from transfer initiation to FG receipt.

### 1.2 Business Objective
Enable real-time monitoring and tracking of SFG to FG conversion activities by providing consolidated reporting across ECC and EWM systems. This will help:
- Track conversion status at pallet level
- Identify bottlenecks in the conversion process
- Monitor transfer timelines from start to finish
- Provide visibility into error scenarios for quick resolution

### 1.3 Scope
- Report on three activity types: SO assigned to pallet, SO unassigned from pallet, and SFG to FG conversion
- Support date range selection for activity filtering
- Provide detailed conversion status for each pallet
- Display comprehensive pallet and product information
- Highlight errors with detailed error descriptions

---

## 2. System Architecture

### 2.1 System Landscape
- **ECC System:** Contains YSTKDTLS, MSEG, MKPF tables
- **EWM System:** Contains ZPALTRFHDR, ZPALTRFITM, /SCWM/AQUA, /SAPAPO/MATKEY tables
- **Target System:** Function modules will be created in EWM system
- **Integration:** RFC or database link required for ECC table access from EWM

### 2.2 Dependencies
- Access to ECC tables from EWM system
- SAP standard tables: VBUK, LIKP
- Custom tables: YSTKDTLS, ZPALTRFHDR, ZPALTRFITM

---

## 3. Function Module Specifications

### 3.1 Main Function Module: ZSCM_SOLAR_ACTIVITY_REPORT

#### 3.1.1 Function Module Details
- **Name:** ZSCM_SOLAR_ACTIVITY_REPORT
- **Function Group:** TBD (e.g., ZSCM_SFG_CONVERSION)
- **Processing Type:** Normal Function Module
- **Remote-Enabled:** Yes (for potential remote calls)

#### 3.1.2 Input Parameters

| Parameter Name | Type | Required | Description | Constraints |
|---------------|------|----------|-------------|-------------|
| IT_ACTIVITY | Table Type | Yes | Activity types to report | Valid values: 'SO_ASSIGNED', 'SO_UNASSIGNED', 'SFG_FG_CONVERSION' |
| IV_DATE_FROM | DATUM | Yes | Start date of date range | Must be ≤ IV_DATE_TO |
| IV_DATE_TO | DATUM | Yes | End date of date range | Must be ≥ IV_DATE_FROM |
| IT_PALLET_NO | Table Type (Optional) | No | Specific pallet numbers to filter | If provided, filter results by these pallets |
| IT_PLANT | Table Type (Optional) | No | Plant codes to filter | If provided, filter by plant |

#### 3.1.3 Output Parameters

| Field Name | Data Type | Length | Description | Source |
|-----------|-----------|--------|-------------|--------|
| TRANSACTION_ID | CHAR | 20 | Transaction identifier | ZPALTRFHDR-TRANSACTION_ID |
| TRANSACTION_DATE | DATS | 8 | Transaction creation date | ZPALTRFHDR-CREATED_ON |
| HANDLING_UNIT | CHAR | 20 | Pallet/HU number | ZPALTRFITM-PALLET_NUMBER |
| PRODUCT | CHAR | 18 | Material ID | /SCWM/AQUA-MATID |
| PRODUCT_DESC | CHAR | 40 | Product description | /SAPAPO/MATKEY-MAKTX |
| TRANSFER_START_DATE | DATS | 8 | Transfer initiation date | YSTKDTLS-CREATED_ON |
| TRANSFER_START_TIME | TIMS | 6 | Transfer initiation time | YSTKDTLS-CREATED_TM |
| FG_RECEIPT_DATE | DATS | 8 | FG pallet receipt date | MKPF-CPUDT |
| FG_RECEIPT_TIME | TIMS | 6 | FG pallet receipt time | MKPF-CPUTM |
| PLANT | WERKS_D | 4 | Plant code | YSTKDTLS-SOURCEWERKS |
| FROM_SLOC | LGORT_D | 4 | Source storage location | MSEG-LGORT |
| TO_SLOC | LGORT_D | 4 | Destination storage location | MSEG-LGORT |
| FROM_STORAGE_BIN | CHAR | 18 | Source storage bin | YSTKDTLS-TUBE_M |
| TO_STORAGE_BIN | CHAR | 18 | Destination storage bin | /SCWM/AQUA-LGPLA |
| SFG_QUANTITY | QUAN | 13 | SFG quantity | Calculated/Derived |
| FG_QUANTITY | QUAN | 13 | FG quantity | /SCWM/AQUA-QUAN |
| BASE_UOM | MEINS | 3 | Base unit of measure | /SCWM/AQUA-ALTME |
| BATCH | CHARG_D | 10 | Batch number | /SCWM/AQUA-CHARG |
| STOCK_TYPE | CHAR | 2 | Stock category | /SCWM/AQUA-CAT |
| SALES_ORDER | VBELN_VL | 10 | Sales order number | /SCWM/AQUA-STOCK_DOCNO |
| SO_ITEM | POSNR_VL | 6 | Sales order item | /SCWM/AQUA-STOCK_ITEMNO |
| USER_ID | SYUNAME | 12 | User who created transfer | YSTKDTLS-CREATED_BY |
| PALLET_PROD_DATE | DATS | 8 | Pallet production date | Calculated/Derived |
| PALLET_GRN_DATE | DATS | 8 | Pallet GRN date | /SCWM/AQUA-WDATU |
| CONVERSION_STATUS | CHAR | 30 | Conversion status | Derived from ZSCM_SFG_CONVERSION_STATUS |
| ERROR_DETAILS | CHAR | 255 | Error description if status is error | Derived from ZSCM_SFG_CONVERSION_STATUS |

#### 3.1.4 Exceptions

| Exception Name | Description | Trigger Condition |
|---------------|-------------|-------------------|
| DATE_RANGE_INVALID | Invalid date range | IV_DATE_FROM > IV_DATE_TO |
| NO_DATA_FOUND | No data for criteria | No records found for input parameters |
| ACTIVITY_TYPE_INVALID | Invalid activity type | Activity type not in allowed values |
| ECC_CONNECTION_ERROR | Cannot connect to ECC | RFC/Database connection failure |
| AUTHORIZATION_FAILED | User not authorized | Missing authorization for plants/data |

---

### 3.2 Helper Function Module: ZSCM_SFG_CONVERSION_STATUS

#### 3.2.1 Function Module Details
- **Name:** ZSCM_SFG_CONVERSION_STATUS
- **Function Group:** ZSCM_SFG_CONVERSION
- **Processing Type:** Normal Function Module
- **Purpose:** Determine conversion status for pallets
- **Requirement ID:** RD2

#### 3.2.2 Input Parameters

| Parameter Name | Type | Required | Description | Constraints |
|---------------|------|----------|-------------|-------------|
| IT_PALLET_NO | Table Type | Yes | List of pallet numbers | Multiple pallets supported |

#### 3.2.3 Output Parameters

| Field Name | Data Type | Description |
|-----------|-----------|-------------|
| PALLET_NO | CHAR(20) | Pallet number |
| STATUS | CHAR(30) | Conversion status |
| MESSAGE | CHAR(255) | Status message/details |

#### 3.2.4 Status Values

| Status Code | Status Description | Business Meaning |
|------------|-------------------|------------------|
| PUTAWAY_DONE | Putaway done | IBD found, VBUK-WBSTK = 'C' |
| IBD_DISTRIB_PENDING | IBD Distribution Pending | IBD found, LIKP-VLSTK = 'B' |
| IBD_CREATION_PENDING | IBD creation Pending | IBD not found in YSTKDTLS |
| CONV_DOC_NOT_POSTED | Conversion document not posted | Pallet not found in YSTKDTLS |

---

## 4. Business Logic

### 4.1 Main Report Logic (ZSCM_SOLAR_ACTIVITY_REPORT)

#### 4.1.1 Input Validation
1. Validate date range: IV_DATE_FROM ≤ IV_DATE_TO
2. Validate activity types against allowed values
3. Check authorization for plants if IT_PLANT provided
4. If date range > 365 days, issue warning for performance

#### 4.1.2 Data Retrieval Process

**Step 1: Retrieve Transaction Headers**
- Select from ZPALTRFHDR where CREATED_ON between IV_DATE_FROM and IV_DATE_TO
- Apply activity filter if IT_ACTIVITY provided
- Store transaction IDs for subsequent queries

**Step 2: Retrieve Pallet Items**
- Select from ZPALTRFITM for transaction IDs from Step 1
- Filter by IT_PALLET_NO if provided
- Store pallet numbers and material IDs

**Step 3: Retrieve Product Information (EWM)**
- Select from /SCWM/AQUA for pallet numbers from Step 2
- Get MATID, QUAN, ALTME, CHARG, CAT, STOCK_DOCNO, STOCK_ITEMNO, LGPLA, WDATU

**Step 4: Retrieve Product Descriptions**
- Select from /SAPAPO/MATKEY for material IDs from Step 3
- Get MAKTX (product description)

**Step 5: Retrieve Transfer Details (ECC)**
- Select from YSTKDTLS for pallet numbers (USRNO field)
- Get CREATED_ON, CREATED_TM, SOURCEWERKS, TUBE_M, CREATED_BY, VBELN
- Filter by IT_PLANT if provided

**Step 6: Retrieve Storage Location Details (ECC)**
- Select from MSEG for relevant material documents
- Get LGORT (from and to storage locations)

**Step 7: Retrieve FG Receipt Information (ECC)**
- Select from MKPF for material documents
- Get CPUDT, CPUTM (FG receipt date/time)

**Step 8: Get Conversion Status**
- Call ZSCM_SFG_CONVERSION_STATUS with pallet numbers
- Get conversion status and error details

**Step 9: Merge Data**
- Combine data from all sources using pallet number as key
- Handle missing data gracefully (populate with blank/zero)
- Sort by transaction date descending

#### 4.1.3 Performance Optimization
- Use FOR ALL ENTRIES for related table queries
- Always check IS NOT INITIAL before FOR ALL ENTRIES
- Use BINARY SEARCH for table lookups after SORT
- Select only required fields (no SELECT *)
- Use parallel processing if data volume > 10,000 records

### 4.2 Status Determination Logic (ZSCM_SFG_CONVERSION_STATUS)

#### 4.2.1 Status Check Flow

```
FOR EACH Pallet Number:
  
  Step 1: Check YSTKDTLS
  ├─ SELECT from YSTKDTLS where USRNO = Pallet_Number
  │
  ├─ IF Pallet NOT found in YSTKDTLS
  │  └─ RETURN Status: "CONV_DOC_NOT_POSTED"
  │     Message: "Conversion document not posted"
  │
  └─ IF Pallet found in YSTKDTLS
     └─ Get IBD Number (VBELN field)
     
     Step 2: Check IBD Status
     ├─ IF IBD Number is INITIAL
     │  └─ RETURN Status: "IBD_CREATION_PENDING"
     │     Message: "IBD creation Pending"
     │
     └─ IF IBD Number exists
        │
        Step 3: Check Putaway Status
        ├─ SELECT from VBUK where VBELN = IBD_Number
        │
        ├─ IF VBUK-WBSTK = 'C'
        │  └─ RETURN Status: "PUTAWAY_DONE"
        │     Message: "Putaway done"
        │
        └─ ELSE
           │
           Step 4: Check Distribution Status
           └─ SELECT from LIKP where VBELN = IBD_Number
              │
              ├─ IF LIKP-VLSTK = 'B'
              │  └─ RETURN Status: "IBD_DISTRIB_PENDING"
              │     Message: "IBD Distribution Pending"
              │
              └─ ELSE
                 └─ RETURN Status: "IBD_DISTRIB_PENDING"
                    Message: "IBD in process"
```

#### 4.2.2 Error Handling
- If YSTKDTLS access fails: Return "ERROR" with system error message
- If VBUK access fails: Log warning, assume IBD distribution pending
- If LIKP access fails: Log warning, assume IBD distribution pending
- Handle multiple pallets in batch for performance

---

## 5. Data Sources and Mappings

### 5.1 Table Mappings

| Output Field | Source System | Table | Field | Join Condition |
|-------------|---------------|-------|-------|----------------|
| TRANSACTION_ID | EWM | ZPALTRFHDR | TRANSACTION_ID | Primary |
| TRANSACTION_DATE | EWM | ZPALTRFHDR | CREATED_ON | Primary |
| HANDLING_UNIT | EWM | ZPALTRFITM | PALLET_NUMBER | ZPALTRFITM.TRANSACTION_ID = ZPALTRFHDR.TRANSACTION_ID |
| PRODUCT | EWM | /SCWM/AQUA | MATID | /SCWM/AQUA.HU = ZPALTRFITM.PALLET_NUMBER |
| PRODUCT_DESC | EWM | /SAPAPO/MATKEY | MAKTX | /SAPAPO/MATKEY.MATID = /SCWM/AQUA.MATID |
| TRANSFER_START_DATE | ECC | YSTKDTLS | CREATED_ON | YSTKDTLS.USRNO = ZPALTRFITM.PALLET_NUMBER |
| TRANSFER_START_TIME | ECC | YSTKDTLS | CREATED_TM | YSTKDTLS.USRNO = ZPALTRFITM.PALLET_NUMBER |
| FG_RECEIPT_DATE | ECC | MKPF | CPUDT | Via material document |
| FG_RECEIPT_TIME | ECC | MKPF | CPUTM | Via material document |
| PLANT | ECC | YSTKDTLS | SOURCEWERKS | YSTKDTLS.USRNO = ZPALTRFITM.PALLET_NUMBER |
| FROM_SLOC | ECC | MSEG | LGORT | From movement record |
| TO_SLOC | ECC | MSEG | LGORT | To movement record |
| FROM_STORAGE_BIN | ECC | YSTKDTLS | TUBE_M | YSTKDTLS.USRNO = ZPALTRFITM.PALLET_NUMBER |
| TO_STORAGE_BIN | EWM | /SCWM/AQUA | LGPLA | /SCWM/AQUA.HU = ZPALTRFITM.PALLET_NUMBER |
| FG_QUANTITY | EWM | /SCWM/AQUA | QUAN | /SCWM/AQUA.HU = ZPALTRFITM.PALLET_NUMBER |
| BASE_UOM | EWM | /SCWM/AQUA | ALTME | /SCWM/AQUA.HU = ZPALTRFITM.PALLET_NUMBER |
| BATCH | EWM | /SCWM/AQUA | CHARG | /SCWM/AQUA.HU = ZPALTRFITM.PALLET_NUMBER |
| STOCK_TYPE | EWM | /SCWM/AQUA | CAT | /SCWM/AQUA.HU = ZPALTRFITM.PALLET_NUMBER |
| SALES_ORDER | EWM | /SCWM/AQUA | STOCK_DOCNO | /SCWM/AQUA.HU = ZPALTRFITM.PALLET_NUMBER |
| SO_ITEM | EWM | /SCWM/AQUA | STOCK_ITEMNO | /SCWM/AQUA.HU = ZPALTRFITM.PALLET_NUMBER |
| USER_ID | ECC | YSTKDTLS | CREATED_BY | YSTKDTLS.USRNO = ZPALTRFITM.PALLET_NUMBER |
| PALLET_GRN_DATE | EWM | /SCWM/AQUA | WDATU | /SCWM/AQUA.HU = ZPALTRFITM.PALLET_NUMBER |

### 5.2 Derived Fields

| Field | Derivation Logic |
|-------|-----------------|
| SFG_QUANTITY | To be determined based on business logic (from YSTKDTLS or calculated) |
| PALLET_PROD_DATE | To be determined based on business logic (from production data) |
| CONVERSION_STATUS | Derived from ZSCM_SFG_CONVERSION_STATUS function module |
| ERROR_DETAILS | Derived from ZSCM_SFG_CONVERSION_STATUS when status indicates error |

---

## 6. User Interface Requirements

### 6.1 Selection Screen Parameters

#### 6.1.1 Required Parameters
- **Date Range:**
  - From Date (P_DFROM) - Type: DATUM, Required
  - To Date (P_DTO) - Type: DATUM, Required
  - Default: Current date - 7 days to Current date

- **Activity Type:**
  - Select-Options (S_ACTIV) - Type: CHAR(20), Required
  - Values: SO_ASSIGNED, SO_UNASSIGNED, SFG_FG_CONVERSION
  - Default: All values

#### 6.1.2 Optional Parameters
- **Pallet Number:**
  - Select-Options (S_PALNO) - Type: CHAR(20), Optional
  
- **Plant:**
  - Select-Options (S_WERKS) - Type: WERKS_D, Optional

- **Sales Order:**
  - Select-Options (S_VBELN) - Type: VBELN_VL, Optional

- **Product:**
  - Select-Options (S_MATNR) - Type: MATNR, Optional

### 6.2 Output Display

#### 6.2.1 ALV Grid Display
- Display results in ALV grid format
- Support sorting, filtering, and Excel export
- Column optimization for readability
- Color coding for status:
  - Green: PUTAWAY_DONE
  - Yellow: IBD_DISTRIB_PENDING, IBD_CREATION_PENDING
  - Red: CONV_DOC_NOT_POSTED or Error status

#### 6.2.2 Display Features
- Subtotals by Plant and Status
- Grand totals for quantities
- Drill-down capability to view detailed transaction
- Navigation to related transactions (IBD, Material Document)

---

## 7. Error Handling and Validation

### 7.1 Input Validation Errors

| Error ID | Error Message | Resolution |
|----------|--------------|------------|
| E001 | Date range is invalid: From date cannot be greater than To date | Correct date range |
| E002 | Date range cannot exceed 365 days | Reduce date range |
| E003 | At least one activity type must be selected | Select activity type |
| E004 | Invalid activity type: &1 | Use valid activity type |

### 7.2 Processing Errors

| Error ID | Error Message | Resolution |
|----------|--------------|------------|
| E101 | No authorization for plant &1 | Request authorization |
| E102 | Unable to connect to ECC system | Check RFC connection |
| E103 | No data found for selection criteria | Modify selection criteria |
| E104 | System error accessing table &1: &2 | Contact system administrator |

### 7.3 Warning Messages

| Warning ID | Warning Message | Action |
|------------|----------------|--------|
| W001 | Large date range may impact performance | Consider reducing range |
| W002 | Some pallet records missing ECC data | Review data completeness |
| W003 | Product description not found for material &1 | Master data maintenance needed |

---

## 8. Performance Requirements

### 8.1 Response Time Targets
- Online execution (< 1000 records): ≤ 3 seconds
- Batch execution (1000-10000 records): ≤ 30 seconds
- Large volume (> 10000 records): Background job recommended

### 8.2 Performance Optimization
- Use database indexes on:
  - ZPALTRFHDR: CREATED_ON
  - ZPALTRFITM: TRANSACTION_ID, PALLET_NUMBER
  - YSTKDTLS: USRNO, CREATED_ON
  - /SCWM/AQUA: HU number fields
- Implement parallel processing for large data volumes
- Use buffering for master data (product descriptions)
- Minimize RFC calls by batching ECC data retrieval

---

## 9. Security and Authorization

### 9.1 Authorization Objects
- **S_TABU_NAM:** Table access authorization
  - ZPALTRFHDR, ZPALTRFITM: Read
- **S_DATASET:** Plant authorization
  - Check authorization for plants in IT_PLANT
- **M_MATE_WRK:** Material authorization
  - Check for material access if needed

### 9.2 Data Privacy
- No PII (Personally Identifiable Information) in output
- User ID displayed only if authorized
- Sensitive data masking based on authorization

---

## 10. Testing Requirements

### 10.1 Unit Test Cases

| Test ID | Test Description | Input | Expected Output |
|---------|-----------------|-------|-----------------|
| UT-001 | Valid date range with data | Date range with existing data | Display records |
| UT-002 | Invalid date range | From > To | Error E001 |
| UT-003 | No data for criteria | Future date range | Error E103 or empty result |
| UT-004 | Single pallet filter | Specific pallet number | Records for that pallet |
| UT-005 | Status determination - Putaway done | Pallet with VBUK-WBSTK = 'C' | Status: PUTAWAY_DONE |
| UT-006 | Status determination - IBD pending | Pallet with LIKP-VLSTK = 'B' | Status: IBD_DISTRIB_PENDING |
| UT-007 | Status determination - No IBD | Pallet without IBD | Status: IBD_CREATION_PENDING |
| UT-008 | Status determination - Not posted | Pallet not in YSTKDTLS | Status: CONV_DOC_NOT_POSTED |

### 10.2 Integration Test Cases

| Test ID | Test Description | Test Scenario |
|---------|-----------------|---------------|
| IT-001 | ECC-EWM data integration | Verify data retrieval from both systems |
| IT-002 | Large volume processing | Test with > 5000 records |
| IT-003 | Cross-system performance | Measure RFC call overhead |
| IT-004 | Concurrent user access | Multiple users running report simultaneously |

### 10.3 User Acceptance Test Cases

| Test ID | Test Description | Acceptance Criteria |
|---------|-----------------|---------------------|
| UAT-001 | Business user executes report | Report displays within 5 seconds for typical data volume |
| UAT-002 | Excel export functionality | All columns exported correctly with formatting |
| UAT-003 | Status accuracy | Status matches actual conversion state |
| UAT-004 | Error scenario handling | Clear error messages displayed to user |

---

## 11. Assumptions and Dependencies

### 11.1 Assumptions
1. RFC connection between EWM and ECC is established and configured
2. User has necessary authorizations for both systems
3. Table YSTKDTLS uses field USRNO to store pallet numbers
4. IBD number is stored in YSTKDTLS-VBELN field
5. Activity type classification logic exists in ZPALTRFHDR
6. Product descriptions maintained in /SAPAPO/MATKEY

### 11.2 Dependencies
1. Custom tables ZPALTRFHDR and ZPALTRFITM must exist with specified fields
2. ECC tables YSTKDTLS, MSEG, MKPF must be accessible from EWM
3. SAP standard tables VBUK and LIKP must be available
4. RFC destination or database link configured for ECC access
5. Data consistency between ECC and EWM systems

### 11.3 Clarifications Needed
1. **SFG_QUANTITY derivation:** How to calculate/retrieve SFG quantity?
2. **PALLET_PROD_DATE source:** Which table/field contains pallet production date?
3. **Activity classification:** How to determine activity type from transaction data?
4. **Material document linkage:** How to link YSTKDTLS records to MSEG/MKPF?
5. **Multiple movement types:** How to differentiate FROM_SLOC and TO_SLOC in MSEG?

---

## 12. Open Items

| Item ID | Description | Priority | Assigned To |
|---------|-------------|----------|-------------|
| OI-001 | Confirm SFG quantity calculation logic | High | Business Team |
| OI-002 | Provide pallet production date source | High | Business Team |
| OI-003 | Define activity type classification rules | High | Business Team |
| OI-004 | Clarify material document linkage | High | Technical Team |
| OI-005 | Confirm RFC destination name for ECC | Medium | Basis Team |
| OI-006 | Define background job scheduling requirements | Medium | Business Team |
| OI-007 | Confirm authorization object usage | Medium | Security Team |

---

## 13. Approval

| Role | Name | Signature | Date |
|------|------|-----------|------|
| Business Owner | | | |
| Functional Lead | | | |
| Technical Lead | | | |
| Security Reviewer | | | |

---

## 14. Revision History

| Version | Date | Author | Description |
|---------|------|--------|-------------|
| 1.0 | 2025-12-31 | System | Initial creation based on BRD |

---

## Appendix A: Field Mappings Reference

### A.1 ZPALTRFHDR Table Structure
| Field Name | Data Element | Description |
|-----------|--------------|-------------|
| TRANSACTION_ID | CHAR20 | Unique transaction identifier |
| CREATED_ON | DATS | Creation date |
| ACTIVITY_TYPE | CHAR20 | Activity classification |
| ... | ... | Additional fields as per table definition |

### A.2 ZPALTRFITM Table Structure
| Field Name | Data Element | Description |
|-----------|--------------|-------------|
| TRANSACTION_ID | CHAR20 | Reference to header |
| PALLET_NUMBER | CHAR20 | Handling unit/pallet number |
| ... | ... | Additional fields as per table definition |

### A.3 YSTKDTLS Table Structure
| Field Name | Data Element | Description |
|-----------|--------------|-------------|
| USRNO | CHAR20 | Pallet number |
| VBELN | VBELN_VL | IBD number |
| CREATED_ON | DATS | Creation date |
| CREATED_TM | TIMS | Creation time |
| SOURCEWERKS | WERKS_D | Source plant |
| TUBE_M | CHAR18 | Storage bin |
| CREATED_BY | SYUNAME | Created by user |
| ... | ... | Additional fields as per table definition |

---

## Appendix B: Status Determination Flow Diagram

```
[Pallet Number Input]
        ↓
[Query YSTKDTLS by USRNO]
        ↓
    ┌───────────────────┐
    │ Pallet Found?     │
    └─────┬─────────┬───┘
         NO        YES
          ↓         ↓
    [Return      [Get IBD Number (VBELN)]
     Status:           ↓
     CONV_DOC_     ┌─────────────┐
     NOT_POSTED]   │ IBD Exists? │
                   └──┬──────┬───┘
                     NO     YES
                      ↓      ↓
                [Return   [Query VBUK]
                 Status:      ↓
                 IBD_    ┌────────────┐
                 CREATION│ WBSTK='C'? │
                 PENDING]└──┬────┬────┘
                           NO   YES
                            ↓    ↓
                      [Query  [Return
                       LIKP]   Status:
                         ↓     PUTAWAY_
                    ┌────────┐ DONE]
                    │VLSTK='B'│
                    └──┬───┬──┘
                      YES  NO
                       ↓    ↓
                  [Return [Return
                   Status: Status:
                   IBD_    IBD_
                   DISTRIB_ DISTRIB_
                   PENDING] PENDING]
```

---

**End of Functional Specification**


