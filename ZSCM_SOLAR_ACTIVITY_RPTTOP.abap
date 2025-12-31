*&---------------------------------------------------------------------*
*& Include ZSCM_SOLAR_ACTIVITY_RPTTOP
*& Global Data Declarations
*&---------------------------------------------------------------------*

" BEGIN: Cursor Generated Code

*----------------------------------------------------------------------*
* Type Definitions for Selection Screen
*----------------------------------------------------------------------*

" Global variables for selection screen
DATA: gv_activity     TYPE char20,
      gv_pallet_no    TYPE char20,
      gv_plant        TYPE werks_d,
      gv_sales_order  TYPE vbeln_vl,
      gv_product      TYPE matnr.

*----------------------------------------------------------------------*
* Global Type Definitions (reuse from function group)
*----------------------------------------------------------------------*

" Activity type table
TYPES: BEGIN OF gty_r_activity,
         sign   TYPE char1,
         option TYPE char2,
         low    TYPE char20,
         high   TYPE char20,
       END OF gty_r_activity.

TYPES: gty_rt_activity TYPE STANDARD TABLE OF gty_r_activity.

" Pallet number range table
TYPES: BEGIN OF gty_r_pallet,
         sign   TYPE char1,
         option TYPE char2,
         low    TYPE char20,
         high   TYPE char20,
       END OF gty_r_pallet.

TYPES: gty_rt_pallet TYPE STANDARD TABLE OF gty_r_pallet.

" Plant range table
TYPES: BEGIN OF gty_r_plant,
         sign   TYPE char1,
         option TYPE char2,
         low    TYPE werks_d,
         high   TYPE werks_d,
       END OF gty_r_plant.

TYPES: gty_rt_plant TYPE STANDARD TABLE OF gty_r_plant.

*----------------------------------------------------------------------*
* Global Data Objects
*----------------------------------------------------------------------*

" Report processor
DATA: go_report       TYPE REF TO lcl_report_processor,
      go_alv          TYPE REF TO lcl_alv_display.

" Output data
DATA: gt_output       TYPE gty_t_report_output,
      gv_record_count TYPE i.

" Selection screen ranges converted to tables
DATA: gt_activity   TYPE gty_t_activity,
      gt_pallet_no  TYPE gty_t_pallet,
      gt_plant      TYPE gty_t_plant.

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*

CONSTANTS: gc_dest_ecc TYPE rfcdest VALUE 'ECC_DEST'.

*----------------------------------------------------------------------*
* Text Symbols
*----------------------------------------------------------------------*
" Text symbols to be maintained in SE63:
" T01: 'Solar Activity Report - SFG to FG Conversion'
"
" Frame titles:
" 001: 'Date Range'
" 002: 'Selection Criteria'
"
" Selection texts:
" P_DFROM   From Date
" P_DTO     To Date
" S_ACTIV   Activity Type
" S_PALNO   Pallet Number
" S_WERKS   Plant
" S_VBELN   Sales Order
" S_MATNR   Product

" END: Cursor Generated Code


