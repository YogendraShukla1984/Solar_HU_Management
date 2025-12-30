*&---------------------------------------------------------------------*
*& Include LZSOLAR_VALTOP
*&---------------------------------------------------------------------*
*& Purpose: Global declarations for ZSOLAR_VAL function group
*& Author: [Author Name]
*& Date: [Creation Date]
*&---------------------------------------------------------------------*

" BEGIN: Cursor Generated Code

*----------------------------------------------------------------------*
* Type Definitions for HU Validation
*----------------------------------------------------------------------*
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

*----------------------------------------------------------------------*
* Type Definitions for SO/SLOC Validation
*----------------------------------------------------------------------*
TYPES: BEGIN OF gty_vbak_data,
         vbeln TYPE vbeln_va,
         erdat TYPE erdat,
         kunnr TYPE kunnr,
       END OF gty_vbak_data.

TYPES: BEGIN OF gty_vbap_data,
         vbeln TYPE vbeln_va,
         posnr TYPE posnr_va,
         matnr TYPE matnr,
         werks TYPE werks_d,
       END OF gty_vbap_data.

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*
CONSTANTS: gc_valid TYPE char1 VALUE 'X',
           gc_invalid TYPE char1 VALUE space.

" END: Cursor Generated Code

