*&---------------------------------------------------------------------*
*& Include LZSOLARTOP
*&---------------------------------------------------------------------*
*& Purpose: Global declarations for ZSOLAR function group
*& Author: [Author Name]
*& Date: [Creation Date]
*&---------------------------------------------------------------------*

" BEGIN: Cursor Generated Code

*----------------------------------------------------------------------*
* Type Definitions for Main Processing
*----------------------------------------------------------------------*
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

TYPES: BEGIN OF gty_input_data,
         matnr TYPE matnr,
       END OF gty_input_data.

TYPES: gty_input_data_tab TYPE STANDARD TABLE OF gty_input_data.

TYPES: BEGIN OF gty_output_data,
         lgnum     TYPE /scwm/lgnum,
         stock_id  TYPE char20,
         parent_id TYPE char20,
       END OF gty_output_data.

TYPES: gty_output_data_tab TYPE STANDARD TABLE OF gty_output_data.

TYPES: BEGIN OF gty_posting_input,
         wh_number        TYPE /scwm/lgnum,
         screen_id        TYPE char2,
         stock_id         TYPE char20,
         parent_id        TYPE char20,
         dest_stockdocno  TYPE vbeln_va,
         dest_stockitmno  TYPE posnr_va,
         im_location      TYPE lgort_d,
       END OF gty_posting_input.

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*
CONSTANTS: gc_batch_size TYPE i VALUE 10,
           gc_activity_p1 TYPE char2 VALUE 'P1',
           gc_activity_p2 TYPE char2 VALUE 'P2',
           gc_activity_a2 TYPE char2 VALUE 'A2',
           gc_status_running TYPE char10 VALUE 'RUNNING',
           gc_status_completed TYPE char10 VALUE 'COMPLETED',
           gc_status_error TYPE char10 VALUE 'ERROR',
           gc_valid TYPE char1 VALUE 'X',
           gc_invalid TYPE char1 VALUE space.

" END: Cursor Generated Code

