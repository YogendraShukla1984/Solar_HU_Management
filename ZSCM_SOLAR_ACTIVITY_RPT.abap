*&---------------------------------------------------------------------*
*& Report ZSCM_SOLAR_ACTIVITY_RPT
*&---------------------------------------------------------------------*
*& Purpose: Solar Activity Report - SFG to FG Conversion Tracking
*& Author: System
*& Creation Date: 2025-12-31
*& Change History:
*& Date       User    Description
*& 31.12.2025 SYSTEM  Initial creation
*&---------------------------------------------------------------------*
REPORT zscm_solar_activity_rpt MESSAGE-ID zscm_sfg.

" BEGIN: Cursor Generated Code

" Include declarations
INCLUDE zscm_solar_activity_rpttop.  " Global declarations
INCLUDE zscm_solar_activity_rptc01.  " Class definitions

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
  PARAMETERS: p_dfrom TYPE datum DEFAULT sy-datum OBLIGATORY,
              p_dto   TYPE datum DEFAULT sy-datum OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
  SELECT-OPTIONS: s_activ FOR gv_activity,
                  s_palno FOR gv_pallet_no,
                  s_werks FOR gv_plant,
                  s_vbeln FOR gv_sales_order,
                  s_matnr FOR gv_product.
SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  " Set default dates (last 7 days)
  p_dfrom = sy-datum - 7.
  p_dto = sy-datum.

*----------------------------------------------------------------------*
* Input validation
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM validate_selection_screen.

*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM execute_report.

*----------------------------------------------------------------------*
* End of selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM display_output.

*&---------------------------------------------------------------------*
*&      Form  VALIDATE_SELECTION_SCREEN
*&---------------------------------------------------------------------*
FORM validate_selection_screen.
  DATA: lv_error TYPE abap_bool.

  CLEAR: lv_error.

  " Validate date range
  IF p_dfrom > p_dto.
    MESSAGE e001(zscm_sfg). " Date range invalid
    lv_error = abap_true.
  ENDIF.

  " Validate activity type if provided
  IF s_activ[] IS NOT INITIAL.
    " Additional validation if needed
  ENDIF.

  IF lv_error = abap_true.
    " Stop processing
    LEAVE PROGRAM.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EXECUTE_REPORT
*&---------------------------------------------------------------------*
FORM execute_report.
  DATA: lo_processor TYPE REF TO lcl_report_processor,
        lo_exception TYPE REF TO cx_root,
        lv_error_msg TYPE string.

  TRY.
      " Create report processor instance
      CREATE OBJECT lo_processor.

      " Execute report
      lo_processor->execute_report(
        EXPORTING
          iv_date_from  = p_dfrom
          iv_date_to    = p_dto
          it_activity   = gt_activity
          it_pallet_no  = gt_pallet_no
          it_plant      = gt_plant
        IMPORTING
          et_output     = gt_output
          ev_count      = gv_record_count ).

    CATCH cx_root INTO lo_exception.
      lv_error_msg = lo_exception->get_text( ).
      MESSAGE lv_error_msg TYPE 'E'.
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_OUTPUT
*&---------------------------------------------------------------------*
FORM display_output.
  DATA: lo_alv_display TYPE REF TO lcl_alv_display.

  IF gt_output IS INITIAL.
    MESSAGE i002(zscm_sfg). " No data found
    RETURN.
  ENDIF.

  " Create ALV display instance
  CREATE OBJECT lo_alv_display.

  " Display ALV
  lo_alv_display->display_alv(
    it_output = gt_output ).
ENDFORM.

" END: Cursor Generated Code


