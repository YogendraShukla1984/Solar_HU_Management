*&---------------------------------------------------------------------*
*& Include ZSCM_SOLAR_ACTIVITY_RPTC01
*& Class Definitions and Implementations
*&---------------------------------------------------------------------*

" BEGIN: Cursor Generated Code

*&---------------------------------------------------------------------*
*& Class LCL_REPORT_PROCESSOR
*&---------------------------------------------------------------------*
*& Purpose: Main report processing logic
*&---------------------------------------------------------------------*
CLASS lcl_report_processor DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor,

      execute_report
        IMPORTING
          iv_date_from  TYPE datum
          iv_date_to    TYPE datum
          it_activity   TYPE gty_t_activity
          it_pallet_no  TYPE gty_t_pallet OPTIONAL
          it_plant      TYPE gty_t_plant OPTIONAL
        EXPORTING
          et_output     TYPE gty_t_report_output
          ev_count      TYPE i,

      validate_inputs
        IMPORTING
          iv_date_from TYPE datum
          iv_date_to   TYPE datum
        RAISING
          cx_parameter_invalid.

  PRIVATE SECTION.
    DATA: mt_output TYPE gty_t_report_output,
          mv_count  TYPE i.

    METHODS:
      call_function_module
        IMPORTING
          iv_date_from  TYPE datum
          iv_date_to    TYPE datum
          it_activity   TYPE gty_t_activity
          it_pallet_no  TYPE gty_t_pallet OPTIONAL
          it_plant      TYPE gty_t_plant OPTIONAL
        EXPORTING
          et_output     TYPE gty_t_report_output
          ev_count      TYPE i
        RAISING
          cx_sy_dyn_call_illegal_func,

      convert_ranges_to_tables
        IMPORTING
          it_activity_range TYPE gty_rt_activity
          it_pallet_range   TYPE gty_rt_pallet
          it_plant_range    TYPE gty_rt_plant
        EXPORTING
          et_activity       TYPE gty_t_activity
          et_pallet         TYPE gty_t_pallet
          et_plant          TYPE gty_t_plant.
ENDCLASS.

CLASS lcl_report_processor IMPLEMENTATION.
  METHOD constructor.
    " Initialize
    CLEAR: mt_output, mv_count.
  ENDMETHOD.

  METHOD validate_inputs.
    " Validate date range
    IF iv_date_from > iv_date_to.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    " Check for reasonable date range (not more than 1 year)
    IF iv_date_to - iv_date_from > 365.
      " Warning: Large date range may impact performance
      " But don't fail - just warn in log
    ENDIF.
  ENDMETHOD.

  METHOD execute_report.
    DATA: lo_exception TYPE REF TO cx_root,
          lv_subrc     TYPE sy-subrc.

    TRY.
        " Validate inputs
        validate_inputs(
          iv_date_from = iv_date_from
          iv_date_to   = iv_date_to ).

        " Call function module
        call_function_module(
          EXPORTING
            iv_date_from  = iv_date_from
            iv_date_to    = iv_date_to
            it_activity   = it_activity
            it_pallet_no  = it_pallet_no
            it_plant      = it_plant
          IMPORTING
            et_output     = et_output
            ev_count      = ev_count ).

      CATCH cx_parameter_invalid INTO lo_exception.
        MESSAGE lo_exception->get_text( ) TYPE 'E'.

      CATCH cx_sy_dyn_call_illegal_func INTO lo_exception.
        MESSAGE 'Function module call failed'(e02) TYPE 'E'.

      CATCH cx_root INTO lo_exception.
        MESSAGE lo_exception->get_text( ) TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD call_function_module.
    DATA: lv_subrc TYPE sy-subrc.

    CALL FUNCTION 'ZSCM_SOLAR_ACTIVITY_REPORT'
      EXPORTING
        iv_date_from     = iv_date_from
        iv_date_to       = iv_date_to
        it_activity      = it_activity
        it_pallet_no     = it_pallet_no
        it_plant         = it_plant
      IMPORTING
        et_output        = et_output
        ev_record_count  = ev_count
      EXCEPTIONS
        date_range_invalid    = 1
        no_data_found         = 2
        activity_type_invalid = 3
        ecc_connection_error  = 4
        authorization_failed  = 5
        OTHERS                = 6.

    lv_subrc = sy-subrc.

    CASE lv_subrc.
      WHEN 1.
        MESSAGE e001(zscm_sfg). " Date range invalid
      WHEN 2.
        MESSAGE i002(zscm_sfg). " No data found
      WHEN 3.
        MESSAGE e003(zscm_sfg). " Invalid activity type
      WHEN 4.
        MESSAGE e004(zscm_sfg). " ECC connection error
      WHEN 5.
        MESSAGE e005(zscm_sfg). " Authorization failed
      WHEN OTHERS.
        MESSAGE e999(zscm_sfg). " Unexpected error
    ENDCASE.
  ENDMETHOD.

  METHOD convert_ranges_to_tables.
    DATA: lw_activity_range TYPE gty_r_activity,
          lw_pallet_range   TYPE gty_r_pallet,
          lw_plant_range    TYPE gty_r_plant,
          lw_activity       TYPE gty_activity,
          lw_pallet         TYPE gty_pallet,
          lw_plant          TYPE gty_plant.

    " Convert activity ranges
    LOOP AT it_activity_range INTO lw_activity_range.
      IF lw_activity_range-sign = 'I' AND
         lw_activity_range-option = 'EQ'.
        CLEAR: lw_activity.
        lw_activity-activity = lw_activity_range-low.
        APPEND lw_activity TO et_activity.
      ENDIF.
    ENDLOOP.

    " Convert pallet ranges
    LOOP AT it_pallet_range INTO lw_pallet_range.
      IF lw_pallet_range-sign = 'I' AND
         lw_pallet_range-option = 'EQ'.
        CLEAR: lw_pallet.
        lw_pallet-pallet_no = lw_pallet_range-low.
        APPEND lw_pallet TO et_pallet.
      ENDIF.
    ENDLOOP.

    " Convert plant ranges
    LOOP AT it_plant_range INTO lw_plant_range.
      IF lw_plant_range-sign = 'I' AND
         lw_plant_range-option = 'EQ'.
        CLEAR: lw_plant.
        lw_plant-werks = lw_plant_range-low.
        APPEND lw_plant TO et_plant.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Class LCL_ALV_DISPLAY
*&---------------------------------------------------------------------*
*& Purpose: ALV grid display logic
*&---------------------------------------------------------------------*
CLASS lcl_alv_display DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      display_alv
        IMPORTING
          it_output TYPE gty_t_report_output.

  PRIVATE SECTION.
    METHODS:
      build_fieldcat
        RETURNING
          VALUE(rt_fieldcat) TYPE lvc_t_fcat,

      set_layout
        RETURNING
          VALUE(rs_layout) TYPE lvc_s_layo,

      set_colors
        CHANGING
          ct_output TYPE gty_t_report_output.
ENDCLASS.

CLASS lcl_alv_display IMPLEMENTATION.
  METHOD display_alv.
    DATA: lt_fieldcat TYPE lvc_t_fcat,
          ls_layout   TYPE lvc_s_layo,
          lt_output   TYPE gty_t_report_output.

    lt_output = it_output.

    " Build field catalog
    lt_fieldcat = build_fieldcat( ).

    " Set layout
    ls_layout = set_layout( ).

    " Set colors
    set_colors( CHANGING ct_output = lt_output ).

    " Display ALV
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program = sy-repid
        is_layout          = ls_layout
        it_fieldcat        = lt_fieldcat
      TABLES
        t_outtab           = lt_output
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.

    IF sy-subrc <> 0.
      MESSAGE 'Error displaying ALV grid'(e01) TYPE 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD build_fieldcat.
    DATA: lw_fieldcat TYPE lvc_s_fcat,
          lv_col_pos  TYPE i.

    lv_col_pos = 0.

    " Transaction ID
    lv_col_pos = lv_col_pos + 1.
    CLEAR: lw_fieldcat.
    lw_fieldcat-fieldname = 'TRANSACTION_ID'.
    lw_fieldcat-seltext_l = 'Transaction ID'.
    lw_fieldcat-seltext_m = 'Trans ID'.
    lw_fieldcat-seltext_s = 'Trans'.
    lw_fieldcat-col_pos = lv_col_pos.
    lw_fieldcat-outputlen = 20.
    APPEND lw_fieldcat TO rt_fieldcat.

    " Transaction Date
    lv_col_pos = lv_col_pos + 1.
    CLEAR: lw_fieldcat.
    lw_fieldcat-fieldname = 'TRANSACTION_DATE'.
    lw_fieldcat-seltext_l = 'Transaction Date'.
    lw_fieldcat-seltext_m = 'Trans Date'.
    lw_fieldcat-seltext_s = 'Date'.
    lw_fieldcat-col_pos = lv_col_pos.
    lw_fieldcat-outputlen = 10.
    APPEND lw_fieldcat TO rt_fieldcat.

    " Handling Unit
    lv_col_pos = lv_col_pos + 1.
    CLEAR: lw_fieldcat.
    lw_fieldcat-fieldname = 'HANDLING_UNIT'.
    lw_fieldcat-seltext_l = 'Handling Unit / Pallet'.
    lw_fieldcat-seltext_m = 'Handling Unit'.
    lw_fieldcat-seltext_s = 'HU'.
    lw_fieldcat-col_pos = lv_col_pos.
    lw_fieldcat-outputlen = 20.
    lw_fieldcat-key = abap_true.
    APPEND lw_fieldcat TO rt_fieldcat.

    " Product
    lv_col_pos = lv_col_pos + 1.
    CLEAR: lw_fieldcat.
    lw_fieldcat-fieldname = 'PRODUCT'.
    lw_fieldcat-seltext_l = 'Product'.
    lw_fieldcat-seltext_m = 'Product'.
    lw_fieldcat-seltext_s = 'Product'.
    lw_fieldcat-col_pos = lv_col_pos.
    lw_fieldcat-outputlen = 18.
    APPEND lw_fieldcat TO rt_fieldcat.

    " Product Description
    lv_col_pos = lv_col_pos + 1.
    CLEAR: lw_fieldcat.
    lw_fieldcat-fieldname = 'PRODUCT_DESC'.
    lw_fieldcat-seltext_l = 'Product Description'.
    lw_fieldcat-seltext_m = 'Prod Desc'.
    lw_fieldcat-seltext_s = 'Desc'.
    lw_fieldcat-col_pos = lv_col_pos.
    lw_fieldcat-outputlen = 40.
    APPEND lw_fieldcat TO rt_fieldcat.

    " Plant
    lv_col_pos = lv_col_pos + 1.
    CLEAR: lw_fieldcat.
    lw_fieldcat-fieldname = 'PLANT'.
    lw_fieldcat-seltext_l = 'Plant'.
    lw_fieldcat-seltext_m = 'Plant'.
    lw_fieldcat-seltext_s = 'Plnt'.
    lw_fieldcat-col_pos = lv_col_pos.
    lw_fieldcat-outputlen = 4.
    APPEND lw_fieldcat TO rt_fieldcat.

    " Transfer Start Date
    lv_col_pos = lv_col_pos + 1.
    CLEAR: lw_fieldcat.
    lw_fieldcat-fieldname = 'TRANSFER_START_DATE'.
    lw_fieldcat-seltext_l = 'Transfer Start Date'.
    lw_fieldcat-seltext_m = 'Start Date'.
    lw_fieldcat-seltext_s = 'Start Dt'.
    lw_fieldcat-col_pos = lv_col_pos.
    lw_fieldcat-outputlen = 10.
    APPEND lw_fieldcat TO rt_fieldcat.

    " Transfer Start Time
    lv_col_pos = lv_col_pos + 1.
    CLEAR: lw_fieldcat.
    lw_fieldcat-fieldname = 'TRANSFER_START_TIME'.
    lw_fieldcat-seltext_l = 'Transfer Start Time'.
    lw_fieldcat-seltext_m = 'Start Time'.
    lw_fieldcat-seltext_s = 'Start Tm'.
    lw_fieldcat-col_pos = lv_col_pos.
    lw_fieldcat-outputlen = 8.
    APPEND lw_fieldcat TO rt_fieldcat.

    " FG Receipt Date
    lv_col_pos = lv_col_pos + 1.
    CLEAR: lw_fieldcat.
    lw_fieldcat-fieldname = 'FG_RECEIPT_DATE'.
    lw_fieldcat-seltext_l = 'FG Receipt Date'.
    lw_fieldcat-seltext_m = 'FG Date'.
    lw_fieldcat-seltext_s = 'FG Dt'.
    lw_fieldcat-col_pos = lv_col_pos.
    lw_fieldcat-outputlen = 10.
    APPEND lw_fieldcat TO rt_fieldcat.

    " FG Receipt Time
    lv_col_pos = lv_col_pos + 1.
    CLEAR: lw_fieldcat.
    lw_fieldcat-fieldname = 'FG_RECEIPT_TIME'.
    lw_fieldcat-seltext_l = 'FG Receipt Time'.
    lw_fieldcat-seltext_m = 'FG Time'.
    lw_fieldcat-seltext_s = 'FG Tm'.
    lw_fieldcat-col_pos = lv_col_pos.
    lw_fieldcat-outputlen = 8.
    APPEND lw_fieldcat TO rt_fieldcat.

    " FG Quantity
    lv_col_pos = lv_col_pos + 1.
    CLEAR: lw_fieldcat.
    lw_fieldcat-fieldname = 'FG_QUANTITY'.
    lw_fieldcat-seltext_l = 'FG Quantity'.
    lw_fieldcat-seltext_m = 'FG Qty'.
    lw_fieldcat-seltext_s = 'FG Qty'.
    lw_fieldcat-col_pos = lv_col_pos.
    lw_fieldcat-outputlen = 13.
    lw_fieldcat-do_sum = abap_true.
    APPEND lw_fieldcat TO rt_fieldcat.

    " Base UOM
    lv_col_pos = lv_col_pos + 1.
    CLEAR: lw_fieldcat.
    lw_fieldcat-fieldname = 'BASE_UOM'.
    lw_fieldcat-seltext_l = 'Unit'.
    lw_fieldcat-seltext_m = 'Unit'.
    lw_fieldcat-seltext_s = 'Unit'.
    lw_fieldcat-col_pos = lv_col_pos.
    lw_fieldcat-outputlen = 3.
    APPEND lw_fieldcat TO rt_fieldcat.

    " Batch
    lv_col_pos = lv_col_pos + 1.
    CLEAR: lw_fieldcat.
    lw_fieldcat-fieldname = 'BATCH'.
    lw_fieldcat-seltext_l = 'Batch'.
    lw_fieldcat-seltext_m = 'Batch'.
    lw_fieldcat-seltext_s = 'Batch'.
    lw_fieldcat-col_pos = lv_col_pos.
    lw_fieldcat-outputlen = 10.
    APPEND lw_fieldcat TO rt_fieldcat.

    " Sales Order
    lv_col_pos = lv_col_pos + 1.
    CLEAR: lw_fieldcat.
    lw_fieldcat-fieldname = 'SALES_ORDER'.
    lw_fieldcat-seltext_l = 'Sales Order'.
    lw_fieldcat-seltext_m = 'Sales Order'.
    lw_fieldcat-seltext_s = 'SO'.
    lw_fieldcat-col_pos = lv_col_pos.
    lw_fieldcat-outputlen = 10.
    APPEND lw_fieldcat TO rt_fieldcat.

    " SO Item
    lv_col_pos = lv_col_pos + 1.
    CLEAR: lw_fieldcat.
    lw_fieldcat-fieldname = 'SO_ITEM'.
    lw_fieldcat-seltext_l = 'SO Item'.
    lw_fieldcat-seltext_m = 'SO Item'.
    lw_fieldcat-seltext_s = 'Item'.
    lw_fieldcat-col_pos = lv_col_pos.
    lw_fieldcat-outputlen = 6.
    APPEND lw_fieldcat TO rt_fieldcat.

    " Conversion Status
    lv_col_pos = lv_col_pos + 1.
    CLEAR: lw_fieldcat.
    lw_fieldcat-fieldname = 'CONVERSION_STATUS'.
    lw_fieldcat-seltext_l = 'Conversion Status'.
    lw_fieldcat-seltext_m = 'Status'.
    lw_fieldcat-seltext_s = 'Status'.
    lw_fieldcat-col_pos = lv_col_pos.
    lw_fieldcat-outputlen = 30.
    lw_fieldcat-emphasize = 'C500'. " Color column
    APPEND lw_fieldcat TO rt_fieldcat.

    " Error Details
    lv_col_pos = lv_col_pos + 1.
    CLEAR: lw_fieldcat.
    lw_fieldcat-fieldname = 'ERROR_DETAILS'.
    lw_fieldcat-seltext_l = 'Error Details / Message'.
    lw_fieldcat-seltext_m = 'Error Details'.
    lw_fieldcat-seltext_s = 'Error'.
    lw_fieldcat-col_pos = lv_col_pos.
    lw_fieldcat-outputlen = 50.
    APPEND lw_fieldcat TO rt_fieldcat.

    " User ID
    lv_col_pos = lv_col_pos + 1.
    CLEAR: lw_fieldcat.
    lw_fieldcat-fieldname = 'USER_ID'.
    lw_fieldcat-seltext_l = 'User ID'.
    lw_fieldcat-seltext_m = 'User'.
    lw_fieldcat-seltext_s = 'User'.
    lw_fieldcat-col_pos = lv_col_pos.
    lw_fieldcat-outputlen = 12.
    APPEND lw_fieldcat TO rt_fieldcat.
  ENDMETHOD.

  METHOD set_layout.
    " Set zebra pattern
    rs_layout-zebra = abap_true.

    " Optimize column width
    rs_layout-cwidth_opt = abap_true.

    " Column optimization
    rs_layout-col_opt = abap_true.

    " Allow column selection
    rs_layout-box_fname = ''.

    " Set list header
    rs_layout-grid_title = 'Solar Activity Report - SFG to FG Conversion'.
  ENDMETHOD.

  METHOD set_colors.
    FIELD-SYMBOLS: <lfs_output> TYPE gty_report_output.

    LOOP AT ct_output ASSIGNING <lfs_output>.
      " Set color based on status
      " Note: Color coding requires color field in output structure
      " This is a placeholder for color logic
      CASE <lfs_output>-conversion_status.
        WHEN gc_status_putaway.
          " Green - Success
        WHEN gc_status_ibd_pending OR gc_status_ibd_create.
          " Yellow - In Progress
        WHEN gc_status_not_posted.
          " Red - Error
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

" END: Cursor Generated Code


