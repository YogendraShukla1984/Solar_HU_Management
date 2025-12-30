*&---------------------------------------------------------------------*
*& Include LZSOLARF01
*&---------------------------------------------------------------------*
*& Purpose: Helper subroutines for ZSOLAR function group
*& Author: [Author Name]
*& Date: [Creation Date]
*&---------------------------------------------------------------------*

" BEGIN: Cursor Generated Code

*&---------------------------------------------------------------------*
*& Form PROCESS_BATCH
*&---------------------------------------------------------------------*
*& Purpose: Process a batch of handling units
*& Parameters: Warehouse, Activity, SO details, HU batch, HU items
*&---------------------------------------------------------------------*
FORM process_batch USING iv_warehouse TYPE /scwm/lgnum
                         iv_activity TYPE char2
                         iv_vbeln TYPE vbeln_va
                         iv_posnr TYPE posnr_va
                         iv_lgort TYPE lgort_d
                         iv_user TYPE syuname
                         it_hu_batch TYPE /scwm/tt_huident
                         it_huitm TYPE /scwm/tt_huitm_int
                  CHANGING cw_message TYPE gty_message.

  " All declarations first
  DATA: lt_input TYPE gty_input_data_tab,
        lw_input TYPE gty_input_data,
        lt_hu_data TYPE gty_hu_data_tab,
        lw_hu_data TYPE gty_hu_data,
        lt_output TYPE gty_output_data_tab,
        lw_output TYPE gty_output_data,
        lw_posting_input TYPE gty_posting_input,
        lv_subrc TYPE sy-subrc,
        lw_huitm TYPE /scwm/s_huitm_int,
        lw_hu_batch TYPE /scwm/s_huident.

  " Initialize
  CLEAR cw_message.
  cw_message-type = 'S'.

  " Prepare input for GET_DET function
  LOOP AT it_hu_batch INTO lw_hu_batch.
    CLEAR lw_huitm.
    READ TABLE it_huitm INTO lw_huitm
      WITH KEY huident = lw_hu_batch-low.
    lv_subrc = sy-subrc.
    IF lv_subrc = 0.
      CLEAR lw_input.
      lw_input-matnr = lw_huitm-matid.
      APPEND lw_input TO lt_input.

      CLEAR lw_hu_data.
      lw_hu_data-hu_id = lw_hu_batch-low.
      lw_hu_data-stk_cat = lw_huitm-cat.
      lw_hu_data-matid = lw_huitm-matid.
      APPEND lw_hu_data TO lt_hu_data.
    ENDIF.
  ENDLOOP.

  " Call GET_DET function
  CALL FUNCTION 'ZSCM_BIN_INTL_POST_GET_DET'
    EXPORTING
      i_warehouse = iv_warehouse
      i_action    = iv_activity
      i_user      = iv_user
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
    cw_message-type = 'E'.
    CONCATENATE 'Error in GET_DET for batch. SY-SUBRC:' lv_subrc
      INTO cw_message-message SEPARATED BY space.
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
      cw_message-type = 'E'.
      CONCATENATE 'Error in POSTING. SY-SUBRC:' lv_subrc
        INTO cw_message-message SEPARATED BY space.
      " Continue with next record
    ENDIF.

    COMMIT WORK AND WAIT.
  ENDLOOP.

  " Success message if no errors
  IF cw_message-type = 'S'.
    cw_message-message = 'Batch processed successfully'.
  ENDIF.

ENDFORM.

" END: Cursor Generated Code

