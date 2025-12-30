FUNCTION z_solar_activity.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_WAREHOUSE) TYPE  /SCWM/LGNUM
*"     VALUE(IV_ACTIVITY) TYPE  CHAR2
*"     VALUE(IV_USER) TYPE  SYUNAME
*"  EXPORTING
*"     VALUE(EV_TRANS_ID) TYPE  CHAR20
*"     VALUE(EV_SUCCESS) TYPE  CHAR1
*"     VALUE(EV_MESSAGE) TYPE  STRING
*"  TABLES
*"      IT_HU STRUCTURE  /SCWM/S_HUIDENT
*"      ET_MESSAGES STRUCTURE  GTY_MESSAGE OPTIONAL
*"  CHANGING
*"     VALUE(IV_VBELN) TYPE  VBELN_VA OPTIONAL
*"     VALUE(IV_POSNR) TYPE  POSNR_VA OPTIONAL
*"     VALUE(IV_LGORT) TYPE  LGORT_D OPTIONAL
*"  EXCEPTIONS
*"      VALIDATION_ERROR
*"      PROCESSING_ERROR
*"----------------------------------------------------------------------
*& Purpose: Main processing function for all HU activities
*& Author: [Author Name]
*& Date: [Creation Date]
*&---------------------------------------------------------------------*

  " BEGIN: Cursor Generated Code

  " All declarations first (NW 7.31 compliance)
  DATA: lv_trans_id TYPE char20,
        lv_timestamp TYPE timestampl,
        lv_pallet_count TYPE i,
        lw_header TYPE zpaltrfhdr,
        lt_item TYPE TABLE OF zpaltrfitm,
        lw_item TYPE zpaltrfitm,
        lv_lgnum TYPE /scwm/lgnum,
        lt_huhdr TYPE /scwm/tt_huhdr_int,
        lw_huhdr TYPE /scwm/s_huhdr_int,
        lw_parent_hu TYPE /scwm/s_huhdr_int,
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
        lv_error_text TYPE string,
        lw_message TYPE gty_message.

  " Initialize output parameters
  CLEAR: ev_trans_id, ev_success, ev_message.
  REFRESH: et_messages.
  ev_success = gc_invalid.
  lv_batch_size = gc_batch_size.

  TRY.
      " Authorization check
      AUTHORITY-CHECK OBJECT 'S_RFC'
                      ID 'RFC_TYPE' FIELD 'FUNC'
                      ID 'RFC_NAME' FIELD 'Z_SOLAR_ACTIVITY'
                      ID 'ACTVT' FIELD '16'.
      lv_subrc = sy-subrc.
      IF lv_subrc <> 0.
        ev_success = gc_invalid.
        ev_message = 'No authorization to execute this function module'.
        RAISE validation_error.
      ENDIF.

      " Validate mandatory inputs
      IF iv_warehouse IS INITIAL.
        ev_success = gc_invalid.
        ev_message = 'Warehouse number is mandatory'.
        RAISE validation_error.
      ENDIF.

      IF iv_activity IS INITIAL.
        ev_success = gc_invalid.
        ev_message = 'Activity code is mandatory'.
        RAISE validation_error.
      ENDIF.

      IF iv_user IS INITIAL.
        ev_success = gc_invalid.
        ev_message = 'User ID is mandatory'.
        RAISE validation_error.
      ENDIF.

      IF it_hu[] IS INITIAL.
        ev_success = gc_invalid.
        ev_message = 'No handling units provided'.
        RAISE validation_error.
      ENDIF.

      " Activity-specific validation
      CASE iv_activity.
        WHEN gc_activity_p1.  " Assign SO
          IF iv_vbeln IS INITIAL OR iv_posnr IS INITIAL.
            ev_success = gc_invalid.
            ev_message = 'Sales order and line item are mandatory for activity P1'.
            RAISE validation_error.
          ENDIF.

        WHEN gc_activity_a2.  " Conversion
          IF iv_lgort IS INITIAL.
            ev_success = gc_invalid.
            ev_message = 'Storage location is mandatory for activity A2'.
            RAISE validation_error.
          ENDIF.

        WHEN gc_activity_p2.  " Unassign SO
          " No additional validation required

        WHEN OTHERS.
          ev_success = gc_invalid.
          ev_message = 'Invalid activity code. Valid values: P1, P2, A2'.
          RAISE validation_error.
      ENDCASE.

      " Generate transaction ID
      GET TIME STAMP FIELD lv_timestamp.
      CONCATENATE 'SOLAR' lv_timestamp INTO lv_trans_id.
      ev_trans_id = lv_trans_id.

      " Count pallets
      DESCRIBE TABLE it_hu LINES lv_pallet_count.

      " Update header table
      CLEAR lw_header.
      lw_header-mandt = sy-mandt.
      lw_header-trans_id = lv_trans_id.
      lw_header-pallet_count = lv_pallet_count.
      lw_header-activity = iv_activity.
      lw_header-status = gc_status_running.
      lw_header-created_by = iv_user.
      lw_header-created_on = sy-datum.
      lw_header-created_at = sy-uzeit.

      INSERT zpaltrfhdr FROM lw_header.
      lv_subrc = sy-subrc.
      IF lv_subrc <> 0.
        ev_success = gc_invalid.
        ev_message = 'Error inserting header record'.
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
        lw_header-status = gc_status_error.
        lw_header-error_msg = 'Error getting HU hierarchy'.
        UPDATE zpaltrfhdr FROM lw_header.
        COMMIT WORK AND WAIT.
        ev_success = gc_invalid.
        ev_message = 'Error getting HU hierarchy'.
        RAISE processing_error.
      ENDIF.

      " Build item table
      LOOP AT lt_huhdr INTO lw_huhdr
        WHERE bottom = 'X'.  " Only lower level HUs

        CLEAR lw_item.
        lw_item-mandt = sy-mandt.
        lw_item-trans_id = lv_trans_id.

        " Find parent HU
        CLEAR lw_parent_hu.
        READ TABLE lt_huhdr INTO lw_parent_hu
          WITH KEY guid_hu = lw_huhdr-higher_guid.
        lv_subrc = sy-subrc.
        IF lv_subrc = 0.
          lw_item-pallet_no = lw_parent_hu-huident.
        ELSE.
          lw_item-pallet_no = lw_huhdr-huident.
        ENDIF.

        lw_item-carton_no = lw_huhdr-huident.

        " Get material and stock category
        CLEAR lw_huitm.
        READ TABLE lt_huitm INTO lw_huitm
          WITH KEY guid_hu = lw_huhdr-guid_hu.
        lv_subrc = sy-subrc.
        IF lv_subrc = 0.
          lw_item-material = lw_huitm-matid.
          lw_item-stock_cat = lw_huitm-cat.
        ENDIF.

        APPEND lw_item TO lt_item.
      ENDLOOP.

      " Insert item records
      IF lt_item[] IS NOT INITIAL.
        INSERT zpaltrfitm FROM TABLE lt_item.
        lv_subrc = sy-subrc.
        IF lv_subrc <> 0.
          lw_header-status = gc_status_error.
          lw_header-error_msg = 'Error inserting item records'.
          UPDATE zpaltrfhdr FROM lw_header.
          COMMIT WORK AND WAIT.
          ev_success = gc_invalid.
          ev_message = 'Error inserting item records'.
          RAISE processing_error.
        ENDIF.

        COMMIT WORK AND WAIT.
      ENDIF.

      " Process in batches
      lv_index = 0.
      LOOP AT lt_huhdr INTO lw_huhdr
        WHERE bottom = 'X'.

        CLEAR lw_hu_batch.
        lw_hu_batch-low = lw_huhdr-huident.
        APPEND lw_hu_batch TO lt_hu_batch.

        lv_index = lv_index + 1.

        " Process batch when size reached or last record
        IF lines( lt_hu_batch ) >= lv_batch_size OR lv_index = lv_pallet_count.
          PERFORM process_batch USING iv_warehouse
                                      iv_activity
                                      iv_vbeln
                                      iv_posnr
                                      iv_lgort
                                      iv_user
                                      lt_hu_batch
                                      lt_huitm
                               CHANGING lw_message.

          IF lw_message-type = 'E'.
            APPEND lw_message TO et_messages.
          ENDIF.

          CLEAR lt_hu_batch.
        ENDIF.
      ENDLOOP.

      " Update status to completed
      lw_header-status = gc_status_completed.
      UPDATE zpaltrfhdr FROM lw_header.
      COMMIT WORK AND WAIT.

      ev_success = gc_valid.
      ev_message = 'Processing completed successfully'.

    CATCH cx_root INTO lo_exception.
      lv_error_text = lo_exception->get_text( ).
      lw_header-status = gc_status_error.
      lw_header-error_msg = lv_error_text.
      UPDATE zpaltrfhdr FROM lw_header.
      COMMIT WORK AND WAIT.
      ev_success = gc_invalid.
      ev_message = lv_error_text.
      RAISE processing_error.
  ENDTRY.

  " END: Cursor Generated Code

ENDFUNCTION.
