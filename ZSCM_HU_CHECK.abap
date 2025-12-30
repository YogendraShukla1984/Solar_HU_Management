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
*"      ET_INVALID_HU STRUCTURE  GTY_INVALID_HU
*"  EXCEPTIONS
*"      INVALID_HU
*"----------------------------------------------------------------------
*& Purpose: Validate handling units in EWM warehouse
*& Author: [Author Name]
*& Date: [Creation Date]
*& System: AD2
*&---------------------------------------------------------------------*

  " BEGIN: Cursor Generated Code

  " All declarations first (NW 7.31 compliance)
  DATA: lt_hu_check TYPE TABLE OF /scwm/aqua,
        lw_hu_check TYPE /scwm/aqua,
        lw_hu_input TYPE /scwm/s_huident,
        lw_invalid_hu TYPE gty_invalid_hu,
        lv_count_input TYPE i,
        lv_count_valid TYPE i,
        lv_message_part TYPE string,
        lv_subrc TYPE sy-subrc,
        lo_exception TYPE REF TO cx_root,
        lv_error_text TYPE string.

  " Initialize output parameters
  CLEAR: ev_valid, ev_message.
  REFRESH: et_invalid_hu.
  ev_valid = gc_valid.

  TRY.
      " Authorization check
      AUTHORITY-CHECK OBJECT 'S_RFC'
                      ID 'RFC_TYPE' FIELD 'FUNC'
                      ID 'RFC_NAME' FIELD 'ZSCM_HU_CHECK'
                      ID 'ACTVT' FIELD '16'.
      lv_subrc = sy-subrc.
      IF lv_subrc <> 0.
        ev_valid = gc_invalid.
        ev_message = 'No authorization to execute this function module'.
        RAISE invalid_hu.
      ENDIF.

      " Validate input
      IF iv_warehouse IS INITIAL.
        ev_valid = gc_invalid.
        ev_message = 'Warehouse number is mandatory'.
        RAISE invalid_hu.
      ENDIF.

      IF it_hu[] IS INITIAL.
        ev_valid = gc_invalid.
        ev_message = 'No handling units provided'.
        RAISE invalid_hu.
      ENDIF.

      " Get HU count from input
      DESCRIBE TABLE it_hu LINES lv_count_input.

      " Check each HU in database
      IF it_hu[] IS NOT INITIAL.
        SELECT lgnum huident
          FROM /scwm/aqua
          INTO CORRESPONDING FIELDS OF TABLE lt_hu_check
          FOR ALL ENTRIES IN it_hu
          WHERE lgnum = iv_warehouse
            AND huident = it_hu-low.
      ENDIF.

      " Sort for binary search
      SORT lt_hu_check BY huident.

      " Check count of valid HUs
      DESCRIBE TABLE lt_hu_check LINES lv_count_valid.

      " If counts don't match, find invalid HUs
      IF lv_count_valid <> lv_count_input.
        LOOP AT it_hu INTO lw_hu_input.
          " Only check LOW value for handling units
          IF lw_hu_input-low IS NOT INITIAL.
            READ TABLE lt_hu_check INTO lw_hu_check
              WITH KEY huident = lw_hu_input-low
              BINARY SEARCH.
            lv_subrc = sy-subrc.
            IF lv_subrc <> 0.
              " HU not found in database
              CLEAR lw_invalid_hu.
              lw_invalid_hu-huident = lw_hu_input-low.
              CONCATENATE 'Handling unit' lw_hu_input-low
                          'not found in warehouse' iv_warehouse
                INTO lw_invalid_hu-message SEPARATED BY space.
              APPEND lw_invalid_hu TO et_invalid_hu.
            ENDIF.
          ENDIF.
        ENDLOOP.

        " Build error message
        ev_valid = gc_invalid.
        ev_message = 'Following handling units not found in warehouse: '.

        LOOP AT et_invalid_hu INTO lw_invalid_hu.
          CONCATENATE ev_message lw_invalid_hu-huident ','
            INTO ev_message.
        ENDLOOP.

        RAISE invalid_hu.
      ENDIF.

      " All validations passed
      ev_valid = gc_valid.
      ev_message = 'All handling units are valid'.

    CATCH cx_root INTO lo_exception.
      lv_error_text = lo_exception->get_text( ).
      ev_valid = gc_invalid.
      ev_message = lv_error_text.
      RAISE invalid_hu.
  ENDTRY.

  " END: Cursor Generated Code

ENDFUNCTION.

