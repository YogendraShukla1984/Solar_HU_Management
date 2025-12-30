FUNCTION zscm_so_sloc_validation.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_VBELN) TYPE  VBELN_VA OPTIONAL
*"     VALUE(IV_POSNR) TYPE  POSNR_VA OPTIONAL
*"     VALUE(IV_WAREHOUSE) TYPE  /SCWM/LGNUM OPTIONAL
*"     VALUE(IV_LGORT) TYPE  LGORT_D OPTIONAL
*"  EXPORTING
*"     VALUE(EV_VALID) TYPE  CHAR1
*"     VALUE(EV_MESSAGE) TYPE  STRING
*"  EXCEPTIONS
*"      INVALID_INPUT
*"----------------------------------------------------------------------
*& Purpose: Validate sales orders and storage locations
*& Author: [Author Name]
*& Date: [Creation Date]
*& System: RD2
*&---------------------------------------------------------------------*

  " BEGIN: Cursor Generated Code

  " All declarations first (NW 7.31 compliance)
  DATA: lw_vbak TYPE vbak,
        lw_vbap TYPE vbap,
        lv_lgnum TYPE /scwm/lgnum,
        lv_werks TYPE werks_d,
        lw_t340d TYPE t340d,
        lw_t320 TYPE t320,
        lw_t001l TYPE t001l,
        lv_subrc TYPE sy-subrc,
        lo_exception TYPE REF TO cx_root,
        lv_error_text TYPE string,
        lv_msg_part TYPE string.

  " Initialize output parameters
  CLEAR: ev_valid, ev_message.
  ev_valid = gc_valid.

  TRY.
      " Authorization check
      AUTHORITY-CHECK OBJECT 'S_RFC'
                      ID 'RFC_TYPE' FIELD 'FUNC'
                      ID 'RFC_NAME' FIELD 'ZSCM_SO_SLOC_VALIDATION'
                      ID 'ACTVT' FIELD '16'.
      lv_subrc = sy-subrc.
      IF lv_subrc <> 0.
        ev_valid = gc_invalid.
        ev_message = 'No authorization to execute this function module'.
        RAISE invalid_input.
      ENDIF.

      " Determine validation type
      IF iv_vbeln IS NOT INITIAL AND iv_posnr IS NOT INITIAL.
        " Validate Sales Order

        " Check sales order header
        SELECT SINGLE vbeln erdat kunnr
          FROM vbak
          INTO CORRESPONDING FIELDS OF lw_vbak
          WHERE vbeln = iv_vbeln.

        lv_subrc = sy-subrc.
        IF lv_subrc <> 0.
          ev_valid = gc_invalid.
          CONCATENATE 'Sales order' iv_vbeln 'does not exist'
            INTO ev_message SEPARATED BY space.
          RAISE invalid_input.
        ENDIF.

        " Check sales order line item
        SELECT SINGLE vbeln posnr matnr werks
          FROM vbap
          INTO CORRESPONDING FIELDS OF lw_vbap
          WHERE vbeln = iv_vbeln
            AND posnr = iv_posnr.

        lv_subrc = sy-subrc.
        IF lv_subrc <> 0.
          ev_valid = gc_invalid.
          CONCATENATE 'Line item' iv_posnr 'does not exist for sales order'
                      iv_vbeln
            INTO ev_message SEPARATED BY space.
          RAISE invalid_input.
        ENDIF.

        ev_message = 'Sales order and line item are valid'.

      ELSEIF iv_warehouse IS NOT INITIAL AND iv_lgort IS NOT INITIAL.
        " Validate Storage Location

        " Get LGNUM from warehouse
        SELECT SINGLE lgnum lgtyp
          FROM t340d
          INTO CORRESPONDING FIELDS OF lw_t340d
          WHERE lgnum = iv_warehouse.

        lv_subrc = sy-subrc.
        IF lv_subrc <> 0.
          ev_valid = gc_invalid.
          CONCATENATE 'Warehouse' iv_warehouse 'not found in T340D'
            INTO ev_message SEPARATED BY space.
          RAISE invalid_input.
        ENDIF.

        lv_lgnum = lw_t340d-lgnum.

        " Get plant from LGNUM
        SELECT SINGLE lgnum werks
          FROM t320
          INTO CORRESPONDING FIELDS OF lw_t320
          WHERE lgnum = lv_lgnum.

        lv_subrc = sy-subrc.
        IF lv_subrc <> 0.
          ev_valid = gc_invalid.
          CONCATENATE 'LGNUM' lv_lgnum 'not found in T320'
            INTO ev_message SEPARATED BY space.
          RAISE invalid_input.
        ENDIF.

        lv_werks = lw_t320-werks.

        " Validate storage location
        SELECT SINGLE werks lgort lgobe
          FROM t001l
          INTO CORRESPONDING FIELDS OF lw_t001l
          WHERE werks = lv_werks
            AND lgort = iv_lgort.

        lv_subrc = sy-subrc.
        IF lv_subrc <> 0.
          ev_valid = gc_invalid.
          CONCATENATE 'Storage location' iv_lgort
                      'is not valid for plant' lv_werks
            INTO ev_message SEPARATED BY space.
          RAISE invalid_input.
        ENDIF.

        ev_message = 'Storage location is valid'.

      ELSE.
        " Invalid input combination
        ev_valid = gc_invalid.
        ev_message = 'Invalid input combination. Provide either SO/Line Item or Warehouse/SLOC'.
        RAISE invalid_input.
      ENDIF.

    CATCH cx_root INTO lo_exception.
      lv_error_text = lo_exception->get_text( ).
      ev_valid = gc_invalid.
      ev_message = lv_error_text.
      RAISE invalid_input.
  ENDTRY.

  " END: Cursor Generated Code

ENDFUNCTION.

