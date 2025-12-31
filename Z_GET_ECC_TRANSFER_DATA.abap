FUNCTION z_get_ecc_transfer_data.
*"----------------------------------------------------------------------
*"*"Remote-Enabled Function Module
*"----------------------------------------------------------------------
*"  IMPORTING
*"     REFERENCE(IT_PALLET_NO) TYPE  GTY_T_PALLET
*"     REFERENCE(IT_PLANT) TYPE  GTY_T_PLANT OPTIONAL
*"  EXPORTING
*"     REFERENCE(ET_YSTKDTLS) TYPE  GTY_T_STKDTLS
*"     REFERENCE(ET_MSEG) TYPE  GTY_T_MSEG
*"     REFERENCE(ET_MKPF) TYPE  GTY_T_MKPF
*"----------------------------------------------------------------------
  " BEGIN: Cursor Generated Code
  " This function module runs in ECC system

  DATA: lw_stkdtls TYPE gty_stkdtls,
        lt_mblnr   TYPE STANDARD TABLE OF mblnr,
        lv_mblnr   TYPE mblnr.

  " Retrieve YSTKDTLS
  IF it_pallet_no IS NOT INITIAL.
    SELECT usrno vbeln created_on created_tm
           sourcewerks tube_m created_by mblnr mjahr
      FROM ystkdtls
      INTO CORRESPONDING FIELDS OF TABLE et_ystkdtls
      FOR ALL ENTRIES IN it_pallet_no
      WHERE usrno = it_pallet_no-pallet_no.

    IF sy-subrc = 0.
      " Apply plant filter if provided
      IF it_plant IS NOT INITIAL.
        DELETE et_ystkdtls WHERE sourcewerks NOT IN it_plant.
      ENDIF.

      " Sort for binary search
      SORT et_ystkdtls BY usrno.
    ENDIF.
  ENDIF.

  " Retrieve MSEG and MKPF based on material documents from YSTKDTLS
  IF et_ystkdtls IS NOT INITIAL.
    " Collect unique MBLNR values from YSTKDTLS
    LOOP AT et_ystkdtls INTO lw_stkdtls.
      IF lw_stkdtls-mblnr IS NOT INITIAL.
        lv_mblnr = lw_stkdtls-mblnr.
        APPEND lv_mblnr TO lt_mblnr.
      ENDIF.
    ENDLOOP.

    " Remove duplicates
    IF lt_mblnr IS NOT INITIAL.
      SORT lt_mblnr.
      DELETE ADJACENT DUPLICATES FROM lt_mblnr.

      " Retrieve MKPF (Material Document Headers)
      SELECT mblnr mjahr cpudt cputm
        FROM mkpf
        INTO CORRESPONDING FIELDS OF TABLE et_mkpf
        FOR ALL ENTRIES IN lt_mblnr
        WHERE mblnr = lt_mblnr-table_line.

      IF sy-subrc = 0.
        " Sort for binary search
        SORT et_mkpf BY mblnr mjahr.
        DELETE ADJACENT DUPLICATES FROM et_mkpf COMPARING mblnr mjahr.
      ENDIF.

      " Retrieve MSEG (Material Document Items)
      SELECT mblnr mjahr zeile matnr werks lgort charg
        FROM mseg
        INTO CORRESPONDING FIELDS OF TABLE et_mseg
        FOR ALL ENTRIES IN lt_mblnr
        WHERE mblnr = lt_mblnr-table_line.

      IF sy-subrc = 0.
        " Sort for binary search
        SORT et_mseg BY mblnr mjahr zeile.
        DELETE ADJACENT DUPLICATES FROM et_mseg COMPARING mblnr mjahr zeile.
      ENDIF.
    ENDIF.
  ENDIF.

  " END: Cursor Generated Code
ENDFUNCTION.


*&---------------------------------------------------------------------*
*&      Alternative: Function module to get YSTKDTLS only
*&---------------------------------------------------------------------*
FUNCTION z_get_ystkdtls_data.
*"----------------------------------------------------------------------
*"*"Remote-Enabled Function Module
*"----------------------------------------------------------------------
*"  IMPORTING
*"     REFERENCE(IT_PALLET_NO) TYPE  GTY_T_PALLET
*"  EXPORTING
*"     REFERENCE(ET_YSTKDTLS) TYPE  STANDARD TABLE
*"----------------------------------------------------------------------
  " BEGIN: Cursor Generated Code

  DATA: lw_pallet TYPE gty_pallet.

  " Retrieve YSTKDTLS records
  IF it_pallet_no IS NOT INITIAL.
    SELECT *
      FROM ystkdtls
      INTO TABLE et_ystkdtls
      FOR ALL ENTRIES IN it_pallet_no
      WHERE usrno = it_pallet_no-pallet_no.
  ENDIF.

  " END: Cursor Generated Code
ENDFUNCTION.


