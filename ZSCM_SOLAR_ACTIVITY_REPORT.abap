FUNCTION zscm_solar_activity_report.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_DATE_FROM) TYPE  DATUM
*"     VALUE(IV_DATE_TO) TYPE  DATUM
*"     REFERENCE(IT_ACTIVITY) TYPE  GTY_T_ACTIVITY
*"     REFERENCE(IT_PALLET_NO) TYPE  GTY_T_PALLET OPTIONAL
*"     REFERENCE(IT_PLANT) TYPE  GTY_T_PLANT OPTIONAL
*"  EXPORTING
*"     REFERENCE(ET_OUTPUT) TYPE  GTY_T_REPORT_OUTPUT
*"     VALUE(EV_RECORD_COUNT) TYPE  I
*"  EXCEPTIONS
*"      DATE_RANGE_INVALID
*"      NO_DATA_FOUND
*"      ACTIVITY_TYPE_INVALID
*"      ECC_CONNECTION_ERROR
*"      AUTHORIZATION_FAILED
*"----------------------------------------------------------------------
  " BEGIN: Cursor Generated Code

  " All data declarations first (NetWeaver 7.31 requirement)
  DATA: lt_trfhdr         TYPE gty_t_trfhdr,
        lt_trfitm         TYPE gty_t_trfitm,
        lt_aqua           TYPE gty_t_aqua,
        lt_matkey         TYPE gty_t_matkey,
        lt_stkdtls        TYPE gty_t_stkdtls,
        lt_mseg           TYPE gty_t_mseg,
        lt_mkpf           TYPE gty_t_mkpf,
        lt_status         TYPE gty_t_status,
        lt_pallet_nos     TYPE gty_t_pallet,
        lt_transaction_id TYPE STANDARD TABLE OF char20,
        lv_transaction_id TYPE char20,
        lw_trfhdr         TYPE gty_trfhdr,
        lw_trfitm         TYPE gty_trfitm,
        lw_pallet         TYPE gty_pallet,
        lv_lines          TYPE i,
        lv_valid          TYPE abap_bool,
        lo_exception      TYPE REF TO cx_root,
        lv_error_text     TYPE string.

  " Validate date range
  IF iv_date_from > iv_date_to.
    RAISE date_range_invalid.
  ENDIF.

  " Validate activity types
  IF it_activity IS INITIAL.
    RAISE activity_type_invalid.
  ENDIF.

  " Step 1: Get transaction headers
  SELECT transaction_id created_on activity_type
    FROM zpaltrfhdr
    INTO TABLE lt_trfhdr
    WHERE created_on BETWEEN iv_date_from AND iv_date_to.

  IF sy-subrc <> 0.
    RAISE no_data_found.
  ENDIF.

  " Filter by activity type if provided
  IF it_activity IS NOT INITIAL.
    DELETE lt_trfhdr WHERE activity_type NOT IN it_activity.
    IF lt_trfhdr IS INITIAL.
      RAISE no_data_found.
    ENDIF.
  ENDIF.

  " Step 2: Get transaction items
  IF lt_trfhdr IS NOT INITIAL.
    SELECT transaction_id pallet_number item_no
      FROM zpaltrfitm
      INTO TABLE lt_trfitm
      FOR ALL ENTRIES IN lt_trfhdr
      WHERE transaction_id = lt_trfhdr-transaction_id.

    IF sy-subrc = 0.
      " Filter by pallet number if provided
      IF it_pallet_no IS NOT INITIAL.
        DELETE lt_trfitm WHERE pallet_number NOT IN it_pallet_no.
        IF lt_trfitm IS INITIAL.
          RAISE no_data_found.
        ENDIF.
      ENDIF.

      " Sort for binary search
      SORT lt_trfitm BY pallet_number.
      DELETE ADJACENT DUPLICATES FROM lt_trfitm COMPARING pallet_number.
    ELSE.
      RAISE no_data_found.
    ENDIF.
  ENDIF.

  " Step 3: Get product information from /SCWM/AQUA
  IF lt_trfitm IS NOT INITIAL.
    " Note: Adjust field names based on actual /SCWM/AQUA structure
    SELECT guid_hu matid quan meins charg
           stock_cat stock_docno stock_itmno
           entitled wdatu
      FROM /scwm/aqua
      INTO CORRESPONDING FIELDS OF TABLE lt_aqua
      FOR ALL ENTRIES IN lt_trfitm
      WHERE guid_hu = lt_trfitm-pallet_number.

    IF sy-subrc = 0.
      SORT lt_aqua BY hu_number.
    ENDIF.
  ENDIF.

  " Step 4: Get material descriptions
  IF lt_aqua IS NOT INITIAL.
    SELECT matid maktx
      FROM /sapapo/matkey
      INTO TABLE lt_matkey
      FOR ALL ENTRIES IN lt_aqua
      WHERE matid = lt_aqua-matid.

    IF sy-subrc = 0.
      SORT lt_matkey BY matid.
      DELETE ADJACENT DUPLICATES FROM lt_matkey COMPARING matid.
    ENDIF.
  ENDIF.

  " Step 5: Get transfer details from ECC (via RFC)
  IF lt_trfitm IS NOT INITIAL.
    " Collect pallet numbers
    LOOP AT lt_trfitm INTO lw_trfitm.
      CLEAR: lw_pallet.
      lw_pallet-pallet_no = lw_trfitm-pallet_number.
      APPEND lw_pallet TO lt_pallet_nos.
    ENDLOOP.

    SORT lt_pallet_nos BY pallet_no.
    DELETE ADJACENT DUPLICATES FROM lt_pallet_nos COMPARING pallet_no.

    " Call RFC function to get ECC data
    TRY.
        CALL FUNCTION 'Z_GET_ECC_TRANSFER_DATA'
          DESTINATION gc_dest_ecc
          EXPORTING
            it_pallet_no = lt_pallet_nos
            it_plant     = it_plant
          IMPORTING
            et_ystkdtls  = lt_stkdtls
            et_mseg      = lt_mseg
            et_mkpf      = lt_mkpf
          EXCEPTIONS
            system_failure        = 1
            communication_failure = 2
            OTHERS                = 3.

        IF sy-subrc <> 0.
          RAISE ecc_connection_error.
        ENDIF.

      CATCH cx_root INTO lo_exception.
        lv_error_text = lo_exception->get_text( ).
        RAISE ecc_connection_error.
    ENDTRY.

    " Sort ECC data for binary search
    IF lt_stkdtls IS NOT INITIAL.
      SORT lt_stkdtls BY usrno.
    ENDIF.
    IF lt_mseg IS NOT INITIAL.
      SORT lt_mseg BY mblnr mjahr.
    ENDIF.
    IF lt_mkpf IS NOT INITIAL.
      SORT lt_mkpf BY mblnr mjahr.
    ENDIF.
  ENDIF.

  " Step 6: Get conversion status
  IF lt_pallet_nos IS NOT INITIAL.
    CALL FUNCTION 'ZSCM_SFG_CONVERSION_STATUS'
      EXPORTING
        it_pallet_no = lt_pallet_nos
      IMPORTING
        et_status    = lt_status.

    IF lt_status IS NOT INITIAL.
      SORT lt_status BY pallet_no.
    ENDIF.
  ENDIF.

  " Step 7: Merge all data
  PERFORM merge_report_data
    TABLES lt_trfhdr
           lt_trfitm
           lt_aqua
           lt_matkey
           lt_stkdtls
           lt_mseg
           lt_mkpf
           lt_status
    CHANGING et_output.

  " Set record count
  DESCRIBE TABLE et_output LINES lv_lines.
  ev_record_count = lv_lines.

  IF et_output IS INITIAL.
    RAISE no_data_found.
  ENDIF.

  " END: Cursor Generated Code
ENDFUNCTION.


*&---------------------------------------------------------------------*
*&      Form  MERGE_REPORT_DATA
*&---------------------------------------------------------------------*
*       Merge all data sources into output structure
*----------------------------------------------------------------------*
FORM merge_report_data
  TABLES pt_trfhdr   TYPE gty_t_trfhdr
         pt_trfitm   TYPE gty_t_trfitm
         pt_aqua     TYPE gty_t_aqua
         pt_matkey   TYPE gty_t_matkey
         pt_stkdtls  TYPE gty_t_stkdtls
         pt_mseg     TYPE gty_t_mseg
         pt_mkpf     TYPE gty_t_mkpf
         pt_status   TYPE gty_t_status
  CHANGING pt_output TYPE gty_t_report_output.

  " BEGIN: Cursor Generated Code

  DATA: lw_output  TYPE gty_report_output,
        lw_trfhdr  TYPE gty_trfhdr,
        lw_trfitm  TYPE gty_trfitm,
        lw_aqua    TYPE gty_aqua,
        lw_matkey  TYPE gty_matkey,
        lw_stkdtls TYPE gty_stkdtls,
        lw_mseg    TYPE gty_mseg,
        lw_mkpf    TYPE gty_mkpf,
        lw_status  TYPE gty_status.

  " Sort all lookup tables for binary search
  SORT pt_trfhdr BY transaction_id.
  SORT pt_aqua BY hu_number.
  SORT pt_matkey BY matid.
  SORT pt_stkdtls BY usrno.
  SORT pt_mseg BY mblnr mjahr.
  SORT pt_mkpf BY mblnr mjahr.
  SORT pt_status BY pallet_no.

  " Loop through items and merge data
  LOOP AT pt_trfitm INTO lw_trfitm.
    CLEAR: lw_output.

    " Get transaction header
    READ TABLE pt_trfhdr INTO lw_trfhdr
      WITH KEY transaction_id = lw_trfitm-transaction_id
      BINARY SEARCH.

    IF sy-subrc = 0.
      lw_output-transaction_id = lw_trfhdr-transaction_id.
      lw_output-transaction_date = lw_trfhdr-created_on.
    ENDIF.

    " Basic item data
    lw_output-handling_unit = lw_trfitm-pallet_number.

    " Get product info from AQUA
    READ TABLE pt_aqua INTO lw_aqua
      WITH KEY hu_number = lw_trfitm-pallet_number
      BINARY SEARCH.

    IF sy-subrc = 0.
      lw_output-product = lw_aqua-matid.
      lw_output-fg_quantity = lw_aqua-quan.
      lw_output-base_uom = lw_aqua-altme.
      lw_output-batch = lw_aqua-charg.
      lw_output-stock_type = lw_aqua-cat.
      lw_output-sales_order = lw_aqua-stock_docno.
      lw_output-so_item = lw_aqua-stock_itemno.
      lw_output-to_storage_bin = lw_aqua-lgpla.
      lw_output-pallet_grn_date = lw_aqua-wdatu.

      " Get material description
      READ TABLE pt_matkey INTO lw_matkey
        WITH KEY matid = lw_aqua-matid
        BINARY SEARCH.

      IF sy-subrc = 0.
        lw_output-product_desc = lw_matkey-maktx.
      ENDIF.
    ENDIF.

    " Get transfer details from ECC
    READ TABLE pt_stkdtls INTO lw_stkdtls
      WITH KEY usrno = lw_trfitm-pallet_number
      BINARY SEARCH.

    IF sy-subrc = 0.
      lw_output-transfer_start_date = lw_stkdtls-created_on.
      lw_output-transfer_start_time = lw_stkdtls-created_tm.
      lw_output-plant = lw_stkdtls-sourcewerks.
      lw_output-from_storage_bin = lw_stkdtls-tube_m.
      lw_output-user_id = lw_stkdtls-created_by.

      " Get material document details if available
      " Note: Logic depends on how MSEG/MKPF are linked to YSTKDTLS
      " This is a placeholder - actual logic needs business clarification
    ENDIF.

    " Get conversion status
    READ TABLE pt_status INTO lw_status
      WITH KEY pallet_no = lw_trfitm-pallet_number
      BINARY SEARCH.

    IF sy-subrc = 0.
      lw_output-conversion_status = lw_status-status.
      lw_output-error_details = lw_status-message.
    ENDIF.

    APPEND lw_output TO pt_output.
  ENDLOOP.

  " Sort output by transaction date
  SORT pt_output BY transaction_date DESCENDING.

  " END: Cursor Generated Code
ENDFORM.


