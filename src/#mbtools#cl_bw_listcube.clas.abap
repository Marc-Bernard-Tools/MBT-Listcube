CLASS /mbtools/cl_bw_listcube DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

************************************************************************
* MBT Listcube
*
* (c) MBT 2021 https://marcbernardtools.com/
************************************************************************
  PUBLIC SECTION.
    TYPES ty_param TYPE rsparamsl_255 .
    TYPES:
      ty_params TYPE STANDARD TABLE OF ty_param WITH DEFAULT KEY .

    CONSTANTS:
      c_listcube_variant TYPE c LENGTH 20 VALUE '/MBTOOLS/VARIANT' ##NO_TEXT.

    CLASS-METHODS get_infoprov_description
      IMPORTING
        !iv_infoprov     TYPE rsinfoprov
      RETURNING
        VALUE(rv_result) TYPE rstxtlg .
    CLASS-METHODS get_variant_description
      IMPORTING
        !iv_infoprov     TYPE rsinfoprov
        !iv_variant      TYPE rsvariant
      RETURNING
        VALUE(rv_result) TYPE rstxtlg .
    CLASS-METHODS f4_variant
      IMPORTING
        !iv_infoprov TYPE rsinfoprov
      EXPORTING
        !ev_variant  TYPE rsvariant
        !ev_vartxt   TYPE rstxtlg .
    CLASS-METHODS get_variant
      EXPORTING
        VALUE(ev_skip)   TYPE abap_bool
        VALUE(et_params) TYPE ty_params .
    CLASS-METHODS backup_variants
      IMPORTING
        !iv_infoprov TYPE rsddatatarget
        !iv_repnm    TYPE sy-repid
        !it_ioinf    TYPE rsdq_t_iobj_info .
    CLASS-METHODS restore_variants
      IMPORTING
        !iv_infoprov TYPE rsddatatarget
        !iv_repnm    TYPE sy-repid
        !it_ioinf    TYPE rsdq_t_iobj_info .
    METHODS initialize .
    METHODS pbo .
    METHODS pai
      CHANGING
        !cv_ok_code TYPE sy-ucomm .
    METHODS screen .
    METHODS call_listcube
      IMPORTING
        !iv_infoprov TYPE rsinfoprov
        !iv_variant  TYPE rsvariant
        !iv_skip     TYPE abap_bool .
  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES ty_var TYPE /mbtools/bwvars.
    TYPES:
      ty_vars TYPE STANDARD TABLE OF ty_var WITH DEFAULT KEY.
    TYPES:
      BEGIN OF ty_value,
        variant TYPE rsvariant,
        vtext   TYPE rvart_vtxt,
      END OF ty_value.
    TYPES:
      ty_values TYPE STANDARD TABLE OF ty_value WITH DEFAULT KEY.
    TYPES:
      BEGIN OF ty_text,
        langu TYPE sy-langu,
        vtext TYPE rvart_vtxt,
      END OF ty_text.
    TYPES:
      ty_texts  TYPE STANDARD TABLE OF ty_text WITH DEFAULT KEY.

ENDCLASS.



CLASS /mbtools/cl_bw_listcube IMPLEMENTATION.


  METHOD backup_variants.

    DATA:
      ls_var        TYPE ty_var,
      lt_vars       TYPE ty_vars,
      ls_prog_range TYPE rsprorange,
      lt_prog_range TYPE TABLE OF rsprorange,
      lt_var_range  TYPE TABLE OF rsvarrange,
      lt_text_range TYPE TABLE OF rstexrange,
      lt_created_by TYPE TABLE OF rsnamrange,
      lt_changed_by TYPE TABLE OF rsnamrange,
      lt_creadate   TYPE TABLE OF rsdatrange,
      lt_changedate TYPE TABLE OF rsdatrange,
      ls_variant    TYPE rsvariinfo,
      lt_variants   TYPE TABLE OF rsvariinfo,
      ls_vari_desc  TYPE varid,
      lt_params     TYPE ty_params,
      lt_texts      TYPE ty_texts.

    " Get all variants
    ls_prog_range-sign   = 'I'.
    ls_prog_range-option = 'EQ'.
    ls_prog_range-low    = iv_repnm.
    APPEND ls_prog_range TO lt_prog_range.

    CALL FUNCTION 'RS_VARIANT_INFO'
      TABLES
        prog_range   = lt_prog_range
        var_range    = lt_var_range
        text_range   = lt_text_range
        created_by   = lt_created_by
        changed_by   = lt_changed_by
        creadate     = lt_creadate
        changedate   = lt_changedate
        variant_info = lt_variants.

    LOOP AT lt_variants INTO ls_variant.
      CLEAR: ls_var.
      ls_var-infoprov = iv_infoprov.
      ls_var-variant  = ls_variant-variant.

      MOVE-CORRESPONDING ls_variant TO ls_vari_desc.

      " Read variant
      CALL FUNCTION 'RS_VARIANT_CONTENTS_255'
        EXPORTING
          report               = ls_vari_desc-report
          variant              = ls_vari_desc-variant
          move_or_write        = 'M'
          execute_direct       = 'X'
        TABLES
          valutab              = lt_params
        EXCEPTIONS
          variant_non_existent = 1
          variant_obsolete     = 2
          OTHERS               = 3.
      IF sy-subrc <> 0.
        BREAK-POINT ID /mbtools/bc.
        CONTINUE.
      ENDIF.

      " Definition
      CALL TRANSFORMATION id
        SOURCE data = ls_vari_desc
        RESULT XML ls_var-varid.
      IF sy-subrc <> 0.
        BREAK-POINT ID /mbtools/bc.
        CONTINUE.
      ENDIF.

      " Values
      CALL TRANSFORMATION id
        SOURCE data = lt_params
        RESULT XML ls_var-params.
      IF sy-subrc <> 0.
        BREAK-POINT ID /mbtools/bc.
        CONTINUE.
      ENDIF.

      " InfoObjects
      CALL TRANSFORMATION id
        SOURCE data = it_ioinf
        RESULT XML ls_var-ioinf.
      IF sy-subrc <> 0.
        BREAK-POINT ID /mbtools/bc.
        CONTINUE.
      ENDIF.

      " Texts
      SELECT langu vtext FROM varit
        INTO CORRESPONDING FIELDS OF TABLE lt_texts
        WHERE report = iv_repnm AND variant = ls_var-variant.
      IF sy-subrc = 0.
        CALL TRANSFORMATION id
          SOURCE data = lt_texts
          RESULT XML ls_var-texts.
        IF sy-subrc <> 0.
          BREAK-POINT ID /mbtools/bc.
          CONTINUE.
        ENDIF.
      ENDIF.

      APPEND ls_var TO lt_vars.
    ENDLOOP.

    " Save changes
    DELETE FROM /mbtools/bwvars WHERE infoprov = iv_infoprov. "#EC CI_SUBRC

    INSERT /mbtools/bwvars FROM TABLE lt_vars.            "#EC CI_SUBRC

    CALL FUNCTION 'RSDU_DB_COMMIT'.

  ENDMETHOD.


  METHOD call_listcube.

    EXPORT infoprov = iv_infoprov variant = iv_variant skip = iv_skip TO MEMORY ID c_listcube_variant.

    SUBMIT rsdd_show_icube
      WITH p_dbagg = abap_true
      WITH p_dta   = iv_infoprov
      WITH p_repnm = ''
      WITH p_sdims = abap_false
      WITH p_ssids = abap_false
      WITH p_tchnm = abap_false
      AND RETURN.

  ENDMETHOD.


  METHOD f4_variant.

    DATA:
      ls_var    TYPE ty_var,
      lt_vars   TYPE ty_vars,
      ls_text   TYPE ty_text,
      lt_texts  TYPE ty_texts,
      ls_value  TYPE ty_value,
      lt_values TYPE ty_values,
      ls_return TYPE ddshretval,
      lt_return TYPE TABLE OF ddshretval.

    SELECT variant texts FROM /mbtools/bwvars
      INTO CORRESPONDING FIELDS OF TABLE lt_vars
      WHERE infoprov = iv_infoprov ##TOO_MANY_ITAB_FIELDS.
    IF sy-subrc <> 0.
      MESSAGE 'No variant found'(001) TYPE 'S'.
      RETURN.
    ENDIF.

    LOOP AT lt_vars INTO ls_var.
      CLEAR ls_value.
      ls_value-variant = ls_var-variant.

      CALL TRANSFORMATION id
        SOURCE XML ls_var-texts
        RESULT data = lt_texts.
      CHECK sy-subrc = 0.

      READ TABLE lt_texts INTO ls_text
        WITH KEY langu = sy-langu.
      IF sy-subrc = 0.
        ls_value-vtext = ls_text-vtext.
      ENDIF.
      APPEND ls_value TO lt_values.
    ENDLOOP.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'VARIANT'
        window_title    = 'MBT Listcube Variants'
        value_org       = 'S'
      TABLES
        value_tab       = lt_values
        return_tab      = lt_return
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    CHECK sy-subrc = 0.

    LOOP AT lt_return INTO ls_return.
      ev_variant = ls_return-fieldval.
    ENDLOOP.

    READ TABLE lt_values INTO ls_value
      WITH KEY variant = ev_variant.
    IF sy-subrc = 0.
      ev_vartxt = ls_value-vtext.
    ENDIF.

  ENDMETHOD.


  METHOD get_infoprov_description.

    DATA:
      lo_dta TYPE REF TO if_rsd_dta,
      ls_dta TYPE rsd_s_dta.

    TRY.
        lo_dta = cl_rsd_dta=>factory( iv_infoprov ).

        lo_dta->dta_get_info( IMPORTING e_s_dta = ls_dta ).
      CATCH cx_root.
        RETURN.
    ENDTRY.

    rv_result = ls_dta-txtlg.

  ENDMETHOD.


  METHOD get_variant.

    DATA:
      ls_var      TYPE ty_var,
      lv_infoprov TYPE rsinfoprov,
      lv_variant  TYPE rsvariant.

    IMPORT infoprov = lv_infoprov variant = lv_variant skip = ev_skip FROM MEMORY ID c_listcube_variant.
    FREE MEMORY ID c_listcube_variant.

    SELECT SINGLE params FROM /mbtools/bwvars
      INTO CORRESPONDING FIELDS OF ls_var
      WHERE infoprov = lv_infoprov AND variant = lv_variant.
    CHECK sy-subrc = 0.

    CALL TRANSFORMATION id
      SOURCE XML ls_var-params
      RESULT data = et_params.
    CHECK sy-subrc = 0.

  ENDMETHOD.


  METHOD get_variant_description.

    DATA:
      ls_var   TYPE ty_var,
      ls_text  TYPE ty_text,
      lt_texts TYPE ty_texts.

    SELECT SINGLE texts FROM /mbtools/bwvars
      INTO CORRESPONDING FIELDS OF ls_var
      WHERE infoprov = iv_infoprov AND variant = iv_variant.
    CHECK sy-subrc = 0.

    CALL TRANSFORMATION id
      SOURCE XML ls_var-texts
      RESULT data = lt_texts.
    CHECK sy-subrc = 0.

    READ TABLE lt_texts INTO ls_text
      WITH KEY langu = sy-langu.
    IF sy-subrc = 0.
      rv_result = ls_text-vtext.
    ENDIF.

  ENDMETHOD.


  METHOD initialize.
    ASSERT 0 = 0.
  ENDMETHOD.


  METHOD pai.
    CLEAR cv_ok_code.
  ENDMETHOD.


  METHOD pbo.
    ASSERT 0 = 0.
  ENDMETHOD.


  METHOD restore_variants.

    DATA:
      ls_var       TYPE ty_var,
      lt_vars      TYPE ty_vars,
      ls_vari_desc TYPE varid,
      ls_vari_text TYPE varit,
      lt_vari_text TYPE TABLE OF varit,
      ls_vari_scr  TYPE rsdynnr,
      lt_vari_scr  TYPE TABLE OF rsdynnr,
      ls_param     TYPE ty_param,
      lt_params    TYPE ty_params,
      ls_text      TYPE ty_text,
      lt_texts     TYPE ty_texts,
      "ls_ioinf     TYPE rsdq_s_iobj_info
      lt_ioinfs    TYPE rsdq_t_iobj_info.

    SELECT * FROM /mbtools/bwvars INTO TABLE lt_vars
      WHERE infoprov = iv_infoprov.
    CHECK sy-subrc = 0.

    LOOP AT lt_vars INTO ls_var.
      CLEAR: ls_vari_desc, lt_vari_text, lt_vari_scr, lt_texts.

      " Definition
      CALL TRANSFORMATION id
        SOURCE XML ls_var-varid
        RESULT data = ls_vari_desc.
      IF sy-subrc <> 0.
        BREAK-POINT ID /mbtools/bc.
        CONTINUE.
      ENDIF.

      ls_vari_desc-report = iv_repnm.

      " Values
      CALL TRANSFORMATION id
        SOURCE XML ls_var-params
        RESULT data = lt_params.
      IF sy-subrc <> 0.
        BREAK-POINT ID /mbtools/bc.
        CONTINUE.
      ENDIF.

      " InfoObjects
      CALL TRANSFORMATION id
        SOURCE XML ls_var-ioinf
        RESULT data = lt_ioinfs.
      IF sy-subrc <> 0.
        BREAK-POINT ID /mbtools/bc.
        CONTINUE.
      ENDIF.

      " Map previous variant definition (param/ioinf) to current InfoObjects
      " since InfoProvider definition or InfoObject selection
      " could have changed
      LOOP AT lt_params INTO ls_param ##TODO.
*        READ TABLE lt_ioinfs INTO ls_ioinf
*          WITH KEY infoprov = iv_infoprov iobjnm = ls_ioinf-iobjnm
*        IF sy-subrc = 0
*          " InfoObject still exists
*        ELSE
*          " InfoObject does not exist anymore
*          DELETE lt_param WHERE selname = ls_ioinf-selname
*        ENDIF
      ENDLOOP.

      " Texts
      CALL TRANSFORMATION id
        SOURCE XML ls_var-texts
        RESULT data = lt_texts.
      IF sy-subrc <> 0.
        BREAK-POINT ID /mbtools/bc.
        CONTINUE.
      ENDIF.

      LOOP AT lt_texts INTO ls_text.
        CLEAR ls_vari_text.
        ls_vari_text-mandt   = sy-mandt.
        ls_vari_text-langu   = ls_text-langu.
        ls_vari_text-report  = iv_repnm.
        ls_vari_text-variant = ls_var-variant.
        ls_vari_text-vtext   = ls_text-vtext.
        APPEND ls_vari_text TO lt_vari_text.
      ENDLOOP.

      " Screens
      ls_vari_scr-dynnr = '0001'.
      APPEND ls_vari_scr TO lt_vari_scr.
      ls_vari_scr-dynnr = '1000'.
      APPEND ls_vari_scr TO lt_vari_scr.

      " Create new variant
      CALL FUNCTION 'RS_CREATE_VARIANT_255'
        EXPORTING
          curr_report               = ls_vari_desc-report
          curr_variant              = ls_vari_desc-variant
          vari_desc                 = ls_vari_desc
        TABLES
          vari_contents             = lt_params
          vari_text                 = lt_vari_text
          vscreens                  = lt_vari_scr
        EXCEPTIONS
          illegal_report_or_variant = 1
          illegal_variantname       = 2
          not_authorized            = 3
          not_executed              = 4
          report_not_existent       = 5
          report_not_supplied       = 6
          variant_exists            = 7
          variant_locked            = 8
          OTHERS                    = 9.
      IF sy-subrc <> 0.
        BREAK-POINT ID /mbtools/bc.
        CONTINUE.
      ENDIF.

    ENDLOOP.

    CALL FUNCTION 'RSDU_DB_COMMIT'.

  ENDMETHOD.


  METHOD screen.

    LOOP AT SCREEN.
      IF screen-name = 'P_DTATXT' OR screen-name = 'P_VARTXT'.
        screen-input = '0'.
      ELSE.
        screen-input = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
