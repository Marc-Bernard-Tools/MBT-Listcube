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
        !it_ioinf    TYPE rsdq_t_iobj_info
      RAISING
        /mbtools/cx_exception.
    CLASS-METHODS restore_variants
      IMPORTING
        !iv_infoprov TYPE rsddatatarget
        !iv_repnm    TYPE sy-repid
        !it_ioinf    TYPE rsdq_t_iobj_info
      RAISING
        /mbtools/cx_exception.
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

    TYPES:
      BEGIN OF ty_value,
        variant TYPE rsvariant,
        vtext   TYPE rvart_vtxt,
      END OF ty_value.
    TYPES:
      ty_values TYPE STANDARD TABLE OF ty_value WITH DEFAULT KEY.
    TYPES:
      ty_iobjs TYPE STANDARD TABLE OF /mbtools/bwvarsi WITH DEFAULT KEY.

    CLASS-METHODS _map_infoobject_selection
      IMPORTING
        !it_ioinf  TYPE rsdq_t_iobj_info
        !it_iobjs  TYPE ty_iobjs
      CHANGING
        !ct_params TYPE ty_params.
    CLASS-METHODS _set_optimistic_lock
      IMPORTING
        !iv_infoprov TYPE rsddatatarget
      RAISING
        /mbtools/cx_exception.
    CLASS-METHODS _promote_lock
      IMPORTING
        !iv_infoprov TYPE rsddatatarget
      RAISING
        /mbtools/cx_exception.
    CLASS-METHODS _release_lock
      IMPORTING
        !iv_infoprov TYPE rsddatatarget
      RAISING
        /mbtools/cx_exception.
ENDCLASS.



CLASS /mbtools/cl_bw_listcube IMPLEMENTATION.


  METHOD backup_variants.

    DATA:
      ls_prog_range TYPE rsprorange,
      lt_prog_range TYPE TABLE OF rsprorange,
      lt_var_range  TYPE TABLE OF rsvarrange,
      lt_text_range TYPE TABLE OF rstexrange,
      lt_created_by TYPE TABLE OF rsnamrange,
      lt_changed_by TYPE TABLE OF rsnamrange,
      lt_creadate   TYPE TABLE OF rsdatrange,
      lt_changedate TYPE TABLE OF rsdatrange,
      lt_variants   TYPE TABLE OF rsvariinfo,
      lt_valutab    TYPE TABLE OF ty_param,
      lt_var        TYPE TABLE OF /mbtools/bwvars,
      lt_iobjs      TYPE TABLE OF /mbtools/bwvarsi,
      lt_params     TYPE TABLE OF /mbtools/bwvarsp,
      lt_texts      TYPE TABLE OF /mbtools/bwvarst.

    FIELD-SYMBOLS:
      <ls_variant> LIKE LINE OF lt_variants,
      <ls_var>     LIKE LINE OF lt_var,
      <ls_valu>    LIKE LINE OF lt_valutab,
      <ls_params>  LIKE LINE OF lt_params,
      <ls_ioinf>   LIKE LINE OF it_ioinf,
      <ls_iobjs>   LIKE LINE OF lt_iobjs,
      <ls_texts>   LIKE LINE OF lt_texts.

    _set_optimistic_lock( iv_infoprov ).

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

    LOOP AT lt_variants ASSIGNING <ls_variant>.
      APPEND INITIAL LINE TO lt_var ASSIGNING <ls_var>.
      <ls_var>-infoprov = iv_infoprov.
      MOVE-CORRESPONDING <ls_variant> TO <ls_var>.

      " Read variant content
      CALL FUNCTION 'RS_VARIANT_CONTENTS_255'
        EXPORTING
          report               = <ls_variant>-report
          variant              = <ls_variant>-variant
          move_or_write        = 'M'
          execute_direct       = 'X'
        TABLES
          valutab              = lt_valutab
        EXCEPTIONS
          variant_non_existent = 1
          variant_obsolete     = 2
          OTHERS               = 3.
      IF sy-subrc <> 0.
        /mbtools/cx_exception=>raise_t100( ).
      ENDIF.

      LOOP AT lt_valutab ASSIGNING <ls_valu>.
        APPEND INITIAL LINE TO lt_params ASSIGNING <ls_params>.
        <ls_params>-pos = sy-tabix.
        MOVE-CORRESPONDING <ls_var> TO <ls_params>.
        MOVE-CORRESPONDING <ls_valu> TO <ls_params>.
        <ls_params>-opt = <ls_valu>-option.
      ENDLOOP.

      " InfoObjects
      LOOP AT it_ioinf ASSIGNING <ls_ioinf>.
        APPEND INITIAL LINE TO lt_iobjs ASSIGNING <ls_iobjs>.
        MOVE-CORRESPONDING <ls_var> TO <ls_iobjs>.
        MOVE-CORRESPONDING <ls_ioinf> TO <ls_iobjs>.
      ENDLOOP.

      " Texts
      SELECT langu vtext FROM varit INTO CORRESPONDING FIELDS OF TABLE lt_texts
        WHERE report = <ls_var>-report AND variant = <ls_var>-variant ##TOO_MANY_ITAB_FIELDS.
      IF sy-subrc = 0.
        LOOP AT lt_texts ASSIGNING <ls_texts>.
          MOVE-CORRESPONDING <ls_var> TO <ls_texts>.
        ENDLOOP.
      ENDIF.

    ENDLOOP.

    " Save changes
    _promote_lock( iv_infoprov ).

    DELETE FROM /mbtools/bwvars WHERE infoprov = iv_infoprov ##SUBRC_OK.
    INSERT /mbtools/bwvars FROM TABLE lt_var ##SUBRC_OK.

    DELETE FROM /mbtools/bwvarsp WHERE infoprov = iv_infoprov ##SUBRC_OK.
    INSERT /mbtools/bwvarsp FROM TABLE lt_params ##SUBRC_OK.

    DELETE FROM /mbtools/bwvarsi WHERE infoprov = iv_infoprov ##SUBRC_OK.
    INSERT /mbtools/bwvarsi FROM TABLE lt_iobjs ##SUBRC_OK.

    DELETE FROM /mbtools/bwvarst WHERE infoprov = iv_infoprov ##SUBRC_OK.
    INSERT /mbtools/bwvarst FROM TABLE lt_texts ##SUBRC_OK.

    CALL FUNCTION 'RSDU_DB_COMMIT'.

    _release_lock( iv_infoprov ).

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
      lt_var    TYPE TABLE OF /mbtools/bwvars-variant,
      lt_texts  TYPE TABLE OF /mbtools/bwvarst,
      lt_values TYPE ty_values,
      lt_return TYPE TABLE OF ddshretval.

    FIELD-SYMBOLS:
      <lv_var>    LIKE LINE OF lt_var,
      <ls_texts>  LIKE LINE OF lt_texts,
      <ls_value>  LIKE LINE OF lt_values,
      <ls_return> TYPE ddshretval.

    SELECT variant FROM /mbtools/bwvars INTO TABLE lt_var
      WHERE infoprov = iv_infoprov.
    IF sy-subrc <> 0.
      MESSAGE 'No variant found'(001) TYPE 'S'.
      RETURN.
    ENDIF.

    SELECT * FROM /mbtools/bwvarst INTO TABLE lt_texts
      WHERE infoprov = iv_infoprov ##SUBRC_OK.

    LOOP AT lt_var ASSIGNING <lv_var>.
      APPEND INITIAL LINE TO lt_values ASSIGNING <ls_value>.
      <ls_value>-variant = <lv_var>.

      READ TABLE lt_texts ASSIGNING <ls_texts>
        WITH KEY variant = <lv_var> langu = sy-langu.
      IF sy-subrc = 0.
        <ls_value>-vtext = <ls_texts>-vtext.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'VARIANT'
        window_title    = 'MBT Listcube Variants'
        value_org       = 'S' "single
      TABLES
        value_tab       = lt_values
        return_tab      = lt_return
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3 ##NO_TEXT.
    CHECK sy-subrc = 0.

    READ TABLE lt_return ASSIGNING <ls_return> INDEX 1.
    IF sy-subrc = 0.
      ev_variant = <ls_return>-fieldval.
    ENDIF.

    READ TABLE lt_values ASSIGNING <ls_value>
      WITH KEY variant = ev_variant.
    IF sy-subrc = 0.
      ev_vartxt = <ls_value>-vtext.
    ENDIF.

  ENDMETHOD.


  METHOD get_infoprov_description.

    DATA:
      lo_dta TYPE REF TO if_rsd_dta,
      ls_dta TYPE rsd_s_dta.

    cl_rsd_dta=>factory(
      EXPORTING
        i_infoprov        = iv_infoprov
      RECEIVING
        r_r_dta           = lo_dta
      EXCEPTIONS
        not_found         = 1
        OTHERS            = 2 ).
    CHECK sy-subrc = 0.

    lo_dta->dta_get_info(
      IMPORTING
        e_s_dta            = ls_dta
      EXCEPTIONS
        dta_not_found      = 1
        iobj_not_found     = 2
        objvers_invalid    = 3
        OTHERS             = 4 ).
    CHECK sy-subrc = 0.

    rv_result = ls_dta-txtlg.

  ENDMETHOD.


  METHOD get_variant.

    DATA:
      lv_infoprov TYPE rsinfoprov,
      lv_variant  TYPE rsvariant,
      lt_params   TYPE TABLE OF /mbtools/bwvarsp.

    FIELD-SYMBOLS:
      <ls_params>     LIKE LINE OF lt_params,
      <ls_params_out> LIKE LINE OF et_params.

    IMPORT infoprov = lv_infoprov variant = lv_variant skip = ev_skip FROM MEMORY ID c_listcube_variant.
    FREE MEMORY ID c_listcube_variant.

    SELECT * FROM /mbtools/bwvarsp
      INTO CORRESPONDING FIELDS OF TABLE lt_params
      WHERE infoprov = lv_infoprov AND variant = lv_variant.
    CHECK sy-subrc = 0.

    LOOP AT lt_params ASSIGNING <ls_params>.
      APPEND INITIAL LINE TO et_params ASSIGNING <ls_params_out>.
      MOVE-CORRESPONDING <ls_params> TO <ls_params_out>.
      <ls_params_out>-option = <ls_params>-opt.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_variant_description.

    SELECT SINGLE vtext FROM /mbtools/bwvarst INTO rv_result
      WHERE infoprov = iv_infoprov AND variant = iv_variant AND langu = sy-langu.
    CHECK sy-subrc = 0.


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
      lt_var        TYPE TABLE OF /mbtools/bwvars,
      lt_iobjs      TYPE TABLE OF /mbtools/bwvarsi,
      lt_params     TYPE TABLE OF /mbtools/bwvarsp,
      lt_texts      TYPE TABLE OF /mbtools/bwvarst,
      ls_vari_desc  TYPE varid,
      lt_vari_text  TYPE TABLE OF varit,
      lt_vari_scr   TYPE TABLE OF rsdynnr,
      lt_vari_param TYPE ty_params.

    FIELD-SYMBOLS:
      <ls_var>        LIKE LINE OF lt_var,
      <ls_params>     LIKE LINE OF lt_params,
      <ls_texts>      LIKE LINE OF lt_texts,
      <ls_vari_text>  LIKE LINE OF lt_vari_text,
      <ls_vari_scr>   LIKE LINE OF lt_vari_scr,
      <ls_vari_param> LIKE LINE OF lt_vari_param.

    SELECT * FROM /mbtools/bwvars INTO TABLE lt_var WHERE infoprov = iv_infoprov.
    CHECK sy-subrc = 0.

    LOOP AT lt_var ASSIGNING <ls_var>.
      CLEAR: ls_vari_desc, lt_vari_text, lt_vari_scr, lt_vari_param.

      " Definition
      MOVE-CORRESPONDING <ls_var> TO ls_vari_desc.
      ls_vari_desc-report = iv_repnm.

      " Values
      SELECT * FROM /mbtools/bwvarsp INTO TABLE lt_params
        WHERE infoprov = <ls_var>-infoprov AND variant = <ls_var>-variant.
      IF sy-subrc = 0.
        LOOP AT lt_params ASSIGNING <ls_params>.
          APPEND INITIAL LINE TO lt_vari_param ASSIGNING <ls_vari_param>.
          MOVE-CORRESPONDING <ls_params> TO <ls_vari_param>.
          <ls_vari_param>-option = <ls_params>-opt.
        ENDLOOP.
      ENDIF.

      " Texts
      SELECT * FROM /mbtools/bwvarst INTO TABLE lt_texts
        WHERE infoprov = <ls_var>-infoprov AND variant = <ls_var>-variant.
      IF sy-subrc = 0.
        LOOP AT lt_texts ASSIGNING <ls_texts>.
          APPEND INITIAL LINE TO lt_vari_text ASSIGNING <ls_vari_text>.
          MOVE-CORRESPONDING <ls_texts> TO <ls_vari_text>.
          <ls_vari_text>-mandt = sy-mandt.
          <ls_vari_text>-report = iv_repnm.
        ENDLOOP.
      ENDIF.

      " InfoObjects
      SELECT * FROM /mbtools/bwvarsi INTO TABLE lt_iobjs
        WHERE infoprov = <ls_var>-infoprov AND variant = <ls_var>-variant.
      IF sy-subrc = 0.
        " Map previous variant definition (param/ioinf) to current InfoObjects
        " since InfoProvider definition or InfoObject selection could have changed
        _map_infoobject_selection(
          EXPORTING
            it_ioinf  = it_ioinf
            it_iobjs  = lt_iobjs
          CHANGING
            ct_params = lt_vari_param ).
      ENDIF.

      " Screens
      APPEND INITIAL LINE TO lt_vari_scr ASSIGNING <ls_vari_scr>.
      <ls_vari_scr>-dynnr = '0001'.
      APPEND INITIAL LINE TO lt_vari_scr ASSIGNING <ls_vari_scr>.
      <ls_vari_scr>-dynnr = '1000'.

      " Variants might already exist for report (if it's not generated)
      ASSERT 0 = 0 ##TODO.

      " Create new variant
      CALL FUNCTION 'RS_CREATE_VARIANT_255'
        EXPORTING
          curr_report               = ls_vari_desc-report
          curr_variant              = ls_vari_desc-variant
          vari_desc                 = ls_vari_desc
        TABLES
          vari_contents             = lt_vari_param
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
        /mbtools/cx_exception=>raise_t100( ).
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


  METHOD _map_infoobject_selection.

    TYPES:
      BEGIN OF ty_map,
        field_old TYPE rsfieldnm,
        field_new TYPE rsfieldnm,
      END OF ty_map.

    DATA:
      ls_map TYPE ty_map,
      lt_map TYPE HASHED TABLE OF ty_map WITH UNIQUE KEY field_old.

    FIELD-SYMBOLS:
      <ls_ioinf>  LIKE LINE OF it_ioinf,
      <ls_iobjs>  LIKE LINE OF it_iobjs,
      <ls_map>    LIKE LINE OF lt_map,
      <ls_params> LIKE LINE OF ct_params.

    LOOP AT it_iobjs ASSIGNING <ls_iobjs>.
      READ TABLE it_ioinf ASSIGNING <ls_ioinf>
        WITH KEY iobjnm = <ls_iobjs>-iobjnm.
      IF sy-subrc = 0.
        IF <ls_iobjs>-selname <> <ls_ioinf>-selname.
          " Map select-option for InfoObject (Cxxx) and field checkbox (Sxxx)
          CLEAR ls_map.
          ls_map-field_old = <ls_iobjs>-selname.
          ls_map-field_new = <ls_ioinf>-selname.
          INSERT ls_map INTO TABLE lt_map.
          ls_map-field_old = 'S' && <ls_iobjs>-selname+1(*).
          ls_map-field_new = 'S' && <ls_ioinf>-selname+1(*).
          INSERT ls_map INTO TABLE lt_map.
        ENDIF.
        IF <ls_iobjs>-sidname <> <ls_ioinf>-sidname.
          " Map select-option for SID (Dxxx) and field checkbox (Rxxx)
          CLEAR ls_map.
          ls_map-field_old = <ls_iobjs>-sidname.
          ls_map-field_new = <ls_ioinf>-sidname.
          INSERT ls_map INTO TABLE lt_map.
          ls_map-field_old = 'R' && <ls_iobjs>-sidname+1(*).
          ls_map-field_new = 'R' && <ls_ioinf>-sidname+1(*).
          INSERT ls_map INTO TABLE lt_map.
        ENDIF.
      ELSE.
        " InfoObject does not exist anymore so remove selections
        DELETE ct_params WHERE selname = <ls_iobjs>-selname OR selname = <ls_iobjs>-sidname.
      ENDIF.
    ENDLOOP.

    LOOP AT ct_params ASSIGNING <ls_params>.
      READ TABLE lt_map ASSIGNING <ls_map>
        WITH TABLE KEY field_old = <ls_params>-selname.
      IF sy-subrc = 0.
        <ls_params>-selname = <ls_map>-field_new.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD _promote_lock.
    " Get exclusive lock just before saving

    DATA lv_msg TYPE string.

    CALL FUNCTION 'ENQUEUE_/MBTOOLS/E_VARNT'
      EXPORTING
        mode_/mbtools/bwvars = 'R'
        infoprov             = iv_infoprov
      EXCEPTIONS
        foreign_lock         = 1
        system_failure       = 2
        OTHERS               = 3.
    IF sy-subrc <> 0.
      " If not optimistic lock exist, lock exclusively
      CALL FUNCTION 'ENQUEUE_/MBTOOLS/E_VARNT'
        EXPORTING
          mode_/mbtools/bwvars = 'E'
          infoprov             = iv_infoprov
        EXCEPTIONS
          foreign_lock         = 1
          system_failure       = 2
          OTHERS               = 3.
      IF sy-subrc <> 0.
        lv_msg = |InfoProvider { iv_infoprov } variants are locked|.
        /mbtools/cx_exception=>raise( lv_msg ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD _release_lock.
    " called after deleting or before re-acquiring

    CALL FUNCTION 'DEQUEUE_/MBTOOLS/E_VARNT'
      EXPORTING
        infoprov = iv_infoprov.

  ENDMETHOD.


  METHOD _set_optimistic_lock.
    " Always set when (re-)reading an entry

    DATA lv_msg TYPE string.

    " Existing lock must be released before acquiring a new one
    _release_lock( iv_infoprov ).

    CALL FUNCTION 'ENQUEUE_/MBTOOLS/E_VARNT'
      EXPORTING
        mode_/mbtools/bwvars = 'O'
        infoprov             = iv_infoprov
      EXCEPTIONS
        foreign_lock         = 1
        system_failure       = 2
        OTHERS               = 3.
    IF sy-subrc <> 0.
      lv_msg = |InfoProvider { iv_infoprov } variants are locked|.
      /mbtools/cx_exception=>raise( lv_msg ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
