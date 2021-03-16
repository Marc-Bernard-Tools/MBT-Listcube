FUNCTION /mbtools/bw_view_infoprov.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_INFOPROV) TYPE  RSINFOPROV
*"     REFERENCE(I_S_DTA) TYPE  RSD_S_DTA
*"     REFERENCE(I_T_DTA_DIME) TYPE  RSD_T_DTA_DIME
*"     VALUE(I_SHOW_SIDS) TYPE  RS_BOOL DEFAULT RS_C_FALSE
*"     VALUE(I_SHOW_DIMIDS) TYPE  RS_BOOL DEFAULT RS_C_FALSE
*"     VALUE(I_USE_DB_AGGREGATION) TYPE  RS_BOOL DEFAULT RS_C_FALSE
*"     VALUE(I_TECH_NMS) TYPE  RS_BOOL DEFAULT RS_C_FALSE
*"     VALUE(I_SHOW_REPORT) TYPE  RS_BOOL DEFAULT RS_C_FALSE
*"     VALUE(I_REPNM) LIKE  SY-REPID OPTIONAL
*"     VALUE(I_TC_NO) TYPE  I OPTIONAL
*"     VALUE(I_TC_ID) TYPE  RSDRSYSID OPTIONAL
*"  CHANGING
*"     REFERENCE(C_T_IOINF) TYPE  RSDQ_T_IOBJ_INFO
*"  EXCEPTIONS
*"      DTA_NOT_FOUND
*"      ILLEGAL_INPUT
*"      X_MESSAGE
*"----------------------------------------------------------------------

*>>> MBT Listcube Enhancement
* Enhanced version of RSDQ_VIEW_INFOPROV
  DATA:
    lv_skip   TYPE abap_bool,
    lt_params TYPE /mbtools/cl_bw_listcube=>ty_params.
*<<< MBT Listcube Enhancement

  DATA:
    lt_init        TYPE rsdq_t_text,
    lt_body        TYPE rsdq_t_text,
    lt_selscr      TYPE rsdq_t_text,
    lt_tables      TYPE rsdq_t_text,
    lt_seltxts     TYPE rsdq_t_seltext,
    lt_report      TYPE rsdq_t_text,
    lt_datadef     TYPE rsdq_t_text,
    lt_prettycode  TYPE rsdq_t_text,
    lt_pretcode_ot TYPE rsdrs_t_abapsource, "ot = Other Type
    lv_repnm       TYPE sy-repid,
    lv_subrc       TYPE sy-subrc.

* check, if report name has been provided,
* otherwise generate report name
  IF i_repnm IS INITIAL.
    PERFORM gen_report_name IN PROGRAM saplrsdq
      CHANGING lv_repnm lv_subrc.
  ELSE.
    lv_repnm = i_repnm.
  ENDIF.

* generate the selection screen of the report
  PERFORM gen_selection_screen IN PROGRAM saplrsdq
    USING    i_infoprov i_s_dta i_t_dta_dime
             i_show_dimids i_show_sids
             i_use_db_aggregation i_tech_nms
             i_repnm i_tc_no i_tc_id
    CHANGING c_t_ioinf lt_selscr lt_body lt_init lt_tables lt_datadef
             lt_seltxts.

* generate the report body text
  PERFORM gen_report_text IN PROGRAM saplrsdq
    USING    lv_repnm i_repnm i_infoprov i_show_sids
             lt_selscr lt_body lt_init lt_tables lt_datadef
             c_t_ioinf  lt_seltxts i_tech_nms i_tc_no i_tc_id
    CHANGING lt_report.

*>>> MBT Listcube Enhancement
  " Adjust report to support variants
  REPLACE 'MESSAGE E222(DBMAN).' IN TABLE lt_report WITH ''.
*<<< MBT Listcube Enhancement

* pretty printer
  CALL FUNCTION 'PRETTY_PRINTER'
    EXPORTING
      inctoo = ' '
    TABLES
      ntext  = lt_pretcode_ot
      otext  = lt_report.

  CLEAR lt_report.

  IF i_show_report = rs_c_true.
*   EDITOR-CALL FOR lt_prettycode DISPLAY-MODE."OSS 1357370
    cl_rsdu_editor=>edit( CHANGING c_t_code = lt_pretcode_ot ).
  ENDIF.

* Store report name to be able to delete report after a crash
  CALL FUNCTION 'RSAPOADM_INSERT_REPID'
    EXPORTING
      i_repid = lv_repnm
      i_appl  = 'RSDQ/SEL_SCREEN'.

* #CP-SUPPRESS: no direct user input
  INSERT REPORT lv_repnm FROM lt_pretcode_ot UNICODE ENABLING rs_c_true.

*>>> MBT Listcube Enhancement
  " Restore all variants
  /mbtools/cl_bw_listcube=>restore_variants(
    iv_infoprov = i_infoprov
    iv_repnm    = lv_repnm
    it_ioinf    = c_t_ioinf ).

  /mbtools/cl_bw_listcube=>get_variant(
    IMPORTING
      ev_skip   = lv_skip
      et_params = lt_params ).

  " Call generated selection report
  IF lt_params IS INITIAL.
    SUBMIT (lv_repnm)
      VIA SELECTION-SCREEN AND RETURN.
  ELSEIF lv_skip = abap_true.
    SUBMIT (lv_repnm)
      WITH SELECTION-TABLE lt_params
      AND RETURN.
  ELSE.
    SUBMIT (lv_repnm)
      WITH SELECTION-TABLE lt_params
      VIA SELECTION-SCREEN AND RETURN.
  ENDIF.

  " Backup all variants
  /mbtools/cl_bw_listcube=>backup_variants(
    iv_infoprov = i_infoprov
    iv_repnm    = lv_repnm
    it_ioinf    = c_t_ioinf ).
*<<< MBT Listcube Enhancement

  CALL FUNCTION 'RSAPOADM_DELETE_REPID'
    EXPORTING
      i_repid = lv_repnm.

* delete report, if name not provided by caller
  IF i_repnm IS INITIAL.
* #CP-SUPPRESS: no direct user input

    PERFORM remove_report IN PROGRAM saplrsdq
      USING lv_repnm.

  ENDIF.

ENDFUNCTION.
