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
  DATA:
    l_t_init        TYPE rsdq_t_text,
    l_t_body        TYPE rsdq_t_text,
    l_t_selscr      TYPE rsdq_t_text,
    l_t_tables      TYPE rsdq_t_text,
    l_t_seltxts     TYPE rsdq_t_seltext,
    l_t_report      TYPE rsdq_t_text,
    l_t_datadef     TYPE rsdq_t_text,
    l_t_prettycode  TYPE rsdq_t_text,
    l_t_pretcode_ot TYPE rsdrs_t_abapsource, "ot = Other Type
    l_repnm         LIKE sy-repid,
    l_subrc         LIKE sy-subrc.

* check, if report name has been provided,
* otherwise generate report name
  IF i_repnm IS INITIAL.
    PERFORM gen_report_name IN PROGRAM saplrsdq
      CHANGING l_repnm l_subrc.
  ELSE.
    l_repnm = i_repnm.
  ENDIF.

* check, if provided report name is allowed,
* otherwise ...
* anyway ... the real check is done in RSDD_SHOW_ICUBE already
* ... which is BEFORE a possible selection if characteristics
* ... so I see no need to do it here again!
****  IF i_repnm IS NOT INITIAL.
****
****    PERFORM check_report_name
****       USING i_repnm
****       CHANGING l_subrc.
****
****    IF l_subrc = 1.
****      MESSAGE ID 'DBMAN' TYPE 'I' NUMBER 99
****        WITH 'Selected report name ' i_repnm ' not allowed!'
****          'Please start name with letter Y or Z.'.
****      EXIT.
****    ELSEIF l_subrc > 1.
****      MESSAGE ID 'DBMAN' TYPE 'I' NUMBER 99
****        WITH 'Selected report name ' i_repnm ' is already in use!'
****          'Please choose another name.'.
****      EXIT.
****    ENDIF.
****
****  ENDIF.

* generate the selection screen of the report
  PERFORM gen_selection_screen IN PROGRAM saplrsdq
    USING    i_infoprov i_s_dta i_t_dta_dime
             i_show_dimids i_show_sids
             i_use_db_aggregation i_tech_nms
             i_repnm i_tc_no i_tc_id
    CHANGING c_t_ioinf l_t_selscr l_t_body l_t_init l_t_tables l_t_datadef
             l_t_seltxts.

* generate the report body text
  PERFORM gen_report_text IN PROGRAM saplrsdq
    USING    l_repnm i_repnm i_infoprov i_show_sids
             l_t_selscr l_t_body l_t_init l_t_tables l_t_datadef
             c_t_ioinf  l_t_seltxts i_tech_nms i_tc_no i_tc_id
    CHANGING l_t_report.

*>>> MBT Listcube Enhancement
  " Adjust report to support variants
  REPLACE 'MESSAGE E222(DBMAN).' IN TABLE l_t_report WITH ''.
*<<< MBT Listcube Enhancement

* pretty printer
  CALL FUNCTION 'PRETTY_PRINTER'
    EXPORTING
      inctoo = ' '
    TABLES      "ntext  = l_t_prettycode
      ntext  = l_t_pretcode_ot
      otext  = l_t_report.

  CLEAR l_t_report.

  IF i_show_report = rs_c_true.
*   EDITOR-CALL FOR l_t_prettycode DISPLAY-MODE."OSS 1357370
    cl_rsdu_editor=>edit(
      CHANGING
        c_t_code = l_t_pretcode_ot ).
  ENDIF.

* Store report name to be able to delete report after a crash
  CALL FUNCTION 'RSAPOADM_INSERT_REPID'
    EXPORTING
      i_repid = l_repnm
      i_appl  = 'RSDQ/SEL_SCREEN'.

* #CP-SUPPRESS: no direct user input
  INSERT REPORT l_repnm FROM l_t_pretcode_ot UNICODE ENABLING rs_c_true.

*>>> MBT Listcube Enhancement
  " Restore all variants
  /mbtools/cl_bw_listcube=>restore_variants(
    iv_infoprov = i_infoprov
    iv_repnm    = l_repnm
    it_ioinf    = c_t_ioinf ).

  DATA:
    lv_skip   TYPE abap_bool,
    lt_params TYPE /mbtools/cl_bw_listcube=>ty_params.

  /mbtools/cl_bw_listcube=>get_variant(
    IMPORTING
      ev_skip   = lv_skip
      et_params = lt_params ).

  " Call generated selection report
  IF lt_params IS INITIAL.
    SUBMIT (l_repnm)
      VIA SELECTION-SCREEN AND RETURN.
  ELSEIF lv_skip = abap_true.
    SUBMIT (l_repnm)
      WITH SELECTION-TABLE lt_params
      AND RETURN.
  ELSE.
    SUBMIT (l_repnm)
      WITH SELECTION-TABLE lt_params
      VIA SELECTION-SCREEN AND RETURN.
  ENDIF.

  " Backup all variants
  /mbtools/cl_bw_listcube=>backup_variants(
    iv_infoprov = i_infoprov
    iv_repnm    = l_repnm
    it_ioinf    = c_t_ioinf ).
*<<< MBT Listcube Enhancement

  CALL FUNCTION 'RSAPOADM_DELETE_REPID'
    EXPORTING
      i_repid = l_repnm.

* delete report, if name not provided by caller
  IF i_repnm IS INITIAL.
* #CP-SUPPRESS: no direct user input

    PERFORM remove_report IN PROGRAM saplrsdq
      USING l_repnm.

  ENDIF.

ENDFUNCTION.
