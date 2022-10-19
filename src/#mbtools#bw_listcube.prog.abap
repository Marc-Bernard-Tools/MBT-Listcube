REPORT /mbtools/bw_listcube.
************************************************************************
* MBT Listcube
*
* This tool adds full variant features to the classic LISTCUBE trans-
* action
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-only
************************************************************************

TABLES:
  sscrfields.
*-----------------------------------------------------------------------

* Main
SELECTION-SCREEN:
  BEGIN OF SCREEN 200 AS SUBSCREEN,
    BEGIN OF BLOCK b200 WITH FRAME,
      COMMENT /1(77) sc_t200,
      COMMENT /1(77) sc_t201,
    END OF BLOCK b200,
    BEGIN OF BLOCK b210 WITH FRAME.
PARAMETERS:
  p_dta    TYPE rsinfoprov OBLIGATORY MEMORY ID rsdq_infoprov,
  p_dtatxt TYPE rstxtlg.
SELECTION-SCREEN: END OF BLOCK b210,
BEGIN OF BLOCK b220 WITH FRAME.
PARAMETERS:
  p_varnm  TYPE rsvariant,
  p_vartxt TYPE rstxtlg.
SELECTION-SCREEN SKIP.
PARAMETERS:
  p_skip AS CHECKBOX.
SELECTION-SCREEN: END OF BLOCK b220,
END OF SCREEN 200.

*-----------------------------------------------------------------------

* About
SELECTION-SCREEN:
  BEGIN OF SCREEN 900 AS SUBSCREEN,
    BEGIN OF BLOCK b900 WITH FRAME,
      COMMENT /1(50) sc_t900,
      COMMENT 60(25) sc_t901,
      SKIP,
      COMMENT /1(77) sc_t902,
    END OF BLOCK b900,
    BEGIN OF BLOCK b910 WITH FRAME,
      PUSHBUTTON /1(55) sc_docu USER-COMMAND docu,
      SKIP,
      PUSHBUTTON /1(55) sc_tool USER-COMMAND tool,
      SKIP,
      PUSHBUTTON /1(55) sc_lice USER-COMMAND lice,
      SKIP,
      PUSHBUTTON /1(55) sc_home USER-COMMAND home,
    END OF BLOCK b910,
  END OF SCREEN 900.

*-----------------------------------------------------------------------

* Header
SELECTION-SCREEN:
  BEGIN OF BLOCK sc_header,
    SKIP,
    SKIP,
    COMMENT /3(77) sc_t001 FOR FIELD p_dta,
    SKIP,
  END OF BLOCK sc_header,
  BEGIN OF TABBED BLOCK sc_tab FOR 22 LINES,
    TAB (40) sc_tab2 USER-COMMAND sc_push2 DEFAULT SCREEN 200,
    TAB (40) sc_tab9 USER-COMMAND sc_push9 DEFAULT SCREEN 900,
  END OF BLOCK sc_tab.

*-----------------------------------------------------------------------

CONSTANTS:
  c_title TYPE string VALUE /mbtools/cl_tool_bw_listcube=>c_tool-title.

DATA:
  gv_ok_code TYPE sy-ucomm,
  go_tool    TYPE REF TO /mbtools/cl_tool,
  go_screen  TYPE REF TO /mbtools/cl_screen,
  go_app     TYPE REF TO /mbtools/cl_bw_listcube.

*-----------------------------------------------------------------------

MODULE pbo_100 OUTPUT.

  go_screen->banner( abap_false ).

  go_app->pbo( ).

ENDMODULE.

MODULE pai_100 INPUT.

  go_app->pai( CHANGING cv_ok_code = gv_ok_code ).

ENDMODULE.

INITIALIZATION.

  IF /mbtools/cl_switches=>is_active( c_title ) = abap_false.
    MESSAGE e004(/mbtools/bc) WITH c_title.
    RETURN.
  ENDIF.

  CREATE OBJECT go_app.

  go_tool   = /mbtools/cl_tool_manager=>factory( c_title ).
  go_screen = /mbtools/cl_screen=>factory( c_title ).

  go_screen->init(
    IMPORTING
      ev_text      = sc_t001
      ev_about     = sc_tab9
      ev_title     = sc_t900
      ev_version   = sc_t901
      ev_copyright = sc_t902
      ev_docu      = sc_docu
      ev_tool      = sc_tool
      ev_home      = sc_home
      ev_lice      = sc_lice ).

  sc_tab2 = go_screen->header(
    iv_icon = icon_biw_info_cube
    iv_text = 'Selection'(001) ).

  sc_t200 = 'Select which InfoProvider to view and optionally'(200).
  sc_t201 = 'which variant to use'(201).

  IF p_dta IS INITIAL.
    GET PARAMETER ID 'RSDQ_INFOPROV' FIELD p_dta.
  ENDIF.

*-----------------------------------------------------------------------

AT SELECTION-SCREEN.

  go_app->screen( ).

  go_screen->ucomm( sscrfields-ucomm ).

*-----------------------------------------------------------------------

AT SELECTION-SCREEN OUTPUT.

  go_screen->banner( ).

  go_app->screen( ).

  p_dtatxt = /mbtools/cl_bw_listcube=>get_infoprov_description( p_dta ).

  p_vartxt = /mbtools/cl_bw_listcube=>get_variant_description(
    iv_infoprov = p_dta
    iv_variant  = p_varnm ).

*-----------------------------------------------------------------------

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_dta.

  cl_rsd_dta=>f4(
    EXPORTING
      i_tlogo    = '%'
    IMPORTING
      e_txtlg    = p_dtatxt
    CHANGING
      c_infoprov = p_dta ).

*-----------------------------------------------------------------------

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varnm.

  /mbtools/cl_bw_listcube=>f4_variant(
    EXPORTING
      iv_infoprov = p_dta
    IMPORTING
      ev_variant  = p_varnm
      ev_vartxt   = p_vartxt ).

*-----------------------------------------------------------------------

START-OF-SELECTION.

  LOG-POINT ID /mbtools/bc SUBKEY c_title FIELDS sy-datum sy-uzeit sy-uname.

  SET PARAMETER ID 'RSDQ_INFOPROV' FIELD p_dta.

  go_app->call_listcube(
    iv_infoprov = p_dta
    iv_variant  = p_varnm
    iv_skip     = p_skip ).
