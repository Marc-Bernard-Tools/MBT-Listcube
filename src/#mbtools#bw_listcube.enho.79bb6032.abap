"Name: \FU:RSDQ_VIEW_INFOPROV\SE:BEGIN\EI
ENHANCEMENT 0 /MBTOOLS/BW_LISTCUBE.
************************************************************************
* MBT Listcube
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-only
************************************************************************

  IF /mbtools/cl_switches=>is_active( /mbtools/cl_switches=>c_tool-mbt_listcube ) = abap_true.

    CALL FUNCTION '/MBTOOLS/BW_VIEW_INFOPROV'
      EXPORTING
        i_infoprov           = i_infoprov
        i_s_dta              = i_s_dta
        i_t_dta_dime         = i_t_dta_dime
        i_show_sids          = i_show_sids
        i_show_dimids        = i_show_dimids
        i_tech_nms           = i_tech_nms
        i_use_db_aggregation = i_use_db_aggregation
        i_repnm              = i_repnm
      CHANGING
        c_t_ioinf            = c_t_ioinf
      EXCEPTIONS
        dta_not_found        = 1
        illegal_input        = 2
        x_message            = 3
        OTHERS               = 4.
    CASE sy-subrc.
      WHEN 0.
        RETURN.
      WHEN 1.
        RAISE dta_not_found.
      WHEN 2.
        RAISE illegal_input.
      WHEN OTHERS.
        RAISE x_message.
    ENDCASE.

  ENDIF.

ENDENHANCEMENT.
