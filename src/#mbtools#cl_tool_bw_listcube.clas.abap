CLASS /mbtools/cl_tool_bw_listcube DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* MBT Listcube
*
* (c) MBT 2021 https://marcbernardtools.com/
************************************************************************
  PUBLIC SECTION.

    INTERFACES /mbtools/if_tool.

    CONSTANTS:
      BEGIN OF c_tool,
        version      TYPE string VALUE '1.0.0' ##NO_TEXT,
        title        TYPE string VALUE 'MBT Listcube' ##NO_TEXT,
        bundle_id    TYPE i VALUE 0,
        download_id  TYPE i VALUE 5227,
        description  TYPE string
        VALUE 'Variants for transaction LISTCUBE: Simple, powerful, free!' ##NO_TEXT,
        has_launch   TYPE abap_bool VALUE abap_true,
        mbt_command  TYPE string VALUE 'LISTCUBE',
        mbt_shortcut TYPE string VALUE 'LC',
      END OF c_tool.

    METHODS constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_tool TYPE REF TO /mbtools/cl_tools.

ENDCLASS.



CLASS /mbtools/cl_tool_bw_listcube IMPLEMENTATION.


  METHOD /mbtools/if_tool~launch.
    /mbtools/cl_sap=>run_transaction( '/MBTOOLS/LISTCUBE' ).
  ENDMETHOD.


  METHOD constructor.
    CREATE OBJECT mo_tool EXPORTING io_tool = me.
    /mbtools/if_tool~ms_manifest = mo_tool->ms_manifest.
  ENDMETHOD.
ENDCLASS.
