*&---------------------------------------------------------------------*
*& Include          ZCOR0600PBO
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  SET PF-STATUS 'PF_0100'.

  IF GV_MODE EQ GC_E.
    " [CO] 설비 WBS 등록 현황(관리자)
    SET TITLEBAR  'TT_0100' WITH TEXT-T02.
  ELSE.
    " [CO] 설비 WBS 등록 현황
    SET TITLEBAR  'TT_0100' WITH TEXT-T01.
  ENDIF.


ENDMODULE.
*&---------------------------------------------------------------------*
*& Module INIT_ALV_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE INIT_ALV_0100 OUTPUT.

  IF GR_ALV IS BOUND.

    GR_ALV->REFRESH( ).

  ELSE.

    ZCL_CO_COMMON=>GET_CONTAINER_01(
      IMPORTING
        ER_SPLIT    = GR_SPLIT   " Splitter Control
        ER_CON_TOP  = GR_CON_TOP " Container - Top of Page
        ER_CON_MAIN = GR_CON_ALV " Container - Main
      EXCEPTIONS
        CNTL_ERROR        = 1
        CNTL_SYSTEM_ERROR = 2
    ).

    CHECK SY-SUBRC EQ 0.

    GR_SPLIT->SET_ROW_HEIGHT(
      EXPORTING
        ID                = 1                " Row ID
        HEIGHT            = 120              " Height
      EXCEPTIONS
        CNTL_ERROR        = 1                " See CL_GUI_CONTROL
        CNTL_SYSTEM_ERROR = 2                " See CL_GUI_CONTROL
        OTHERS            = 3
    ).

    PERFORM CREATE_TOP_OF_PAGE_0100.
    PERFORM CREATE_MAIN_GRID_0100.

  ENDIF.

ENDMODULE.
