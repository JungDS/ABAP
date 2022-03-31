*&---------------------------------------------------------------------*
*& Include          ZCOR0530PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  CASE GC_X.
    WHEN PA_UPLD.
      SET PF-STATUS 'PF_0100'.
      SET TITLEBAR  'TT_0100' WITH TEXT-T06.   " [CO] 설비WBS 속성 업로드
    WHEN PA_DISP.
      SET PF-STATUS 'PF_0100' EXCLUDING 'SAVE'.
      SET TITLEBAR  'TT_0100' WITH TEXT-T07.   " [CO] 설비WBS 속성 조회
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module INIT_ALV_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE INIT_ALV_0100 OUTPUT.

  IF GR_CON_SPLIT IS BOUND.
    IF GV_TREE EQ 'X'.
      IF GR_TREE IS NOT INITIAL.
        GR_TREE->DISPLAY( ).
      ENDIF.
    ELSE.
      IF GR_ALV IS NOT INITIAL.
        GR_ALV->REFRESH( ).
      ENDIF.
    ENDIF.

  ELSE.

    ZCL_CO_COMMON=>GET_CONTAINER_01(
      IMPORTING
        ER_SPLIT          = GR_CON_SPLIT  " Splitter Control
        ER_CON_TOP        = GR_CON_TOP    " Container - Top of Page
        ER_CON_MAIN       = GR_CON_MAIN   " Container - Main
      EXCEPTIONS
        CNTL_ERROR        = 1
        CNTL_SYSTEM_ERROR = 2
    ).

    CHECK SY-SUBRC EQ 0.

    PERFORM CREATE_CONTAINER_MAIN.


    IF PA_DISP EQ GC_X.
      GR_CON_SPLIT->SET_ROW_HEIGHT(
        EXPORTING
          ID                = 1                 " Row ID
          HEIGHT            = 60                " Height
        EXCEPTIONS
          CNTL_ERROR        = 1                " See CL_GUI_CONTROL
          CNTL_SYSTEM_ERROR = 2                " See CL_GUI_CONTROL
          OTHERS            = 3
      ).
    ENDIF.

    PERFORM CREATE_TOP_OF_PAGE_0100.

    IF GV_TREE EQ 'X'.
      PERFORM CREATE_SALV_TREE_0100.
    ELSE.
      PERFORM CREATE_MAIN_GRID_0100.
    ENDIF.

  ENDIF.
ENDMODULE.
