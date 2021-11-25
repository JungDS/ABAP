*&---------------------------------------------------------------------*
*& Include          ZCOR0540PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.


  IF GV_MODE EQ GC_E.

    SET PF-STATUS 'PF_0100'.
    SET TITLEBAR  'TT_0100' WITH TEXT-T01 TEXT-T02.   " [CO] BU 매핑정보관리 수정

  ELSE.

    SET PF-STATUS 'PF_0100' EXCLUDING 'SAVE'.
    SET TITLEBAR  'TT_0100' WITH TEXT-T01 TEXT-T03.   " [CO] BU 매핑정보관리 조회

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module INIT_ALV_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE INIT_ALV_0100 OUTPUT.

  IF GR_ALV IS BOUND.
    GR_ALV->REFRESH( ).
  ELSE.
    PERFORM CREATE_MAIN_GRID_0100.
  ENDIF.



  IF GV_MODE EQ GC_E.
    GR_ALV->MR_ALV_GRID->SET_READY_FOR_INPUT( 1 ).
  ELSE.
    GR_ALV->MR_ALV_GRID->SET_READY_FOR_INPUT( 0 ).
  ENDIF.

ENDMODULE.
