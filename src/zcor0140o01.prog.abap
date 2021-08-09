*&---------------------------------------------------------------------*
*& Include          ZCOR0140O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
* - Prefix 정의
*   1. PF_xxxx  : STATUS Prefix : PF_   //   xxxx: Screen No
*   2. TT_xxxx  : TITLEBAR Prefix : TT_   //   xxxx: Screen No


  SET PF-STATUS 'PF_0100'.
  SET TITLEBAR 'TT_0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ALV_INIT_DISPLAY_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALV_INIT_DISPLAY_0100 OUTPUT.

  "-- 화면의  GRID가 BOUND되었는지 확인한다.
  IF GR_GRID1 IS NOT BOUND.

    "-- GRID의 INSTANCE를 생성한다.
    PERFORM CREATE_INSTANCE_0100.

    "-- GRID의 LAYOUT 속성을 정의한다.
    PERFORM INIT_LAYOUT_0100.

    "-- ALV Standard toolbar button cotrol
    PERFORM SET_GRID_EXCLUDE_0100.

    "-- ALV Sort
    PERFORM ALV_SORT_0100.

    "-- Field Attribute을 사용자의 요구사항에 맞게 변경
    PERFORM APPEND_FIELDCAT_0100.

    "-- ALV Events 등록
    PERFORM REGIST_ALV_EVENT_0100 USING GR_GRID1.

    "-- ALV Display
    PERFORM DISPLAY_ALV_GRID_0100.



*    "-- ALV Title
*    PERFORM DISPLAY_ALV_TITLE_0100.

  ELSE.

    "-- ALV Refresh
    PERFORM REFRESH_GRID_0100.

  ENDIF.

ENDMODULE.                 " ALV_INIT_DISPLAY_0100  OUTPUT
