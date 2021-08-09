*&---------------------------------------------------------------------*
*& Include          ZCOR0190O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  SET PF-STATUS 'PF_0100'.
  SET TITLEBAR  'TT_0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ALV_INIT_DISPLAY_0100  OUTPUT
*&---------------------------------------------------------------------*
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

    "-- ALV Top of page - create object
    PERFORM TOP_OF_PAGE_CREATE_OBJECT_0100.

    "-- ALV Top of page - make data & display
    PERFORM MAKE_TOP_OF_PAGE_DATA_0100.

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
