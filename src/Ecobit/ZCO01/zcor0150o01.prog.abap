*&---------------------------------------------------------------------*
*& Include          ZCOR0150O01
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

    "-- Field DropDown List.
    PERFORM APPEND_DROPDOWN_LIST_0100.

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
*&---------------------------------------------------------------------*
*& Module STATUS_0110 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0110 OUTPUT.
* - Prefix 정의
*   1. PF_xxxx  : STATUS Prefix : PF_   //   xxxx: Screen No
*   2. TT_xxxx  : TITLEBAR Prefix : TT_   //   xxxx: Screen No


  SET PF-STATUS 'PF_0110'.
  SET TITLEBAR 'TT_0110'.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module MODIFY_SCREEN_0110 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE MODIFY_SCREEN_0110 OUTPUT.

  LOOP AT SCREEN.

    IF GV_RAD1 IS NOT INITIAL.
      CASE SCREEN-NAME.
        WHEN 'GV_VER1'.
          SCREEN-INPUT = 0.
        WHEN 'GV_VER2'.
          SCREEN-INPUT = 0.
        WHEN 'GV_VER3'.
          SCREEN-INPUT = 0.
      ENDCASE.
    ELSEIF GV_RAD2 IS NOT INITIAL.
      CASE SCREEN-NAME.
        WHEN 'GV_VER1'.
          SCREEN-INPUT = 0.
        WHEN 'GV_VER2'.
          SCREEN-INPUT = 1.
        WHEN 'GV_VER3'.
          SCREEN-INPUT = 0.
      ENDCASE.
    ELSEIF GV_RAD3 IS NOT INITIAL.
      CASE SCREEN-NAME.
        WHEN 'GV_VER1'.
          SCREEN-INPUT = 0.
        WHEN 'GV_VER2'.
          SCREEN-INPUT = 0.
        WHEN 'GV_VER3'.
          SCREEN-INPUT = 1.
      ENDCASE.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.



*      LOOP AT SCREEN.
*
*        IF SCREEN-NAME = 'GV_VER1'.
*          SCREEN-INPUT = 0.
*          MODIFY SCREEN.
*        ENDIF.
*
*        IF GV_RAD2 IS NOT INITIAL.
*          CLEAR GV_VER3.
*
*          IF SCREEN-NAME = 'GV_VER2'.
*            SCREEN-INPUT = 1.
*            MODIFY SCREEN.
*          ELSEIF SCREEN-NAME = 'GV_VER3'.
*            SCREEN-INPUT = 0.
*            MODIFY SCREEN.
*          ENDIF.
*
*        ELSEIF GV_RAD3 IS NOT INITIAL.
*          CLEAR GV_VER2.
*
*          IF SCREEN-NAME = 'GV_VER2'.
*            SCREEN-INPUT = 0.
*            MODIFY SCREEN.
*          ELSEIF SCREEN-NAME = 'GV_VER3'.
*            SCREEN-INPUT = 1.
*            MODIFY SCREEN.
*          ENDIF.
*
*        ENDIF.
*
*      ENDLOOP.
ENDMODULE.
