*&---------------------------------------------------------------------*
*& Include          ZCOR0040O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA fcode TYPE TABLE OF sy-ucomm.

  REFRESH fcode.

  CASE gv_mode.

    WHEN 'S'.
      APPEND '&SAV' TO fcode.
      " ADD BSGSM_FCM
      APPEND '&EDIT' TO fcode.
      APPEND '&DELE' TO fcode.
    WHEN OTHERS.

       IF gv_flag IS INITIAL.  " IT FCM  롤...
        APPEND '&DELE' TO fcode.
      ENDIF.


  ENDCASE.

  SET PF-STATUS 'PF_0100' EXCLUDING fcode.
  SET TITLEBAR  'TT_0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ALV_INIT_DISPLAY_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE alv_init_display_0100 OUTPUT.

  "-- 화면의  GRID가 BOUND되었는지 확인한다.
  IF gr_grid1 IS NOT BOUND.

    "-- GRID의 INSTANCE를 생성한다.
    PERFORM create_instance_0100.

    "-- GRID의 LAYOUT 속성을 정의한다.
    PERFORM init_layout_0100.

    "-- ALV Standard toolbar button cotrol
    PERFORM set_grid_exclude_0100.

    "-- ALV Sort
    PERFORM alv_sort_0100.

    "-- Field Attribute을 사용자의 요구사항에 맞게 변경
    PERFORM append_fieldcat_0100.

    "-- ALV Top of page - create object
    PERFORM top_of_page_create_object_0100.
    "-- ALV Top of page - make data & display
    PERFORM make_top_of_page_data_0100.

    "-- ALV Events 등록
    PERFORM regist_alv_event_0100 USING gr_grid1.

    "-- ALV Display
    PERFORM display_alv_grid_0100.

*    "-- ALV Title
*    PERFORM DISPLAY_ALV_TITLE_0100.

  ELSE.

    "-- ALV Refresh
    PERFORM refresh_grid_0100.

  ENDIF.

ENDMODULE.                 " ALV_INIT_DISPLAY_0100  OUTPUT
