*&---------------------------------------------------------------------*
*& Include          ZCOR0250O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  DATA FCODE TYPE TABLE OF SY-UCOMM.
  DATA LV_TITLE TYPE CHAR100.

  REFRESH FCODE.

  CASE GV_MODE.

    WHEN 'S'.
      APPEND '&SAV' TO FCODE.
      LV_TITLE = TEXT-I02.

    WHEN OTHERS.
      LV_TITLE = TEXT-I01.

  ENDCASE.

  SET PF-STATUS 'PF_0100' EXCLUDING FCODE.
  SET TITLEBAR  'TT_0100' WITH LV_TITLE.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ALV_INIT_DISPLAY_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE ALV_INIT_DISPLAY_0100 OUTPUT.

  IF GR_GRID1 IS NOT BOUND.


    PERFORM CREATE_INSTANCE_0100.
    PERFORM INIT_LAYOUT_0100.
    PERFORM SET_GRID_EXCLUDE_0100.
    PERFORM ALV_SORT_0100.
    PERFORM APPEND_FIELDCAT_0100.

    PERFORM TOP_OF_PAGE_CREATE_OBJECT_0100.
    PERFORM MAKE_TOP_OF_PAGE_DATA_0100.
    PERFORM REGIST_ALV_EVENT_0100 USING GR_GRID1.
    PERFORM DISPLAY_ALV_GRID_0100.


  ELSE.

    PERFORM REFRESH_GRID_0100.

  ENDIF.

ENDMODULE.                 " ALV_INIT_DISPLAY_0100  OUTPUT
