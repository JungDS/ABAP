*&---------------------------------------------------------------------*
*& Include          ZCOR0330O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  SET PF-STATUS 'PF_0100'.
  SET TITLEBAR 'TT_0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ALV_INIT_DISPLAY_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE ALV_INIT_DISPLAY_0100 OUTPUT.

  IF GR_GRID_HEAD IS NOT BOUND.
    PERFORM CREATE_INSTANCE_0100.

    PERFORM INIT_LAYOUT_0100.
    PERFORM SET_GRID_EXCLUDE_0100.

    PERFORM ALV_SORT_0100.
    PERFORM APPEND_FIELDCAT_0100.

    PERFORM TOP_OF_PAGE_CREATE_OBJECT_0100.
    PERFORM MAKE_TOP_OF_PAGE_DATA_0100.

    PERFORM REGIST_ALV_EVENT_0100 USING GR_GRID_HEAD.

    PERFORM DISPLAY_ALV_GRID_0100.

    PERFORM APPEND_FIELDCAT_0100_01 USING ''.
    PERFORM DISPLAY_ALV_GRID_0100_01.

*  ELSE.
*
*    "-- ALV Refresh
*    PERFORM REFRESH_GRID_0100.
*    PERFORM REFRESH_GRID_0100_01.

  ENDIF.

ENDMODULE.                 " ALV_INIT_DISPLAY_0100  OUTPUT
