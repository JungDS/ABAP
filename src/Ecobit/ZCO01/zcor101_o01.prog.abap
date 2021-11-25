*&---------------------------------------------------------------------*
*& Include          ZCOR101_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR  'TITLE_0100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module INIT_ALV_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE INIT_ALV_0100 OUTPUT.
  IF GO_CONTAINER IS INITIAL.
    PERFORM CREATE_OBJ.
    PERFORM SETTING_LAYOUT.
    PERFORM SETTING_FCAT.
    PERFORM SETTING_EVENT.
    PERFORM SETTING_EXCLUDE CHANGING GT_EXCLUDE[].
    PERFORM DISPLAY_ALV.
  ELSE.
    PERFORM REFRESH_ALV.
  ENDIF.
ENDMODULE.
