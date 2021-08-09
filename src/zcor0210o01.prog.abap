*&---------------------------------------------------------------------*
*& Include          ZCOR0160O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  SET PF-STATUS 'PF_0100'.
  SET TITLEBAR  'TT_0100'.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module ALV_INIT_DISPLAY_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE ALV_INIT_DISPLAY_0100 OUTPUT.

  IF GR_TREE_DATA IS NOT BOUND.

    PERFORM CREATE_INSTANCE_0100.

    PERFORM BUILD_HIERARCHY_HEADER_0100_01.
    PERFORM APPEND_FIELDCAT_0100_01.
    PERFORM DISPLAY_ALV_TREE_0100_01.
    PERFORM REGIST_ALV_EVENT_0100_01.    "TREE EVENT

  ENDIF.

ENDMODULE.
