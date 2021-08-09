*&---------------------------------------------------------------------*
*& Include          ZCOR0160I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE EXIT INPUT.

*-- OK_CODE ACTIONS.
  CASE OK_CODE.
    WHEN '&CNC'.
      LEAVE TO SCREEN 0.
    WHEN '&EXT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CLEAR: SAVE_OK, GV_EXIT.

  SAVE_OK = OK_CODE.   CLEAR: OK_CODE.
  CASE SAVE_OK.

    WHEN '&BCK'.
      LEAVE TO SCREEN 0.

    WHEN '&HELP'.
      PERFORM CALL_POPUP_HELP(ZCAR9000)
               USING SY-REPID SY-DYNNR SY-LANGU ''.

    WHEN '&DOWN'.
      PERFORM EXCEL_DOWNLOAD.

    WHEN OTHERS.

  ENDCASE.

ENDMODULE.
