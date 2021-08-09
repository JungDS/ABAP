*&---------------------------------------------------------------------*
*& Include          ZCOR0240I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.

*-- OK_CODE ACTIONS.
  CASE OK_CODE.
    WHEN '&CNC'.
      LEAVE TO SCREEN 0.
    WHEN '&EXT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CLEAR: SAVE_OK, GV_EXIT, GV_ANSWER.

  "-- move ok code.
  SAVE_OK = OK_CODE.   CLEAR: OK_CODE.

*-- ALV CHECKED CHANGE DATA
  CALL METHOD GR_GRID1->CHECK_CHANGED_DATA( ).

*-- SAVE_OK ACTIONS.
  CASE SAVE_OK.
    WHEN '&BCK'.
      LEAVE TO SCREEN 0.

    WHEN '&SAVE'.

*      "-- checked saved data.
*      PERFORM CHECKED_SAVED_DATA.
*
*      CHECK GV_EXIT IS INITIAL.
*
      "-- Popup to confirm
      PERFORM POPUP_TO_CONFIRM.

      CHECK GV_ANSWER EQ '1'.

      PERFORM BAPI_COSTACTPLN_POSTPRIMCOST.
      CHECK GV_EXIT IS INITIAL.

      "-- Save data
      PERFORM SAVE_DATA_RTN.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
