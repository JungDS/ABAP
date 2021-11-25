*&---------------------------------------------------------------------*
*& Include          ZCOR0330I01
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

  SAVE_OK = OK_CODE.   CLEAR: OK_CODE.

  CASE SAVE_OK.

    WHEN '&BCK'.
      LEAVE TO SCREEN 0.

    WHEN '&DELE'.
      PERFORM CHECKED_SAVED_DATA USING SAVE_OK.

      CHECK GV_EXIT IS INITIAL.
      PERFORM POPUP_TO_CONFIRM USING TEXT-PT1
                                     TEXT-QT1.

      CHECK GV_ANSWER EQ '1'.
      PERFORM SAVE_DATA_RTN.

    WHEN '&URL'.
      PERFORM CHECKED_SAVED_DATA USING SAVE_OK.

      CHECK GV_EXIT IS INITIAL.
      PERFORM CALL_URL.

    WHEN '&HELP'.
      PERFORM CALL_POPUP_HELP(ZCAR9000)
               USING SY-REPID SY-DYNNR SY-LANGU ''.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
