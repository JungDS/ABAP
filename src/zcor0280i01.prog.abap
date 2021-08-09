*&---------------------------------------------------------------------*
*& Include          ZCOR0280I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.


  CLEAR: GV_EXIT, GV_ANSWER.

*-- OK_CODE ACTIONS.
  CASE OK_CODE.

    WHEN '&CNC'.
      LEAVE TO SCREEN 0.
    WHEN '&EXT'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CLEAR: SAVE_OK, GV_EXIT, GV_ANSWER, GV_VALID.

  "-- move ok code.
  SAVE_OK = OK_CODE.   CLEAR: OK_CODE.

  CASE SAVE_OK.

    WHEN '&BCK'.
      LEAVE TO SCREEN 0.

    WHEN '&HELP'.
      PERFORM CALL_POPUP_HELP(ZCAR9000)
               USING SY-REPID SY-DYNNR SY-LANGU ''.

    WHEN '&SAVE'.
      PERFORM POPUP_TO_CONFIRM USING TEXT-PT1
                                     TEXT-QT1.

      CHECK GV_ANSWER = '1'.

      PERFORM BAPI_COSTACTPLN_POSTPRIMCOST.

    WHEN '&DOWN'.
      PERFORM CHECK_CHANGE CHANGING GV_VALID.
      IF GV_VALID IS NOT INITIAL.
        MESSAGE I001 WITH TEXT-E01.
      ELSE.
        PERFORM EXCEL_DOWNLOAD.
      ENDIF.

    WHEN OTHERS.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
