*&---------------------------------------------------------------------*
*& Include          ZCOR0050I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.

  CLEAR: GV_EXIT, GV_ANSWER, GV_CHANGE, GV_VALID.

*-- OK_CODE ACTIONS.
  CASE OK_CODE.

    WHEN '&CNC' OR '&BCK'.
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

  CLEAR: SAVE_OK, GV_EXIT, GV_ANSWER, GV_CHANGE,
         GV_VALID.

  "-- move ok code.
  SAVE_OK = OK_CODE.   CLEAR: OK_CODE.

  CASE SAVE_OK.

    WHEN '&HELP'.
      PERFORM CALL_POPUP_HELP(ZCAR9000)
               USING SY-REPID SY-DYNNR SY-LANGU ''.

    WHEN '&SUB'.
      PERFORM CHECKED_SAVED_DATA.
      CHECK GV_EXIT IS INITIAL.

      PERFORM POPUP_TO_CONFIRM USING TEXT-PT1
                                     TEXT-QT2.

      CHECK GV_ANSWER = '1'.
      PERFORM SUBMIT_DATA_RTN.

    WHEN '&SAV'.
      PERFORM CHECKED_SAVED_DATA.

      CHECK GV_EXIT IS INITIAL.
      PERFORM POPUP_TO_CONFIRM USING TEXT-PT1
                                     TEXT-QT1.

      CHECK GV_ANSWER = '1'.
      PERFORM SAVE_DATA_RTN.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  CALCULATION  INPUT
*&---------------------------------------------------------------------*
MODULE CALCULATION INPUT.

  ZCOS0041-THSTL = ZCOS0041-FHSTL + ZCOS0041-IHSTL.

  IF GV_MODE = 'S' AND ZCOS0041-THSTL < 0.
    MESSAGE E000 WITH TEXT-E06.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_NO  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_NO INPUT.

  CLEAR GV_LINE_NO.
  CLEAR GV_FIELD_NAME.

  GET CURSOR LINE  GV_LINE_NO.
  GET CURSOR FIELD GV_FIELD_NAME.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SET_CHANG_DATA  INPUT
*&---------------------------------------------------------------------*
MODULE SET_CHANG_DATA INPUT.

  READ TABLE GT_OUTTAB  INDEX TC1-CURRENT_LINE.

  MOVE-CORRESPONDING ZCOS0041 TO GT_OUTTAB.
  MODIFY GT_OUTTAB INDEX TC1-CURRENT_LINE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CALC_SUM  INPUT
*&---------------------------------------------------------------------*
MODULE CALC_SUM INPUT.

  CLEAR: GV_ISUM, GV_TSUM.

  LOOP AT GT_OUTTAB.
    GV_ISUM = GV_ISUM + GT_OUTTAB-IHSTL.
    GV_TSUM = GV_TSUM + GT_OUTTAB-THSTL.
  ENDLOOP.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GET_CURSOR  INPUT
*&---------------------------------------------------------------------*
MODULE GET_CURSOR INPUT.

  GET CURSOR FIELD GV_FIELDNAME LINE GV_CURLINE.

ENDMODULE.
