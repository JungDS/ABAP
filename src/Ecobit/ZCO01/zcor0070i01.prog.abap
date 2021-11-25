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

    WHEN '&SAV'.
      PERFORM CHECKED_SAVED_DATA.

      CHECK GV_EXIT IS INITIAL.
      PERFORM POPUP_TO_CONFIRM USING TEXT-PT1
                                     TEXT-QT1.

      CHECK GV_ANSWER = '1'.
      PERFORM SAVE_DATA_RTN.

    WHEN '&SUB'.
      PERFORM CHECKED_SAVED_DATA.
      CHECK GV_EXIT IS INITIAL.

      PERFORM POPUP_TO_CONFIRM USING TEXT-PT1
                                     TEXT-QT2.

      CHECK GV_ANSWER = '1'.
      PERFORM SUBMIT_DATA_RTN.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  CALCULATION  INPUT
*&---------------------------------------------------------------------*
MODULE CALCULATION INPUT.

  ZCOS0051-MSHSTL = ZCOS0051-SHSTL - ZCOS0051-MHSTL.
  ZCOS0051-RSHSTL = ZCOS0051-RHSTL + ZCOS0051-MHSTL.

  IF GV_MODE = 'S' AND ZCOS0051-MSHSTL < 0.
    MESSAGE E000 WITH TEXT-E01.
  ENDIF.

  IF GV_MODE = 'S' AND ZCOS0051-RSHSTL < 0.
    MESSAGE E000 WITH TEXT-E05.
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

  MOVE-CORRESPONDING ZCOS0051 TO GT_OUTTAB.
  MODIFY GT_OUTTAB INDEX TC1-CURRENT_LINE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GET_CURSOR  INPUT
*&---------------------------------------------------------------------*
MODULE GET_CURSOR INPUT.

  GET CURSOR FIELD GV_FIELDNAME LINE GV_CURLINE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GET_SUM  INPUT
*&---------------------------------------------------------------------*
MODULE GET_SUM INPUT.

*-- 이동금액 누적
  CLEAR GV_MSUM.

  LOOP AT GT_OUTTAB.
    GV_MSUM = GV_MSUM + GT_OUTTAB-MHSTL.
  ENDLOOP.

ENDMODULE.
