*&---------------------------------------------------------------------*
*& Include          ZCOR0190I01
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

    WHEN '&CNC'.

      PERFORM CHECK_CHANGE CHANGING GV_VALID.

      IF GV_VALID IS NOT INITIAL.

        PERFORM POPUP_TO_CONFIRM USING TEXT-PT2
                                       TEXT-QT2.
        CASE GV_ANSWER.

          WHEN '1'.
            LEAVE TO SCREEN 0.
          WHEN OTHERS.
            EXIT.

        ENDCASE.

      ELSE.
        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN '&EXT'.

      PERFORM CHECK_CHANGE CHANGING GV_VALID.

      IF GV_VALID IS NOT INITIAL.

        PERFORM POPUP_TO_CONFIRM USING TEXT-PT2
                                       TEXT-QT2.

        CASE GV_ANSWER.

          WHEN '1'.
            LEAVE PROGRAM.
          WHEN OTHERS.
            EXIT.

        ENDCASE.

      ELSE.
        LEAVE PROGRAM.
      ENDIF.

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

    WHEN '&BCK'.

      PERFORM CHECK_CHANGE CHANGING GV_VALID.

      IF GV_VALID IS NOT INITIAL.

        PERFORM POPUP_TO_CONFIRM USING TEXT-PT3
                                       TEXT-QT3.
        CASE GV_ANSWER.

          WHEN '1'.
            PERFORM SAVE_DATA_RTN.
            LEAVE TO SCREEN 0.

          WHEN '2'.
            LEAVE TO SCREEN 0.

          WHEN OTHERS.
            EXIT.

        ENDCASE.

      ELSE.
        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN '&HELP'.
      PERFORM CALL_POPUP_HELP(ZCAR9000)
               USING SY-REPID SY-DYNNR SY-LANGU ''.

    WHEN '&SAV'.

      PERFORM POPUP_TO_CONFIRM USING TEXT-PT1
                                     TEXT-QT1.

      CHECK GV_ANSWER = '1'.
      PERFORM SAVE_DATA_RTN.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
