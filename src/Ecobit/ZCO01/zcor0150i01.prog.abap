*&---------------------------------------------------------------------*
*& Include          ZCOR0150I01
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

    WHEN '&SAV'.

      "-- checked saved data.
      PERFORM CHECKED_SAVED_DATA.

      CHECK GV_EXIT IS INITIAL.

      PERFORM SET_SAVE_GUBUN.

      CHECK GV_EXIT IS INITIAL.

      "-- Popup to confirm
      PERFORM POPUP_TO_CONFIRM USING TEXT-PT1
                                     TEXT-QT1.

      CHECK GV_ANSWER EQ '1'.

      "-- Save data
      PERFORM SAVE_DATA_RTN.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0110 INPUT.

  CLEAR: SAVE_OK, GV_EXIT, GV_ANSWER.
  DATA: LV_CHECK_STR    TYPE C,
        LV_CHECK_NUM    TYPE STRING,
        LV_PATTERN(200) TYPE C,
        LV_SUCCESS      TYPE C,
        LO_MATCHER      TYPE REF TO CL_ABAP_MATCHER.

  "-- move ok code.
  SAVE_OK = OK_CODE.   CLEAR: OK_CODE.

  CASE SAVE_OK.
    WHEN '&CNC'.
      GV_EXIT = 'X'.
      CLEAR: GV_RAD1, GV_RAD2, GV_RAD3,
             GV_VER1, GV_VER2, GV_VER3.
      LEAVE TO SCREEN 0.

    WHEN '&ENT'.

      IF GV_RAD2 IS NOT INITIAL OR GV_RAD3 IS NOT INITIAL.

        IF GV_RAD2 IS NOT INITIAL.

          LV_CHECK_STR = GV_VER2(1).
          LV_CHECK_NUM = GV_VER2+1(2).

        ELSEIF GV_RAD3 IS NOT INITIAL.

          LV_CHECK_STR = GV_VER3(1).
          LV_CHECK_NUM = GV_VER3+1(2).

        ENDIF.

        IF LV_CHECK_STR <> 'V'.
          MESSAGE I049 DISPLAY LIKE 'S'.
          EXIT.
        ENDIF.

        " 정규식으로 숫자만 포함되어 있는지 점검
        CLEAR : LV_PATTERN, LO_MATCHER, LV_SUCCESS.
        CONCATENATE `[0-9]+$` '' INTO LV_PATTERN.
        LO_MATCHER = CL_ABAP_MATCHER=>CREATE( PATTERN = LV_PATTERN TEXT = LV_CHECK_NUM ).
        CALL METHOD LO_MATCHER->MATCH RECEIVING SUCCESS = LV_SUCCESS.

        IF LV_SUCCESS IS INITIAL.
          MESSAGE I050 DISPLAY LIKE 'S'.
          EXIT.
        ENDIF.

      ENDIF.

      IF GV_RAD3 IS NOT INITIAL.

        SELECT SINGLE VERSN INTO @DATA(LV_VERSN)
          FROM ZCOT0150
         WHERE KOKRS = @PA_KOKRS
           AND VERSN = @GV_VER3.
        IF SY-SUBRC = 0.
          MESSAGE I051 DISPLAY LIKE 'S'.
          EXIT.
        ENDIF.

      ENDIF.

      IF GV_RAD2 IS NOT INITIAL.
        PA_VERSN = GV_VER2.
      ELSEIF GV_RAD3 IS NOT INITIAL.
        PA_VERSN = GV_VER3.
      ENDIF.

      LEAVE TO SCREEN 0.

    WHEN 'F_RAD'.
      IF GV_RAD2 IS NOT INITIAL.
        CLEAR GV_VER3.
      ELSEIF GV_RAD3 IS NOT INITIAL.
        CLEAR GV_VER2.
      ENDIF.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  F4_VERSN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_VERSN INPUT.

  PERFORM F4_VERSN CHANGING GV_VER2.

ENDMODULE.
