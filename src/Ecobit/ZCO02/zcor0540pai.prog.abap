*&---------------------------------------------------------------------*
*& Include          ZCOR0540PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT_0100  INPUT
*&---------------------------------------------------------------------*
MODULE EXIT_0100 INPUT.

  SAVE_OK = OK_CODE. CLEAR OK_CODE.

  CASE SAVE_OK.
    WHEN 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  SAVE_OK = OK_CODE. CLEAR OK_CODE.

  GR_ALV->MR_ALV_GRID->CHECK_CHANGED_DATA( ).

  CASE SAVE_OK.
    WHEN 'SAVE'.
      PERFORM SAVE_DATA.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'TOGGLE'.
      IF GV_MODE EQ GC_E.
        IF GT_DISPLAY_2[] NE GT_DISPLAY[] AND
           GC_X NE ZCL_CO_COMMON=>POPUP_CONFIRM(
            I_TITLEBAR = '확인'(PT1)
            I_QUESTION = CONV #( '조회로 전환합니다. 변경 중인 내용은 제거됩니다.'(QT2) )
        ).
          MESSAGE '취소되었습니다.' TYPE GC_S DISPLAY LIKE GC_W.
          EXIT.
        ENDIF.
        GV_MODE = GC_D.
        PERFORM REFRESH_DATA.
      ELSE.
        GV_MODE = GC_E.
        GT_DISPLAY_2[] = GT_DISPLAY[].
      ENDIF.
    WHEN OTHERS.
      OK_CODE = SAVE_OK.
  ENDCASE.

ENDMODULE.
