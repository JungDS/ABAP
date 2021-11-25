*&---------------------------------------------------------------------*
*& Include          ZCOR0630PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT_0100  INPUT
*&---------------------------------------------------------------------*
MODULE EXIT_0100 INPUT.

  SAVE_OK = OK_CODE. CLEAR OK_CODE.

  CASE SAVE_OK.
    WHEN 'CANC'.
      " 프로그램을 종료하시겠습니까?
      CHECK GC_X EQ ZCL_CO_COMMON=>POPUP_CONFIRM( CONV #( TEXT-M02 ) ).
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      " 프로그램을 종료하시겠습니까?
      CHECK GC_X EQ ZCL_CO_COMMON=>POPUP_CONFIRM( CONV #( TEXT-M02 ) ).
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  SAVE_OK = OK_CODE. CLEAR OK_CODE.

  CASE SAVE_OK.
    WHEN 'BACK'.
      " 프로그램을 종료하시겠습니까?
      CHECK GC_X EQ ZCL_CO_COMMON=>POPUP_CONFIRM( CONV #( TEXT-M02 ) ).
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      PERFORM SAVE_DATA.

      " ALV Refresh
      GR_ALV->REFRESH( ).
    WHEN OTHERS.
      OK_CODE = SAVE_OK.
  ENDCASE.

ENDMODULE.
