*&---------------------------------------------------------------------*
*& Include          ZCOR101_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT_0100 INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CALL METHOD GO_GRID->CHECK_CHANGED_DATA.

  CASE GV_OKCODE.
    WHEN 'EDIT'.
      IF GV_EDIT IS INITIAL.
        PERFORM DISP_EDIT_MODE USING GV_EDIT.
        GV_EDIT = 'X'.

      ELSE.
        PERFORM DISP_EDIT_MODE USING GV_EDIT.
        CLEAR GV_EDIT.
      ENDIF.

    WHEN 'SAVE'.
      PERFORM GET_SELECTED_ROW.
      PERFORM RUN_BAPI_PROJECT_MAINTAIN.
  ENDCASE.
ENDMODULE.
