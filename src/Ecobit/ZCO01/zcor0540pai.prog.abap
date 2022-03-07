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
      PERFORM EXIT_PROGRAM.
    WHEN 'EXIT'.
      PERFORM EXIT_PROGRAM.
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
      PERFORM EXIT_PROGRAM.
    WHEN 'TOGGLE'.
      PERFORM TOGGLE_GRID.
    WHEN 'LOG'.
      PERFORM DISPLAY_LOG.
    WHEN OTHERS.
      OK_CODE = SAVE_OK.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT_0200  INPUT
*&---------------------------------------------------------------------*
MODULE EXIT_0200 INPUT.

  CLEAR: R_BUKRS,
         R_BUKRS[].

  LEAVE TO SCREEN 0.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.

  SAVE_OK = OK_CODE. CLEAR OK_CODE.

  CASE SAVE_OK.
    WHEN 'ENTR'.
      PERFORM SET_COMPANY_CODE.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  F4_BUKRS_0200  INPUT
*&---------------------------------------------------------------------*
MODULE F4_BUKRS_0200 INPUT.

  DATA LT_RETURN TYPE TABLE OF DDSHRETVAL WITH HEADER LINE.


  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      TABNAME             = 'T001'            " Table/structure name from Dictionary
      FIELDNAME           = 'BUKRS'           " Field name from Dictionary
      SEARCHHELP          = 'ZSH_BUKRS'       " Search help as screen field attribute
      SHLPPARAM           = 'BUKRS'           " Search help parameter in screen field
    TABLES
      RETURN_TAB          = LT_RETURN
    EXCEPTIONS
      FIELD_NOT_FOUND     = 1                " Field does not exist in the Dictionary
      NO_HELP_FOR_FIELD   = 2                " No F4 help is defined for the field
      INCONSISTENT_HELP   = 3                " F4 help for the field is inconsistent
      NO_VALUES_FOUND     = 4                " No values found
      OTHERS              = 5.

  CHECK SY-SUBRC EQ 0.

  READ TABLE LT_RETURN INDEX 1.
  R_BUKRS-LOW = LT_RETURN-FIELDVAL.
  PERFORM SET_BUTXT USING R_BUKRS-LOW.

  ZCL_CO_COMMON=>SET_DYNP_VALUE(
    EXPORTING
      I_FIELD = 'BUTXT'           " Field Name
*      I_REPID = SY-CPROG         " ABAP Program: Current Main Program
*      I_DYNNR = SY-DYNNR         " ABAP Program: Current Screen No
      I_VALUE = BUTXT             " 화면 필드값
  ).


ENDMODULE.
