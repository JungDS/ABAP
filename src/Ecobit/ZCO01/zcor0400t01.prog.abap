*&---------------------------------------------------------------------*
*& Include          ZCOR0400T01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: SSCRFIELDS.

TYPE-POOLS: ICON, ABAP.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS GC_KTOPL TYPE KTOPL VALUE '1000'.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* VARIABLE
*----------------------------------------------------------------------*

DATA: GV_EXIT   TYPE XFELD,
      GV_ANSWER TYPE C,
      OK_CODE   TYPE SY-UCOMM,   "예외
      SAVE_OK   TYPE SY-UCOMM.   "예외

DATA: GV_CHANGE TYPE XFELD.      "변경체크

DATA GV_MODE.
DATA GV_VALID.

DATA LOOPLINES LIKE SY-LOOPC.
DATA LINECOUNT LIKE SY-TABIX.

DATA GV_LINE_NO LIKE SY-TABIX.
DATA GV_FIELD_NAME LIKE SCREEN-NAME.

DATA: GV_FIELDNAME TYPE CHAR255,
      GV_CURLINE   TYPE I.

DATA GV_WAERS TYPE WAERS.

DATA: GT_RETURN TYPE TABLE OF BAPIRET2 WITH HEADER LINE.

DATA GV_OBJNR TYPE OBJNR.

DATA GV_URL TYPE AGR_URL.

DATA: GV_KTEXT TYPE KTEXT,
      GV_POST1 TYPE PS_POST1.

*----------------------------------------------------------------------*
* STRUCTURE
*----------------------------------------------------------------------*
DATA : GS_FUNTXT TYPE SMP_DYNTXT.

*----------------------------------------------------------------------*
* INTERNAL TABLE
*----------------------------------------------------------------------*

DATA GT_OUTTAB      TYPE TABLE OF ZCOS0380 WITH HEADER LINE.

*----------------------------------------------------------------------*
* RANGES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* MACRO (Define)
*----------------------------------------------------------------------*

DEFINE _CONVERSION_IN.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
  EXPORTING
  input = &1
  IMPORTING
  output = &1.
END-OF-DEFINITION.

DEFINE _CONVERSION_OUT.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
  EXPORTING
  input = &1
  IMPORTING
  output = &1.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
* Table Controls
*----------------------------------------------------------------------*
CONTROLS TC1 TYPE TABLEVIEW USING SCREEN 100.

*----------------------------------------------------------------------*
* Custom Controls
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. CC_ : Custom Controls

*----------------------------------------------------------------------*
* Tabstrip Controls
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. TS_ : Tabstrip Controls
