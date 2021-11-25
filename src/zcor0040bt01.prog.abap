*&---------------------------------------------------------------------*
*& Include          ZCOR0040T01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: SFLIGHT, SSCRFIELDS. "선택화면의 필드(Function Key)

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

DATA ERROR_IN_DATA.

*DATA: GV_CTYPE TYPE ZCOT0010b-CTYPE.
*----------------------------------------------------------------------*
* STRUCTURE
*----------------------------------------------------------------------*
DATA: GS_FUNTXT  TYPE SMP_DYNTXT. "Excel 양식 Download(펑션키)
DATA: GS_DISPLAY TYPE ZCOS0010b.

*----------------------------------------------------------------------*
* INTERNAL TABLE
*----------------------------------------------------------------------*

DATA: GT_DISPLAY     LIKE TABLE OF GS_DISPLAY. "DISPLAY DATA
DATA: GT_DISPLAY_LOG LIKE TABLE OF GS_DISPLAY. "

*----------------------------------------------------------------------*
* RANGES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* MACRO (Define)
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. _ : '_' 1개로 시작; Global, Local 구분 없음.

DEFINE _STYLE_DISABLED.
  GS_STYLE-FIELDNAME = &1.
  GS_STYLE-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
  INSERT GS_STYLE INTO TABLE GT_STYLE.
END-OF-DEFINITION.

DEFINE _STYLE_ENABLED.
  GS_STYLE-FIELDNAME = &1.
  GS_STYLE-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
  INSERT GS_STYLE INTO TABLE GT_STYLE.
END-OF-DEFINITION.

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
* - Prefix 정의
*   1. TC_ : Table Controls

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
