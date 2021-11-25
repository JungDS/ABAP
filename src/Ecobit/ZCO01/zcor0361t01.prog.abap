*&---------------------------------------------------------------------*
*& Include          ZCOR0360T01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*

TYPE-POOLS: ICON, ABAP.

TABLES : SSCRFIELDS, PRPS.


*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS GC_KTOPL TYPE KTOPL VALUE '1000'.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: BEGIN OF TY_DATA,
         VERSN  TYPE VERSN,
         WRTTP  TYPE CO_WRTTP,
         GJAHR  TYPE GJAHR,
         KSTAR  TYPE KSTAR,
         WKG001 TYPE WKGXXX,
         WKG002 TYPE WKGXXX,
         WKG003 TYPE WKGXXX,
         WKG004 TYPE WKGXXX,
         WKG005 TYPE WKGXXX,
         WKG006 TYPE WKGXXX,
         WKG007 TYPE WKGXXX,
         WKG008 TYPE WKGXXX,
         WKG009 TYPE WKGXXX,
         WKG010 TYPE WKGXXX,
         WKG011 TYPE WKGXXX,
         WKG012 TYPE WKGXXX,
       END OF TY_DATA.

*----------------------------------------------------------------------*
TYPES: BEGIN OF TY_OUTTAB,
         GUBUN TYPE CHAR30,         "구분
         P1    TYPE WKGXXX,         "년 계획
         M1    TYPE WKGXXX,         "당월 계획
         M2    TYPE WKGXXX,         "당월 실적
         M3    TYPE P DECIMALS 1,   "당월 달성율
         C1    TYPE WKGXXX,         "누계 계획
         C2    TYPE WKGXXX,         "누계 실적
         C3    TYPE P DECIMALS 1,   "누계 달성율
         D1    TYPE P DECIMALS 1,   "진척율
        WAERS  TYPE WAERS,
        ETC    TYPE C,
       END OF TY_OUTTAB.

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

DATA GV_KTOPL TYPE KTOPL VALUE '1000'.
DATA GV_WAERS TYPE WAERS.

*----------------------------------------------------------------------*
* STRUCTURE
*----------------------------------------------------------------------*
DATA : GS_FUNTXT TYPE SMP_DYNTXT.

*----------------------------------------------------------------------*
* INTERNAL TABLE
*----------------------------------------------------------------------*

DATA GT_DATA   TYPE TABLE OF TY_DATA   WITH HEADER LINE.
DATA GT_OUTTAB TYPE TABLE OF TY_OUTTAB WITH HEADER LINE.
DATA GS_OUTTAB TYPE TY_OUTTAB.

*----------------------------------------------------------------------*
* RANGES
*----------------------------------------------------------------------*
RANGES: R_KSTAR FOR CSKA-KSTAR,
        R_SPMON FOR ZFIT0620-SPMON,
        R_OBJNR FOR COSP-OBJNR.

*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* MACRO (Define)
*----------------------------------------------------------------------*

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
