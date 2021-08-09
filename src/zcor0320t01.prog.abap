*&---------------------------------------------------------------------*
*& Include          ZCOR0320T01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: SSCRFIELDS, USR01.

TYPE-POOLS: ICON, ABAP.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. GC_  : Global Constants
*   2. LC_  : Local Constants
* - EX). CONSTANTS: GC_E  TYPE C VALUE 'E'.
CONSTANTS: GC_KTOPL TYPE KTOPL VALUE '1000'.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. TY_  : Global, Local Types

*----------------------------------------------------------------------*
* VARIABLE
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. GV_  : Global Variable
*   2. LV_  : Local Variable

*EX) DATA: GV_EXIT   TYPE C,
*          GV_ANSWER TYPE C,
*          OK_CODE   TYPE SY-UCOMM,   "예외
*          SAVE_OK   TYPE SY-UCOMM.   "예외


DATA: GV_EXIT   TYPE XFELD,
      GV_ANSWER TYPE C,
      OK_CODE   TYPE SY-UCOMM,   "예외
      SAVE_OK   TYPE SY-UCOMM.   "예외

DATA GV_VALID.

DATA: ERROR_IN_DATA.

DATA: GV_KTEXT  TYPE KTEXT.

*DATA: GV_RAD1 TYPE C,
*      GV_RAD2 TYPE C,
*      GV_RAD3 TYPE C.
*
*DATA: GV_VER1 TYPE VERSN,
*      GV_VER2 TYPE VERSN,
*      GV_VER3 TYPE VERSN.

*----------------------------------------------------------------------*
* STRUCTURE
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. GS_  : Global Structure
*   2. LS_  : Local Structure

*EX) DATA: GS_SFLIGHT TYPE SFLIGHT

DATA: BEGIN OF GS_DISPLAY.
        INCLUDE TYPE ZCOS0320.
        DATA:   STYLE TYPE LVC_T_STYL,
      END OF GS_DISPLAY.

*----------------------------------------------------------------------*
* INTERNAL TABLE
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. GT_  : Global Internal Table
*   2. LT_  : Local Internal Table

*EX) DATA: GT_SFLIGHT TYPE TABLE OF SFLIGHT

DATA: GT_DISPLAY     LIKE TABLE OF GS_DISPLAY,
      GT_DISPLAY_LOG LIKE TABLE OF GS_DISPLAY,
      GT_DISPLAY_DEL LIKE TABLE OF GS_DISPLAY.

*DATA: GT_RETURN TYPE TABLE OF BAPIRET2 WITH HEADER LINE.
*
*DATA: GT_VALUES TYPE TABLE OF GRPVALUES WITH HEADER LINE.

*----------------------------------------------------------------------*
* RANGES
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. R_  : Global, Local Ranges

* EX) RANGES: R_FKART  FOR VBRK-FKART,
*             GS_FKDAT LIKE LINE OF R_FKART.

*RANGES: R_OBJNR FOR PRPS-OBJNR.

*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. <FS_  >  : Global, Local Field-symbols

* EX) FIELD-SYMBOLS <FS_SFLIGHT> TYPE SFLIGHT.

*----------------------------------------------------------------------*
* MACRO (Define)
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. _ : '_' 1개로 시작; Global, Local 구분 없음.

DEFINE _ADD_STYLE.

  CLEAR GS_STYLE.
  GS_STYLE-FIELDNAME = &1.
  GS_STYLE-STYLE     = &2.
  APPEND GS_STYLE TO GT_STYLE.

END-OF-DEFINITION.

DEFINE _CONVERSION_ABPSN_INPUT.
  CALL FUNCTION 'CONVERSION_EXIT_ABPSN_INPUT'
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
