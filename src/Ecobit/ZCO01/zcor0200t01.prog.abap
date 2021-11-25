*&---------------------------------------------------------------------*
*& Include          ZCOR0200T01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: SSCRFIELDS, T001.

TYPE-POOLS: ICON, ABAP.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. GC_  : Global Constants
*   2. LC_  : Local Constants
* - EX). CONSTANTS: GC_E  TYPE C VALUE 'E'.
CONSTANTS: GC_KTOPL    TYPE KTOPL VALUE '1000'.

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

DATA: GV_CHANGE TYPE XFELD.      "변경체크

DATA GV_MODE.
DATA GV_VALID.

DATA ERROR_IN_DATA.

DATA GV_COUNT TYPE char20.

*----------------------------------------------------------------------*
* STRUCTURE
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. GS_  : Global Structure
*   2. LS_  : Local Structure

*EX) DATA: GS_SFLIGHT TYPE SFLIGHT

DATA: GS_DISPLAY TYPE ZCOS0200.

*----------------------------------------------------------------------*
* INTERNAL TABLE
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. GT_  : Global Internal Table
*   2. LT_  : Local Internal Table

*EX) DATA: GT_SFLIGHT TYPE TABLE OF SFLIGHT

DATA: GT_DISPLAY     LIKE TABLE OF GS_DISPLAY. "DISPLAY DATA
*DATA: GT_DISPLAY_LOG LIKE TABLE OF GS_DISPLAY. "


*----------------------------------------------------------------------*
* RANGES
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. R_  : Global, Local Ranges

* EX) RANGES: R_FKART  FOR VBRK-FKART,
*             GS_FKDAT LIKE LINE OF R_FKART.

RANGES R_RACCT FOR ACDOCA-RACCT.

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
