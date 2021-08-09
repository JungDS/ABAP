*&---------------------------------------------------------------------*
*& Include          ZCOR0390T01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: SSCRFIELDS, PRPS, COSP, T001.

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

*----------------------------------------------------------------------*
* STRUCTURE
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. GS_  : Global Structure
*   2. LS_  : Local Structure

*EX) DATA: GS_SFLIGHT TYPE SFLIGHT

DATA: BEGIN OF GS_ITAB,
        POSID	 TYPE	PS_POSID,
        POST1	 TYPE	PS_POST1,
        TWAER	 TYPE	TWAER,
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
      END OF GS_ITAB.

DATA: GS_DISPLAY TYPE ZCOS0370.


*----------------------------------------------------------------------*
* INTERNAL TABLE
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. GT_  : Global Internal Table
*   2. LT_  : Local Internal Table

*EX) DATA: GT_SFLIGHT TYPE TABLE OF SFLIGHT
DATA: GT_VALUES TYPE TABLE OF GRPVALUES WITH HEADER LINE.

DATA: GT_ITAB LIKE TABLE OF GS_ITAB.

DATA: GT_DISPLAY  LIKE TABLE OF ZCOS0370 WITH HEADER LINE.
DATA: LT_DISPLAY  LIKE TABLE OF ZCOS0370 WITH HEADER LINE.

*----------------------------------------------------------------------*
* RANGES
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. R_  : Global, Local Ranges

* EX) RANGES: R_FKART  FOR VBRK-FKART,
*             GS_FKDAT LIKE LINE OF R_FKART.

RANGES: R_SALE  FOR COSP-KSTAR,
        R_COST  FOR COSP-KSTAR,
        R_KSTAR FOR COSP-KSTAR,
        R_OBJNR FOR PRPS-OBJNR.

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
