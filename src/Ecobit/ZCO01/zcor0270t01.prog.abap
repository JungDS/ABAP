*&---------------------------------------------------------------------*
*& Include          ZCOR0270T01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: sscrfields, prps, csks.

TYPE-POOLS: icon, abap.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. GC_  : Global Constants
*   2. LC_  : Local Constants
* - EX). CONSTANTS: GC_E  TYPE C VALUE 'E'.
CONSTANTS: gc_ktopl TYPE ktopl VALUE '1000'.

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


DATA: gv_exit   TYPE xfeld,
      gv_answer TYPE c,
      gv_error  TYPE c LENGTH 30,
      ok_code   TYPE sy-ucomm,   "예외
      save_ok   TYPE sy-ucomm.   "예외

DATA gv_valid.

DATA: error_in_data.

DATA: gv_ktext  TYPE ktext.

data : gv_super(1).

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

*DATA: BEGIN OF GS_DISPLAY.
*        INCLUDE TYPE ZCOS0290.
*        DATA:   COLOR TYPE LVC_T_SCOL,
*      END OF GS_DISPLAY.

DATA: gs_display TYPE zcos0290.

*----------------------------------------------------------------------*
* INTERNAL TABLE
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. GT_  : Global Internal Table
*   2. LT_  : Local Internal Table

*EX) DATA: GT_SFLIGHT TYPE TABLE OF SFLIGHT

DATA: gt_display     LIKE TABLE OF gs_display,
      gt_display_log LIKE TABLE OF gs_display.
DATA: gt_return TYPE TABLE OF bapiret2 WITH HEADER LINE.

DATA: gt_values TYPE TABLE OF grpvalues WITH HEADER LINE.

*----------------------------------------------------------------------*
* RANGES
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. R_  : Global, Local Ranges

* EX) RANGES: R_FKART  FOR VBRK-FKART,
*             GS_FKDAT LIKE LINE OF R_FKART.

RANGES: r_objnr FOR prps-objnr.
RANGES: gr_kostl FOR csks-kostl.



CONSTANTS : gc_e TYPE char01 VALUE 'E'.


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
