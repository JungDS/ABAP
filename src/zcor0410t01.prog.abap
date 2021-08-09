*&---------------------------------------------------------------------*
*& Include          ZCOR0400T01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: sscrfields, t001.
TABLES: PRPS,zcot0320.



TYPE-POOLS: icon, abap.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS gc_ktopl TYPE ktopl VALUE '1000'.
data : gv_super(1).
*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* VARIABLE
*----------------------------------------------------------------------*

DATA: gv_exit   TYPE xfeld,
      gv_answer TYPE c,
      ok_code   TYPE sy-ucomm,   "예외
      save_ok   TYPE sy-ucomm.   "예외

DATA: gv_change TYPE xfeld.      "변경체크

DATA gv_mode.
DATA gv_valid.

DATA looplines LIKE sy-loopc.
DATA linecount LIKE sy-tabix.

DATA gv_line_no LIKE sy-tabix.
DATA gv_field_name LIKE screen-name.

DATA: gv_fieldname TYPE char255,
      gv_curline   TYPE i.

DATA gv_waers TYPE waers.

DATA: gt_return TYPE TABLE OF bapiret2 WITH HEADER LINE.

DATA gv_url TYPE agr_url.

DATA: gv_ktext TYPE ktext,
      gv_post1 TYPE ps_post1.

*----------------------------------------------------------------------*
* STRUCTURE
*----------------------------------------------------------------------*
DATA : gs_funtxt TYPE smp_dyntxt.

*----------------------------------------------------------------------*
* INTERNAL TABLE
*----------------------------------------------------------------------*

DATA gt_outtab  TYPE TABLE OF zcos0390 WITH HEADER LINE.

DATA: BEGIN OF gt_kostl OCCURS 0 ,
        kostl TYPE kostl,
        ktext TYPE ktext,
        objnr TYPE j_objnr,
        bukrs TYPE bukrs,
        prctr TYPE prctr,
      END OF gt_kostl.

DATA: BEGIN OF gt_prps OCCURS 0 ,
        posid TYPE ps_posid,
        post1 TYPE ps_post1,
        objnr TYPE j_objnr,
        pbukr TYPE bukrs,
        prctr TYPE prctr,
      END OF gt_prps.

*----------------------------------------------------------------------*
* RANGES
*----------------------------------------------------------------------*
RANGES:r_bukrs  FOR zcot0320-bukrs.


*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* MACRO (Define)
*----------------------------------------------------------------------*

DEFINE _conversion_in.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
  EXPORTING
  input = &1
  IMPORTING
  output = &1.
END-OF-DEFINITION.

DEFINE _conversion_out.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
  EXPORTING
  input = &1
  IMPORTING
  output = &1.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
* Table Controls
*----------------------------------------------------------------------*
CONTROLS tc1 TYPE TABLEVIEW USING SCREEN 100.

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
