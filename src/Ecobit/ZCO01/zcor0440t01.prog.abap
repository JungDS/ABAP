*&---------------------------------------------------------------------*
*& Include          ZCOR0440T01
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: SSCRFIELDS, T001, PRPS, PROJ, CSKS.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: BEGIN OF TY_COSP,
         VERSN  TYPE VERSN,
         OBJNR  TYPE J_OBJNR,
         KSTAR  TYPE KSTAR,
         KSTXT  TYPE KTEXT,
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
       END OF TY_COSP.

*----------------------------------------------------------------------*
* VARIABLE
*----------------------------------------------------------------------*

DATA: GV_EXIT   TYPE XFELD.

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

DATA GV_URL TYPE AGR_URL.

DATA: GV_KTEXT TYPE KTEXT,
      GV_POST1 TYPE PS_POST1.

*
DATA : GV_CHECK_GJAHR TYPE C.

*
DATA GV_KTOPL TYPE KTOPL VALUE '1000'."계정과목표
DATA GV_KOKRS TYPE KOKRS VALUE '1000'."경영단위

* ALV
DATA: GT_FIELDCAT TYPE LVC_T_FCAT,
      GS_FIELDCAT TYPE LVC_S_FCAT.

DATA:
      OK_CODE TYPE SY-UCOMM,   "예외
      SAVE_OK TYPE SY-UCOMM.   "예외

DATA: GV_TOTAL  TYPE C LENGTH 30,
      GV_SUCESS TYPE C LENGTH 30,
      GV_ERROR  TYPE C LENGTH 30.

DATA GV_ANSWER.

DATA GV_MESSAGE TYPE C LENGTH 100.

DATA: G_VALID TYPE C.
DATA: LS_TOOLBAR TYPE STB_BUTTON.

TYPES: BEGIN OF TY_LAYOUT,
         REPID    TYPE SYREPID,
         RESTRICT TYPE SALV_DE_LAYOUT_RESTRICTION,
         DEFAULT  TYPE SAP_BOOL,
         LAYOUT   TYPE DISVARIANT-VARIANT,
       END OF TY_LAYOUT.

DATA: GO_ALV        TYPE REF TO CL_SALV_TABLE,
      GO_FUNCTIONS  TYPE REF TO CL_SALV_FUNCTIONS_LIST,
      GO_LAYOUT     TYPE REF TO CL_SALV_LAYOUT,
      GO_DSPSET     TYPE REF TO CL_SALV_DISPLAY_SETTINGS,
      GS_KEY        TYPE SALV_S_LAYOUT_KEY,
      GT_COLUMN_REF TYPE SALV_T_COLUMN_REF.

DATA GS_COLOR  TYPE LVC_S_COLO.

DATA GR_COLUMN TYPE REF TO CL_SALV_COLUMN_TABLE.

DATA: GT_ROWS TYPE SALV_T_ROW,
      GS_ROW  TYPE INT4.  "ALV position 기억

DATA: GV_COLUMN_TEXT TYPE STRING,
      GV_SCRTEXT_S   TYPE SCRTEXT_S,
      GV_SCRTEXT_M   TYPE SCRTEXT_M,
      GV_SCRTEXT_L   TYPE SCRTEXT_L.

DATA: GS_SALV_LAYOUT TYPE TY_LAYOUT.

DATA  GV_REPID TYPE SYREPID.

DATA GV_COUNT TYPE I.

DATA: GR_DATA TYPE REF TO DATA.
*----------------------------------------------------------------------*
* STRUCTURE
*----------------------------------------------------------------------*
DATA : GS_FUNTXT TYPE SMP_DYNTXT.


*----------------------------------------------------------------------*
* INTERNAL TABLE
*----------------------------------------------------------------------*
DATA : BEGIN OF GT_OUTTAB OCCURS 0.
  INCLUDE STRUCTURE ZCOT0040.
DATA : END OF GT_OUTTAB.

DATA: BEGIN OF GT_KOSTL OCCURS 0 ,
        KOSTL TYPE KOSTL,
        KTEXT TYPE KTEXT,
        OBJNR TYPE J_OBJNR,
      END OF GT_KOSTL.

DATA: BEGIN OF GT_PRPS OCCURS 0 ,
        POSID TYPE PS_POSID,
        POST1 TYPE PS_POST1,
        OBJNR TYPE J_OBJNR,
        PSPNR LIKE PRPS-PSPNR,
      END OF GT_PRPS.

DATA GT_COSP TYPE TABLE OF TY_COSP.

DATA: BEGIN OF GT_MONTH OCCURS 0,
        V1 TYPE N LENGTH 2,
      END OF GT_MONTH.

DATA GT_TKVS TYPE TABLE OF TKVS WITH HEADER LINE.

DATA: GT_VALUES LIKE GRPVALUES  OCCURS 0 WITH HEADER LINE.

*---------------------------------------------------------------------*
* FIELD-SYMBOLS
*---------------------------------------------------------------------*
FIELD-SYMBOLS: <GT_TABLE> TYPE TABLE,
               <GS_LINE>  TYPE ANY.

*---------------------------------------------------------------------*
* MACRO (Define)
*---------------------------------------------------------------------*
* - Prefix 정의
*   1. _ : '_' 1개로 시작; Global, Local 구분 없음.

DEFINE _SET_COLOR.

  GS_COLOR-COL = &1.
  GS_COLOR-INT = &2.
  GS_COLOR-INV = &3.

END-OF-DEFINITION.
