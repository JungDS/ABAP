*&---------------------------------------------------------------------*
*& Include          ZCOR0160T01
*&---------------------------------------------------------------------*
TYPE-POOLS : ICON,ABAP.

TABLES : SSCRFIELDS, "선택화면의 필드(Function Key)
         CSKS,
         T001,
         TKA01,
         TKA02,
         TKA05,
         TCURC,
         CEPC,
         PRPS,
         CSKA,
         PROJ,
         ZCOT0040.

*---------------------------------------------------------------------*
* CONSTANTS
*---------------------------------------------------------------------*
CONSTANTS : GC_LZONE TYPE LZONE    VALUE '0000000001',
            GC_A     TYPE CHAR01   VALUE 'A',
            GC_S     TYPE CHAR01   VALUE 'S',
            GC_E     TYPE CHAR01   VALUE 'E',
            GC_X     TYPE CHAR01   VALUE 'X',
            GC_N     TYPE CHAR01   VALUE 'N',
            GC_TCODE TYPE SY-TCODE VALUE 'KS01'.

CONSTANTS GC_KTOPL TYPE KTOPL VALUE '1000'.

*---------------------------------------------------------------------*
* TYPES
*---------------------------------------------------------------------*
TYPES: BEGIN OF TY_ACDOCA,
         RBUKRS TYPE BUKRS,
         RACCT  TYPE RACCT,
         KTEXT  TYPE KTEXT,
         OBJNR  TYPE J_OBJNR,
         HSL    TYPE FINS_VHCUR12,
       END OF TY_ACDOCA.

TYPES: BEGIN OF TY_DATA,
         VERSN  TYPE VERSN,
         WRTTP  TYPE CO_WRTTP,
         GJAHR  TYPE GJAHR,
         BUKRS  TYPE BUKRS,
         OBJNR  TYPE J_OBJNR,
         KSTAR  TYPE KSTAR,
         KTEXT  TYPE KTEXT,
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

TYPES: BEGIN OF TY_CSKA,
         KSTAR TYPE KSTAR,
         KTEXT TYPE KTEXT,
       END OF TY_CSKA.

TYPES: BEGIN OF TY_BASE,
         PRCTR TYPE PRCTR,
         BUKRS TYPE BUKRS,
       END OF TY_BASE.

*---------------------------------------------------------------------*
* VARIABLE
*---------------------------------------------------------------------*
* - Prefix 정의
*   1. GV_  : Global Variable
*   2. LV_  : Local Variable

*EX) DATA: GV_EXIT   TYPE C,
*          GV_ANSWER TYPE C,
*          OK_CODE   TYPE SY-UCOMM,   "예외
*          SAVE_OK   TYPE SY-UCOMM.   "예외

DATA: GV_EXIT TYPE C,
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

DATA GT_RETURN TYPE TABLE OF BAPIRET2 WITH HEADER LINE.

DATA GV_COUNT TYPE I.

DATA GV_KTOPL TYPE KTOPL VALUE '1000'.
DATA GV_WAERS TYPE WAERS.

DATA GV_POSID TYPE PS_POSID.

DATA: GR_DATA  TYPE REF TO DATA.
DATA: GR_DATA2 TYPE REF TO DATA.

*---------------------------------------------------------------------*
* STRUCTURE
*---------------------------------------------------------------------*
DATA : GS_FUNTXT TYPE SMP_DYNTXT.

*---------------------------------------------------------------------*
* INTERNAL TABLE
*---------------------------------------------------------------------*
DATA: GT_VALUES LIKE GRPVALUES  OCCURS 0 WITH HEADER LINE,
      GT_NODES  LIKE GRPOBJECTS OCCURS 0 WITH HEADER LINE,
      GT_INFO   LIKE GRPHINFO   OCCURS 0 WITH HEADER LINE.

FIELD-SYMBOLS: <GT_TABLE> TYPE TABLE,
               <GS_LINE>  TYPE ANY,
               <GS_LINE_TEMP> TYPE ANY,
               <GT_TABLE_DISPLAY> TYPE TABLE.

DATA GT_PRPS TYPE TABLE OF PRPS WITH HEADER LINE.

DATA: BEGIN OF GT_KOSTL OCCURS 0 ,
        KOSTL TYPE KOSTL,
        KTEXT TYPE KTEXT,
      END OF GT_KOSTL.

DATA GT_ZCOT0150 TYPE TABLE OF ZCOT0150 WITH HEADER LINE.

DATA GT_DATA   TYPE TABLE OF TY_DATA   WITH HEADER LINE.

DATA GT_SETNODE     TYPE TABLE OF SETNODE WITH HEADER LINE.

DATA GT_CSKA TYPE TABLE OF TY_CSKA WITH HEADER LINE  .

DATA GS_SELECT TYPE ZCOS0180.

TYPES: BEGIN OF TY_TOPNODES,
         NODEKEY TYPE LVC_NKEY,
       END OF TY_TOPNODES.

DATA: GT_TOPNODES TYPE STANDARD TABLE OF TY_TOPNODES,
      GS_TOPNODES LIKE LINE OF GT_TOPNODES.

DATA GT_BASE TYPE TABLE OF TY_BASE WITH HEADER LINE.

*---------------------------------------------------------------------*
* RANGES
*---------------------------------------------------------------------*
RANGES R_OBJNR FOR ZCOT0040-ROBJNR.
RANGES R_KSTAR FOR ZCOT0040-RKSTAR.

RANGES: R_PRCTR1 FOR ZCOT0320-PRCTR1,
        R_BUKRS  FOR ZCOT0320-BUKRS,
        R_PSPID  FOR ZCOT0320-PSPID.

RANGES: R_POSID2 FOR PRPS-POSID,
        R_SPMON  FOR ZFIT0620-SPMON.

*---------------------------------------------------------------------*
* FIELD-SYMBOLS
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
* MACRO (Define)
*---------------------------------------------------------------------*
* - Prefix 정의
*   1. _ : '_' 1개로 시작; Global, Local 구분 없음.
DEFINE _INITIAL_CHK.
  IF &1 IS INITIAL.
  GS_OUTTAB-STATUS    = 'E'.
  GS_OUTTAB-MESSAGE   = &2.
  ENDIF.
END-OF-DEFINITION.

DEFINE _CONVERSION_IN.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
  EXPORTING
  input = &1
  IMPORTING
  output = &1.
END-OF-DEFINITION.

DEFINE _CONVERSION_WBS_IN.
  CALL FUNCTION 'CONVERSION_EXIT_ABPSN_INPUT'
  EXPORTING
  input = &1
  IMPORTING
  output = &1.
END-OF-DEFINITION.

DEFINE _CONVERSION_WBS_OUT.
  CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
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

DEFINE _CLEAR.
  CLEAR &1. REFRESH &1.
END-OF-DEFINITION.

DEFINE _MAKEICON.

  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      NAME                  = &1
      INFO                  = &2
    IMPORTING
      RESULT                = &3
    EXCEPTIONS
      ICON_NOT_FOUND        = 1
      OUTPUTFIELD_TOO_SHORT = 2
      OTHERS                = 3.

END-OF-DEFINITION.

DEFINE _SET_COLOR.

  GS_COLOR-COL = &1.
  GS_COLOR-INT = &2.
  GS_COLOR-INV = &3.

END-OF-DEFINITION.

DEFINE _STRING_REPLACE.

  IF &1 IS NOT INITIAL.

  CALL FUNCTION 'STRING_REPLACE'
    EXPORTING
      PATTERN    = ','
      SUBSTITUTE = ''
    CHANGING
      TEXT       = &1.

 CONDENSE  &1.

  TRY.

    MOVE &1 TO A.

    CATCH CX_SY_CONVERSION_NO_NUMBER INTO DATA(ERR).

      GS_OUTTAB-STATUS =  'E'.
      GS_OUTTAB-MESSAGE = TEXT-E01.

      CLEAR  &1.

  ENDTRY.

 ENDIF.

END-OF-DEFINITION.
DEFINE _CURR_INPUT.
  WRITE &1 CURRENCY 'KRW' TO &2.
  CONDENSE &2.
END-OF-DEFINITION.

DEFINE _CONV_KRW.
  IF &1 IS NOT INITIAL .
    &1 = &1 / 100.
  ENDIF.
END-OF-DEFINITION.

*---------------------------------------------------------------------*
* Table Controls
*---------------------------------------------------------------------*
* - Prefix 정의
*   1. TC_ : Table Controls

*---------------------------------------------------------------------*
* Custom Controls
*---------------------------------------------------------------------*
* - Prefix 정의
*   1. CC_ : Custom Controls

*---------------------------------------------------------------------*
* Tabstrip Controls
*---------------------------------------------------------------------*
* - Prefix 정의
*   1. TS_ : Tabstrip Controls
