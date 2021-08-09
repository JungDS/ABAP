*&---------------------------------------------------------------------*
*& Include          ZCOR0330T01
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
         ZCOT1190.

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

TYPES: BEGIN OF TY_ZCOT1200,
        GWKEY     TYPE ZEGWKEY,
        GWTYP     TYPE ZEGWTYP,
        GWTXT     TYPE CHAR30,
        GWSTS     TYPE ZEGWSTS,
        GWSXT     TYPE CHAR30,
        ERDAT     TYPE ERDAT,
        ERNAM     TYPE ERNAM,
        ERZET     TYPE ERZET,
        TITLE     TYPE CHAR255,
        RSEQ      TYPE ZERSEQ,
        PROCESS_9 TYPE BUKU_PROCESS,
        ROBJNR    TYPE J_OBJNR,
        RKSTAR    TYPE KSTAR,
        KTEXT     TYPE KTEXT,
        FAMOUNT   TYPE HSLVT12,
        HSL01     TYPE HSLVT9_CS,
        HSL02     TYPE HSLVT9_CS,
        HSL03     TYPE HSLVT9_CS,
        HSL04     TYPE HSLVT9_CS,
        HSL05     TYPE HSLVT9_CS,
        HSL06     TYPE HSLVT9_CS,
        HSL07     TYPE HSLVT9_CS,
        HSL08     TYPE HSLVT9_CS,
        HSL09     TYPE HSLVT9_CS,
        HSL10     TYPE HSLVT9_CS,
        HSL11     TYPE HSLVT9_CS,
        HSL12     TYPE HSLVT9_CS,
        WAERS     TYPE WAERS,
     END OF TY_ZCOT1200.

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

DATA GV_COUNT TYPE I.

DATA GV_KTOPL TYPE KTOPL VALUE '1000'.
DATA GV_WAERS TYPE WAERS.

DATA: GT_CELL_COLOR TYPE LVC_T_SCOL,
      GS_CELL_COLOR TYPE LVC_S_SCOL.

DATA GV_LINES TYPE I.

*---------------------------------------------------------------------*
* STRUCTURE
*---------------------------------------------------------------------*
DATA : GS_FUNTXT TYPE SMP_DYNTXT.

*---------------------------------------------------------------------*
* INTERNAL TABLE
*---------------------------------------------------------------------*
DATA GT_VALUES TYPE TABLE OF GRPVALUES WITH HEADER LINE.

DATA GT_DISPLAY TYPE TABLE OF ZCOS0330 WITH HEADER LINE.
DATA GT_ITEM    TYPE TABLE OF ZCOS0340 WITH HEADER LINE.

DATA GT_ZCOT1200 TYPE TABLE OF TY_ZCOT1200 WITH HEADER LINE.


*---------------------------------------------------------------------*
* RANGES
*---------------------------------------------------------------------*
RANGES R_OBJNR FOR ZCOT0040-ROBJNR.
RANGES R_KSTAR FOR ZCOT0040-RKSTAR.
RANGES R_FYEAR FOR ACDOCA-FISCYEARPER.

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

DEFINE ADD_COLOR.

  CLEAR GS_CELL_COLOR.
  GS_CELL_COLOR-FNAME     = &1.
  GS_CELL_COLOR-COLOR-COL = &2.
  GS_CELL_COLOR-COLOR-INT = 1.
  GS_CELL_COLOR-COLOR-INV = 0.
  GS_CELL_COLOR-NOKEYCOL  = 'X'.
  APPEND GS_CELL_COLOR TO GT_CELL_COLOR.

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
