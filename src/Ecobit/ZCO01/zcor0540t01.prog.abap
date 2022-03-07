*&---------------------------------------------------------------------*
*& Include          ZCOR0540T01
*&---------------------------------------------------------------------*
TYPE-POOLS : ICON,ABAP.

TABLES : SSCRFIELDS. "선택화면의 필드(Function Key)


CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.

*---------------------------------------------------------------------*
* CONSTANTS
*---------------------------------------------------------------------*
CONSTANTS : GC_A     TYPE CHAR01   VALUE 'A',
            GC_S     TYPE CHAR01   VALUE 'S',
            GC_W     TYPE CHAR01   VALUE 'W',
            GC_D     TYPE CHAR01   VALUE 'D',
            GC_E     TYPE CHAR01   VALUE 'E',
            GC_X     TYPE CHAR01   VALUE 'X',
            GC_N     TYPE CHAR01   VALUE 'N',
            GC_TCODE TYPE SY-TCODE VALUE ''.

*---------------------------------------------------------------------*
* TYPES
*---------------------------------------------------------------------*
* - Prefix 정의
*   1. TY_  : Global, Local Types

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

DATA: GV_EXIT     TYPE C,
      GV_ANSWER   TYPE C,
      OK_CODE     TYPE SY-UCOMM,   "예외
      SAVE_OK     TYPE SY-UCOMM.   "예외

DATA: GV_MODE        TYPE C.
DATA: GV_REFRESH     TYPE C.
DATA: GV_DRDN_HANDLE TYPE I.
DATA: BUTXT          TYPE T001-BUTXT.

*---------------------------------------------------------------------*
* STRUCTURE
*---------------------------------------------------------------------*
DATA GS_FUNTXT        TYPE SMP_DYNTXT. "Excel 양식 Download(펑션키)

DATA: BEGIN OF GS_DATA,
        BUKRS        LIKE ZCOT1310-BUKRS,
        BUTXT        LIKE T001-BUTXT,
        ZZBGU        LIKE ZCOT1310-ZZBGU,
        ZZBGD        LIKE ZCOT1310-ZZBGD,
        ZZPRG        LIKE ZCOT1310-ZZPRG,
        WW120        LIKE ZCOT1310-WW120,
        ERDAT        LIKE ZCOT1310-ERDAT,
        ERZET        LIKE ZCOT1310-ERZET,
        ERNAM        LIKE ZCOT1310-ERNAM,
        AEDAT        LIKE ZCOT1310-AEDAT,
        AEZET        LIKE ZCOT1310-AEZET,
        AENAM        LIKE ZCOT1310-AENAM,
      END OF GS_DATA.

DATA: BEGIN OF GS_DISPLAY,
        MARK(1),
        BUKRS        LIKE ZCOT1310-BUKRS,
        BUTXT        LIKE T001-BUTXT,
        ZZBGU_DRDN   LIKE CSKT-LTEXT,
        ZZBGD_HANDLE LIKE LVC_S_DROP-HANDLE,
        ZZBGD_DRDN   LIKE CSKT-LTEXT,
        ZZPRG_DRDN   LIKE CSKT-LTEXT,
        WW120_DRDN   LIKE CSKT-LTEXT,
        ERDAT        LIKE ZCOT1310-ERDAT,
        ERZET        LIKE ZCOT1310-ERZET,
        ERNAM        LIKE ZCOT1310-ERNAM,
        AEDAT        LIKE ZCOT1310-AEDAT,
        AEZET        LIKE ZCOT1310-AEZET,
        AENAM        LIKE ZCOT1310-AENAM,
        STYLE        TYPE LVC_T_STYL,
        COLOR        TYPE LVC_T_SCOL,

        O_BUKRS      LIKE ZCOT1310-BUKRS,
        O_ZZBGU      LIKE ZCOT1310-ZZBGU,
        O_ZZBGD      LIKE ZCOT1310-ZZBGD,
        O_ZZPRG      LIKE ZCOT1310-ZZPRG,
        O_WW120      LIKE ZCOT1310-WW120,
      END OF GS_DISPLAY.

DATA: BEGIN OF GS_LOG,

*.. 미출력
        LOGID        LIKE ZCOT1340-LOGID,
        METHOD       LIKE ZCOT1340-METHOD,
        BUKRS        LIKE ZCOT1340-BUKRS,

*.. 출력
        TABIX        LIKE ZCOT1340-TABIX,
        O_ZZBGU      LIKE CSKT-LTEXT,
        O_ZZBGD      LIKE CSKT-LTEXT,
        O_ZZPRG      LIKE CSKT-LTEXT,
        O_WW120      LIKE CSKT-LTEXT,
        STATUS       LIKE ICON-ID,
        N_ZZBGU      LIKE CSKT-LTEXT,
        N_ZZBGD      LIKE CSKT-LTEXT,
        N_ZZPRG      LIKE CSKT-LTEXT,
        N_WW120      LIKE CSKT-LTEXT,
        AEDAT        LIKE ZCOT1340-AEDAT,
        AEZET        LIKE ZCOT1340-AEZET,
        AENAM        LIKE ZCOT1340-AENAM,

*.. 미출력
        STYLE        TYPE LVC_T_STYL,
        COLOR        TYPE LVC_T_SCOL,
      END OF GS_LOG.

DATA: BEGIN OF GS_1040,
        ZZBGU   LIKE ZCOT1040-ZZBGU,
        ZZBGUTX LIKE ZCOT1040T-ZZBGUTX,
        HANDLE  LIKE LVC_S_DROP-HANDLE,
        HANDLE2 LIKE LVC_S_DROP-HANDLE,
        VALUE   LIKE LVC_S_DROP-VALUE,
      END OF GS_1040.

DATA: BEGIN OF GS_1050,
        ZZBGU   LIKE ZCOT1050-ZZBGU,
        ZZBGD   LIKE ZCOT1050-ZZBGD,
        ZZBGDTX LIKE ZCOT1050T-ZZBGDTX,
        HANDLE  LIKE LVC_S_DROP-HANDLE,
        VALUE   LIKE LVC_S_DROP-VALUE,
      END OF GS_1050.

DATA: BEGIN OF GS_1100,
        ZZPRG   LIKE ZCOT1100-ZZPRG,
        ZZPRGTX LIKE ZCOT1100T-ZZPRGTX,
        HANDLE  LIKE LVC_S_DROP-HANDLE,
        VALUE   LIKE LVC_S_DROP-VALUE,
      END OF GS_1100.

DATA: BEGIN OF GS_T2501,
        WW120   LIKE T2501-WW120,
        BEZEK   LIKE T25A1-BEZEK,
        HANDLE  LIKE LVC_S_DROP-HANDLE,
        VALUE   LIKE LVC_S_DROP-VALUE,
      END OF GS_T2501.

DATA: BEGIN OF GS_T001,
        BUKRS LIKE T001-BUKRS,
        BUTXT LIKE T001-BUTXT,
      END OF GS_T001.
*---------------------------------------------------------------------*
* INTERNAL TABLE
*---------------------------------------------------------------------*
DATA GT_DATA          LIKE TABLE OF GS_DATA.
DATA GT_DISPLAY       LIKE TABLE OF GS_DISPLAY.
DATA GT_DISPLAY_2     LIKE TABLE OF GS_DISPLAY.
DATA GT_1310          TYPE TABLE OF ZCOT1310.
DATA GT_1340          TYPE TABLE OF ZCOT1340.
DATA GT_1040          LIKE TABLE OF GS_1040.
DATA GT_1050          LIKE TABLE OF GS_1050.
DATA GT_1100          LIKE TABLE OF GS_1100.
DATA GT_T2501         LIKE TABLE OF GS_T2501.
DATA GT_T001          LIKE TABLE OF GS_T001.

DATA GT_LOG_ALL       LIKE TABLE OF GS_LOG.
DATA GT_LOG_DISP      LIKE TABLE OF GS_LOG.

*---------------------------------------------------------------------*
* ALV
*---------------------------------------------------------------------*
DATA GR_CON             TYPE REF TO CL_GUI_DOCKING_CONTAINER.
DATA GR_ALV             TYPE REF TO ZCL_CO_ALV.
DATA GR_GRID            TYPE REF TO CL_GUI_ALV_GRID.
DATA GR_EVENT_RECEIVER  TYPE REF TO LCL_EVENT_RECEIVER.

*---------------------------------------------------------------------*
* RANGES
*---------------------------------------------------------------------*
* - Prefix 정의
*   1. R_  : Global, Local Ranges

* EX) RANGES: R_FKART  FOR VBRK-FKART,
*             GS_FKDAT LIKE LINE OF R_FKART.

RANGES: R_BUKRS FOR T001-BUKRS.

*---------------------------------------------------------------------*
* FIELD-SYMBOLS
*---------------------------------------------------------------------*
* - Prefix 정의
*   1. <FS_  >  : Global, Local Field-symbols

* EX) FIELD-SYMBOLS <FS_SFLIGHT> TYPE SFLIGHT.


*---------------------------------------------------------------------*
* MACRO (Define)
*---------------------------------------------------------------------*
* - Prefix 정의
*   1. _ : '_' 1개로 시작; Global, Local 구분 없음.
DEFINE _CONVERSION_IN.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING input  = &1
    IMPORTING output = &1.
END-OF-DEFINITION.

DEFINE _CONVERSION_WBS_IN.
  CALL FUNCTION 'CONVERSION_EXIT_ABPSN_INPUT'
    EXPORTING input  = &1
    IMPORTING output = &1.
END-OF-DEFINITION.

DEFINE _CONVERSION_WBS_OUT.
  CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
    EXPORTING input  = &1
    IMPORTING output = &1.
END-OF-DEFINITION.

DEFINE _CONVERSION_OUT.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
  EXPORTING input  = &1
  IMPORTING output = &1.
END-OF-DEFINITION.

DEFINE _CLEAR_ITAB.
  CLEAR &1. REFRESH &1.
END-OF-DEFINITION.

DEFINE _MAKE_ICON.
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
    CATCH CX_SY_CONVERSION_NO_NUMBER.
      GS_OUTTAB-STATUS =  'E'.
      GS_OUTTAB-MESSAGE = TEXT-E01.
      CLEAR  &1.
    ENDTRY.
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
