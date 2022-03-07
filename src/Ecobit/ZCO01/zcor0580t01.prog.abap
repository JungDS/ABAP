*&---------------------------------------------------------------------*
*& Include          ZCOR0580T01
*&---------------------------------------------------------------------*
TYPE-POOLS : ICON,ABAP.

TABLES: SSCRFIELDS. "선택화면의 필드(Function Key)
TABLES: T001, PRPS.



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

DATA: GV_REPID    TYPE SY-REPID.
DATA: GV_REFRESH  TYPE C.

DATA: GV_CHK_YELLOW,
      GV_CHK_GREEN,
      GV_CHK_RED.

DATA: GV_VERSN_TXT LIKE TKT09-TXT.

*---------------------------------------------------------------------*
* STRUCTURE
*---------------------------------------------------------------------*
DATA GS_FUNTXT        TYPE SMP_DYNTXT. "Excel 양식 Download(펑션키)

DATA: BEGIN OF GS_DATA,
        PRONR LIKE PROJ-PSPNR,
        PSPNR LIKE PRPS-PSPNR,
        POSID LIKE PRPS-POSID,
        POST1 LIKE PRPS-POST1,
        OBJNR LIKE PRPS-OBJNR,
        PBUKR LIKE PRPS-PBUKR,
        PKOKR LIKE PRPS-PKOKR,
        ZZBGU LIKE PRPS-ZZBGU,
        ZZBGD LIKE PRPS-ZZBGD,
        ZZPRG LIKE PRPS-ZZPRG,
        WW120 LIKE T2501-WW120,
      END OF GS_DATA.

DATA: BEGIN OF GS_DISPLAY,
        STATUS        LIKE ICON-ID,
        BUKRS         LIKE T001-BUKRS,
        PRONR         LIKE PROJ-PSPNR,
        PSPNR         LIKE PRPS-PSPNR,
        POSID         LIKE PRPS-POSID,
        POST1         LIKE PRPS-POST1,
        OBJNR         LIKE PRPS-OBJNR,
        ZZBGU         LIKE PRPS-ZZBGU,
        ZZBGD         LIKE PRPS-ZZBGD,
        ZZBGDTX       LIKE ZCOT1050T-ZZBGDTX,
        ZZPRG         LIKE PRPS-ZZPRG,
        WW120         LIKE T2501-WW120,
        BEZEK         LIKE T25A1-BEZEK,
        MTYPE         TYPE C,
        MTYPETX       LIKE BAPIRET2-MESSAGE,
        WW120_S       LIKE T2501-WW120,
        BEZEK_S       LIKE T25A1-BEZEK,
        ZZBGU_S       LIKE PRPS-ZZBGU,
        ZZBGD_S       LIKE PRPS-ZZBGD,
        ZZPRG_S       LIKE PRPS-ZZPRG,
        LETJA         LIKE COBRB-LETJA,
        LETPE         LIKE COBRB-LETPE,
        MESSAGE       LIKE BAPIRET2-MESSAGE,
        MESSAGE_TAB   TYPE TAB_BDCMSGCOLL,
        STYLE         TYPE LVC_T_STYL,
        COLOR         TYPE LVC_T_SCOL,
      END OF GS_DISPLAY.

DATA: BEGIN OF GS_COBRB,
        OBJNR LIKE COBRB-OBJNR,
        LFDNR LIKE COBRB-LFDNR,
        GABJA	LIKE COBRB-GABJA, " 효력 시작 연도
        GABPE	LIKE COBRB-GABPE, " 효력 시작 기간
        GBISJ	LIKE COBRB-GBISJ, " 효력 종료 연도
        GBISP	LIKE COBRB-GBISP, " 효력 종료일
        PROZS	LIKE COBRB-PROZS, " 정산백분율
        LETJA LIKE COBRB-LETJA,
        LETPE LIKE COBRB-LETPE,
        AVORG LIKE COBRB-AVORG,
        VERSN LIKE COBRB-VERSN,
        VRGNG LIKE CE41000_ACCT-ACCT_VRGNG,
        WW040 LIKE CE41000_ACCT-WW040,
        WW050 LIKE CE41000_ACCT-WW050,
        WW100 LIKE CE41000_ACCT-WW100,
        WW120 LIKE CE41000_ACCT-WW120,
      END OF GS_COBRB.

DATA: BEGIN OF GS_JEST,
        OBJNR LIKE JEST-OBJNR,
        STAT  LIKE JEST-STAT,
      END OF GS_JEST.

DATA: BEGIN OF GS_T001,
        BUKRS LIKE T001-BUKRS,
        BUTXT LIKE T001-BUTXT,
      END OF GS_T001.

DATA: BEGIN OF GS_T2501,
        WW120   LIKE T2501-WW120,
        BEZEK   LIKE T25A1-BEZEK,
      END OF GS_T2501.

DATA: BEGIN OF GS_1040,
        ZZBGU   LIKE ZCOT1040-ZZBGU,
        ZZBGUTX LIKE ZCOT1040T-ZZBGUTX,
      END OF GS_1040.

DATA: BEGIN OF GS_1050,
        ZZBGU   LIKE ZCOT1050-ZZBGU,
        ZZBGD   LIKE ZCOT1050-ZZBGD,
        ZZBGDTX LIKE ZCOT1050T-ZZBGDTX,
      END OF GS_1050.

DATA: BEGIN OF GS_1100,
        ZZPRG   LIKE ZCOT1100-ZZPRG,
        ZZPRGTX LIKE ZCOT1100T-ZZPRGTX,
      END OF GS_1100.

DATA: BEGIN OF GS_CHECK,
        AVORG   TYPE COBRB-AVORG,
        VERSN   TYPE COBRB-VERSN,
        YEAR    TYPE GJAHR,
        MONTH   TYPE MONTH,
        PROZS   TYPE COBRB-PROZS,
      END OF GS_CHECK.


DATA: BEGIN OF GS_1310,
        BUKRS   LIKE ZCOT1310-BUKRS,
        BUTXT   LIKE T001-BUTXT,
        ZZBGU   LIKE ZCOT1310-ZZBGU,
        ZZBGUTX LIKE ZCOT1040T-ZZBGUTX,
        ZZBGD   LIKE ZCOT1310-ZZBGD,
        ZZBGDTX LIKE ZCOT1050T-ZZBGDTX,
        ZZPRG   LIKE ZCOT1310-ZZPRG,
        ZZPRGTX LIKE ZCOT1100T-ZZPRGTX,
        WW120   LIKE ZCOT1310-WW120,
        BEZEK   LIKE T25A1-BEZEK,
        AEDAT   LIKE ZCOT1310-AEDAT,
        AEZET   LIKE ZCOT1310-AEZET,
        AENAM   LIKE ZCOT1310-AENAM,
      END OF GS_1310.

DATA: BEGIN OF GS_1320,
        PSPNR   LIKE ZCOT1320-PSPNR,
        POST1   LIKE PRPS-POST1,
        WW120   LIKE ZCOT1320-WW120,
        BEZEK   LIKE T25A1-BEZEK,
        AEDAT   LIKE ZCOT1320-AEDAT,
        AEZET   LIKE ZCOT1320-AEZET,
        AENAM   LIKE ZCOT1320-AENAM,
      END OF GS_1320.
*---------------------------------------------------------------------*
* INTERNAL TABLE
*---------------------------------------------------------------------*
DATA GT_DATA          LIKE TABLE OF GS_DATA.
DATA GT_DISPLAY       LIKE TABLE OF GS_DISPLAY.
DATA GT_COBRB         LIKE TABLE OF GS_COBRB.
DATA GT_JEST          LIKE TABLE OF GS_JEST.
DATA GT_T2501         LIKE TABLE OF GS_T2501.
DATA GT_T001          LIKE TABLE OF GS_T001.
*DATA GT_1040          LIKE TABLE OF GS_1040.
DATA GT_1050          LIKE TABLE OF GS_1050.
*DATA GT_1100          LIKE TABLE OF GS_1100.
DATA GT_CHECK         LIKE TABLE OF GS_CHECK.
DATA GT_1310          LIKE TABLE OF GS_1310.
DATA GT_1320          LIKE TABLE OF GS_1320.

*---------------------------------------------------------------------*
* ALV
*---------------------------------------------------------------------*
DATA GR_SPLIT           TYPE REF TO CL_GUI_SPLITTER_CONTAINER.
DATA GR_CON_TOP         TYPE REF TO CL_GUI_CONTAINER.
DATA GR_CON_ALV         TYPE REF TO CL_GUI_CONTAINER.
DATA GR_DDOC            TYPE REF TO CL_DD_DOCUMENT.
DATA GR_ALV             TYPE REF TO ZCL_CO_ALV.
DATA GR_GRID            TYPE REF TO CL_GUI_ALV_GRID.
DATA GR_EVENT_RECEIVER  TYPE REF TO LCL_EVENT_RECEIVER.

DATA GR_CON_DIALOG      TYPE REF TO CL_GUI_DIALOGBOX_CONTAINER.
DATA GR_ALV_DIALOG      TYPE REF TO ZCL_CO_ALV.

*---------------------------------------------------------------------*
* BDC
*---------------------------------------------------------------------*
DATA GT_BDC_DATA TYPE TABLE OF BDCDATA.
DATA GS_BDC_DATA TYPE BDCDATA.
DATA GT_BDC_MSG  TYPE TABLE OF BDCMSGCOLL.
DATA GS_BDC_MSG  TYPE BDCMSGCOLL.
DATA GS_BDC_OPT  TYPE CTU_PARAMS.
DATA GV_BDC_CHK  TYPE C.


*---------------------------------------------------------------------*
* RANGES
*---------------------------------------------------------------------*
* - Prefix 정의
*   1. R_  : Global, Local Ranges

* EX) RANGES: R_FKART  FOR VBRK-FKART,
*             GS_FKDAT LIKE LINE OF R_FKART.
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
