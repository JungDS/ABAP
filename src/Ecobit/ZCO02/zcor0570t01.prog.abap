*&---------------------------------------------------------------------*
*& Include          ZCOR0570T01
*&---------------------------------------------------------------------*
TYPE-POOLS : ICON,ABAP.

TABLES: SSCRFIELDS. "선택화면의 필드(Function Key)
TABLES: ACDOCA.



CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.

*---------------------------------------------------------------------*
* CONSTANTS
*---------------------------------------------------------------------*
CONSTANTS : GC_A     TYPE CHAR01   VALUE 'A',
            GC_C     TYPE CHAR01   VALUE 'C',
            GC_D     TYPE CHAR01   VALUE 'D',
            GC_E     TYPE CHAR01   VALUE 'E',
            GC_N     TYPE CHAR01   VALUE 'N',
            GC_S     TYPE CHAR01   VALUE 'S',
            GC_W     TYPE CHAR01   VALUE 'W',
            GC_X     TYPE CHAR01   VALUE 'X',
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

DATA: GV_MONTH_MIN TYPE ACDOCA-POPER,
      GV_MONTH_MAX TYPE ACDOCA-POPER.

*---------------------------------------------------------------------*
* STRUCTURE
*---------------------------------------------------------------------*
DATA GS_FUNTXT        TYPE SMP_DYNTXT. "Excel 양식 Download(펑션키)
DATA GS_TKA01         TYPE TKA01.

DATA: BEGIN OF GS_DATA,
        KOSTL         LIKE CSKS-KOSTL,
        DATBI         LIKE CSKS-DATBI,
        DATAB         LIKE CSKS-DATAB,
        BUKRS         LIKE CSKS-BUKRS,
        OBJNR         LIKE CSKS-OBJNR,
        KTEXT         LIKE CSKT-KTEXT,
        BUTXT         LIKE T001-BUTXT,
      END OF GS_DATA.

DATA: BEGIN OF GS_DISPLAY,
        STATUS        LIKE ICON-ID,
        BUKRS         LIKE T001-BUKRS,
        BUTXT         LIKE T001-BUTXT,
        KOSTL         LIKE CSKS-KOSTL,
        KTEXT         LIKE CSKT-KTEXT,
        SETID1        LIKE GRPOBJECTS-SETID,
        SETTX1        LIKE GRPOBJECTS-DESCRIPT,
        SETID2        LIKE GRPOBJECTS-SETID,
        SETTX2        LIKE GRPOBJECTS-DESCRIPT,
        AMT_P         LIKE COSP-WKG001, " 기표
        AMT_D         LIKE COSP-WKG001, " 배부
*        AMT_S         LIKE COSP-WKG001, " 정산
        AMT_B         LIKE COSP-WKG001, " 잔고
        MESSAGE       LIKE BAPIRET2-MESSAGE,
*--------------------------------------------------------------------*
        STYLE         TYPE LVC_T_STYL,
        COLOR         TYPE LVC_T_SCOL,
      END OF GS_DISPLAY.

DATA: BEGIN OF GS_COSP,
        OBJNR   LIKE COSP-OBJNR,
        VRGNG   LIKE COSP-VRGNG,
        WKGSUM  LIKE COSP-WKG001,
      END OF GS_COSP.

DATA: BEGIN OF GS_COSS,
        OBJNR   LIKE COSS-OBJNR,
        VRGNG   LIKE COSS-VRGNG,
        WKGSUM  LIKE COSS-WKG001,
      END OF GS_COSS.

*---------------------------------------------------------------------*
* INTERNAL TABLE
*---------------------------------------------------------------------*
DATA GT_DATA          LIKE TABLE OF GS_DATA.
DATA GT_DISPLAY       LIKE TABLE OF GS_DISPLAY.
DATA GT_COSP          LIKE TABLE OF GS_COSP.
DATA GT_COSS          LIKE TABLE OF GS_COSS.

DATA GT_SETH_NODE     TYPE GSETH_NODE_TAB.
DATA GT_SETH_VAL      TYPE GSETH_VAL_TAB.
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
