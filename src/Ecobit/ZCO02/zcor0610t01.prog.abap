*&---------------------------------------------------------------------*
*& Include          ZCOR0610T01
*&---------------------------------------------------------------------*
TYPE-POOLS : ICON,ABAP.

TABLES: SSCRFIELDS. "선택화면의 필드(Function Key)
TABLES: TKA01, T001, PRPS, COSP.



CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.

*---------------------------------------------------------------------*
* CONSTANTS
*---------------------------------------------------------------------*
CONSTANTS : GC_A     TYPE CHAR01   VALUE 'A',
            GC_C     TYPE CHAR01   VALUE 'C',
            GC_D     TYPE CHAR01   VALUE 'D',
            GC_E     TYPE CHAR01   VALUE 'E',
            GC_I     TYPE CHAR01   VALUE 'I',
            GC_N     TYPE CHAR01   VALUE 'N',
            GC_S     TYPE CHAR01   VALUE 'S',
            GC_P     TYPE CHAR01   VALUE 'P',
            GC_W     TYPE CHAR01   VALUE 'W',
            GC_X     TYPE CHAR01   VALUE 'X',
            GC_TCODE TYPE SY-TCODE VALUE '',
            GC_TAB_CE71000 TYPE T811C-TAB VALUE 'CE71000'.

CONSTANTS : GC_0     TYPE N VALUE '0',
            GC_1     TYPE N VALUE '1',
            GC_2     TYPE N VALUE '2',
            GC_3     TYPE N VALUE '3',
            GC_4     TYPE N VALUE '4'.

CONSTANTS : GC_I0001 TYPE TJ02-ISTAT VALUE 'I0001'.
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

DATA: GV_EXIT       TYPE C,
      GV_ANSWER     TYPE C,
      OK_CODE       TYPE SY-UCOMM,   "예외
      SAVE_OK       TYPE SY-UCOMM.   "예외

DATA: GV_REPID      TYPE SY-REPID.
DATA: GV_REFRESH    TYPE C.
DATA: GV_I0001_TXT  LIKE TJ02T-TXT30.

DATA GV_TEMP1       TYPE TEXT255.
DATA GV_TEMP2       TYPE TEXT255.
*---------------------------------------------------------------------*
* STRUCTURE
*---------------------------------------------------------------------*
DATA GS_FUNTXT        TYPE SMP_DYNTXT. "Excel 양식 Download(펑션키)


DATA: BEGIN OF GS_WBS,
        PBUKR           LIKE PRPS-PBUKR,          " 회사코드
        ZZIZW           LIKE PRPS-ZZIZW,          " 투자사유
        PSPID           LIKE PROJ-PSPID,          " 프로젝트
        POST0           LIKE PROJ-POST1,          " 프로젝트명
        STUFE           LIKE PRPS-STUFE,          " Lvl
        POSID           LIKE PRPS-POSID,          " WBS
        POST1           LIKE PRPS-POST1,          " WBS명
        ZZCD1           LIKE PRPS-ZZCD1,          " 대분류
        ZZCD2           LIKE PRPS-ZZCD2,          " 중분류
        ZZCD3           LIKE PRPS-ZZCD3,          " 소분류
        ZZUNT           LIKE PRPS-ZZUNT,          " 호기
        ZZCMD           LIKE PRPS-ZZCMD,          " 공사착공일
        ZZCPD           LIKE PRPS-ZZCPD,          " 공사준공일
        PSTRT           LIKE PRTE-PSTRT,          " 시작일
        PENDE           LIKE PRTE-PENDE,          " 종료일
        OBJNR           LIKE PRPS-OBJNR,          " 오브젝트
      END OF GS_WBS.

DATA: BEGIN OF GS_COSP,
        OBJNR           LIKE COSP-OBJNR,
        GJAHR           LIKE COSP-GJAHR,
        KSTAR           LIKE COSP-KSTAR,
        WRTTP           LIKE COSP-WRTTP,
        WKG001          LIKE COSP-WKG001,         " 01월
        WKG002          LIKE COSP-WKG002,         " 02월
        WKG003          LIKE COSP-WKG003,         " 03월
        WKG004          LIKE COSP-WKG004,         " 04월
        WKG005          LIKE COSP-WKG005,         " 05월
        WKG006          LIKE COSP-WKG006,         " 06월
        WKG007          LIKE COSP-WKG007,         " 07월
        WKG008          LIKE COSP-WKG008,         " 08월
        WKG009          LIKE COSP-WKG009,         " 09월
        WKG010          LIKE COSP-WKG010,         " 10월
        WKG011          LIKE COSP-WKG011,         " 11월
        WKG012          LIKE COSP-WKG012,         " 12월
      END OF GS_COSP.


DATA: BEGIN OF GS_DISPLAY,
*        ID              LIKE BAPITLINE-TDLINE,
*--------------------------------------------------------------------*
        PBUKR           LIKE PRPS-PBUKR,          " 회사코드
        ZZIZW           LIKE PRPS-ZZIZW,          " 투자사유
        ZZIZWTX         LIKE ZCOT1270T-ZZIZWTX,   " 투자사유명

        PSPID           LIKE PROJ-PSPID,          " 프로젝트
        POST0           LIKE PROJ-POST1,          " 프로젝트명
        STUFE           LIKE PRPS-STUFE,          " Lvl
        POSID           LIKE PRPS-POSID,          " WBS
        POST1           LIKE PRPS-POST1,          " WBS명

        ZZCD1           LIKE PRPS-ZZCD1,          " 대분류
        ZZCD1TX         LIKE ZCOT1280T-ZZCD1TX,   " 대분류명
        ZZCD2           LIKE PRPS-ZZCD2,          " 중분류
        ZZCD2TX         LIKE ZCOT1290T-ZZCD2TX,   " 중분류명
        ZZCD3           LIKE PRPS-ZZCD3,          " 소분류
        ZZCD3TX         LIKE ZCOT1300T-ZZCD3TX,   " 소분류명
        ZZUNT           LIKE PRPS-ZZUNT,          " 호기
        ZZCMD           LIKE PRPS-ZZCMD,          " 공사착공일
        ZZCPD           LIKE PRPS-ZZCPD,          " 공사준공일

        GJAHR           LIKE COSP-GJAHR,          " 연도
        KSTAR           LIKE COSP-KSTAR,          " 원가요소
        KTEXT           LIKE CSKU-KTEXT,          " 원가요소명

        WKGSUMP         LIKE COSP-WKG001,         " 연간계획
        WKGSUMA         LIKE COSP-WKG001,         " 연간실적

        WKG001P         LIKE COSP-WKG001,         " 01월계획
        WKG001A         LIKE COSP-WKG001,         " 01월실적
        WKG002P         LIKE COSP-WKG002,         " 02월계획
        WKG002A         LIKE COSP-WKG002,         " 02월실적
        WKG003P         LIKE COSP-WKG003,         " 03월계획
        WKG003A         LIKE COSP-WKG003,         " 03월실적
        WKG004P         LIKE COSP-WKG004,         " 04월계획
        WKG004A         LIKE COSP-WKG004,         " 04월실적
        WKG005P         LIKE COSP-WKG005,         " 05월계획
        WKG005A         LIKE COSP-WKG005,         " 05월실적
        WKG006P         LIKE COSP-WKG006,         " 06월계획
        WKG006A         LIKE COSP-WKG006,         " 06월실적
        WKG007P         LIKE COSP-WKG007,         " 07월계획
        WKG007A         LIKE COSP-WKG007,         " 07월실적
        WKG008P         LIKE COSP-WKG008,         " 08월계획
        WKG008A         LIKE COSP-WKG008,         " 08월실적
        WKG009P         LIKE COSP-WKG009,         " 09월계획
        WKG009A         LIKE COSP-WKG009,         " 09월실적
        WKG010P         LIKE COSP-WKG010,         " 10월계획
        WKG010A         LIKE COSP-WKG010,         " 10월실적
        WKG011P         LIKE COSP-WKG011,         " 11월계획
        WKG011A         LIKE COSP-WKG011,         " 11월실적
        WKG012P         LIKE COSP-WKG012,         " 12월계획
        WKG012A         LIKE COSP-WKG012,         " 12월실적

*        PSTRT           LIKE PRTE-PSTRT,          " 시작일
*        PENDE           LIKE PRTE-PENDE,          " 종료일
        OBJNR           LIKE PRPS-OBJNR,          " 오브젝트
        TDLINE          LIKE BAPITLINE-TDLINE,    " 비고
*--------------------------------------------------------------------*
        STYLE           TYPE LVC_T_STYL,
        COLOR           TYPE LVC_T_SCOL,
      END OF GS_DISPLAY.


DATA: BEGIN OF GS_1270T,
        ZZIZW           LIKE ZCOT1270T-ZZIZW,
        ZZIZWTX         LIKE ZCOT1270T-ZZIZWTX,
      END OF GS_1270T.

DATA: BEGIN OF GS_1280T,
        ZZCD1           LIKE ZCOT1280T-ZZCD1,
        ZZCD1TX         LIKE ZCOT1280T-ZZCD1TX,
      END OF GS_1280T.

DATA: BEGIN OF GS_1290T,
        ZZCD1           LIKE ZCOT1290T-ZZCD1,
        ZZCD2           LIKE ZCOT1290T-ZZCD2,
        ZZCD2TX         LIKE ZCOT1290T-ZZCD2TX,
      END OF GS_1290T.

DATA: BEGIN OF GS_1300T,
        ZZCD1           LIKE ZCOT1300T-ZZCD1,
        ZZCD2           LIKE ZCOT1300T-ZZCD2,
        ZZCD3           LIKE ZCOT1300T-ZZCD3,
        ZZCD3TX         LIKE ZCOT1300T-ZZCD3TX,
      END OF GS_1300T.

DATA: BEGIN OF GS_CSKU,
        KSTAR           LIKE CSKU-KSTAR,
        KTEXT           LIKE CSKU-KTEXT,
      END OF GS_CSKU.

*---------------------------------------------------------------------*
* INTERNAL TABLE
*---------------------------------------------------------------------*

DATA GT_WBS             LIKE TABLE OF GS_WBS.
DATA GT_COSP            LIKE TABLE OF GS_COSP.
DATA GT_DISPLAY         LIKE TABLE OF GS_DISPLAY.
DATA GT_1270T           LIKE TABLE OF GS_1270T.
DATA GT_1280T           LIKE TABLE OF GS_1280T.
DATA GT_1290T           LIKE TABLE OF GS_1290T.
DATA GT_1300T           LIKE TABLE OF GS_1300T.
DATA GT_CSKU            LIKE TABLE OF GS_CSKU.


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
*DATA GT_BDC_DATA TYPE TABLE OF BDCDATA.
*DATA GS_BDC_DATA TYPE BDCDATA.
*DATA GT_BDC_MSG  TYPE TABLE OF BDCMSGCOLL.
*DATA GS_BDC_MSG  TYPE BDCMSGCOLL.
*DATA GS_BDC_OPT  TYPE CTU_PARAMS.
*DATA GV_BDC_CHK  TYPE C.


*---------------------------------------------------------------------*
* RANGES
*---------------------------------------------------------------------*
* - Prefix 정의
*   1. R_  : Global, Local Ranges

* EX) RANGES: R_FKART  FOR VBRK-FKART,
*             GS_FKDAT LIKE LINE OF R_FKART.

RANGES: R_OBJNR FOR PRPS-OBJNR.

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

DEFINE __ADD_SELOPT_TEXT.
  CLEAR: &1, GV_TEMP1, GV_TEMP2.

  LOOP AT &2.
    IF &1 IS NOT INITIAL.
      &1 = &1 && ','.
    ENDIF.

    WRITE &2-LOW  TO GV_TEMP1.

    IF &2-OPTION EQ 'BT'.
      WRITE &2-HIGH TO GV_TEMP2.
      &1 = |{ &1 } { GV_TEMP1 } ~ { GV_TEMP2 }|.
    ELSE.
      CASE &2-OPTION.
        WHEN 'GT'.    GV_TEMP2 = '>'.
        WHEN 'GE'.    GV_TEMP2 = '>='.
        WHEN 'LT'.    GV_TEMP2 = '<'.
        WHEN 'LE'.    GV_TEMP2 = '<='.
        WHEN 'NE'.    GV_TEMP2 = 'Not'.
        WHEN OTHERS.  CLEAR GV_TEMP2.
      ENDCASE.

      CONCATENATE &1 GV_TEMP2 GV_TEMP1 INTO &1 SEPARATED BY SPACE.
    ENDIF.


    IF &2-SIGN EQ GC_E.
      CONCATENATE &1 '제외' INTO &1 SEPARATED BY SPACE.
    ENDIF.
  ENDLOOP.

  CONDENSE &1.
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
