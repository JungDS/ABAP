*&---------------------------------------------------------------------*
*& Include          ZCOR0600T01
*&---------------------------------------------------------------------*
TYPE-POOLS : ICON,ABAP.

TABLES: SSCRFIELDS. "선택화면의 필드(Function Key)
TABLES: T001, PRPS.



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

DATA: GV_MODE       TYPE C.
DATA: GV_REPID      TYPE SY-REPID.
DATA: GV_REFRESH    TYPE C.
DATA: GV_I0001_TXT  LIKE TJ02T-TXT30.

DATA GV_TEMP1       TYPE TEXT255.
DATA GV_TEMP2       TYPE TEXT255.

*---------------------------------------------------------------------*
* STRUCTURE
*---------------------------------------------------------------------*
DATA GS_FUNTXT        TYPE SMP_DYNTXT. "Excel 양식 Download(펑션키)
DATA GS_TKA01         TYPE TKA01.

DATA: BEGIN OF GS_DATA,
        PBUKR           LIKE PRPS-PBUKR,
        PGSBR           LIKE PRPS-PGSBR,

        PSPID           LIKE PROJ-PSPID,
        POST0           LIKE PROJ-POST1,
        UPDAT           LIKE PROJ-CPD_UPDAT,
        AENAM           LIKE PROJ-AENAM,

        STUFE           LIKE PRPS-STUFE,
        PSPNR           LIKE PRPS-PSPNR,
        POSID           LIKE PRPS-POSID,
        POST1           LIKE PRPS-POST1,

        PSTRT           LIKE PRTE-PSTRT,
        PENDE           LIKE PRTE-PENDE,
        ZZIZW           LIKE PRPS-ZZIZW,          " 투자사유
        ZZCD1           LIKE PRPS-ZZCD1,          " 대분류
        ZZCD2           LIKE PRPS-ZZCD2,          " 중분류
        ZZCD3           LIKE PRPS-ZZCD3,          " 소분류
*        ZZTRD           LIKE PRPS-ZZTRD,          " 거래처
        ZZTCV           LIKE PRPS-ZZTCV,          " 계약금액
        ZZWAE           LIKE PRPS-ZZWAE,          " 통화 키
*        ZZRT1           LIKE PRPS-ZZRT1,          " 계약금 비율(%)
*        ZZRT2           LIKE PRPS-ZZRT2,          " 중도금 비율(%)
*        ZZRT3           LIKE PRPS-ZZRT3,          " 잔금 비율(%)
*        ZZDT1           LIKE PRPS-ZZDT1,          " 계약금 지급예정일
*        ZZDT2           LIKE PRPS-ZZDT2,          " 중도금 지급예정일
*        ZZDT3           LIKE PRPS-ZZDT3,          " 잔금 지급예정일
        ZZUNT           LIKE PRPS-ZZUNT,          " 호기
        ZZCMD           LIKE PRPS-ZZCMD,          " 공사착공일
        ZZCPD           LIKE PRPS-ZZCPD,          " 공사준공일
*        USR00           LIKE PRPS-USR00,          " 발주자
*        USR01           LIKE PRPS-USR01,          " 부서
*        USR02           LIKE PRPS-USR02,          " 점검유형
*        USR03           LIKE PRPS-USR03,          " 고장유형
*        USR08           LIKE PRPS-USR08,          " 계약발주일
*        USR09           LIKE PRPS-USR09,          " 점검일
*        USR10           LIKE PRPS-USR10,          " 사전계획
        ZZWBT           LIKE PRPS-ZZWBT,          " WBS유형
        ZZCYP           LIKE PRPS-ZZCYP,          " 통제유형
        ERDAT           LIKE PRPS-ERDAT,          " 생성일
        ERNAM           LIKE PRPS-ERNAM,          " 생성자
        OBJNR           LIKE PRPS-OBJNR,          " 오브젝트
        TDLINE          LIKE BAPITLINE-TDLINE,    " 비고
      END OF GS_DATA.


DATA: BEGIN OF GS_DISPLAY,
*--------------------------------------------------------------------*
        PBUKR           LIKE PRPS-PBUKR,          " 회사코드
        PGSBR           LIKE PRPS-PGSBR,          " 사업영역
        GTEXT           LIKE TGSBT-GTEXT,         " 사업영역명
                                                  "
        PSPID           LIKE PROJ-PSPID,          " 프로젝트
        POST0           LIKE PROJ-POST1,          " 프로젝트명

        STUFE           LIKE PRPS-STUFE,          " Lvl
        POSID           LIKE PRPS-POSID,          " WBS
        POST1           LIKE PRPS-POST1,          " WBS명

        PSTRT           LIKE PRTE-PSTRT,          " 시작일
        PENDE           LIKE PRTE-PENDE,          " 종료일

        ZZIZW           LIKE PRPS-ZZIZW,          " 투자사유
        ZZIZWTX         LIKE ZCOT1270T-ZZIZWTX,   " 투자사유명

        ZZCD1           LIKE PRPS-ZZCD1,          " 대분류
        ZZCD1TX         LIKE ZCOT1280T-ZZCD1TX,   " 대분류명
        ZZCD2           LIKE PRPS-ZZCD2,          " 중분류
        ZZCD2TX         LIKE ZCOT1290T-ZZCD2TX,   " 중분류명
        ZZCD3           LIKE PRPS-ZZCD3,          " 소분류
        ZZCD3TX         LIKE ZCOT1300T-ZZCD3TX,   " 소분류명

*        ZZTRD           LIKE PRPS-ZZTRD,          " 거래처명
        ZZTCV           LIKE ZCOS0021-ZZTCV,      " 계약금액
        ZZWAE           LIKE PRPS-ZZWAE,          " 통화
*        ZZRT1           LIKE PRPS-ZZRT1,          " 계약금%
*        ZZRT2           LIKE PRPS-ZZRT2,          " 중도금%
*        ZZRT3           LIKE PRPS-ZZRT3,          " 잔금%
*        ZZDT1           LIKE PRPS-ZZDT1,          " 계약금예정일
*        ZZDT2           LIKE PRPS-ZZDT2,          " 중도금예정일
*        ZZDT3           LIKE PRPS-ZZDT3,          " 잔금예정일
        ZZUNT           LIKE PRPS-ZZUNT,          " 호기
        ZZCMD           LIKE PRPS-ZZCMD,          " 공사착공일
        ZZCPD           LIKE PRPS-ZZCPD,          " 공사준공일

*        USR00           LIKE PRPS-USR00,          " 발주자
*        USR01           LIKE PRPS-USR01,          " 부서
*        USR02           LIKE PRPS-USR02,          " 점검유형
*        USR03           LIKE PRPS-USR03,          " 고장유형
*        USR08           LIKE PRPS-USR08,          " 계약발주일
*        USR09           LIKE PRPS-USR09,          " 점검일
*        USR10           LIKE PRPS-USR10,          " 사전계획

        ZZWBT           LIKE PRPS-ZZWBT,          " WBS유형
        ZZCYP           LIKE PRPS-ZZCYP,          " 통제유형
        KONTY           LIKE COBRB-KONTY,         " 정산규칙
        EXTNR           LIKE COBRB-EXTNR,         " 배부규칙에 대한 외부번호
        PROZS           LIKE COBRB-PROZS,         " 정산백분율
        S_ANLN1         LIKE ANLA-ANLN1,          " S_FXA
        S_TXT50         LIKE ANLA-TXT50,          " 정산FXA명
        S_POSID         LIKE PRPS-POSID,          " S.WBS
        S_POST1         LIKE PRPS-POST1,          " 정산WBS명
        STTXT           LIKE CNJ_STAT-STTXT_INT,  " 상태

        FLG_PLN         LIKE FOAP_S_RFAVIS40-CHECKBOX, " 계획여부
        FLG_ACT         LIKE FOAP_S_RFAVIS40-CHECKBOX, " 실적여부

        ERDAT           LIKE PRPS-ERDAT,          " 생성일
                                                  " 생성시간
        ERNAM           LIKE PRPS-ERNAM,          " 생성자
        ERNAMTX         LIKE ADRP-NAME_TEXT,      " 생성자명
        AEDAT           LIKE PROJ-AEDAT,          " 수정일
        AEZET           LIKE ZCAS0010-AEZET,      " 수정시간
        AENAM           LIKE PROJ-AENAM,          " 수정자
        AENAMTX         LIKE ADRP-NAME_TEXT,      " 수정자명
        OBJNR           LIKE PRPS-OBJNR,          " 오브젝트
        TDLINE          LIKE BAPITLINE-TDLINE,    " 비고
*--------------------------------------------------------------------*
        STYLE           TYPE LVC_T_STYL,
        COLOR           TYPE LVC_T_SCOL,
      END OF GS_DISPLAY.

DATA: BEGIN OF GS_COBRB,
        OBJNR           LIKE COBRB-OBJNR,
        LFDNR           LIKE COBRB-LFDNR,
        KONTY           LIKE COBRB-KONTY,
        EXTNR           LIKE COBRB-EXTNR,
        PROZS           LIKE COBRB-PROZS,
        REC_OBJNR1      LIKE COBRB-REC_OBJNR1,
      END OF GS_COBRB.

DATA: BEGIN OF GS_COSP,
        OBJNR           LIKE COSP-OBJNR,
        WRTTP           LIKE COSP-WRTTP,
      END OF GS_COSP.

DATA: BEGIN OF GS_PRPS,
        OBJNR           LIKE PRPS-OBJNR,
        POSID           LIKE PRPS-POSID,
        POST1           LIKE PRPS-POST1,
      END OF GS_PRPS.

DATA: BEGIN OF GS_ANLA,
        OBJNR           LIKE ANLA-OBJNR,
        ANLN1           LIKE ANLA-ANLN1,
        TXT50           LIKE ANLA-TXT50,
      END OF GS_ANLA.

DATA: BEGIN OF GS_TGSBT,
        GSBER           LIKE TGSBT-GSBER,
        GTEXT           LIKE TGSBT-GTEXT,
      END OF GS_TGSBT.

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

DATA: BEGIN OF GS_ADRP,
        BNAME           LIKE USR21-BNAME,
        DATE_FROM       LIKE ADRP-DATE_FROM,
        NAME_TEXT       LIKE ADRP-NAME_TEXT,
      END OF GS_ADRP.

DATA: BEGIN OF GS_OBJ_STATUS,
        OBJNR           LIKE PRPS-OBJNR,
        RANK            TYPE N,
        STAT            LIKE JEST-STAT,
        TXT30           LIKE TJ02T-TXT30,
      END OF GS_OBJ_STATUS.

DATA: BEGIN OF GS_STXH,
        TDNAME          LIKE STXH-TDNAME,
        TDSPRAS         LIKE STXH-TDSPRAS,
      END OF GS_STXH.

*---------------------------------------------------------------------*
* INTERNAL TABLE
*---------------------------------------------------------------------*

DATA GT_DATA            LIKE TABLE OF GS_DATA.
DATA GT_DISPLAY         LIKE TABLE OF GS_DISPLAY.
DATA GT_COBRB           LIKE TABLE OF GS_COBRB.
DATA GT_COSP            LIKE TABLE OF GS_COSP.
DATA GT_PRPS            LIKE TABLE OF GS_PRPS.
DATA GT_ANLA            LIKE TABLE OF GS_ANLA.
DATA GT_TGSBT           LIKE TABLE OF GS_TGSBT.
DATA GT_1270T           LIKE TABLE OF GS_1270T.
DATA GT_1280T           LIKE TABLE OF GS_1280T.
DATA GT_1290T           LIKE TABLE OF GS_1290T.
DATA GT_1300T           LIKE TABLE OF GS_1300T.
DATA GT_ADRP            LIKE TABLE OF GS_ADRP.
DATA GT_OBJ_STATUS      LIKE TABLE OF GS_OBJ_STATUS.
DATA GT_STXH            LIKE TABLE OF GS_STXH.


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

RANGES: R_OBJNR FOR GS_DATA-OBJNR,
        R_ERNAM FOR GS_DATA-ERNAM.

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
