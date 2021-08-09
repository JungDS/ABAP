*&---------------------------------------------------------------------*
*& Include          ZCOR0450T01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES : ZCOT1260.
TYPE-POOLS: ICON, ABAP.

TABLES : SSCRFIELDS. "선택화면의 필드(Function Key)
DATA : GS_FUNTXT TYPE SMP_DYNTXT.
*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. GC_  : Global Constants
*   2. LC_  : Local Constants
* - EX). CONSTANTS: GC_E  TYPE C VALUE 'E'.
CONSTANTS : GC_X   TYPE CHAR1 VALUE 'X',
            GC_C   TYPE CHAR1 VALUE 'C',
            GC_E   TYPE CHAR1 VALUE 'E',
            GC_S   TYPE CHAR1 VALUE 'S',
            GC_D   TYPE CHAR1 VALUE 'D',
            GC_R   TYPE CHAR1 VALUE 'R',
            GC_KRW TYPE TCURR_CURR VALUE 'KRW'.
CONSTANTS : GC_ZM LIKE BKPF-BLART VALUE 'ZM'.
CONSTANTS : GC_WZ LIKE BKPF-BLART VALUE 'WZ'.
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

DATA : GV_WE_NM TYPE T001W-NAME1. " 플랜트명

DATA: GV_TREE_GL TYPE XFELD,
      GV_EXIT    TYPE XFELD,
      GV_ERROR   TYPE XFELD,
      GV_ANSWER  TYPE C,
      GV_MODE    TYPE C,          "N:생성, C:변경, D:조회
      OK_CODE    TYPE SY-UCOMM,   "예외
      SAVE_OK    TYPE SY-UCOMM.   "예외
DATA:
  GV_LINES   LIKE SY-TABIX,
  GV_TITLE   LIKE SY-TITLE,
  GV_MESSAGE TYPE BAPI_MSG.
DATA :GV_WAERS TYPE WAERS.

DATA : GV_M1    TYPE SPMON,
       GV_M2    TYPE SPMON,
*       GV_TDATE TYPE SY-DATUM, "입력기간 말일
       GV_NDATE TYPE SY-DATUM, "익월1일
       GV_DATUM TYPE SY-DATUM, "전기일
       GV_SPMON TYPE SY-DATUM. "전기일

DATA :  GV_LFGJA TYPE MARD-LFGJA.  " mm 전년
DATA :  GV_LFMON  TYPE MARD-LFMON.  " mm 전월

DATA : BEGIN OF GT_MARD OCCURS 0,
         LFGJA TYPE MARD-LFGJA,
         LFMON TYPE MARD-LFMON,
         MATNR TYPE MARD-MATNR,
         LABST TYPE MARD-LABST,
       END OF GT_MARD.



*DATA : GV_DATUM TYPE SY-DATUM."
DATA : GV_CDATE TYPE SY-DATUM.""매출확정일
*----------------------------------------------------------------------*
* STRUCTURE
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. GS_  : Global Structure
*   2. LS_  : Local Structure

*EX) DATA: GS_SFLIGHT TYPE SFLIGHT

"__ 리스트 조회
DATA: GS_DISPLAY  TYPE ZCOS0450.


TYPES: BEGIN OF TY_RAW ,
         WERKS        TYPE ZMMT0610-WERKS  , " 플랜트
         WERKS_NAME   TYPE ZCOS0450-WERKS_NAME, " 플랜트명

         RMBLNR       TYPE ZMMT0610-RMBLNR, " 원자재의 자재문서번호
         MATKL        TYPE ZCOS0450-MATKL,  "자재 그룹
         WGBEZ        TYPE ZCOS0450-WGBEZ,  " 자재 그룹 내역
         FMATNR       TYPE ZCOS0450-FMATNR, "[CO] 제품 자재코드
         FMATNR_MAKTX TYPE MAKTX, " 자재내역
         FMENGE       TYPE ZCOS0450-FMENGE,  " [CO]수량(생산)
         FMEINS       TYPE ZCOS0450-FMEINS,  "기본 단위
         RMATNR       TYPE ZCOS0450-RMATNR,  " [CO]원자재
         RMATNR_MAKTX TYPE MAKTX, " 자재내역
         RMENGE       TYPE ZCOS0450-RMENGE,  " 수량
         RMEINS       TYPE ZCOS0450-RMEINS ,  "기본 단위
         RWRBTR       TYPE ZCOS0450-RWRBTR,
         TWAER        TYPE ZCOS0450-TWAER,
       END OF TY_RAW.

DATA : GT_RAW TYPE STANDARD TABLE OF TY_RAW    .
*
*ZMMT0600 MATNR   KEY  제품
*        MATNR2       원자재


TYPES: BEGIN OF TY_RAW2 ,
         MM_RWRBTR TYPE ZCOS0450-RWRBTR,       " mmt0610-dmbtr
         FI_HSL    LIKE ACDOCA-HSL,   " fi 재료비 sum
         DIFF      LIKE ACDOCA-HSL,  " 차이
         TWAER     LIKE BKPF-WAERS,
       END OF TY_RAW2.
DATA : GT_RAW2 TYPE STANDARD TABLE OF TY_RAW2.  " 원재료비


DATA : GT_RAW_DISTRI TYPE STANDARD TABLE OF TY_RAW2,  " 배부대상  원자재 금액
       GS_RAW_DISTRI LIKE LINE OF GT_RAW_DISTRI.


DATA : GV_FMATNR_MENGE_TOT LIKE ZCOS0450-FMENGE.

* 구매거래처별 자재그룹별 제품별 수량 비율
TYPES: BEGIN OF TY_RAW3 ,
         LIFNR     TYPE MARA-WRKST, " mm01  기본뷰 2에 기본자재 필드
         MATKL     TYPE ZCOS0450-MATKL,  "완제품의  자재 그룹
         FMATNR    TYPE ZCOS0450-FMATNR,  " 자재
         FMENGE    TYPE ZCOS0450-FMENGE,  " [CO]수량(생산)
         FMEINS    TYPE ZCOS0450-FMEINS,  "기본 단위

*         TOT_MENGE TYPE ZCOS0450-FMENGE, " 구매처별 자재그룹별 수량 total
         SUB_MENGE TYPE ZCOS0450-FMENGE, " 자재그룹별 수량 total
         TWAER     TYPE  ZCOS0450-TWAER,
         RATE3     TYPE P DECIMALS 9,

       END OF TY_RAW3.

DATA : GT_RATE3 TYPE STANDARD TABLE OF TY_RAW3,
       GS_RATE3 LIKE LINE OF GT_RATE3.

* 구매거래처별 자재그룹별
TYPES: BEGIN OF TY_RAW3A ,
         LIFNR  TYPE MARA-WRKST, " mm01  기본뷰 2에 기본자재 필드
         MATKL  TYPE ZCOS0450-MATKL,  "완제품의  자재 그룹
         FMENGE TYPE ZCOS0450-FMENGE, " 생산량
         RATE   TYPE P DECIMALS 9,
       END OF TY_RAW3A.

DATA : GT_RATE3A TYPE STANDARD TABLE OF TY_RAW3A,
       GS_RATE3A LIKE LINE OF GT_RATE3A.

TYPES: BEGIN OF TY_RAW5 ,
         FMATNR    TYPE ZCOS0450-FMATNR,
         FMENGE    TYPE ZCOS0450-FMENGE,  " [CO]수량(생산)
         FMEINS    TYPE ZCOS0450-FMEINS,  "기본 단위
         TOT_MENGE TYPE ZCOS0450-FMENGE, " 총생산수량

         RATE5     TYPE P DECIMALS 9,
       END OF TY_RAW5.

DATA : GT_RATE5 TYPE STANDARD TABLE OF TY_RAW5,
       GS_RATE5 LIKE LINE OF GT_RATE5.

****  구매처별 자재 별  생산수량

TYPES: BEGIN OF TY_RAW4 ,
         LIFNR     TYPE ZCOS0450-MATKL,
         FMATNR    TYPE ZCOS0450-FMATNR,
         FMENGE    TYPE ZCOS0450-FMENGE,
         SUB_MENGE TYPE ZCOS0450-FMENGE, "
         RATE4     TYPE P DECIMALS 9,
         TWAER     TYPE ZCOS0450-TWAER,
       END OF TY_RAW4.

DATA : GT_RATE4 TYPE STANDARD TABLE OF TY_RAW4,
       GS_RATE4 LIKE LINE OF GT_RATE4.




TYPES: BEGIN OF TY_RAW4A ,
         LIFNR TYPE ZCOS0450-MATKL,
         MATKL TYPE ZCOS0450-MATKL,
       END OF TY_RAW4A.

DATA : GT_RATE4A TYPE STANDARD TABLE OF TY_RAW4A,
       GS_RATE4A LIKE LINE OF GT_RATE4A.




DATA : BEGIN OF GS_ZCOT1260  .
         INCLUDE STRUCTURE ZCOT1260.
       DATA : END OF GS_ZCOT1260.

DATA : BEGIN OF GS_ACDOCA ,
         GUBUN         TYPE CHAR2,
         BUKRS         LIKE ACDOCA-RBUKRS,
         WERKS         LIKE ACDOCA-WERKS,
         MATNR         LIKE ACDOCA-MATNR,
         MAKTX         LIKE MAKT-MAKTX,
         GJAHR         LIKE ACDOCA-GJAHR,
         BELNR         LIKE ACDOCA-BELNR,
         BUZEI         LIKE ACDOCA-BUZEI,
         RACCT         LIKE ACDOCA-RACCT,
         TXT20         LIKE SKAT-TXT20,
         TWAER         LIKE ACDOCA-RHCUR,
         HSL           LIKE ACDOCA-HSL,
         PS_POSID      LIKE ACDOCA-PS_POSID,
         AWREF         LIKE ACDOCA-AWREF,
         AWREF_REV     LIKE ACDOCA-AWREF_REV,
         MSG_TEXT(100) , " 직접 귀속 여부  , 배부시 적용 된 우선순위 표시
       END OF GS_ACDOCA.


DATA : BEGIN OF GS_ACDOCA2 ,
         MATNR  LIKE ACDOCA-MATNR,
         FI_HSL LIKE ACDOCA-HSL,
         TWAER  LIKE ZCOS0450-TWAER,
         ERROR  TYPE C LENGTH 100,
       END OF GS_ACDOCA2.


*제조원가 쪽 WBS
*From : "김호준(BSG_CO)"<bsg_co1@tsk.co.kr>
* 1. 플랜트에 해당 하는 wbs를 찾는 모든 로직에 적용입니다. (재수정)
*   1.1  ZSDT0120 테이블에서 해당 WERKS = 화면입력 플랜트 , BUKRS = 화면입력 회사코드 ,  MODUL = 'M'  인 WBSNR 필드를 WBS로 인식
*    - acdoca에서 금액집계할 때의 wbs를 선택하는 부분도 해당
*    - 제조원가 전표처리하는 로직에도 해당

DATA : BEGIN OF GT_WBS OCCURS 0,
         POSID LIKE PRPS-POSID,
       END OF GT_WBS.

*From : "김호준(BSG_CO)"<bsg_co1@tsk.co.kr>,
*매출원가 쪽 WBS.........
*
*  1.2  ZSDT0120 테이블에서 해당 WERKS = 화면입력 플랜트 , BUKRS = 화면입력 회사코드 ,  MODUL = 'S'  인 WBSNR 필드를 WBS로 인식
*    - 매출원가 기표할 때 매출원가 계정에 대한 wbs 코드를 결정하는 로직  (==> 이부분은 제가 zcor0380 한번 더 보고 말씀드리겠습니다. )

DATA : BEGIN OF GT_WBS_COGS OCCURS 0,
         POSID LIKE PRPS-POSID,
       END OF GT_WBS_COGS,
       GS_WBS_COGS LIKE LINE OF GT_WBS_COGS.




*전표 상세  테이블
DATA : GT_505101001_ACDOCA  LIKE GS_ACDOCA2 OCCURS 0. "  505101001 원재료  전표 와 cbo의 차이 :  원자재별  비교용
DATA : GT_504101003_ACDOCA  LIKE GS_ACDOCA2 OCCURS 0. "  505411003 구매가격차이   전표
DATA : GT_505101002_ACDOCA  LIKE GS_ACDOCA2 OCCURS 0. "  505101002  재고조정금액

DATA : GT_DIFF_001  LIKE GS_ACDOCA OCCURS 0. " 원재료비 차이 전표  원재료별 합산
DATA : GT_DIFF_001B LIKE GS_ACDOCA OCCURS 0. "  FI 있고  MMT0610에 없는 전표리스트
DATA : GT_DIFF_002  LIKE GS_ACDOCA OCCURS 0.  "  504101003 구매가격 차이    원재료별 합산

* 구매가격차이1 당월 해당 원재료의 투입 및 제품생산이 있는 경우  504101003  해당 제품에 직접귀속

DATA : GT_DIFF_003  LIKE GS_ACDOCA OCCURS 0.   "재고조정금액  505101002재고조정계정 배부방법1

DATA : GT_LABOR LIKE GS_ACDOCA OCCURS 0.  "  노무비
DATA : GT_MCOST LIKE GS_ACDOCA OCCURS 0.   " 경비

* 차이 합계
DATA : GV_DIFF_ACDOCA_SUM LIKE GS_ACDOCA-HSL,

       GV_TOT_MM0610_SUM  LIKE ZMMT0610-RDMBTR, "원재료비

       GV_505101001_SUM   LIKE GS_ACDOCA-HSL, " 원재료비차이gl..
       GV_504101003_SUM   LIKE GS_ACDOCA-HSL,  "구매가격차이 gl ..
       GV_505101002_SUM   LIKE GS_ACDOCA-HSL,  "재고조정  gl ..

       GV_LABOR_AMT_SUM   LIKE GS_ACDOCA-HSL,
       GV_COST_AMT_SUM    LIKE GS_ACDOCA-HSL.


"DOC RESET
DATA: BEGIN OF GS_INFO,
        "__ INPUT
        GJAHR   TYPE BSEG-GJAHR,
        BELNR   TYPE BSEG-BELNR,

        "__RETURN
        RESULT  TYPE AS4FLAG,
        ERR_TXT TYPE NATXT,
        RBELNR  TYPE BELNR_D,
        MSG     TYPE BAPI_MSG,

        "__DO INDEX
        INDEX   TYPE SY-INDEX,
      END OF GS_INFO.

*----------------------------------------------------------------------*
* INTERNAL TABLE
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. GT_  : Global Internal Table
*   2. LT_  : Local Internal Table

*EX) DATA: GT_SFLIGHT TYPE TABLE OF SFLIGHT


"__ 리스트 조회
DATA: GT_DISPLAY  LIKE TABLE OF GS_DISPLAY, "DISPLAY DATA
      GT_ZCOT1260 LIKE TABLE OF GS_ZCOT1260,
      GT_ZMMT600  TYPE TABLE OF ZMMT0600,
      GS_ZMMT600  LIKE LINE OF GT_ZMMT600,
      GT_T134G    TYPE TABLE OF T134G,
      GT_ACDOCA   LIKE TABLE OF GS_ACDOCA.


*제품의 자재그룹
DATA : BEGIN OF GT_T023T OCCURS 0,
         FMATNR LIKE MARA-MATNR,
         MATKL  LIKE T023T-MATKL,
         WGBEZ  LIKE T023T-WGBEZ,
         WRKST  LIKE MARA-WRKST,  "기존자재 필드 , 구매처
         MTART   LIKE MARA-MTART,
       END OF  GT_T023T,
       GS_T023T LIKE LINE OF GT_T023T.

**20200311  정의  거의  원자재와 제품의 벤더 99% 일치한다라고 함

*원자재 의 벤더로   !!  IF 문의  구매거래처도 원자재의 구매처, 제품의 자재그룹
DATA : BEGIN OF GT_LFA1 OCCURS 0,
         RMATNR LIKE MARA-MATNR,
         LIFNR  LIKE MARA-WRKST,
         NAME1  LIKE LFA1-NAME1,

         MATKL  LIKE MARA-MATKL,  " 자재그룹 원자재

       END OF  GT_LFA1,
       GS_LFA1 LIKE LINE OF GT_LFA1.



DATA: BEGIN OF GT_ZCOT1230 OCCURS 0,
        POSID TYPE PS_POSID,
        SAKNR TYPE SAKNR,
        TXT20 TYPE CHAR20,
      END OF GT_ZCOT1230,
      GS_ZCOT1230 LIKE LINE OF GT_ZCOT1230.



DATA : GV_DISTRL_ERROR1(2).
DATA : GV_DISTRL_ERROR2(2).
DATA : GV_DISTRL_ERROR3(2).


DATA : GV_EXE_FLAG_RAW.
DATA : GV_EXE_FLAG_LABOR.
DATA : GV_EXE_FLAG_COGS.

*
*제품매출액  1000  402101001
*제품매출액_수출 1000  402101003


DATA :  BEGIN OF GT_FI_REV  OCCURS 0,
        MATNR  LIKE ACDOCA-MATNR,
        HSL   LIKE ACDOCA-HSL,
       END OF GT_FI_REV,
        GS_FI_REV LIKE GT_FI_REV.




" POSTING 을 위한 정의
DATA: GT_BK    LIKE ZFIS0050 OCCURS 1 WITH HEADER LINE,
      GT_BS    LIKE ZFIS0120 OCCURS 1 WITH HEADER LINE,
      GT_FIMSG LIKE FIMSG OCCURS 1 WITH HEADER LINE,
      GT_WI    LIKE ZFIS0070 OCCURS 1 WITH HEADER LINE,
      GT_TA    LIKE ZFIS0090  OCCURS 1 WITH HEADER LINE.


DATA : GS_IBKPF  LIKE  ZFIS0050,
       GT_IBKPF  LIKE TABLE OF ZFIS0050,
       GS_IBSEG  LIKE ZFIS0120,
       GT_IBSEG  LIKE TABLE OF ZFIS0120,
*       GT_FIMSG  LIKE TABLE OF ZFIS0080,
       GS_FIMSG  LIKE  ZFIS0080,
       GS_ISELK  LIKE  ZFIS0130,
       GT_ISELK  LIKE TABLE OF ZFIS0130,
       GS_IBSELP LIKE  ZFIS0140,
       GT_IBSELP LIKE TABLE OF ZFIS0140,
       GT_SELP   LIKE TABLE OF ZFIS0280,
       GS_SELP   LIKE ZFIS0280.
*----------------------------------------------------------------------*
* RANGES
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. R_  : Global, Local Ranges

* EX) RANGES: R_FKART  FOR VBRK-FKART,
*             GS_FKDAT LIKE LINE OF R_FKART.
RANGES : GR_GJAHR FOR ACDOCA-GJAHR,
         GR_BUDAT FOR ACDOCA-BUDAT,
         GR_RACCT FOR ACDOCA-RACCT,
         GR_POSID FOR ACDOCA-PS_POSID,
         GR_GSBER FOR T134G-GSBER.

*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. <FS_  >  : Global, Local Field-symbols

* EX) FIELD-SYMBOLS <FS_SFLIGHT> TYPE SFLIGHT.
FIELD-SYMBOLS <FS_DISP> LIKE GS_DISPLAY.
*----------------------------------------------------------------------*
* MACRO (Define)
*----------------------------------------------------------------------*
* - Prefix 정의
*   1. _ : '_' 1개로 시작; Global, Local 구분 없음.

DEFINE _SET_RANGES.
  &1-SIGN = &2.
  &1-OPTION = &3.
  &1-LOW = &4.
  &1-HIGH = &5.

  APPEND &1.
END-OF-DEFINITION.
*DEFINE _MAKE_ACDOCA_ITAB.
*  &1-SIGN = &2.
*  &1-OPTION = &3.
*  &1-LOW = &4.
*
*  APPEND &1.
*END-OF-DEFINITION.
DEFINE $_SET_REPLACE.
  IF &1 IS NOT INITIAL.
    REPLACE ALL OCCURRENCES OF &2 IN &1 WITH space.
    CONDENSE &1 NO-GAPS.
  ENDIF.
END-OF-DEFINITION.
DEFINE    _GET_LAST_DATE.
  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      DATE      = &1
      DAYS      = &2
      MONTHS    = &3
      YEARS     = &5
      SIGNUM    = &4         "'+'
    IMPORTING
      CALC_DATE = &6.
END-OF-DEFINITION.
DEFINE _INVDT_INPUT.
  CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
    EXPORTING
      INPUT  = &1
    IMPORTING
      OUTPUT = &2.
END-OF-DEFINITION.

DEFINE _MAKE_ACDOCA_ITAB_1260.
  CLEAR GS_ACDOCA.
  MOVE-CORRESPONDING GS_ZCOT1260 TO GS_ACDOCA.
  CLEAR GS_ACDOCA-WSL.
    GS_ACDOCA-GJAHR = &1.
    GS_ACDOCA-HSL = &2.
    GS_ACDOCA-CNT = &3.
    GS_ACDOCA-CDATE = GV_CDATE.
    COLLECT GS_ACDOCA INTO GT_ACDOCA.
END-OF-DEFINITION.


DEFINE _MAKE_ACDOCA_ITAB.
  CLEAR GS_ACDOCA.
  MOVE-CORRESPONDING LS_ACDOCA TO GS_ACDOCA.
  CLEAR GS_ACDOCA-WSL.
    GS_ACDOCA-GJAHR = &1.
    GS_ACDOCA-RACCT = &2.
    GS_ACDOCA-CNT = &3.
    GS_ACDOCA-CDATE = GV_CDATE.

    COLLECT GS_ACDOCA INTO GT_ACDOCA.

END-OF-DEFINITION.

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
