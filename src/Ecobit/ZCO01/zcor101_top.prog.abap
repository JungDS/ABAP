*&---------------------------------------------------------------------*
*& Include ZCOR101_TOP                              - Report ZCOR101
*&---------------------------------------------------------------------*

* Tables
TABLES: PROJ,
        PRPS,
        SSCRFIELDS.

* TYPE-POOLS
TYPE-POOLS CO1.

*Local class
CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.

* Fieldcat Merge Internal Table
DATA: BEGIN OF GT_FCTAB OCCURS 0,
        MARK(1),
        ICON     LIKE ICON-ID,       "처리상태
        PSPID    LIKE PROJ-PSPID,    "프로젝트
        PRPOST1  LIKE PROJ-POST1,    "프로젝트 내역
        PROFL    LIKE PROJ-PROFL,    "PRJ.PRO
        VBUKR    LIKE PROJ-VBUKR,    "회사코드
        VGSBR    LIKE PROJ-VGSBR,    "사업영역
        VPRCTR   LIKE PROJ-PRCTR,    "손익센터
        BESTA    LIKE PROJ-BESTA,    "프로젝트 재고
        POSID    LIKE PRPS-POSID,    "WBS 요소
        WBSPOST1 LIKE PRPS-POST1,    "WBS 내역
        PBUKR    LIKE PRPS-PBUKR,    "회사코드
        PPRCTR   LIKE PRPS-PRCTR,    "손익센터
        PGSBR    LIKE PRPS-PGSBR,    "사업영역
        GTEXT    LIKE TGSBT-GTEXT,   "사업영역 내역
        WERKS    LIKE PROJ-WERKS,    "플랜트
        NAME1    LIKE T001W-NAME1,   "플랜트 내역
        PSTRT    LIKE PRTE-PSTRT,    "시작일자
        PENDE    LIKE PRTE-PENDE,    "종료일자
        STUFE    LIKE PRPS-STUFE,    "레벨
        PLINT    LIKE PRPS-PLINT,    "통합계획
        PLAKZ    LIKE PRPS-PLAKZ,    "계획요소
        BELKZ    LIKE PRPS-BELKZ,    "계정지정 요소
        FAKKZ    LIKE PRPS-FAKKZ,    "대금청구요소
        CLASF    LIKE PRPS-CLASF,    "프로젝트요약
        ABGSL    LIKE PRPS-ABGSL,    "RA_KEY
        ZZSCT	   LIKE PRPS-ZZSCT,    "매출유형
        ZZPHA    LIKE PRPS-ZZPHA,    "프로젝트단계
        ZZWBT    LIKE PRPS-ZZWBT,    "WBS 유형
        ZZBGU	   LIKE PRPS-ZZBGU,    "사업구분
        ZZBGD	   LIKE PRPS-ZZBGD,    "사업구분상세
        ZZPRG    LIKE PRPS-ZZPRG,    "발주처유형
        ZZADT    LIKE PRPS-ZZADT,    "행정구역
        ZZHWB    LIKE PRPS-ZZHWB,    "Huvis.W 하위 본부
        ZZBAG    LIKE PRPS-ZZBAG,    "사업소유무
        ZZIVC    LIKE PRPS-ZZIVC,    "투자유무
        ZZCOP    LIKE PRPS-ZZCOP,    "수주유형
        ZZCYP    LIKE PRPS-ZZCYP,    "통제유형
        MESSAGE  TYPE STRING,        "경고내용
        CELLTAB  TYPE LVC_T_STYL,    "EDIT 여부
      END OF GT_FCTAB.

* ALV Output Internal Table
DATA: GS_OUTTAB LIKE GT_FCTAB,
      GT_OUTTAB LIKE TABLE OF GT_FCTAB.
* Internal Table
DATA: GS_TEMP   LIKE GT_FCTAB,
      GT_TEMP   LIKE TABLE OF GT_FCTAB.
DATA: BEGIN OF GS_FLAG,
        PSPID(1),    "프로젝트
        PRPOST1(1),  "프로젝트 내역
        PROFL(1),    "PRJ.PRO
        VBUKR(1),    "회사코드
        VGSBR(1),    "사업영역
        VPRCTR(1),   "손익센터
        BESTA(1),    "프로젝트 재고
        POSID(1),    "WBS 요소
        WBSPOST1(1), "WBS 내역
        PBUKR(1),    "회사코드
        PPRCTR(1),   "손익센터
        PGSBR(1),    "사업영역
        GTEXT(1),    "사업영역 내역
        WERKS(1),    "플랜트
        NAME1(1),    "플랜트 내역
        PSTRT(1),    "시작일자
        PENDE(1),    "종료일자
        STUFE(1),    "레벨
        PLINT(1),    "통합계획
        PLAKZ(1),    "계획요소
        BELKZ(1),    "계정지정 요소
        FAKKZ(1),    "대금청구요소
        CLASF(1),    "프로젝트요약
        ABGSL(1),    "RA_KEY
        ZZSCT(1),    "매출유형
        ZZPHA(1),    "프로젝트단계
        ZZWBT(1),    "WBS 유형
        ZZBGU(1),    "사업구분
        ZZBGD(1),    "사업구분상세
        ZZPRG(1),    "발주처유형
        ZZADT(1),    "행정구역
        ZZHWB(1),    "Huvis.W 하위 본부
        ZZBAG(1),    "사업소유무
        ZZIVC(1),    "투자유무
        ZZCOP(1),    "수주유형
        ZZCYP(1),    "통제유형
      END OF GS_FLAG,
      GT_FLAG LIKE TABLE OF GS_FLAG.

* ALV component
DATA: GO_CONTAINER TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      GO_GRID      TYPE REF TO CL_GUI_ALV_GRID,
      GO_EVENT     TYPE REF TO LCL_EVENT_RECEIVER,
      GS_LAYOUT    TYPE LVC_S_LAYO,
      GS_VARIANT   TYPE DISVARIANT,
      GS_FCAT      TYPE LVC_S_FCAT,
      GT_FCAT      TYPE LVC_T_FCAT,
      GT_EXCLUDE   TYPE UI_FUNCTIONS,
      GS_STBL      TYPE LVC_S_STBL.

DATA: GS_ROW    TYPE LVC_S_ROW,
      GT_ROW    TYPE LVC_T_ROW,
      GV_OKCODE TYPE SY-UCOMM.

* VARIABLE
DATA: GV_EDIT(1),
      GV_PROJ(1),
      GV_WBS(1).

* Function Key
DATA: G_FUNCTION_KEY TYPE SMP_DYNTXT.

* Excel Layout download
DATA: GT_UPLOAD LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE,
      GS_KEY    LIKE WWWDATATAB.

* Project / WBS 정보
DATA: GS_PROJ       TYPE BAPI_PROJECT_DEFINITION,
      GS_PROJDEF_UP TYPE BAPI_PROJECT_DEFINITION_UP,
      GT_METHPROJ   TYPE TABLE OF BAPI_METHOD_PROJECT WITH HEADER LINE,
      GT_WBS_TAB_UP TYPE BAPI_WBS_ELEMENT_UPDATE OCCURS 0 WITH HEADER LINE,
      GT_WBS_TAB    TYPE BAPI_WBS_ELEMENT OCCURS 0 WITH HEADER LINE,
      GS_RETURN	    TYPE BAPIRETURN1,
      GT_MSGTAB	    TYPE BAPI_METH_MESSAGE OCCURS 0 WITH HEADER LINE.

* CONSTANTS
CONSTANTS: GC_E         TYPE C VALUE 'E',
           GC_N         TYPE C VALUE 'N',
           GC_S         TYPE C VALUE 'S',
           GC_X         TYPE C VALUE 'X',

           GC_REFNO     TYPE BAPI_METHOD_PROJECT-REFNUMBER VALUE '000001',
           GC_PROJFNAME TYPE STRING VALUE 'PROJECT_DEFINITION',
           GC_WBSFNAME  TYPE STRING VALUE 'WBS_ELEMENT'.
