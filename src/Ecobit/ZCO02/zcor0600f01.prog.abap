*&---------------------------------------------------------------------*
*& Include          ZCOR0600F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION .

  GV_REPID = SY-REPID.

  " 기본값 : 관리회계영역
  P_KOKRS  = ZCL_CO_COMMON=>GET_DEFAULT_KOKRS( ).
  IF P_KOKRS IS INITIAL.
    P_KOKRS = '1000'.
  ENDIF.

  ZCL_CO_COMMON=>SET_KOKRS( P_KOKRS ).


  " 기본값 : 회사코드
  S_BUKRS-LOW = ZCL_CO_COMMON=>GET_DEFAULT_BUKRS( ).

  IF S_BUKRS-LOW IS NOT INITIAL.
    APPEND S_BUKRS.
  ENDIF.


  " Selection Screen 텍스트
  TEXT_S01 = '실행조건'(S01).
  TEXT_S02 = '선택조건'(S02).

  IF SY-TCODE EQ 'ZCOR0600'.
    GV_MODE  = GC_E.
    SY-TITLE = TEXT-T02.  " [CO] 설비 WBS 등록 현황(관리자)
  ELSE.
    GV_MODE  = GC_D.
    SY-TITLE = TEXT-T01.  " [CO] 설비 WBS 등록 현황
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCRFIELDS_FUNCTXT
*&---------------------------------------------------------------------*
FORM SCRFIELDS_FUNCTXT .

  CLEAR GS_FUNTXT.
  GS_FUNTXT-ICON_ID   = ICON_INFORMATION.
  GS_FUNTXT-QUICKINFO = 'Program Help'.

  SSCRFIELDS-FUNCTXT_01 = GS_FUNTXT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_SCR
*&---------------------------------------------------------------------*
FORM MODIFY_SCR .

  LOOP AT SCREEN.
    CASE SCREEN-NAME.
      WHEN 'P_KOKRS'.
        SCREEN-INPUT = 0.
    ENDCASE.

    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCR_USER_COMMAND
*&---------------------------------------------------------------------*
FORM SCR_USER_COMMAND .

  CASE SSCRFIELDS-UCOMM.
    WHEN 'FC01'.
      PERFORM CALL_POPUP_HELP(ZCAR9000) USING SY-REPID
                                              SY-DYNNR
                                              SY-LANGU ''.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_DATA_RTN
*&---------------------------------------------------------------------*
FORM SELECTED_DATA_RTN .

  ZCL_CO_COMMON=>SET_KOKRS( P_KOKRS ).

  PERFORM CLEAR_ITAB.
  PERFORM SELECT_WBS.
  PERFORM SELECT_COBRB.
  PERFORM SELECT_COSP.
  PERFORM SELECT_TEXT.

  PERFORM MAKE_DISPLAY_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CLEAR_ITAB
*&---------------------------------------------------------------------*
FORM CLEAR_ITAB .

  REFRESH: GT_DATA   ,
           GT_DISPLAY,
           GT_COBRB  ,
           GT_PRPS   ,
           GT_ANLA   ,
           GT_TGSBT  ,
           GT_1270T  ,
           GT_1280T  ,
           GT_1290T  ,
           GT_1300T  ,
           GT_ADRP   ,
           GT_OBJ_STATUS.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_WBS
*&---------------------------------------------------------------------*
FORM SELECT_WBS .

  SELECT A~PSPID,
         A~POST1     AS POST0,
         A~CPD_UPDAT AS UPDAT,
         B~*,
         C~PSTRT,
         C~PENDE

    FROM PROJ AS A
    JOIN PRPS AS B ON B~PSPHI EQ A~PSPNR
    LEFT
    JOIN PRTE AS C ON C~POSNR EQ B~PSPNR
   WHERE A~PROFL EQ 'Z000003'
     AND B~PKOKR EQ @P_KOKRS
     AND B~PBUKR IN @S_BUKRS
     AND B~ZZIZW IN @S_ZZIZW
     AND B~ZZCD1 IN @S_ZZCD1
     AND B~POSID IN @S_POSID
     AND B~ERDAT IN @S_ERDAT
     AND A~LOEVM EQ @SPACE
     AND B~LOEVM EQ @SPACE
    INTO CORRESPONDING FIELDS OF TABLE @GT_DATA.

  CHECK SY-SUBRC EQ 0.

  R_OBJNR = VALUE #( SIGN = 'I' OPTION = 'EQ' ).
  R_ERNAM = VALUE #( SIGN = 'I' OPTION = 'EQ' ).

  LOOP AT GT_DATA INTO GS_DATA.
    R_OBJNR-LOW = GS_DATA-OBJNR. APPEND R_OBJNR.
    R_ERNAM-LOW = GS_DATA-ERNAM. APPEND R_ERNAM.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM MAKE_DISPLAY_DATA .

  DATA LT_DISPLAY LIKE TABLE OF GS_DISPLAY.


  LOOP AT GT_DATA INTO GS_DATA.

    GS_DISPLAY = CORRESPONDING #( GS_DATA ).

    CALL FUNCTION 'ABI_TIMESTAMP_CONVERT_FROM'
      EXPORTING
        IV_TIMESTAMP     = GS_DATA-UPDAT    " UTC time stamp in short form (YYYYMMDDhhmmss)
      IMPORTING
        O_DATE           = GS_DISPLAY-AEDAT " Date and time, local date of user
        O_TIME           = GS_DISPLAY-AEZET " Date and time, local time for user
      EXCEPTIONS
        CONVERSION_ERROR = 1
        OTHERS           = 2.

    PERFORM SET_TEXT.
    PERFORM SET_STATUS.

    PERFORM CHECK_WBS_PLAN_ACTUAL.


    PERFORM SET_REMARK.

    PERFORM APPEND_DISPLAY.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_TOP_OF_PAGE_0100
*&---------------------------------------------------------------------*
FORM CREATE_TOP_OF_PAGE_0100 .

  IF GR_DDOC IS INITIAL.
    CREATE OBJECT GR_DDOC
      EXPORTING
        STYLE = 'ALV_GRID'.
  ENDIF.

  GR_DDOC->INITIALIZE_DOCUMENT( ).

  PERFORM WRITE_DOCUMENT.

  GR_DDOC->MERGE_DOCUMENT( ).
  GR_DDOC->DISPLAY_DOCUMENT(
    EXPORTING
      REUSE_CONTROL      = GC_X
      PARENT             = GR_CON_TOP  " Contain Object Already Exists
    EXCEPTIONS
      " Error Displaying the Document in the HTML Control
      HTML_DISPLAY_ERROR = 1
      OTHERS             = 2
  ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form WRITE_DOCUMENT
*&---------------------------------------------------------------------*
FORM WRITE_DOCUMENT .

*--------------------------------------------------------------------*
* Top of Page Layout 설정
*--------------------------------------------------------------------*
  DATA LR_TABLE     TYPE REF TO CL_DD_TABLE_ELEMENT.
  DATA LR_COL_L     TYPE REF TO CL_DD_AREA. " Left
  DATA LR_COL_R     TYPE REF TO CL_DD_AREA. " Right

  DATA LR_TABLE_B01 TYPE REF TO CL_DD_TABLE_ELEMENT.
  DATA LR_COL_B01_I TYPE REF TO CL_DD_AREA. " Icon
  DATA LR_COL_B01_L TYPE REF TO CL_DD_AREA. " Label
  DATA LR_COL_B01_C TYPE REF TO CL_DD_AREA. " Conditions

  DATA LR_TABLE_B02 TYPE REF TO CL_DD_TABLE_ELEMENT.
  DATA LR_COL_B02_I TYPE REF TO CL_DD_AREA. " Icon
  DATA LR_COL_B02_L TYPE REF TO CL_DD_AREA. " Label

  PERFORM ADD_TABLE  USING GR_DDOC  LR_TABLE '100%'  2.
  PERFORM ADD_COLUMN USING LR_TABLE LR_COL_L '400px'.
  PERFORM ADD_COLUMN USING LR_TABLE LR_COL_R '*'.

  PERFORM ADD_TABLE  USING LR_COL_L     LR_TABLE_B01 '100%'  3.
  PERFORM ADD_COLUMN USING LR_TABLE_B01 LR_COL_B01_I '30px'.
  PERFORM ADD_COLUMN USING LR_TABLE_B01 LR_COL_B01_L '100px'.
  PERFORM ADD_COLUMN USING LR_TABLE_B01 LR_COL_B01_C '*'.

  PERFORM ADD_TABLE  USING LR_COL_R     LR_TABLE_B02 '100%'  2.
  PERFORM ADD_COLUMN USING LR_TABLE_B02 LR_COL_B02_I '30px'.
  PERFORM ADD_COLUMN USING LR_TABLE_B02 LR_COL_B02_L '*'.
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Selection Screen Block [B01] : Parameter & Select Options
*--------------------------------------------------------------------*
  PERFORM WRITE_DOCUMENT_B01 USING LR_TABLE_B01
                                   LR_COL_B01_I
                                   LR_COL_B01_L
                                   LR_COL_B01_C.

*--------------------------------------------------------------------*
* Selection Screen Block [B02] : Checkbox
*--------------------------------------------------------------------*
  PERFORM WRITE_DOCUMENT_B02 USING LR_TABLE_B02
                                   LR_COL_B02_I
                                   LR_COL_B02_L.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADD_TABLE
*&---------------------------------------------------------------------*
FORM ADD_TABLE  USING PR_DDOC
                      PR_TABLE          TYPE REF TO CL_DD_TABLE_ELEMENT
                      VALUE(PV_WIDTH)   TYPE SDYDO_VALUE
                      VALUE(PV_NUMBER)  TYPE I.

  CHECK PR_DDOC IS BOUND AND PR_DDOC IS INSTANCE OF CL_DD_AREA.

  DATA LR_DDOC TYPE REF TO CL_DD_AREA.

  LR_DDOC = PR_DDOC.

  LR_DDOC->ADD_TABLE(
    EXPORTING
      " Number of Table Columns
      NO_OF_COLUMNS               = PV_NUMBER
      " Width of Table Frame; '0' = No Frame
      BORDER                      = '0'
      " Width of Table; '100%' = Entire Width of Control
      WIDTH                       = PV_WIDTH
    IMPORTING
      TABLE                       = PR_TABLE  " Table Element
*      TABLEAREA                   =          " Table Area
    EXCEPTIONS
      " Reference Variable for TABLE Already Used
      TABLE_ALREADY_USED          = 1
      OTHERS                      = 2
  ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADD_COLUMN
*&---------------------------------------------------------------------*
FORM ADD_COLUMN  USING PR_TABLE         TYPE REF TO CL_DD_TABLE_ELEMENT
                       PR_COLUMN        TYPE REF TO CL_DD_AREA
                       VALUE(PV_WIDTH)  TYPE SDYDO_VALUE.

  CHECK PR_TABLE IS BOUND.

  PR_TABLE->ADD_COLUMN(
    EXPORTING
      " Width of Column (Example '20%')
      WIDTH               = PV_WIDTH
    IMPORTING
      COLUMN              = PR_COLUMN  " Column Area
    EXCEPTIONS
      " Reference Variable for COLUMN has Already Been Used
      COLUMN_ALREADY_USED = 1
      OTHERS              = 2
  ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form WRITE_DOCUMENT_B01
*&---------------------------------------------------------------------*
FORM WRITE_DOCUMENT_B01 USING PR_TABLE TYPE REF TO CL_DD_TABLE_ELEMENT
                              PR_COL_I TYPE REF TO CL_DD_AREA
                              PR_COL_L TYPE REF TO CL_DD_AREA
                              PR_COL_C TYPE REF TO CL_DD_AREA.

  DATA LV_LABEL   TYPE TEXT255.
  DATA LV_CONDI   TYPE TEXT255.

*--------------------------------------------------------------------*
*  LV_LABEL = '관리회계영역'(L01).
*  LV_CONDI = P_KOKRS.
*  PR_COL_I->ADD_ICON( 'ICON_PARAMETER' ).
*  PR_COL_L->ADD_TEXT( TEXT = LV_LABEL ).
*  PR_COL_C->ADD_TEXT( TEXT = LV_CONDI ).
*--------------------------------------------------------------------*
  LV_LABEL = '회사코드'(L02).
  __ADD_SELOPT_TEXT LV_CONDI S_BUKRS.

  PR_COL_I->ADD_ICON( COND #( LET  LINES  = LINES( S_BUKRS ) IN
                              WHEN LINES LE 1
                              THEN 'ICON_ENTER_MORE'
                              ELSE 'ICON_DISPLAY_MORE' ) ).
  PR_COL_L->ADD_TEXT( TEXT = LV_LABEL ).
  PR_COL_C->ADD_TEXT( TEXT = LV_CONDI ).
*--------------------------------------------------------------------*
  LV_LABEL = '투자사유'(L03).
  __ADD_SELOPT_TEXT LV_CONDI S_ZZIZW.

  PR_TABLE->NEW_ROW( ).
  PR_COL_I->ADD_ICON( COND #( LET  LINES  = LINES( S_ZZIZW ) IN
                              WHEN LINES LE 1
                              THEN 'ICON_ENTER_MORE'
                              ELSE 'ICON_DISPLAY_MORE' ) ).
  PR_COL_L->ADD_TEXT( TEXT = LV_LABEL ).
  PR_COL_C->ADD_TEXT( TEXT = LV_CONDI ).
*--------------------------------------------------------------------*
  LV_LABEL = '설비대분류'(L04).
  __ADD_SELOPT_TEXT LV_CONDI S_ZZCD1.

  PR_TABLE->NEW_ROW( ).
  PR_COL_I->ADD_ICON( COND #( LET  LINES  = LINES( S_ZZIZW ) IN
                              WHEN LINES LE 1
                              THEN 'ICON_ENTER_MORE'
                              ELSE 'ICON_DISPLAY_MORE' ) ).
  PR_COL_L->ADD_TEXT( TEXT = LV_LABEL ).
  PR_COL_C->ADD_TEXT( TEXT = LV_CONDI ).
*--------------------------------------------------------------------*
  LV_LABEL = '설비WBS'(L05).
  __ADD_SELOPT_TEXT LV_CONDI S_POSID.

  PR_TABLE->NEW_ROW( ).
  PR_COL_I->ADD_ICON( COND #( LET  LINES  = LINES( S_POSID ) IN
                              WHEN LINES LE 1
                              THEN 'ICON_ENTER_MORE'
                              ELSE 'ICON_DISPLAY_MORE' ) ).
  PR_COL_L->ADD_TEXT( TEXT = LV_LABEL ).
  PR_COL_C->ADD_TEXT( TEXT = LV_CONDI ).
*--------------------------------------------------------------------*
  LV_LABEL = 'WBS생성일'(L06).
  __ADD_SELOPT_TEXT LV_CONDI S_ERDAT.

  PR_TABLE->NEW_ROW( ).
  PR_COL_I->ADD_ICON( COND #( LET  LINES  = LINES( S_ERDAT ) IN
                              WHEN LINES LE 1
                              THEN 'ICON_ENTER_MORE'
                              ELSE 'ICON_DISPLAY_MORE' ) ).
  PR_COL_L->ADD_TEXT( TEXT = LV_LABEL ).
  PR_COL_C->ADD_TEXT( TEXT = LV_CONDI ).
*--------------------------------------------------------------------*

ENDFORM.
*&---------------------------------------------------------------------*
*& Form WRITE_DOCUMENT_B02
*&---------------------------------------------------------------------*
FORM WRITE_DOCUMENT_B02 USING PR_TABLE TYPE REF TO CL_DD_TABLE_ELEMENT
                              PR_COL_I TYPE REF TO CL_DD_AREA
                              PR_COL_L TYPE REF TO CL_DD_AREA.

  DATA LV_LABEL   TYPE TEXT255.

*--------------------------------------------------------------------*
  LV_LABEL = '정산규칙 상세정보 조회'(L07).

  PR_COL_I->ADD_ICON( COND #( WHEN P_DETAIL EQ GC_X
                              THEN 'ICON_WD_RADIO_BUTTON'
                              ELSE 'ICON_WD_RADIO_BUTTON_EMPTY' ) ).
  PR_COL_L->ADD_TEXT( TEXT = LV_LABEL ).
*--------------------------------------------------------------------*

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_MAIN_GRID_0100
*&---------------------------------------------------------------------*
FORM CREATE_MAIN_GRID_0100 .

  IF GR_ALV IS NOT BOUND.
    GR_ALV = NEW #( GR_CON_ALV ).
  ENDIF.

  PERFORM MAKE_FIELDCATALOG_0100.
  PERFORM REGISTER_EVENT_0100.

  GR_ALV->SET_LAYOUT(
    I_TYPE       = 'B'
    I_STYLEFNAME = 'STYLE'
    I_CTAB_FNAME = 'COLOR'
  ).

  GR_ALV->SET_SORT( IT_FIELD = VALUE #( ( 'PBUKR' )
                                        ( 'PGSBR' )
                                        ( 'PSPID' )
                                        ( 'POST0' )
                                        ( 'POSID' )
                                        ) ).

  GR_ALV->MS_VARIANT-REPORT = SY-REPID.
  GR_ALV->MV_SAVE = 'A'.
  GR_ALV->DISPLAY( CHANGING T_OUTTAB = GT_DISPLAY ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_FIELDCATALOG_0100
*&---------------------------------------------------------------------*
FORM MAKE_FIELDCATALOG_0100 .

  GR_ALV->SET_FIELD_CATALOG(
    EXPORTING
      I_TABNAME               = 'GS_DISPLAY'
    EXCEPTIONS
      INVALID_INPUT_PARAMETER = 1
      EMPTY_FIELD_CATALOG     = 2
      OTHERS                  = 3
  ).

  IF SY-SUBRC <> 0.
    FREE GR_ALV.
    MESSAGE '필드카탈로그가 비어있습니다.' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE TO SCREEN 0.
  ENDIF.


  DATA LV_TEXT        TYPE TEXT100.
  DATA LV_TOOLTIP     TYPE TEXT100.
  DATA LV_KEY_FIX     TYPE C.

  LV_KEY_FIX = GC_X.

  LOOP AT GR_ALV->MT_FIELDCAT INTO DATA(LS_FIELDCAT).

    CLEAR LV_TEXT.
    CLEAR LV_TOOLTIP.
    CLEAR LS_FIELDCAT-KEY.

*-- 열최적화
*    LS_FIELDCAT-COL_OPT = GC_X.

*-- 열고정
    LS_FIELDCAT-KEY = LS_FIELDCAT-FIX_COLUMN = LV_KEY_FIX.


*-- Field 속성
    CASE LS_FIELDCAT-FIELDNAME.
      WHEN 'PBUKR'.
*        LS_FIELDCAT-JUST   = GC_C.
        LS_FIELDCAT-OUTPUTLEN = 5.

      WHEN 'PGSBR'.
*        LS_FIELDCAT-JUST   = GC_C.
        LS_FIELDCAT-NO_OUT = GC_X.
        LS_FIELDCAT-OUTPUTLEN = 10.

      WHEN 'GTEXT'.    " 사업영역명
        LS_FIELDCAT-NO_OUT = GC_X.
        LS_FIELDCAT-OUTPUTLEN = 25.

      WHEN 'PSPID'.
        LS_FIELDCAT-HOTSPOT = GC_X.
        LS_FIELDCAT-OUTPUTLEN = 11.

      WHEN 'POST0'.
        LS_FIELDCAT-OUTPUTLEN = 30.

      WHEN 'STUFE'.
*        LS_FIELDCAT-JUST   = GC_C.
        LS_FIELDCAT-NO_OUT = GC_X.
        LS_FIELDCAT-OUTPUTLEN = 4.

      WHEN 'POSID'.
        LS_FIELDCAT-HOTSPOT = GC_X.
        LS_FIELDCAT-OUTPUTLEN = 17.

      WHEN 'POST1'.
        CLEAR LV_KEY_FIX. " 처음 ~ 종료일 까지 틀고정
        LS_FIELDCAT-OUTPUTLEN = 40.

      WHEN 'PSTRT'.
        LS_FIELDCAT-NO_OUT = GC_X.
*        LS_FIELDCAT-JUST   = GC_C.

      WHEN 'PENDE'.
        LS_FIELDCAT-NO_OUT = GC_X.
*        LS_FIELDCAT-JUST   = GC_C.

      WHEN 'ZZIZW'.
*        LS_FIELDCAT-JUST   = GC_C.
        LS_FIELDCAT-OUTPUTLEN = 4.

      WHEN 'ZZIZWTX'.
        LS_FIELDCAT-NO_OUT = GC_X.
        LS_FIELDCAT-OUTPUTLEN = 15.

      WHEN 'ZZCD1'.
*        LS_FIELDCAT-JUST   = GC_C.
        LS_FIELDCAT-OUTPUTLEN = 4.

      WHEN 'ZZCD1TX'.
        LS_FIELDCAT-OUTPUTLEN = 9.

      WHEN 'ZZCD2'.
*        LS_FIELDCAT-JUST   = GC_C.
        LS_FIELDCAT-OUTPUTLEN = 4.

      WHEN 'ZZCD2TX'.
        LS_FIELDCAT-OUTPUTLEN = 9.

      WHEN 'ZZCD3'.
*        LS_FIELDCAT-JUST   = GC_C.
        LS_FIELDCAT-OUTPUTLEN = 4.

      WHEN 'ZZCD3TX'.
        LS_FIELDCAT-OUTPUTLEN = 9.

      WHEN 'ZZTCV'.
        LS_FIELDCAT-OUTPUTLEN = 12.
        LS_FIELDCAT-NO_SIGN = GC_X.

      WHEN 'ZZWAE'.
*        LS_FIELDCAT-JUST   = GC_C.
        LS_FIELDCAT-OUTPUTLEN = 4.

      WHEN 'ZZUNT'.
*        LS_FIELDCAT-JUST   = GC_C.
        LS_FIELDCAT-OUTPUTLEN = 4.

      WHEN 'ZZCMD'.
*        LS_FIELDCAT-JUST   = GC_C.
        LS_FIELDCAT-OUTPUTLEN = 10.

      WHEN 'ZZCPD'.
*        LS_FIELDCAT-JUST   = GC_C.
        LS_FIELDCAT-OUTPUTLEN = 10.

      WHEN 'ZZWBT'.
*        LS_FIELDCAT-JUST   = GC_C.
        LS_FIELDCAT-OUTPUTLEN = 4.

      WHEN 'ZZCYP'.
*        LS_FIELDCAT-JUST   = GC_C.
        LS_FIELDCAT-OUTPUTLEN = 4.

      WHEN 'KONTY'.
*        LS_FIELDCAT-JUST   = GC_C.
        LS_FIELDCAT-OUTPUTLEN = 8.

      WHEN 'EXTNR'.
*        LS_FIELDCAT-JUST   = GC_C.
        LS_FIELDCAT-OUTPUTLEN = 4.

        IF P_DETAIL IS INITIAL.
          LS_FIELDCAT-TECH = GC_X.
       	ENDIF.

      WHEN 'PROZS'.
        LS_FIELDCAT-NO_ZERO = GC_X.
        LS_FIELDCAT-NO_SIGN = GC_X.
        LS_FIELDCAT-OUTPUTLEN = 6.

        IF P_DETAIL IS INITIAL.
          LS_FIELDCAT-TECH = GC_X.
       	ENDIF.

      WHEN 'S_ANLN1'.
        LS_FIELDCAT-OUTPUTLEN = 8.

        IF P_DETAIL IS INITIAL.
          LS_FIELDCAT-TECH = GC_X.
       	ENDIF.

      WHEN 'S_TXT50'.
        LS_FIELDCAT-OUTPUTLEN = 20.

        IF P_DETAIL IS INITIAL.
          LS_FIELDCAT-TECH = GC_X.
       	ENDIF.

      WHEN 'S_POSID'.
        LS_FIELDCAT-OUTPUTLEN = 9.

        IF P_DETAIL IS INITIAL.
          LS_FIELDCAT-TECH = GC_X.
       	ENDIF.

      WHEN 'S_POST1'.
        LS_FIELDCAT-OUTPUTLEN = 20.

        IF P_DETAIL IS INITIAL.
          LS_FIELDCAT-TECH = GC_X.
       	ENDIF.

      WHEN 'STTXT'.
        LS_FIELDCAT-OUTPUTLEN = 8.


      WHEN 'FLG_PLN'.
*        LS_FIELDCAT-JUST   = GC_C.
        LS_FIELDCAT-OUTPUTLEN = 4.
*        LS_FIELDCAT-CHECKBOX = GC_X.

      WHEN 'FLG_ACT'.
*        LS_FIELDCAT-JUST   = GC_C.
        LS_FIELDCAT-OUTPUTLEN = 4.
*        LS_FIELDCAT-CHECKBOX = GC_X.

      WHEN 'ERDAT'.
*        LS_FIELDCAT-JUST   = GC_C.
        LS_FIELDCAT-NO_OUT = GC_X.

      WHEN 'ERNAM'.
        LS_FIELDCAT-NO_OUT = GC_X.

      WHEN 'ERNAMTX'. " 생성자명
        LS_FIELDCAT-NO_OUT = GC_X.

      WHEN 'AEDAT'.
        LS_FIELDCAT-NO_OUT = GC_X.
*        LS_FIELDCAT-JUST   = GC_C.

      WHEN 'AEZET'.
        LS_FIELDCAT-NO_OUT = GC_X.
*        LS_FIELDCAT-JUST   = GC_C.

      WHEN 'AENAM'.
        LS_FIELDCAT-NO_OUT = GC_X.

      WHEN 'AENAMTX'. " 수정자명
        LS_FIELDCAT-NO_OUT = GC_X.

      WHEN 'OBJNR'.
        LS_FIELDCAT-NO_OUT = GC_X.
*        LS_FIELDCAT-JUST   = GC_C.

      WHEN 'TDLINE'.
        LS_FIELDCAT-NO_OUT = GC_X.
        LS_FIELDCAT-OUTPUTLEN = 20.

      WHEN 'STYLE'
        OR 'COLOR'.
        LS_FIELDCAT-TECH = GC_X.

    ENDCASE.


*-- Field 텍스트
    CASE LS_FIELDCAT-FIELDNAME.
      WHEN 'PBUKR'.     LV_TEXT     = '회사'.
      WHEN 'PGSBR'.     LV_TEXT     = '사업영역'.
      WHEN 'GTEXT'.     LV_TEXT     = '사업영역명'.
      WHEN 'PSPID'.     LV_TEXT     = '프로젝트'.
      WHEN 'POST0'.     LV_TEXT     = '프로젝트명'.
      WHEN 'STUFE'.     LV_TEXT     = 'Lvl'.
                        LV_TOOLTIP  = '프로젝트 계층구조내 레벨'.
      WHEN 'POSID'.     LV_TEXT     = 'WBS'.
      WHEN 'POST1'.     LV_TEXT     = 'WBS명'.
      WHEN 'PSTRT'.     LV_TEXT     = '시작일'.
      WHEN 'PENDE'.     LV_TEXT     = '종료일'.

      WHEN 'ZZIZW'.     LV_TEXT     = '사유'.
                        LV_TOOLTIP  = '투자사유'.
      WHEN 'ZZIZWTX'.   LV_TEXT     = '투자사유명'.
      WHEN 'ZZCD1'.     LV_TEXT     = '대'.
                        LV_TOOLTIP  = '설비대분류'.
      WHEN 'ZZCD1TX'.   LV_TEXT     = '대분류명'.
                        LV_TOOLTIP  = '설비대분류명'.
      WHEN 'ZZCD2'.     LV_TEXT     = '중'.
                        LV_TOOLTIP  = '설비중분류'.
      WHEN 'ZZCD2TX'.   LV_TEXT     = '중분류명'.
                        LV_TOOLTIP  = '설비중분류명'.
      WHEN 'ZZCD3'.     LV_TEXT     = '소'.
                        LV_TOOLTIP  = '설비소분류'.
      WHEN 'ZZCD3TX'.   LV_TEXT     = '소분류명'.
                        LV_TOOLTIP  = '설비소분류명'.
      WHEN 'ZZTRD'.     LV_TEXT     = '거래처명'.
      WHEN 'ZZTCV'.     LV_TEXT     = '계약금액'.
      WHEN 'ZZWAE'.     LV_TEXT     = '통화'.
      WHEN 'ZZUNT'.     LV_TEXT     = '호기'.
      WHEN 'ZZCMD'.     LV_TEXT     = '공사착공일'.
      WHEN 'ZZCPD'.     LV_TEXT     = '공사준공일'.

      WHEN 'ZZWBT'.     LV_TEXT     = '유형'.
                        LV_TOOLTIP  = 'WBS유형'.
      WHEN 'ZZCYP'.     LV_TEXT     = '통제'.
                        LV_TOOLTIP  = '통제유형'.
      WHEN 'KONTY'.     LV_TEXT     = '정산규칙'.
                        LV_TOOLTIP  = '정산규칙( FXA:자산화 / WBS:비용화 )'.
      WHEN 'EXTNR'.     LV_TEXT     = 'No.'.
                        LV_TOOLTIP  = '정산규칙번호'.
      WHEN 'PROZS'.     LV_TEXT     = '비율'.
                        LV_TOOLTIP  = '배부비율'.
      WHEN 'S_ANLN1'.   LV_TEXT     = 'S.FXA'.
                        LV_TOOLTIP  = 'Sender 고정자산'.
      WHEN 'S_TXT50'.   LV_TEXT     = '정산FXA명'.
      WHEN 'S_POSID'.   LV_TEXT     = 'S.WBS'.
                        LV_TOOLTIP  = 'Sender WBS 요소'.
      WHEN 'S_POST1'.   LV_TEXT     = '정산WBS명'.
      WHEN 'STTXT'.     LV_TEXT     = '상태정보'.
      WHEN 'FLG_PLN'.   LV_TEXT     = '계획'.
      WHEN 'FLG_ACT'.   LV_TEXT     = '실적'.
      WHEN 'ERDAT'.     LV_TEXT     = '생성일자'.
      WHEN 'ERNAM'.     LV_TEXT     = '생성자'.
      WHEN 'ERNAMTX'.   LV_TEXT     = '생성자명'.
      WHEN 'AEDAT'.     LV_TEXT     = '최종수정일자(Prj)'.
      WHEN 'AEZET'.     LV_TEXT     = '최종수정시간(Prj)'.
      WHEN 'AENAM'.     LV_TEXT     = '수정자'.
      WHEN 'AENAMTX'.   LV_TEXT     = '수정자명'.
      WHEN 'OBJNR'.     LV_TEXT     = '오브젝트'.
      WHEN 'TDLINE'.    LV_TEXT     = '비고'.

    ENDCASE.

    IF LV_TEXT IS NOT INITIAL.
      LS_FIELDCAT-REPTEXT   = LV_TEXT.
      LS_FIELDCAT-COLTEXT   = LV_TEXT.
      LS_FIELDCAT-SCRTEXT_L = LV_TEXT.
      LS_FIELDCAT-SCRTEXT_M = LV_TEXT.
      LS_FIELDCAT-SCRTEXT_S = LV_TEXT.

      IF LV_TOOLTIP IS NOT INITIAL.
        LS_FIELDCAT-TOOLTIP = LV_TOOLTIP.
      ENDIF.
    ENDIF.


    MODIFY GR_ALV->MT_FIELDCAT FROM LS_FIELDCAT.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REGISTER_EVENT_0100
*&---------------------------------------------------------------------*
FORM REGISTER_EVENT_0100 .

*  GR_ALV->MR_ALV_GRID->REGISTER_EDIT_EVENT(
*    EXPORTING
*      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED " Event ID
*    EXCEPTIONS
*      ERROR      = 1                " Error
*      OTHERS     = 2
*  ).

  IF GR_EVENT_RECEIVER IS INITIAL.
    CREATE OBJECT GR_EVENT_RECEIVER.
  ENDIF.

  SET HANDLER:
    GR_EVENT_RECEIVER->ON_HOTSPOT_CLICK FOR GR_ALV->MR_ALV_GRID,
    GR_EVENT_RECEIVER->ON_TOOLBAR       FOR GR_ALV->MR_ALV_GRID,
    GR_EVENT_RECEIVER->ON_USER_COMMAND  FOR GR_ALV->MR_ALV_GRID.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM HANDLE_HOTSPOT_CLICK
  USING PS_ROW_ID     TYPE LVC_S_ROW
        PS_COLUMN_ID  TYPE LVC_S_COL
        PS_ROW_NO     TYPE LVC_S_ROID
        PR_SENDER     TYPE REF TO CL_GUI_ALV_GRID.


  DATA LT_SPAGPA TYPE RFC_T_SPAGPA.

  CASE PR_SENDER.
    WHEN GR_ALV->MR_ALV_GRID.

      READ TABLE GT_DISPLAY INTO GS_DISPLAY INDEX PS_ROW_ID-INDEX.
      CHECK SY-SUBRC EQ 0.

      ASSIGN COMPONENT PS_COLUMN_ID-FIELDNAME
          OF STRUCTURE GS_DISPLAY
          TO FIELD-SYMBOL(<FS_VALUE>).

      CHECK SY-SUBRC EQ 0 AND <FS_VALUE> IS NOT INITIAL.

      CASE PS_COLUMN_ID-FIELDNAME.
        WHEN 'PSPID'.
          LT_SPAGPA = VALUE #( ( PARID = 'PSP' PARVAL = <FS_VALUE> )
                               ( PARID = 'PRO' PARVAL = SPACE )
                               ( PARID = 'ANR' PARVAL = SPACE ) ).


        WHEN 'POSID'.
          LT_SPAGPA = VALUE #( ( PARID = 'PSP' PARVAL = SPACE )
                               ( PARID = 'PRO' PARVAL = <FS_VALUE> )
                               ( PARID = 'ANR' PARVAL = SPACE ) ).

        WHEN OTHERS. EXIT.
      ENDCASE.


      ZCL_CO_COMMON=>CALL_TRANSACTION(
        EXPORTING
          I_TCODE                 = 'CJ20N'   " Transaction Code
          I_SKIP_SCREEN           = GC_X      " Skip First Screen
          I_NEW_SESSION           = GC_X      " Call from New Session
          IT_SPAGPA               = LT_SPAGPA " Transaction Parameters
        EXCEPTIONS
          CALL_TRANSACTION_DENIED = 1         " No Authorization
          TCODE_INVALID           = 2
          UNKNOWN_EXCEPTION       = 3
          OTHERS                  = 4
      ).

      IF SY-SUBRC EQ 1.
        MESSAGE I077(S#) WITH 'CJ20N'.
      ENDIF.

*     IF GV_MODE EQ GC_E.
*       CALL TRANSACTION 'CJ02' AND SKIP FIRST SCREEN.
*     ELSE.
*       CALL TRANSACTION 'CJ03' AND SKIP FIRST SCREEN.
*     ENDIF.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_TEXT
*&---------------------------------------------------------------------*
FORM SELECT_TEXT.

  SELECT SINGLE *
    FROM TKA01
   WHERE KOKRS EQ @P_KOKRS
    INTO @GS_TKA01.

*-- 사업영역명
  SELECT *
    FROM TGSBT
   WHERE SPRAS EQ @SY-LANGU
    INTO CORRESPONDING FIELDS OF TABLE @GT_TGSBT.

*-- 투자사유 내역
  SELECT *
    FROM ZCOT1270T
   WHERE SPRAS EQ @SY-LANGU
    INTO CORRESPONDING FIELDS OF TABLE @GT_1270T.

*-- 설비대분류명
  SELECT *
    FROM ZCOT1280T
   WHERE SPRAS EQ @SY-LANGU
    INTO CORRESPONDING FIELDS OF TABLE @GT_1280T.

*-- 설비중분류명
  SELECT *
    FROM ZCOT1290T
   WHERE SPRAS EQ @SY-LANGU
    INTO CORRESPONDING FIELDS OF TABLE @GT_1290T.

*-- 설비중분류명
  SELECT *
    FROM ZCOT1300T
   WHERE SPRAS EQ @SY-LANGU
    INTO CORRESPONDING FIELDS OF TABLE @GT_1300T.


*-- 생성자명
  IF R_ERNAM[] IS NOT INITIAL.
    SELECT DISTINCT
           A~BNAME,
           B~DATE_FROM,
           B~NAME_TEXT
      FROM ADRP  AS B
      JOIN USR21 AS A ON A~PERSNUMBER EQ B~PERSNUMBER
     WHERE BNAME IN @R_ERNAM
      INTO TABLE @GT_ADRP.
  ENDIF.

*-- 오브젝트 상태 생성 ( I0001 ) 에 대한 텍스트 조회
  SELECT SINGLE TXT30
    FROM TJ02T
   WHERE ISTAT EQ @GC_I0001
     AND SPRAS EQ @SY-LANGU
    INTO @GV_I0001_TXT.

  IF R_OBJNR[] IS NOT INITIAL.

*-- 오브젝트 상태 텍스트 조회
    SELECT A~OBJNR,
           CASE A~STAT WHEN 'I0043' THEN @GC_0  " LKD
                       WHEN 'I0013' THEN @GC_1  " DLT
                       WHEN 'I0076' THEN @GC_2  " DLFL
                       WHEN 'I0002' THEN @GC_3  " REL
                       ELSE @GC_4 END,
           A~STAT,
           B~TXT30
      FROM JEST  AS A
      LEFT
      JOIN TJ02T AS B  ON B~ISTAT EQ A~STAT
                      AND B~SPRAS EQ @SY-LANGU
     WHERE A~OBJNR IN @R_OBJNR
       AND A~INACT EQ @SPACE
       AND A~STAT  NE 'I0028'
      INTO TABLE @GT_OBJ_STATUS.


*-- WBS 설명(Longtext) 헤더 조회
    RANGES LR_TDNAME FOR STXH-TDNAME.
    LR_TDNAME = VALUE #( SIGN = 'I' OPTION = 'EQ' ).
    LOOP AT R_OBJNR.
      LR_TDNAME-LOW = 'E' && R_OBJNR-LOW+2.
      APPEND LR_TDNAME.
    ENDLOOP.

    SELECT TDNAME,
           TDSPRAS
      FROM STXH
     WHERE TDOBJECT EQ 'PMS'
       AND TDNAME   IN @LR_TDNAME
       AND TDID     EQ 'LTXT'
      INTO TABLE @GT_STXH.
  ENDIF.



  SORT GT_TGSBT       BY GSBER.
  SORT GT_1270T       BY ZZIZW.
  SORT GT_1280T       BY ZZCD1.
  SORT GT_1290T       BY ZZCD1 ZZCD2.
  SORT GT_1300T       BY ZZCD1 ZZCD2 ZZCD3.
  SORT GT_ADRP        BY BNAME DATE_FROM DESCENDING.
  SORT GT_OBJ_STATUS  BY OBJNR RANK STAT DESCENDING.
  SORT GT_STXH        BY TDNAME.

  DELETE ADJACENT DUPLICATES FROM GT_ADRP COMPARING BNAME.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TEXT
*&---------------------------------------------------------------------*
FORM SET_TEXT .

  " 사업영역 텍스트
  IF GS_DISPLAY-PGSBR IS NOT INITIAL.
    READ TABLE GT_TGSBT INTO GS_TGSBT
                        WITH KEY GSBER = GS_DISPLAY-PGSBR
                                 BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GS_DISPLAY-GTEXT = GS_TGSBT-GTEXT.
    ENDIF.
  ENDIF.

  " 투자사유명
  IF GS_DISPLAY-ZZIZW IS NOT INITIAL.
    READ TABLE GT_1270T INTO GS_1270T
                        WITH KEY ZZIZW = GS_DISPLAY-ZZIZW
                                 BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GS_DISPLAY-ZZIZWTX = GS_1270T-ZZIZWTX.
    ENDIF.
  ENDIF.

  " 설비대분류명
  IF GS_DISPLAY-ZZCD1 IS NOT INITIAL.
    READ TABLE GT_1280T INTO GS_1280T
                        WITH KEY ZZCD1 = GS_DISPLAY-ZZCD1
                                 BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GS_DISPLAY-ZZCD1TX = GS_1280T-ZZCD1TX.
    ENDIF.

    " 설비중분류명
    IF GS_DISPLAY-ZZCD2 IS NOT INITIAL.
      READ TABLE GT_1290T INTO GS_1290T
                          WITH KEY ZZCD1 = GS_DISPLAY-ZZCD1
                                   ZZCD2 = GS_DISPLAY-ZZCD2
                                   BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        GS_DISPLAY-ZZCD2TX = GS_1290T-ZZCD2TX.
      ENDIF.

      " 설비소분류명
      IF GS_DISPLAY-ZZCD3 IS NOT INITIAL.
        READ TABLE GT_1300T INTO GS_1300T
                            WITH KEY ZZCD1 = GS_DISPLAY-ZZCD1
                                     ZZCD2 = GS_DISPLAY-ZZCD2
                                     ZZCD3 = GS_DISPLAY-ZZCD3
                                     BINARY SEARCH.
        IF SY-SUBRC EQ 0.
          GS_DISPLAY-ZZCD3TX = GS_1300T-ZZCD3TX.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.


  IF GS_DISPLAY-ERNAM IS NOT INITIAL.
    READ TABLE GT_ADRP INTO GS_ADRP
                       WITH KEY BNAME = GS_DISPLAY-ERNAM
                                BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GS_DISPLAY-ERNAMTX = GS_ADRP-NAME_TEXT.
    ENDIF.
  ENDIF.

  IF GS_DISPLAY-AENAM IS NOT INITIAL.
    READ TABLE GT_ADRP INTO GS_ADRP
                       WITH KEY BNAME = GS_DISPLAY-AENAM
                                BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GS_DISPLAY-AENAMTX = GS_ADRP-NAME_TEXT.
    ENDIF.
  ENDIF.




ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_COBRB
*&---------------------------------------------------------------------*
FORM SELECT_COBRB.

  REFRESH GT_COBRB.

  CHECK R_OBJNR[] IS NOT INITIAL.

  DATA LV_GJAHR TYPE GJAHR.
  DATA LV_POPER TYPE POPER.

  LV_GJAHR = SY-DATUM(4).
  LV_POPER = SY-DATUM+4(2).


  SELECT DISTINCT
         A~OBJNR,
         A~LFDNR,
         A~KONTY,
         A~EXTNR,
         A~PROZS,
         A~ANLN1,
         A~REC_OBJNR1

    FROM COBRB        AS A
   WHERE A~OBJNR      IN @R_OBJNR
*     AND A~PERBZ      EQ 'PER'      " 기간별 정산
     AND A~AVORG      EQ 'KOAO'     " 실제정산
*     AND B~ACCT_VRGNG EQ 'KABK'     " 정산계정지정
     AND ( A~GABJA LT @LV_GJAHR OR
         ( A~GABJA EQ @LV_GJAHR AND A~GABPE LE @LV_POPER ) )
     AND ( A~GBISJ EQ '0000'    OR
           A~GBISJ GT @LV_GJAHR OR
         ( A~GBISJ EQ @LV_GJAHR AND A~GBISP GE @LV_POPER ) )
    INTO TABLE @GT_COBRB.

  CHECK SY-SUBRC EQ 0.

  SORT GT_COBRB BY OBJNR EXTNR.


  RANGES: LR_OBJNR_PR FOR GS_PRPS-OBJNR.
  RANGES: LR_OBJNR_AN FOR GS_ANLA-OBJNR.

  LR_OBJNR_PR = VALUE #( SIGN = 'I' OPTION = 'EQ' ).
  LR_OBJNR_AN = VALUE #( SIGN = 'I' OPTION = 'EQ' ).

  LOOP AT GT_COBRB INTO GS_COBRB.
    CASE GS_COBRB-REC_OBJNR1(2).
      WHEN 'PR'.
        LR_OBJNR_PR-LOW = GS_COBRB-REC_OBJNR1.
        APPEND LR_OBJNR_PR.
      WHEN 'AN'.
        LR_OBJNR_AN-LOW = GS_COBRB-REC_OBJNR1.
        APPEND LR_OBJNR_AN.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.


  IF LR_OBJNR_PR[] IS NOT INITIAL.
    SELECT OBJNR, POSID, POST1
      FROM PRPS
     WHERE OBJNR IN @LR_OBJNR_PR
      INTO TABLE @GT_PRPS.

    SORT GT_PRPS BY OBJNR.
  ENDIF.

  IF LR_OBJNR_AN[] IS NOT INITIAL.
    SELECT OBJNR, ANLN1, TXT50
      FROM ANLA
     WHERE OBJNR IN @LR_OBJNR_AN
      INTO TABLE @GT_ANLA.

    SORT GT_ANLA BY OBJNR.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form APPEND_DISPLAY
*&---------------------------------------------------------------------*
FORM APPEND_DISPLAY.

  DATA LS_DISPLAY LIKE GS_DISPLAY.

*-- APPEND 하기 전에 정산규칙이 있는지 점검
  READ TABLE GT_COBRB TRANSPORTING NO FIELDS
                      WITH KEY OBJNR = GS_DATA-OBJNR
                               BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    LS_DISPLAY = GS_DISPLAY.

    LOOP AT GT_COBRB INTO GS_COBRB FROM SY-TABIX.
      IF GS_COBRB-OBJNR NE GS_DATA-OBJNR.
        EXIT.
      ENDIF.

      GS_DISPLAY = LS_DISPLAY.
      GS_DISPLAY-KONTY = GS_COBRB-KONTY.
      GS_DISPLAY-EXTNR = GS_COBRB-EXTNR.
      GS_DISPLAY-PROZS = GS_COBRB-PROZS.

      PERFORM SET_SETTLEMENT_RULE.

      APPEND GS_DISPLAY TO GT_DISPLAY.
      CLEAR  GS_DISPLAY.

      IF P_DETAIL IS INITIAL.
        EXIT.
      ENDIF.
    ENDLOOP.

    CHECK GS_DISPLAY IS NOT INITIAL.

  ENDIF.

  APPEND GS_DISPLAY TO GT_DISPLAY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SETTLEMENT_RULE
*&---------------------------------------------------------------------*
FORM SET_SETTLEMENT_RULE .

  "-- 정산규칙은 PR / AN 으로 구분
  CASE GS_COBRB-REC_OBJNR1(2).
    WHEN 'PR'.

      READ TABLE GT_PRPS INTO GS_PRPS
                         WITH KEY OBJNR = GS_COBRB-REC_OBJNR1
                                  BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        GS_DISPLAY-S_POSID = GS_PRPS-POSID.
        GS_DISPLAY-S_POST1 = GS_PRPS-POST1.
      ELSE.
        GS_DISPLAY-S_POSID = GS_COBRB-REC_OBJNR1.
      ENDIF.

    WHEN 'AN'.

      READ TABLE GT_ANLA INTO GS_ANLA
                         WITH KEY OBJNR = GS_COBRB-REC_OBJNR1
                                  BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        GS_DISPLAY-S_ANLN1 = GS_ANLA-ANLN1.
        GS_DISPLAY-S_TXT50 = GS_ANLA-TXT50.
      ELSE.
        GS_DISPLAY-S_ANLN1 = GS_COBRB-ANLN1.
      ENDIF.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_STATUS
*&---------------------------------------------------------------------*
FORM SET_STATUS .

  READ TABLE GT_OBJ_STATUS INTO GS_OBJ_STATUS
                           WITH KEY OBJNR = GS_DATA-OBJNR
                                    BINARY SEARCH.

  CHECK SY-SUBRC EQ 0.

  IF GS_OBJ_STATUS-RANK BETWEEN GC_0 AND GC_2.
    GS_DISPLAY-STTXT = GS_OBJ_STATUS-TXT30.
  ELSEIF GS_OBJ_STATUS-RANK EQ GC_3.

    ADD 1 TO SY-TABIX .
    READ TABLE GT_OBJ_STATUS INTO DATA(LS_OBJ_STATUS) INDEX SY-TABIX.

    IF SY-SUBRC EQ 0 AND GS_DATA-OBJNR EQ LS_OBJ_STATUS-OBJNR.
      GS_DISPLAY-STTXT = LS_OBJ_STATUS-TXT30.
    ELSE.
      GS_DISPLAY-STTXT = GS_OBJ_STATUS-TXT30.
    ENDIF.

  ELSE.
    GS_DISPLAY-STTXT = GV_I0001_TXT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_REMARK
*&---------------------------------------------------------------------*
FORM SET_REMARK .

  DATA LT_TLINE   LIKE TABLE OF TLINE WITH HEADER LINE.
  DATA LV_TDNAME  LIKE THEAD-TDNAME.

  LV_TDNAME = 'E' && GS_DATA-PSPNR.

  READ TABLE GT_STXH INTO GS_STXH
                     WITH KEY TDNAME  = LV_TDNAME
                              TDSPRAS = SY-LANGU
                              BINARY SEARCH.
  IF SY-SUBRC NE 0.
    READ TABLE GT_STXH INTO GS_STXH
                     WITH KEY TDNAME  = LV_TDNAME
                              BINARY SEARCH.
  ENDIF.

  CHECK SY-SUBRC EQ 0.


  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      ID                      = 'LTXT'          " 텍스트 ID
      LANGUAGE                = GS_STXH-TDSPRAS " 텍스트언어
      NAME                    = GS_STXH-TDNAME  " 텍스트이름
      OBJECT                  = 'PMS'           " 텍스트 오브젝트
    TABLES
      LINES                   = LT_TLINE        " 텍스트라인
    EXCEPTIONS
      ID                      = 1 " 부적절한 텍스트 ID
      LANGUAGE                = 2 " 부적절한 언어
      NAME                    = 3 " 부적절한 텍스트이름
      NOT_FOUND               = 4 " 텍스트를 찾지 못했습니다
      OBJECT                  = 5 " 부적절한 텍스트 오브젝트
      REFERENCE_CHECK         = 6 " 참조체인을 인터럽트했습니다
      WRONG_ACCESS_TO_ARCHIVE = 7 " 접근에 대해 부적절한 아카이브조정
      OTHERS                  = 8.

  READ TABLE LT_TLINE INDEX 1.
  CHECK SY-SUBRC EQ 0.

  GS_DISPLAY-TDLINE = LT_TLINE-TDLINE.

  LOOP AT LT_TLINE FROM 2.
    CONCATENATE GS_DISPLAY-TDLINE LT_TLINE
           INTO GS_DISPLAY-TDLINE SEPARATED BY SPACE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
FORM HANDLE_TOOLBAR
  USING PR_OBJECT TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET
        PR_SENDER TYPE REF TO CL_GUI_ALV_GRID.


  CASE PR_SENDER.
    WHEN GR_ALV->MR_ALV_GRID.

      DATA LT_TOOLBAR LIKE PR_OBJECT->MT_TOOLBAR.

      LT_TOOLBAR = VALUE #(
        ( BUTN_TYPE = 3 )
        ( FUNCTION = 'COSTPLAN' TEXT = '비용계획' )
        ( BUTN_TYPE = 3 )
        ( FUNCTION = 'COSTACT'  TEXT = '비용계획 vs 실적비교' )
        ( BUTN_TYPE = 3 )
        ( FUNCTION = 'HISTORY'  TEXT = '정보변경이력' )
      ).

      APPEND LINES OF LT_TOOLBAR TO PR_OBJECT->MT_TOOLBAR.


  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
FORM HANDLE_USER_COMMAND  USING PV_UCOMM
                                PR_SENDER TYPE REF TO CL_GUI_ALV_GRID.

  DATA LT_ROWS TYPE LVC_T_ROID.


  CASE PR_SENDER.
    WHEN GR_ALV->MR_ALV_GRID.

      CASE PV_UCOMM.
        WHEN 'COSTPLAN'
          OR 'COSTACT'
          OR 'HISTORY'.

          CASE GR_ALV->GET_SELECTED_ROWS( IMPORTING ET_ROWS = LT_ROWS ).
            WHEN 0.
              " 선택된 Row가 없습니다.
              MESSAGE I015.
            WHEN 1.
              READ TABLE GT_DISPLAY INTO GS_DISPLAY INDEX LT_ROWS[ 1 ]-ROW_ID.


              CASE PV_UCOMM.
                WHEN 'COSTPLAN'.
                  PERFORM BDC_INIT.
                  PERFORM BDC_DATA.
                  PERFORM BDC_EXECUTE.

                WHEN 'COSTACT'.
                  TRY.

                    EXPORT POSID FROM GS_DISPLAY-POSID
                           ZZCMD FROM GS_DISPLAY-ZZCMD
                        TO MEMORY ID 'ZCOR0600'.

                    SET PARAMETER ID 'BUK' FIELD GS_DISPLAY-PBUKR.
*                    SET PARAMETER ID 'PRO' FIELD GS_DISPLAY-POSID.
*                    SET PARAMETER ID 'GJR' FIELD SY-DATUM(4).

                    CALL TRANSACTION 'ZCOR0610' WITHOUT AUTHORITY-CHECK.

                  CATCH CX_SY_AUTHORIZATION_ERROR INTO DATA(LX_AUTH).
                    DATA(LV_MESSGAE) = LX_AUTH->GET_TEXT( ).
                    MESSAGE S000 DISPLAY LIKE GC_E WITH LV_MESSGAE.
                  ENDTRY.

                WHEN 'HISTORY'.
                  TRY.
                    SET PARAMETER ID 'PDB' FIELD '000000000001'.
                    SET PARAMETER ID 'PSP' FIELD SPACE.
                    SET PARAMETER ID 'PRO' FIELD GS_DISPLAY-POSID.
                    SET PARAMETER ID 'ANR' FIELD SPACE.
                    SET PARAMETER ID 'VGN' FIELD SPACE.
                    CALL TRANSACTION 'CN60' WITHOUT AUTHORITY-CHECK.

                  CATCH CX_SY_AUTHORIZATION_ERROR INTO LX_AUTH.
                    LV_MESSGAE = LX_AUTH->GET_TEXT( ).
                    MESSAGE S000 DISPLAY LIKE GC_E WITH LV_MESSGAE.
                  ENDTRY.

              ENDCASE.

            WHEN OTHERS.
              " 한 Row만 선택 하십시오.
              MESSAGE I016.
          ENDCASE.

      ENDCASE.


  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BDC_INIT
*&---------------------------------------------------------------------*
FORM BDC_INIT .

  CLEAR: GT_BDC_DATA, GT_BDC_DATA[],
         GS_BDC_DATA,
         GT_BDC_MSG, GT_BDC_MSG[],
         GS_BDC_MSG,
         GS_BDC_OPT,
         GV_BDC_CHK.

  GS_BDC_OPT = VALUE #(
    DISMODE = GC_E
    UPDMODE = GC_A
  ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BDC_DATA
*&---------------------------------------------------------------------*
FORM BDC_DATA.

  GT_BDC_DATA = VALUE #(
    ( PROGRAM = 'SAPLKPP0'          DYNPRO   = '1000'
                                    DYNBEGIN = GC_X         )
    (    FNAM = 'BDC_OKCODE'        FVAL     = '/00'        )
    (    FNAM = 'KPP1B-ONLY'        FVAL     = GC_X         )
    (    FNAM = 'KPP0B-VALUE(01)'   FVAL     = 'E1'         )
    (    FNAM = 'KPP0B-VALUE(02)'   FVAL     = '001'        )
    (    FNAM = 'KPP0B-VALUE(03)'   FVAL     = '012'        )
    (    FNAM = 'KPP0B-VALUE(04)'   FVAL     = SY-DATUM(4)  )
    (    FNAM = 'KPP0B-VALUE(06)'   FVAL     = GS_DISPLAY-POSID )
    (    FNAM = 'KPP0B-VALUE(07)'   FVAL     = SPACE        )
    (    FNAM = 'KPP0B-VALUE(08)'   FVAL     = SPACE        )
    (    FNAM = 'KPP0B-VALUE(09)'   FVAL     = '*'          )
    (    FNAM = 'KPP0B-VALUE(10)'   FVAL     = SPACE        )
    (    FNAM = 'KPP0B-VALUE(11)'   FVAL     = SPACE        )
 ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BDC_EXECUTE
*&---------------------------------------------------------------------*
FORM BDC_EXECUTE.

  TRY.

    IF GV_MODE EQ GC_E.
      CALL TRANSACTION 'CJR2' WITH AUTHORITY-CHECK
                              USING GT_BDC_DATA
                              OPTIONS FROM GS_BDC_OPT.
    ELSE.
      CALL TRANSACTION 'CJR3' WITHOUT AUTHORITY-CHECK
                              USING GT_BDC_DATA
                              OPTIONS FROM GS_BDC_OPT.
    ENDIF.

  CATCH CX_SY_AUTHORIZATION_ERROR INTO DATA(LX_AUTH).
    DATA(LV_MESSGAE) = LX_AUTH->GET_TEXT( ).
    MESSAGE S000 DISPLAY LIKE GC_E WITH LV_MESSGAE.
  ENDTRY.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_COSP
*&---------------------------------------------------------------------*
FORM SELECT_COSP .

  CHECK R_OBJNR[] IS NOT INITIAL.

  SELECT DISTINCT
         OBJNR,
         CASE WRTTP WHEN '01' THEN '01' ELSE '04' END AS WRTTP
    FROM COSP
   WHERE LEDNR EQ '00'
     AND OBJNR IN @R_OBJNR
         " 01: 계획 / 04: 실적 / 60: 임시전표
     AND ( ( VRGNG IN ('COIN','RKP1') AND ( WRTTP EQ '01'
                                       OR ( WRTTP IN ('04','60')
                                        AND VERSN EQ '000' ) ) )
         " 구매약정
        OR ( VRGNG IN ('RMBE','RMBA')
         AND WRTTP IN ('21','22')
         AND VERSN EQ '000' ) )

     AND BEKNZ EQ 'S'
    INTO TABLE @GT_COSP.

  CHECK SY-SUBRC EQ 0.


  SORT GT_COSP BY OBJNR
                  WRTTP.

  DELETE ADJACENT DUPLICATES FROM GT_COSP COMPARING OBJNR WRTTP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_WBS_PLAN_ACTUAL
*&---------------------------------------------------------------------*
FORM CHECK_WBS_PLAN_ACTUAL .

  READ TABLE GT_COSP TRANSPORTING NO FIELDS
                     WITH KEY OBJNR = GS_DISPLAY-OBJNR
                              BINARY SEARCH.

  CHECK SY-SUBRC EQ 0.


  LOOP AT GT_COSP INTO GS_COSP FROM SY-TABIX.

    IF GS_COSP-OBJNR NE GS_DISPLAY-OBJNR.
      EXIT.
    ENDIF.

    IF GS_COSP-WRTTP EQ '01'.
      GS_DISPLAY-FLG_PLN = GC_X.
    ELSE.
      GS_DISPLAY-FLG_ACT = GC_X.
    ENDIF.

  ENDLOOP.


ENDFORM.
