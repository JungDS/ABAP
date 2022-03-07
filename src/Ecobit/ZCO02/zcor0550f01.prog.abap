*&---------------------------------------------------------------------*
*& Include          ZCOR0550F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION .

  GV_REPID = SY-REPID.
  P_KOKRS  = ZCL_CO_COMMON=>GET_DEFAULT_KOKRS( ).
  IF P_KOKRS IS INITIAL.
    P_KOKRS = '1000'.
  ENDIF.

*  S_BUKRS      = 'IEQ'.
  S_BUKRS-LOW  = ZCL_CO_COMMON=>GET_DEFAULT_BUKRS( ).
  IF S_BUKRS-LOW IS INITIAL.
    S_BUKRS-LOW = '1100'.
  ENDIF.
  APPEND S_BUKRS.

  " 1년간 실적있는 WBS 만 수행되는 체크박스를 기본값으로 설정.
  P_ACTUAL = GC_X.
  P_BDC    = GC_N.

  TEXT_S01 = '실행조건'(S01).
  TEXT_S02 = '선택조건'(S02).

  GV_CHK_YELLOW = GV_CHK_GREEN = GV_CHK_RED = GC_X.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCRFIELDS_FUNCTXT
*&---------------------------------------------------------------------*
FORM SCRFIELDS_FUNCTXT .

  CLEAR GS_FUNTXT.
  GS_FUNTXT-ICON_ID   = ICON_INFORMATION.
  GS_FUNTXT-QUICKINFO = 'Program Help'.

  SSCRFIELDS-FUNCTXT_01 = GS_FUNTXT.

  CLEAR GS_FUNTXT.
  GS_FUNTXT-TEXT      = 'BU 매핑조회(WBS)'.
  SSCRFIELDS-FUNCTXT_02 = GS_FUNTXT.

  CLEAR GS_FUNTXT.
  GS_FUNTXT-TEXT      = 'BU 매핑조회(속성)'.
  SSCRFIELDS-FUNCTXT_03 = GS_FUNTXT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_SCR
*&---------------------------------------------------------------------*
FORM MODIFY_SCR .

  LOOP AT SCREEN.

    IF SCREEN-NAME EQ 'P_KOKRS'.
      SCREEN-INPUT = 0.
    ENDIF.

    IF SCREEN-GROUP1 EQ 'BDC'.
*      IF SY-SYSID NE 'TCD'.
        SCREEN-ACTIVE = 0.
*      ENDIF.
    ENDIF.

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

    WHEN 'FC02'.
*      CALL TRANSACTION 'ZCOR0540'.
      PERFORM SHOW_WBS_MAPPING.

    WHEN 'FC03'.
      PERFORM SHOW_WBS_ATTR_MAPPING.


    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_DATA_RTN
*&---------------------------------------------------------------------*
FORM SELECTED_DATA_RTN .

  SET PARAMETER ID 'BUK' FIELD S_BUKRS-LOW.

  PERFORM CLEAR_ITAB.
  PERFORM SELECT_WBS.
  PERFORM SELECT_COBRB USING GT_DATA.
  PERFORM SELECT_JEST  USING GT_DATA.
  PERFORM SELECT_OTHERS.

  PERFORM MAKE_DISPLAY_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CLEAR_ITAB
*&---------------------------------------------------------------------*
FORM CLEAR_ITAB .

  _CLEAR_ITAB : GT_DATA,
                GT_DISPLAY,
                GT_COBRB,
                GT_JEST,
                GT_T2501,
*                GT_T001,
*                GT_1040,
                GT_1050,
*                GT_1100,
                GT_CHECK.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_WBS
*&---------------------------------------------------------------------*
FORM SELECT_WBS .


  " WBS 정보 추출

  SELECT A~PSPNR,
         B~PSPNR,
         B~POSID,
         B~POST1,
         B~OBJNR,
         B~PBUKR,
         B~PKOKR,
         B~ZZBGU,
         B~ZZBGD,
         B~ZZPRG
    FROM PROJ AS A
    JOIN PRPS AS B ON B~PSPHI EQ A~PSPNR
   WHERE A~PROFL NE 'Z000003'
     AND A~XSTAT IS INITIAL
     AND A~LOEVM IS INITIAL
     AND B~PKOKR EQ @P_KOKRS
     AND B~PBUKR IN @S_BUKRS
     AND B~POSID IN @S_POSID
     AND B~LOEVM IS INITIAL
    INTO TABLE @GT_DATA.

  CHECK SY-SUBRC EQ 0 AND GT_DATA[] IS NOT INITIAL.


  " 선택조건 기준 실적 발생 점검(선택조건 선택의 경우)

  IF P_ACTUAL EQ GC_X.
    DATA LR_GJAHR TYPE RANGE OF COSP-GJAHR.

    LR_GJAHR = VALUE #(
      ( SIGN   = 'I'
        OPTION = 'GE'
        LOW    = SY-DATUM(4) - 1
      )
    ).

    SELECT DISTINCT OBJNR
      FROM COSP
     WHERE LEDNR EQ '00'
       AND OBJNR LIKE 'PR%'       " WBS Element 만
       AND GJAHR IN @LR_GJAHR
       AND WRTTP EQ '04'          " 실제만 취급
      INTO TABLE @DATA(LT_COSP).

    SORT LT_COSP BY OBJNR.

    LOOP AT GT_DATA INTO GS_DATA.
      READ TABLE LT_COSP TRANSPORTING NO FIELDS
                         WITH KEY OBJNR = GS_DATA-OBJNR
                                  BINARY SEARCH.
      CHECK SY-SUBRC NE 0.
      DELETE GT_DATA.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_COBRB
*&---------------------------------------------------------------------*
FORM SELECT_COBRB USING PT_DATA LIKE GT_DATA.

  " 기등록 정산규칙 정보 추출

  DATA LR_OBJNR LIKE RANGE OF COBRB-OBJNR WITH HEADER LINE.

  LR_OBJNR[] = CORRESPONDING #( PT_DATA[] MAPPING LOW = OBJNR ).
  LR_OBJNR = 'IEQ'.
  MODIFY LR_OBJNR TRANSPORTING SIGN OPTION WHERE SIGN IS INITIAL.


  REFRESH GT_COBRB.

  CHECK LR_OBJNR[] IS NOT INITIAL.

  SELECT DISTINCT
          A~OBJNR,
          A~LFDNR,
          A~GABJA,
          A~GABPE,
          A~GBISJ,
          A~GBISP,
          A~PROZS,
          A~LETJA,
          A~LETPE,
          A~AVORG,
          A~VERSN,
          B~ACCT_VRGNG,
          B~WW040,
          B~WW050,
          B~WW100,
          B~WW120
    FROM COBRB        AS A
    JOIN CE41000_ACCT AS B ON B~PAOBJNR EQ A~PAOBJNR  " PSG 정보
   WHERE A~OBJNR      IN @LR_OBJNR
     AND A~PERBZ      EQ 'PER'      " 기간별 정산
*     AND A~AVORG      EQ 'KOAO'     " 실제정산
     AND B~ACCT_VRGNG IN ('KABK',   " 정산계정지정
                          'KABP')   " 계획정산 계정지정
    INTO TABLE @GT_COBRB.

  SORT GT_COBRB BY OBJNR
                   AVORG  " 정산트랜잭션( 실제: KOAO / 계획: KOAP )
                   VERSN  " 버전
                   LFDNR
                   VRGNG.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_OTHERS
*&---------------------------------------------------------------------*
FORM SELECT_OTHERS.

*  SELECT FROM ZCOT1040  AS A
*    LEFT JOIN ZCOT1040T AS B  ON  B~SPRAS EQ @SY-LANGU
*                              AND B~ZZBGU EQ A~ZZBGU
*         FIELDS   A~ZZBGU, B~ZZBGUTX
*         ORDER BY A~ZZBGU
*         INTO TABLE @GT_1040 .


  SELECT FROM ZCOT1050  AS A
    LEFT JOIN ZCOT1050T AS B  ON  B~SPRAS EQ @SY-LANGU
                              AND B~ZZBGU EQ A~ZZBGU
                              AND B~ZZBGD EQ A~ZZBGD
         FIELDS   A~ZZBGU, A~ZZBGD, B~ZZBGDTX
         ORDER BY A~ZZBGU, A~ZZBGD
         INTO TABLE @GT_1050 .

*  SELECT FROM ZCOT1100  AS A
*    LEFT JOIN ZCOT1100T AS B  ON  B~SPRAS EQ @SY-LANGU
*                              AND B~ZZPRG EQ A~ZZPRG
*         FIELDS   A~ZZPRG, B~ZZPRGTX
*         ORDER BY A~ZZPRG
*         INTO TABLE @GT_1100.

  SELECT FROM T2501  AS A
    LEFT JOIN T25A1  AS B  ON  B~SPRAS EQ @SY-LANGU
                           AND B~WW120 EQ A~WW120
         FIELDS   A~WW120, B~BEZEK
         ORDER BY A~WW120
         INTO TABLE @GT_T2501.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM MAKE_DISPLAY_DATA .

  " WBS 정산규칙에서 마지막 라인을 기준으로 정렬
  SORT GT_COBRB BY OBJNR
                   AVORG
                   VERSN
                   LFDNR DESCENDING
                   VRGNG.
*                   LETJA DESCENDING
*                   LETPE DESCENDING.


  DATA LV_LOCKED TYPE C.

  LOOP AT GT_DATA INTO GS_DATA.

    CLEAR LV_LOCKED.

    GS_DISPLAY = CORRESPONDING #( GS_DATA MAPPING BUKRS = PBUKR ).
    GS_DISPLAY-STATUS = ICON_YELLOW_LIGHT.

    READ TABLE GT_JEST TRANSPORTING NO FIELDS
                       WITH KEY OBJNR = GS_DATA-OBJNR
                                BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      LOOP AT GT_JEST INTO GS_JEST FROM SY-TABIX.
        IF GS_JEST-OBJNR NE GS_DATA-OBJNR.
          EXIT.
        ENDIF.

        CASE GS_JEST-STAT.
          WHEN 'I0043'. LV_LOCKED = GC_X.
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.
    ENDIF.

    CHECK LV_LOCKED IS INITIAL.


*    IF GS_DATA-ZZBGU IS NOT INITIAL.
*      READ TABLE GT_1040 INTO GS_1040
*                         WITH KEY ZZBGU = GS_DATA-ZZBGU
*                                  BINARY SEARCH.
*      IF SY-SUBRC EQ 0.
*      ELSE.
*      ENDIF.
*    ENDIF.


    IF GS_DATA-ZZBGD IS NOT INITIAL.
      READ TABLE GT_1050 INTO GS_1050
                         WITH KEY ZZBGU = GS_DATA-ZZBGU
                                  ZZBGD = GS_DATA-ZZBGD
                                  BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        GS_DISPLAY-ZZBGDTX = GS_1050-ZZBGDTX.
*      ELSE.
      ENDIF.
    ENDIF.


*    IF GS_DATA-ZZPRG IS NOT INITIAL.
*      READ TABLE GT_1100 INTO GS_1100
*                         WITH KEY ZZPRG = GS_DATA-ZZPRG
*                                  BINARY SEARCH.
*      IF SY-SUBRC EQ 0.
*      ELSE.
*      ENDIF.
*    ENDIF.


    CALL FUNCTION 'ZCO_GET_BU_TYPE_BY_MAPPING'
      EXPORTING
        I_PSPNR = GS_DATA-PSPNR                 " WBS 요소
      IMPORTING
        E_WW120 = GS_DISPLAY-WW120              " BU구분
        E_MAPPING_TYPE = GS_DISPLAY-MTYPE.      " 'W' = WBS / 'A' = 속성정보

    CASE GS_DISPLAY-MTYPE.
      WHEN GC_W. GS_DISPLAY-MTYPETX = 'WBS'.
      WHEN GC_A. GS_DISPLAY-MTYPETX = '속성정보'.
    ENDCASE.


    IF GS_DISPLAY-WW120 IS INITIAL.

      GS_DISPLAY-STATUS  = ICON_RED_LIGHT.
      GS_DISPLAY-MESSAGE = TEXT-E01.  " BU 매핑실패

    ELSE.
      READ TABLE GT_T2501 INTO GS_T2501
                          WITH KEY WW120 = GS_DISPLAY-WW120
                                   BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        GS_DISPLAY-BEZEK = GS_T2501-BEZEK.
*      ELSE.
      ENDIF.
    ENDIF.


    READ TABLE GT_COBRB INTO GS_COBRB
                        WITH KEY OBJNR = GS_DATA-OBJNR
                                 AVORG = 'KOAO' " 실제정산
                                 BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GS_DISPLAY-WW120_S = GS_COBRB-WW120.
      GS_DISPLAY-ZZBGU_S = GS_COBRB-WW040.
      GS_DISPLAY-ZZBGD_S = GS_COBRB-WW050.
      GS_DISPLAY-ZZPRG_S = GS_COBRB-WW100.
      GS_DISPLAY-LETJA   = GS_COBRB-LETJA.
      GS_DISPLAY-LETPE   = GS_COBRB-LETPE.


      READ TABLE GT_T2501 INTO GS_T2501
                          WITH KEY WW120 = GS_DISPLAY-WW120_S
                                   BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        GS_DISPLAY-BEZEK_S = GS_T2501-BEZEK.
*      ELSE.
      ENDIF.
    ENDIF.

*-- < 표시 대상 >
* BU를 추출하지 못한 경우
* 신규 정보와 기등록 정보가 다른 경우

    IF GS_DISPLAY-WW120 IS INITIAL OR
       GS_DISPLAY-WW120 NE GS_DISPLAY-WW120_S OR
       GS_DISPLAY-ZZBGU NE GS_DISPLAY-ZZBGU_S OR
       GS_DISPLAY-ZZBGD NE GS_DISPLAY-ZZBGD_S OR
       GS_DISPLAY-ZZPRG NE GS_DISPLAY-ZZPRG_S.
      APPEND GS_DISPLAY TO GT_DISPLAY.
*    ELSE.
*      GS_DISPLAY-STATUS = ICON_GREEN_LIGHT.
*      APPEND GS_DISPLAY TO GT_DISPLAY.
    ENDIF.
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
      PARENT             = GR_CON_TOP          " Contain Object Already Exists
    EXCEPTIONS
      HTML_DISPLAY_ERROR = 1                " Error Displaying the Document in the HTML Control
      OTHERS             = 2
  ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form WRITE_DOCUMENT
*&---------------------------------------------------------------------*
FORM WRITE_DOCUMENT .

  DATA LR_TABLE   TYPE REF TO CL_DD_TABLE_ELEMENT.
  DATA LR_COLUMN1 TYPE REF TO CL_DD_AREA.
  DATA LR_COLUMN2 TYPE REF TO CL_DD_AREA.

  PERFORM ADD_TABLE  USING LR_TABLE 2 '100%'.
  PERFORM ADD_COLUMN USING LR_TABLE LR_COLUMN1 '150px'.
  PERFORM ADD_COLUMN USING LR_TABLE LR_COLUMN2 '*'.


  DATA LV_TEXT1   TYPE TEXT255.
  DATA LV_TEXT2   TYPE TEXT255.


  DATA: LV_TARGET   TYPE I,
        LV_COMPLETE TYPE I,
        LV_ERROR    TYPE I.


  LOOP AT GT_DISPLAY INTO GS_DISPLAY.
    CASE GS_DISPLAY-STATUS.
      WHEN ICON_YELLOW_LIGHT.
        ADD 1 TO LV_TARGET.
      WHEN ICON_GREEN_LIGHT.
        ADD 1 TO LV_COMPLETE.
      WHEN OTHERS.
        ADD 1 TO LV_ERROR.
    ENDCASE.
  ENDLOOP.

*-- 변경대상 수 : N
  LV_TEXT1 = '변경대상 수 : '(L02).
  WRITE LV_TARGET TO LV_TEXT2 NO-GAP.

  LR_TABLE->NEW_ROW( ).
  LR_COLUMN1->ADD_ICON( 'ICON_YELLOW_LIGHT' ).
  LR_COLUMN1->ADD_TEXT( TEXT = LV_TEXT1 SAP_EMPHASIS = CL_DD_AREA=>STRONG ).
  LR_COLUMN2->ADD_TEXT( TEXT = LV_TEXT2 ).


*-- 변경완료 수 : N
  LV_TEXT1 = '변경완료 수 : '(L03).
  WRITE LV_COMPLETE TO LV_TEXT2 NO-GAP.

  LR_TABLE->NEW_ROW( ).
  LR_COLUMN1->ADD_ICON( 'ICON_GREEN_LIGHT' ).
  LR_COLUMN1->ADD_TEXT( TEXT = LV_TEXT1 SAP_EMPHASIS = CL_DD_AREA=>STRONG ).
  LR_COLUMN2->ADD_TEXT( TEXT = LV_TEXT2 ).

*-- 매핑오류 수 : N
  LV_TEXT1 = '매핑오류 수 : '(L04).
  WRITE LV_ERROR TO LV_TEXT2 NO-GAP.

  LR_TABLE->NEW_ROW( ).
  LR_COLUMN1->ADD_ICON( 'ICON_RED_LIGHT' ).
  LR_COLUMN1->ADD_TEXT( TEXT = LV_TEXT1 SAP_EMPHASIS = CL_DD_AREA=>STRONG ).
  LR_COLUMN2->ADD_TEXT( TEXT = LV_TEXT2 ).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADD_TABLE
*&---------------------------------------------------------------------*
FORM ADD_TABLE  USING PR_TABLE          TYPE REF TO CL_DD_TABLE_ELEMENT
                      VALUE(PV_NUMBER)  TYPE I
                      VALUE(PV_WIDTH)   TYPE SDYDO_VALUE.

  CHECK GR_DDOC IS BOUND.

  GR_DDOC->ADD_TABLE(
    EXPORTING
      NO_OF_COLUMNS               = PV_NUMBER " Number of Table Columns
      BORDER                      = '0'       " Width of Table Frame; '0' = No Frame
      WIDTH                       = PV_WIDTH  " Width of Table; '100%' = Entire Width of Control
    IMPORTING
      TABLE                       = PR_TABLE  " Table Element
*      TABLEAREA                   =          " Table Area
    EXCEPTIONS
      TABLE_ALREADY_USED          = 1         " Reference Variable for TABLE Already Used
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
      WIDTH               = PV_WIDTH   " Width of Column (Example '20%')
    IMPORTING
      COLUMN              = PR_COLUMN  " Column Area
    EXCEPTIONS
      COLUMN_ALREADY_USED = 1          " Reference Variable for COLUMN has Already Been Used
      OTHERS              = 2
  ).

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

  GR_ALV->SET_LAYOUT( I_TYPE       = 'A'
                      I_STYLEFNAME = 'STYLE'
                      I_CTAB_FNAME = 'COLOR' ).

  GR_ALV->SET_SORT( IT_FIELD = VALUE #( ( 'BUKRS' )
                                        ( 'PSPNR' )
                                        ( 'POSID' ) ) ).

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


  DATA LV_TEXT TYPE TEXT100.

  CLEAR GS_DISPLAY.

  LOOP AT GR_ALV->MT_FIELDCAT INTO DATA(LS_FIELDCAT).

    CLEAR LV_TEXT.
    CLEAR LS_FIELDCAT-KEY.
    LS_FIELDCAT-COL_OPT = GC_X.

    CASE LS_FIELDCAT-FIELDNAME.
      WHEN 'STATUS'.
        LS_FIELDCAT-KEY  = GC_X.
        LS_FIELDCAT-ICON = GC_X.

      WHEN 'BUKRS'.
        LS_FIELDCAT-KEY  = GC_X.

      WHEN 'PRONR'.
        LS_FIELDCAT-KEY  = GC_X.
        LS_FIELDCAT-NO_OUT = GC_X.

      WHEN 'PSPNR'.
        LS_FIELDCAT-TECH = GC_X.

      WHEN 'POSID'.
        LS_FIELDCAT-KEY     = GC_X.
        LS_FIELDCAT-HOTSPOT = GC_X.

      WHEN 'POST1'.
        LS_FIELDCAT-COL_OPT   = SPACE.
        LS_FIELDCAT-OUTPUTLEN = 25.
        LS_FIELDCAT-EMPHASIZE = 'C500'.

      WHEN 'OBJNR'.
        LS_FIELDCAT-TECH = GC_X.

      WHEN 'ZZBGU'.
      WHEN 'ZZBGD'.
      WHEN 'ZZBGDTX'.
      WHEN 'ZZPRG'.
      WHEN 'WW120'.
      WHEN 'BEZEK'.
      WHEN 'MTYPE'.
        LS_FIELDCAT-TECH = GC_X.
      WHEN 'MTYPETX'.
      WHEN 'WW120_S'.
      WHEN 'BEZEK_S'.
      WHEN 'ZZBGU_S'.
        LS_FIELDCAT-TECH = GC_X.
      WHEN 'ZZBGD_S'.
        LS_FIELDCAT-TECH = GC_X.
      WHEN 'ZZPRG_S'.
        LS_FIELDCAT-TECH = GC_X.
      WHEN 'LETPE'.
*        LS_FIELDCAT-TECH = GC_X.
      WHEN 'LETJA'.
*        LS_FIELDCAT-TECH = GC_X.
      WHEN 'MESSAGE'.
        LS_FIELDCAT-COL_OPT   = SPACE.
        LS_FIELDCAT-OUTPUTLEN = 60.
        LS_FIELDCAT-HOTSPOT   = GC_X.
      WHEN OTHERS.
*        LS_FIELDCAT-TECH = GC_X.

    ENDCASE.


    CASE LS_FIELDCAT-FIELDNAME.
      WHEN 'STATUS'.       LV_TEXT = '구분'.
      WHEN 'BUKRS'.        LV_TEXT = '회사코드'.
      WHEN 'POSID'.        LV_TEXT = 'WBS'.
      WHEN 'POST1'.        LV_TEXT = 'WBS 이름'.
      WHEN 'ZZBGU'.        LV_TEXT = '사업'.
      WHEN 'ZZBGD'.        LV_TEXT = '세부사업'.
      WHEN 'ZZBGDTX'.      LV_TEXT = '세부사업구분명'.
      WHEN 'ZZPRG'.        LV_TEXT = '발주처유형'.
      WHEN 'WW120'.        LV_TEXT = 'BU(신)'.
      WHEN 'BEZEK'.        LV_TEXT = 'BU명(신)'.
      WHEN 'MTYPETX'.      LV_TEXT = 'BU적용전략'.
      WHEN 'WW120_S'.      LV_TEXT = 'BU(S)'.
      WHEN 'BEZEK_S'.      LV_TEXT = 'BU명(S)'.
      WHEN 'LETPE'.        LV_TEXT = '기간'.
      WHEN 'LETJA'.        LV_TEXT = '년도'.
      WHEN 'MESSAGE'.      LV_TEXT = '메세지'.
    ENDCASE.

    IF LV_TEXT IS NOT INITIAL.
      LS_FIELDCAT-REPTEXT   = LV_TEXT.
      LS_FIELDCAT-COLTEXT   = LV_TEXT.
      LS_FIELDCAT-SCRTEXT_L = LV_TEXT.
      LS_FIELDCAT-SCRTEXT_M = LV_TEXT.
      LS_FIELDCAT-SCRTEXT_S = LV_TEXT.
    ENDIF.

    MODIFY GR_ALV->MT_FIELDCAT FROM LS_FIELDCAT.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SAVE_DATA
*&---------------------------------------------------------------------*
FORM SAVE_DATA .

  IF GR_ALV->GET_SELECTED_ROWS( ) EQ 0.

    MESSAGE '선택된 라인이 없습니다...' TYPE GC_S DISPLAY LIKE GC_W.

  ELSEIF ZCL_CO_COMMON=>POPUP_CONFIRM(
      I_TITLEBAR = '확인'(PT1)
      I_QUESTION = CONV #( '정산규칙을 등록하시겠습니까?'(QT1) ) ) NE GC_X.

    MESSAGE '취소되었습니다.' TYPE GC_S DISPLAY LIKE GC_W.

  ELSE.

    DATA LT_DATA LIKE TABLE OF GS_DATA.

    PERFORM GET_TARGET_DATA USING LT_DATA.
    PERFORM EXECUTE_TARGET  USING LT_DATA.
    PERFORM REFRESH_SCREEN  USING LT_DATA.

    MESSAGE 'BDC CJ02가 수행되었습니다. 각 라인의 상태 및 메시지열을 확인하세요.'
       TYPE GC_S.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_TARGET_DATA
*&---------------------------------------------------------------------*
FORM GET_TARGET_DATA  USING PT_DATA LIKE GT_DATA.

  LOOP AT GR_ALV->MT_ROWS INTO DATA(LS_ROW).

    READ TABLE GT_DISPLAY INTO GS_DISPLAY INDEX LS_ROW-ROW_ID.

    CHECK SY-SUBRC EQ 0
      AND GS_DISPLAY-STATUS EQ ICON_YELLOW_LIGHT.

    GS_DATA = CORRESPONDING #( GS_DISPLAY ).

    APPEND GS_DATA TO PT_DATA.  CLEAR GS_DATA.
  ENDLOOP.

  SORT PT_DATA BY OBJNR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXECUTE_TARGET
*&---------------------------------------------------------------------*
FORM EXECUTE_TARGET  USING PT_DATA LIKE GT_DATA.

  DATA LV_TEXT  TYPE TEXT100.


  PERFORM INIT_CHECK.
  PERFORM SELECT_COBRB USING PT_DATA.


  LOOP AT GR_ALV->MT_ROWS INTO DATA(LS_ROW).

    READ TABLE GT_DISPLAY INTO GS_DISPLAY INDEX LS_ROW-ROW_ID.
    CHECK SY-SUBRC EQ 0.

    IF GS_DISPLAY-WW120  IS NOT INITIAL AND
       ( GS_DISPLAY-STATUS EQ ICON_YELLOW_LIGHT OR
         GS_DISPLAY-STATUS EQ ICON_RED_LIGHT  ).

*--------------------------------------------------------------------*
*     B D C  E x e c u t e  -  C J 0 2                               *
*--------------------------------------------------------------------*

      PERFORM CALL_TRANSACTION_CJ02.

*--------------------------------------------------------------------*
    ELSE.
      GS_DISPLAY-MESSAGE = '정산규칙 업데이트 대상이 아닙니다.'.
    ENDIF.

    WRITE SY-UZEIT TO LV_TEXT.
    GS_DISPLAY-MESSAGE = |[{ LV_TEXT }] { GS_DISPLAY-MESSAGE }|.

    MODIFY GT_DISPLAY FROM GS_DISPLAY INDEX LS_ROW-ROW_ID.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFRESH_SCREEN
*&---------------------------------------------------------------------*
FORM REFRESH_SCREEN  USING PT_DATA LIKE GT_DATA.

  PERFORM REFRESH_DISPLAY_DATA  USING PT_DATA.
  PERFORM CREATE_TOP_OF_PAGE_0100.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFRESH_DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM REFRESH_DISPLAY_DATA   USING PT_DATA  LIKE GT_DATA.

  DATA LV_TEXT  TYPE TEXT100.


  " 업데이트된 정산규칙정보 재조회
  PERFORM SELECT_COBRB USING PT_DATA.

  " WBS 정산규칙에서 마지막 라인을 기준으로 정렬
  SORT GT_COBRB BY OBJNR
                   AVORG
                   VERSN
                   LFDNR DESCENDING
                   VRGNG.
*                   LETJA DESCENDING
*                   LETPE DESCENDING.


  LOOP AT GR_ALV->MT_ROWS INTO DATA(LS_ROW).

    READ TABLE GT_DISPLAY INTO GS_DISPLAY INDEX LS_ROW-ROW_ID.
    CHECK SY-SUBRC EQ 0 AND GS_DISPLAY-WW120 IS NOT INITIAL.


    " WBS 의 정산규칙 데이터를 읽어와 각 라인에 맞춰 작업을 수행한다.
    CLEAR GS_COBRB.
    READ TABLE GT_COBRB INTO GS_COBRB
                        WITH KEY OBJNR = GS_DISPLAY-OBJNR
                                 AVORG = 'KOAO' " 실제정산
                                 BINARY SEARCH.


    GS_DISPLAY-WW120_S = GS_COBRB-WW120.
    GS_DISPLAY-ZZBGU_S = GS_COBRB-WW040.
    GS_DISPLAY-ZZBGD_S = GS_COBRB-WW050.
    GS_DISPLAY-ZZPRG_S = GS_COBRB-WW100.
    GS_DISPLAY-LETJA   = GS_COBRB-LETJA.
    GS_DISPLAY-LETPE   = GS_COBRB-LETPE.


    CLEAR GS_T2501.
    READ TABLE GT_T2501 INTO GS_T2501
                        WITH KEY WW120 = GS_DISPLAY-WW120_S
                                 BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GS_DISPLAY-BEZEK_S = GS_T2501-BEZEK.
    ENDIF.


    IF GS_DISPLAY-WW120 EQ GS_DISPLAY-WW120_S.
      GS_DISPLAY-STATUS = ICON_GREEN_LIGHT.
    ELSE.
      GS_DISPLAY-STATUS = ICON_RED_LIGHT.
      WRITE SY-UZEIT TO LV_TEXT.
      GS_DISPLAY-MESSAGE = |[{ LV_TEXT }] 정산규칙 업데이트 실패|.
    ENDIF.

    MODIFY GT_DISPLAY FROM GS_DISPLAY INDEX LS_ROW-ROW_ID.
  ENDLOOP.

  GR_ALV->REFRESH( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM HANDLE_DATA_CHANGED  USING PR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
                                PV_ONF4
                                PV_ONF4_BEFORE
                                PV_ONF4_AFTER
                                PV_UCOMM
                                PR_SENDER TYPE REF TO CL_GUI_ALV_GRID.

  CHECK PR_SENDER EQ GR_ALV->MR_ALV_GRID.

  DATA(LT_MOD) = PR_DATA_CHANGED->MT_MOD_CELLS.

  LOOP AT LT_MOD INTO DATA(LS_MOD).

    CLEAR GS_DISPLAY.
    READ TABLE GT_DISPLAY INTO GS_DISPLAY INDEX LS_MOD-ROW_ID.
    CHECK SY-SUBRC EQ 0.

    CASE LS_MOD-FIELDNAME.
      WHEN 'BUKRS'.

*        IF GS_DISPLAY-BUKRS NE LS_MOD-VALUE.
*
*          IF GT_T001 IS INITIAL.
*            PERFORM SELECT_T001.
*          ENDIF.
*
*          CLEAR GS_T001.
*          READ TABLE GT_T001 INTO GS_T001 WITH KEY BUKRS = LS_MOD-VALUE BINARY SEARCH.
*
*          PR_DATA_CHANGED->MODIFY_CELL(
*            I_ROW_ID    = LS_MOD-ROW_ID " Row ID
*            I_TABIX     = LS_MOD-TABIX  " Row Index
*            I_FIELDNAME = 'BUTXT'       " Field Name
*            I_VALUE     = GS_T001-BUTXT " Value
*          ).
*        ENDIF.

    ENDCASE.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_FINISHED
*&---------------------------------------------------------------------*
FORM HANDLE_FINISHED  USING PV_MODIFIED
                            PT_GOOD_CELLS TYPE LVC_T_MODI
                            PR_SENDER TYPE REF TO CL_GUI_ALV_GRID.

  CHECK GV_REFRESH EQ GC_X.
  CLEAR GV_REFRESH.

  PR_SENDER->REFRESH_TABLE_DISPLAY(
    EXPORTING
      IS_STABLE      = VALUE #( ROW = GC_X
                                COL = GC_X )  " With Stable Rows/Columns
      I_SOFT_REFRESH = GC_X                   " Without Sort, Filter, etc.
    EXCEPTIONS
      FINISHED       = 1                " Display was Ended (by Export)
      OTHERS         = 2
  ).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form REGISTER_EVENT_0100
*&---------------------------------------------------------------------*
FORM REGISTER_EVENT_0100 .

  GR_ALV->MR_ALV_GRID->REGISTER_EDIT_EVENT(
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED " Event ID
    EXCEPTIONS
      ERROR      = 1                " Error
      OTHERS     = 2
  ).

  IF GR_EVENT_RECEIVER IS INITIAL.
    CREATE OBJECT GR_EVENT_RECEIVER.
  ENDIF.

  SET HANDLER GR_EVENT_RECEIVER->ON_DATA_CHANGED  FOR GR_ALV->MR_ALV_GRID.
  SET HANDLER GR_EVENT_RECEIVER->ON_FINISHED      FOR GR_ALV->MR_ALV_GRID.
  SET HANDLER GR_EVENT_RECEIVER->ON_HOTSPOT_CLICK FOR GR_ALV->MR_ALV_GRID.
  SET HANDLER GR_EVENT_RECEIVER->ON_TOOLBAR       FOR GR_ALV->MR_ALV_GRID.
  SET HANDLER GR_EVENT_RECEIVER->ON_USER_COMMAND  FOR GR_ALV->MR_ALV_GRID.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM HANDLE_HOTSPOT_CLICK  USING    PS_ROW_ID     TYPE LVC_S_ROW
                                    PS_COLUMN_ID  TYPE LVC_S_COL
                                    PS_ROW_NO     TYPE LVC_S_ROID
                                    PR_SENDER     TYPE REF TO CL_GUI_ALV_GRID.

  CASE PR_SENDER.
    WHEN GR_ALV->MR_ALV_GRID.

      READ TABLE GT_DISPLAY INTO GS_DISPLAY INDEX PS_ROW_ID-INDEX.
      CHECK SY-SUBRC EQ 0.

      CASE PS_COLUMN_ID-FIELDNAME.
        WHEN 'POSID'.
          SET PARAMETER ID 'PSP' FIELD SPACE.
          SET PARAMETER ID 'PRO' FIELD GS_DISPLAY-POSID.
          CALL TRANSACTION 'CJ03' AND SKIP FIRST SCREEN.
        WHEN 'MESSAGE'.
          PERFORM SHOW_MESSAGE.
        WHEN OTHERS.
      ENDCASE.


  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_TRANSACTION_CJ02
*&---------------------------------------------------------------------*
FORM CALL_TRANSACTION_CJ02 .

  CLEAR GV_BDC_CHK.

  " 정산규칙 데이터가 깨진 경우가 있어
  " 단순한 화면 이동 후 저장하는 과정을 거친다.
  PERFORM BDC_INIT.
  PERFORM BDC_FILL_CHECK.
  PERFORM BDC_CALL_CHECK.

  PERFORM BDC_INIT.
  PERFORM BDC_FILL.
  PERFORM BDC_CALL.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BDC_INIT
*&---------------------------------------------------------------------*
FORM BDC_INIT .

  REFRESH GT_BDC_DATA.

  GS_BDC_OPT = VALUE #(
    DISMODE  = P_BDC   " CTU_MODE      "  TRANSACTION USING...에 대한 처리모드
    UPDMODE  = GC_S    " CTU_UPDATE    "  CALL TRANSACTION USING...의 업데이트 모드
    CATTMODE = SPACE   " CTU_CATT      "  CALL TRANSACTION USING에 대한 CATT 모드...
    DEFSIZE  = GC_X    " CTU_DEFSZE    "  CALL TRANSACTION USING...에 대한 기본화면 크기
    RACOMMIT = SPACE   " CTU_RAFC      "  COMMIT으로 완료되지 않은 CALL TRANSACTION USING...
    NOBINPT  = SPACE   " CTU_NOBIM     "  CALL TRANSACTION USING에 대한 SY-BINPT=SPACE...
    NOBIEND  = SPACE   " CTU_NOBEN     "  CALL TRANSACTION USING에 대한 일자종료후 SY-BINPT=SPACE...
  ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BDC_FILL
*&---------------------------------------------------------------------*
FORM BDC_FILL .

  DATA: LT_CHECK        LIKE TABLE OF GS_CHECK.

  DATA: LV_ROW          TYPE NUMC2.

  DATA: LV_FIELD_GBISP  LIKE BDCDATA-FNAM,
        LV_FIELD_GBISJ  LIKE BDCDATA-FNAM.

  DATA: LV_DATUM        LIKE SY-DATUM,
        LV_LETPE        LIKE COBRB-LETPE,
        LV_LETJA        LIKE COBRB-LETJA.


  " 프로젝트 공란으로 두고, WBS 만 입력한다.
  PERFORM BDC_FILL_PGM USING  'SAPLCJWB'    '0100'.
  PERFORM BDC_FILL_FLD USING: 'BDC_OKCODE'  '=LETB',
                              '*PROJ-PSPID' SPACE,
                              '*PRPS-POSID' GS_DISPLAY-POSID.


  " WBS 의 정산규칙으로 이동한다.
  PERFORM BDC_FILL_PGM USING  'SAPLCJWB'    '0901'.
  PERFORM BDC_FILL_FLD USING: 'BDC_OKCODE'  '=ABRV',
                              'BDC_CURSOR'  'RCWBS-IDENT(01)'.

  IF GV_BDC_CHK EQ GC_X.
    PERFORM BDC_FILL_PGM USING  'SAPMSSY0'    '0120'.
    PERFORM BDC_FILL_FLD USING: 'BDC_OKCODE'  '=&ONT'.
  ENDIF.


  " WBS 의 정산규칙 데이터를 읽어와 각 라인에 맞춰 작업을 수행한다.
  READ TABLE GT_COBRB TRANSPORTING NO FIELDS
                      WITH KEY OBJNR = GS_DISPLAY-OBJNR
                               BINARY SEARCH.

  IF SY-SUBRC EQ 0.

    LOOP AT GT_COBRB INTO GS_COBRB FROM SY-TABIX.
      IF GS_COBRB-OBJNR NE GS_DISPLAY-OBJNR.
        EXIT.
      ENDIF.

      IF GS_COBRB-AVORG EQ 'KOAO'.  " 실제정산

        " 정산규칙 라인정보
        ADD 1 TO LV_ROW.

        " 효력종료(기간/년도) 를 각 라인별로 다룬다.
        LV_FIELD_GBISP = |COBRB-GBISP({ LV_ROW })|.
        LV_FIELD_GBISJ = |COBRB-GBISJ({ LV_ROW })|.

        " 기존의 정산을 수행한 이력이 있으면(최종정산연도가 있으면)
        IF GS_COBRB-LETJA IS NOT INITIAL.

          IF GS_COBRB-GBISJ IS INITIAL OR GS_COBRB-GBISP IS INITIAL.
            " 효력종료(기간/년도)이 비었다면 최종정산연도로 기입한다.
            PERFORM BDC_FILL_PGM USING  'SAPLKOBS'      '0130'.
            PERFORM BDC_FILL_FLD USING: 'BDC_OKCODE'    '/00',
                                        LV_FIELD_GBISP  GS_COBRB-LETPE, " 효력종료 기간
                                        LV_FIELD_GBISJ  GS_COBRB-LETJA. " 효력종료 연도
          ENDIF.

          LV_LETPE = GS_COBRB-LETPE.
          LV_LETJA = GS_COBRB-LETJA.

        ELSE. " 기존의 정산을 수행한 이력이 없으나 정산규칙만 있으면((최종정산연도가 없으면)

          " 해당라인을 삭제한다.
          PERFORM BDC_FILL_PGM USING  'SAPLKOBS'      '0130'.
          PERFORM BDC_FILL_FLD USING: 'BDC_OKCODE'    '=DELL',
                                      'BDC_CURSOR'    LV_FIELD_GBISP.

          " 라인 삭제 후에는 라인이 줄어들기 때문에
          " LV_ROW / GT_COBRB 도 해당 부분을 고려한다.
          SUBTRACT 1 FROM LV_ROW.
          DELETE GT_COBRB.

        ENDIF.
      ENDIF.


      IF GS_COBRB-GBISJ EQ '0000'.
        GS_COBRB-GBISJ = '9999'.
      ENDIF.

      " 기간별 정산 백분율 계산
      LOOP AT GT_CHECK INTO GS_CHECK
                       WHERE YEAR BETWEEN GS_COBRB-GABJA
                                      AND GS_COBRB-GBISJ.

        GS_CHECK-AVORG = GS_COBRB-AVORG.
        GS_CHECK-VERSN = GS_COBRB-VERSN.


        CASE GS_CHECK-YEAR.

          WHEN GS_COBRB-GABJA.  " 시작년도가 같은 경우

            " 시작기간보단 크거나 같아야 한다.
            CHECK GS_CHECK-MONTH GE GS_COBRB-GABPE.

            " 종료년도보다 작은 경우 OK
            " 종료년도와 같은 경우 종료기간보다 작거나 같아야 한다.
            CHECK ( GS_CHECK-YEAR  LT GS_COBRB-GBISJ )
               OR ( GS_CHECK-YEAR  EQ GS_COBRB-GBISJ AND
                    GS_CHECK-MONTH LE GS_COBRB-GBISP ).

          WHEN GS_COBRB-GBISJ.  " 종료년도가 같은 경우

            " 종료기간보단 작거나 같아야 한다.
            CHECK GS_CHECK-MONTH LE GS_COBRB-GBISP.

            " 시작년도보다 큰 경우 OK
            " 시작년도와 같은 경우 시작기간보다 크거나 같아야 한다.
            CHECK ( GS_CHECK-YEAR  GT GS_COBRB-GABJA )
               OR ( GS_CHECK-YEAR  EQ GS_COBRB-GABJA AND
                    GS_CHECK-MONTH GE GS_COBRB-GABPE ).

          WHEN OTHERS.

            " 시작년도, 종료년도와 다른 경우
            " 그 사이에 해당하는 기간만 허용한다.
            CHECK GS_CHECK-YEAR BETWEEN GS_COBRB-GABJA
                                    AND GS_COBRB-GBISJ.

        ENDCASE.

        GS_CHECK-PROZS = GS_COBRB-PROZS.
        COLLECT GS_CHECK INTO LT_CHECK.

      ENDLOOP.
    ENDLOOP.
  ENDIF.


  IF LV_LETPE IS NOT INITIAL.
    LV_DATUM = LV_LETJA && LV_LETPE+1(2) && '01'.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        DATE      = LV_DATUM
        DAYS      = '00'
        MONTHS    = '01'
        YEARS     = '00'
      IMPORTING
        CALC_DATE = LV_DATUM.

    LV_LETJA = LV_DATUM+0(4).
    LV_LETPE = LV_DATUM+4(2).
  ENDIF.


  " 신규 정산규칙 생성
  PERFORM BDC_FILL_PGM USING  'SAPLKOBS'        '0130'.
  PERFORM BDC_FILL_FLD USING: 'BDC_OKCODE'      '=NEUR'.

  PERFORM BDC_FILL_PGM USING  'SAPLKOBS'        '0100'.
  PERFORM BDC_FILL_FLD USING: 'BDC_OKCODE'      '=COBL_XERGO'.

  PERFORM BDC_FILL_PGM USING  'SAPLKEAK'        '0300'.
  PERFORM BDC_FILL_FLD USING: 'BDC_OKCODE'      '=WEIT'.

  PERFORM BDC_FILL_PGM USING  'SAPLKOBS'        '0100'.
  PERFORM BDC_FILL_FLD USING: 'BDC_OKCODE'      '=BACK',
                              'COBRB-GABPE'     LV_LETPE,
                              'COBRB-GABJA'     LV_LETJA.


  " WBS 정산규칙 설정화면을 벗어난다.
  PERFORM BDC_FILL_PGM USING  'SAPLKOBS'        '0130'.
  PERFORM BDC_FILL_FLD USING: 'BDC_OKCODE'      '=BACK'.


  " 배부율 100 이 아닌 경우에 대한 오류 팝업창 대응
  SORT LT_CHECK BY PROZS DESCENDING.

  CLEAR GS_CHECK.
  READ TABLE LT_CHECK INTO GS_CHECK INDEX 1.

  IF SY-SUBRC EQ 0 AND GS_CHECK-PROZS NE 100.
    PERFORM BDC_FILL_PGM USING  'SAPMSSY0'        '0120'.
    PERFORM BDC_FILL_FLD USING: 'BDC_OKCODE'      '=&ONT'.
  ENDIF.


  " WBS 저장
  PERFORM BDC_FILL_PGM USING  'SAPLCJWB'        '0901'.
  PERFORM BDC_FILL_FLD USING: 'BDC_OKCODE'      '=BU'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BDC_FILL_PGM
*&---------------------------------------------------------------------*
FORM BDC_FILL_PGM  USING PV_PROGRAM
                         PV_DYNPRO.

  GS_BDC_DATA = VALUE #(
    PROGRAM   = PV_PROGRAM
    DYNPRO    = PV_DYNPRO
    DYNBEGIN  = GC_X
  ).

  APPEND GS_BDC_DATA TO GT_BDC_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BDC_FILL_FLD
*&---------------------------------------------------------------------*
FORM BDC_FILL_FLD  USING PV_FNAM
                         PV_FVAL.

  GS_BDC_DATA = VALUE #(
    FNAM = PV_FNAM
    FVAL = PV_FVAL
  ).

  APPEND GS_BDC_DATA TO GT_BDC_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BDC_CALL
*&---------------------------------------------------------------------*
FORM BDC_CALL.

  REFRESH GT_BDC_MSG.

  CALL TRANSACTION 'CJ02' USING GT_BDC_DATA
                          OPTIONS FROM GS_BDC_OPT
                          MESSAGES INTO GT_BDC_MSG.

  READ TABLE GT_BDC_MSG INTO GS_BDC_MSG WITH KEY MSGTYP = GC_E.

  IF SY-SUBRC EQ 0.
    GS_DISPLAY-STATUS = ICON_RED_LIGHT.

    MESSAGE ID      GS_BDC_MSG-MSGID
            TYPE    GS_BDC_MSG-MSGTYP
            NUMBER  GS_BDC_MSG-MSGNR
            WITH    GS_BDC_MSG-MSGV1
                    GS_BDC_MSG-MSGV2
                    GS_BDC_MSG-MSGV3
                    GS_BDC_MSG-MSGV4
            INTO    GS_DISPLAY-MESSAGE.
  ELSE.

    READ TABLE GT_BDC_MSG INTO GS_BDC_MSG
                          WITH KEY MSGID = '00'
                                   MSGNR = '344'.

    IF SY-SUBRC EQ 0 AND GV_BDC_CHK IS INITIAL.

      GV_BDC_CHK = GC_X.

      PERFORM BDC_INIT.
      PERFORM BDC_FILL.
      PERFORM BDC_CALL.

    ELSE.

      GS_DISPLAY-STATUS   = ICON_GREEN_LIGHT.
      GS_DISPLAY-MESSAGE = '변경 완료'.

    ENDIF.
  ENDIF.

  GS_DISPLAY-MESSAGE_TAB = GT_BDC_MSG[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form INIT_CHECK
*&---------------------------------------------------------------------*
FORM INIT_CHECK .

  REFRESH GT_CHECK.

  " 2021 년 부터 확인한다.
  GS_CHECK = VALUE #( YEAR = '2021' MONTH = '000' ).

  DO.
    IF GS_CHECK-MONTH EQ '012'.
      IF SY-DATUM(4) EQ GS_CHECK-YEAR.
        EXIT.
      ELSE.
        GS_CHECK-YEAR  = GS_CHECK-YEAR + 1.
        GS_CHECK-MONTH = '001'.
      ENDIF.
    ELSE.
      GS_CHECK-MONTH = GS_CHECK-MONTH + 1.
    ENDIF.

    APPEND GS_CHECK TO GT_CHECK.
  ENDDO.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SHOW_MESSAGE
*&---------------------------------------------------------------------*
FORM SHOW_MESSAGE.

  DATA: BEGIN OF LS_MESSAGE,
          LINE    TYPE ROW_POS,
          MESSAGE TYPE GHO_MESSAGE,
        END OF LS_MESSAGE.

  DATA LT_MESSAGE LIKE TABLE OF LS_MESSAGE.

  CHECK GS_DISPLAY-MESSAGE     IS NOT INITIAL
     OR GS_DISPLAY-MESSAGE_TAB IS NOT INITIAL.

  TRY.

    CLEAR SY-TABIX.

    LOOP AT GS_DISPLAY-MESSAGE_TAB INTO DATA(LS_MESSAGE_BDC).

      LS_MESSAGE-LINE = SY-TABIX.

      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          MSGID = LS_MESSAGE_BDC-MSGID " Message ID
          MSGNR = LS_MESSAGE_BDC-MSGNR " Number of message
          MSGV1 = LS_MESSAGE_BDC-MSGV1 " Parameter 1
          MSGV2 = LS_MESSAGE_BDC-MSGV2 " Parameter 2
          MSGV3 = LS_MESSAGE_BDC-MSGV3 " Parameter 3
          MSGV4 = LS_MESSAGE_BDC-MSGV4 " Parameter 4
        IMPORTING
          MESSAGE_TEXT_OUTPUT = LS_MESSAGE-MESSAGE. " Output message text

      APPEND LS_MESSAGE TO LT_MESSAGE.

    ENDLOOP.

    IF GS_DISPLAY-MESSAGE IS NOT INITIAL.
      LS_MESSAGE-LINE    = LS_MESSAGE-LINE + 1.
      LS_MESSAGE-MESSAGE = GS_DISPLAY-MESSAGE.
      APPEND LS_MESSAGE TO LT_MESSAGE.
    ENDIF.

    CL_SALV_TABLE=>FACTORY(
*      EXPORTING
*        LIST_DISPLAY   = IF_SALV_C_BOOL_SAP=>FALSE " ALV Displayed in List Mode
*        R_CONTAINER    =                           " Abstract Container for GUI Controls
*        CONTAINER_NAME =
      IMPORTING
        R_SALV_TABLE   = DATA(LR_ALV_MSG)   " Basis Class Simple ALV Tables
      CHANGING
        T_TABLE        = LT_MESSAGE[]
    ).

    DATA(LR_COLUMNS) = LR_ALV_MSG->GET_COLUMNS( ).

    DATA(LT_COLUMNS) = LR_COLUMNS->GET( ).

    LOOP AT LT_COLUMNS INTO DATA(LS_COLUMN).
      CASE LS_COLUMN-COLUMNNAME.
        WHEN 'LINE'.
          LS_COLUMN-R_COLUMN->SET_OUTPUT_LENGTH( 8 ).
        WHEN 'MESSAGE'.
          LS_COLUMN-R_COLUMN->SET_OUTPUT_LENGTH( 70 ).

      ENDCASE.
    ENDLOOP.

    LR_ALV_MSG->SET_SCREEN_POPUP(
      EXPORTING
        START_COLUMN = 20
        END_COLUMN   = 100
        START_LINE   = 5
        END_LINE     = 25
    ).

    LR_ALV_MSG->DISPLAY( )..

  CATCH CX_SALV_MSG. " ALV: General Error Class with Message
  CATCH CX_ROOT.

  ENDTRY.

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
        (
          BUTN_TYPE = 3
        )
        (
          BUTN_TYPE = 0
          FUNCTION  = 'SHOW_YELLOW'
          ICON      = ICON_LED_YELLOW
          QUICKINFO = '변경대상만'
          CHECKED   = GV_CHK_YELLOW
        )
        (
          BUTN_TYPE = 0
          FUNCTION  = 'SHOW_GREEN'
          ICON      = ICON_LED_GREEN
          QUICKINFO = '변경완료만'
          CHECKED   = GV_CHK_GREEN
        )
        (
          BUTN_TYPE = 0
          FUNCTION  = 'SHOW_RED'
          ICON      = ICON_LED_RED
          QUICKINFO = '매핑오류만'
          CHECKED   = GV_CHK_RED
        )
      ).

      APPEND LINES OF LT_TOOLBAR TO PR_OBJECT->MT_TOOLBAR.


    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
FORM HANDLE_USER_COMMAND
  USING PV_UCOMM  TYPE SY-UCOMM
        PR_SENDER TYPE REF TO CL_GUI_ALV_GRID.

  CASE PR_SENDER.
    WHEN GR_ALV->MR_ALV_GRID.

      PR_SENDER->GET_FILTER_CRITERIA(
        IMPORTING
          ET_FILTER = DATA(LT_FILTER)                 " Filter Criteria
      ).

      CASE PV_UCOMM.
        WHEN 'SHOW_YELLOW'.

          DELETE LT_FILTER WHERE FIELDNAME EQ 'STATUS'
                             AND LOW       EQ ICON_YELLOW_LIGHT.

          IF GV_CHK_YELLOW IS INITIAL.
            GV_CHK_YELLOW = GC_X.
          ELSE.
            LT_FILTER = VALUE #( BASE LT_FILTER (
              FIELDNAME = 'STATUS'
              SIGN      = 'E'
              OPTION    = 'EQ'
              LOW       = ICON_YELLOW_LIGHT
            ) ).

            CLEAR GV_CHK_YELLOW.
          ENDIF.


        WHEN 'SHOW_GREEN'.

          DELETE LT_FILTER WHERE FIELDNAME EQ 'STATUS'
                             AND LOW       EQ ICON_GREEN_LIGHT.

          IF GV_CHK_GREEN IS INITIAL.
            GV_CHK_GREEN = GC_X.
          ELSE.
            LT_FILTER = VALUE #( BASE LT_FILTER (
              FIELDNAME = 'STATUS'
              SIGN      = 'E'
              OPTION    = 'EQ'
              LOW       = ICON_GREEN_LIGHT
            ) ).

            CLEAR GV_CHK_GREEN.
          ENDIF.


        WHEN 'SHOW_RED'.

          DELETE LT_FILTER WHERE FIELDNAME EQ 'STATUS'
                             AND LOW       EQ ICON_RED_LIGHT.

          IF GV_CHK_RED IS INITIAL.
            GV_CHK_RED = GC_X.
          ELSE.
            LT_FILTER = VALUE #( BASE LT_FILTER (
              FIELDNAME = 'STATUS'
              SIGN      = 'E'
              OPTION    = 'EQ'
              LOW       = ICON_RED_LIGHT
            ) ).

            CLEAR GV_CHK_RED.
          ENDIF.


        WHEN OTHERS.
          EXIT.
      ENDCASE.



      PR_SENDER->SET_FILTER_CRITERIA(
        EXPORTING
          IT_FILTER                 = LT_FILTER        " Filter Conditions
        EXCEPTIONS
          NO_FIELDCATALOG_AVAILABLE = 1                " Instance Variable of Field Catalog not yet Set
          OTHERS                    = 2
      ).
      PR_SENDER->REFRESH_TABLE_DISPLAY( ).

    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BDC_FILL_CHECK
*&---------------------------------------------------------------------*
FORM BDC_FILL_CHECK .

  " 프로젝트 공란으로 두고, WBS 만 입력한다.
  PERFORM BDC_FILL_PGM USING  'SAPLCJWB'    '0100'.
  PERFORM BDC_FILL_FLD USING: 'BDC_OKCODE'  '=LETB',
                              '*PROJ-PSPID' SPACE,
                              '*PRPS-POSID' GS_DISPLAY-POSID.

  " WBS 의 정산규칙으로 이동한다.
  PERFORM BDC_FILL_PGM USING  'SAPLCJWB'    '0901'.
  PERFORM BDC_FILL_FLD USING: 'BDC_OKCODE'  '=ABRV',
                              'BDC_CURSOR'  'RCWBS-IDENT(01)'.

  " WBS 정산규칙을 벗어난다.
  PERFORM BDC_FILL_PGM USING  'SAPLKOBS'    '0130'.
  PERFORM BDC_FILL_FLD USING: 'BDC_OKCODE'  '=BACK'.

  " WBS 변경 종료
  PERFORM BDC_FILL_PGM USING  'SAPLCJWB'    '0901'.
  PERFORM BDC_FILL_FLD USING: 'BDC_OKCODE'  '/EBCK'.

  " WBS 정상규칙이 오류가 있을 경우 저장여부 팝업창 출력, 저장으로 대응
  PERFORM BDC_FILL_PGM USING  'SAPLSPO1'    '0100'.
  PERFORM BDC_FILL_FLD USING: 'BDC_OKCODE'  '=YES'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BDC_CALL_CHECK
*&---------------------------------------------------------------------*
FORM BDC_CALL_CHECK .

  REFRESH GT_BDC_MSG.

  CALL TRANSACTION 'CJ02' USING GT_BDC_DATA
                          OPTIONS FROM GS_BDC_OPT
                          MESSAGES INTO GT_BDC_MSG.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_JEST
*&---------------------------------------------------------------------*
FORM SELECT_JEST  USING PT_DATA LIKE GT_DATA.

  " 기등록 정산규칙 정보 추출

  DATA LR_OBJNR LIKE RANGE OF COBRB-OBJNR WITH HEADER LINE.

  LR_OBJNR[] = CORRESPONDING #( PT_DATA[] MAPPING LOW = OBJNR ).
  LR_OBJNR = 'IEQ'.
  MODIFY LR_OBJNR TRANSPORTING SIGN OPTION WHERE SIGN IS INITIAL.


  REFRESH GT_JEST.

  CHECK LR_OBJNR[] IS NOT INITIAL.

  SELECT DISTINCT
         OBJNR,
         STAT
    FROM JEST
   WHERE OBJNR IN @LR_OBJNR
    INTO TABLE @GT_JEST.

  SORT GT_JEST BY OBJNR
                  STAT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SHOW_WBS_MAPPING
*&---------------------------------------------------------------------*
FORM SHOW_WBS_MAPPING .

*  CALL TRANSACTION 'ZCOV1320' .

  PERFORM SELECT_1320.

  PERFORM FREE_DIALOGBOX   USING GR_CON_DIALOG
                                 GR_ALV_DIALOG.
  PERFORM CREATE_DIALOGBOX USING 1000 200 'BU 매핑조회(WBS)'.
  PERFORM CREATE_DIALOGALV USING 'GS_1320'.
  PERFORM SHOW_DIALOGALV   TABLES GT_1320.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_1320
*&---------------------------------------------------------------------*
FORM SELECT_1320 .

  REFRESH GT_1320.

  SELECT A~PSPNR,
         B~POST1,
         A~WW120,
         C~BEZEK,
         A~AEDAT,
         A~AEZET,
         A~AENAM
    FROM ZCOT1320   AS A
    LEFT JOIN PRPS  AS B ON B~PSPNR EQ A~PSPNR
    LEFT JOIN T25A1 AS C ON C~SPRAS EQ @SY-LANGU
                        AND C~WW120 EQ A~WW120
    INTO TABLE @GT_1320.

  SORT GT_1320 BY PSPNR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FREE_DIALOGBOX_1
*&---------------------------------------------------------------------*
FORM FREE_DIALOGBOX USING PR_DIA TYPE REF TO CL_GUI_DIALOGBOX_CONTAINER
                          PR_ALV TYPE REF TO ZCL_CO_ALV.

  IF PR_ALV IS NOT INITIAL.
    IF PR_ALV->MR_ALV_GRID IS BOUND.
      PR_ALV->MR_ALV_GRID->FREE(
        EXCEPTIONS
          CNTL_ERROR        = 1
          CNTL_SYSTEM_ERROR = 2
          OTHERS            = 3 ).
    ENDIF.

    IF PR_ALV->MR_CONTAINER IS BOUND.
      PR_ALV->MR_CONTAINER->FREE(
        EXCEPTIONS
          CNTL_ERROR        = 1
          CNTL_SYSTEM_ERROR = 2
          OTHERS            = 3 ).
    ENDIF.

    FREE PR_ALV.
  ENDIF.

  IF PR_DIA IS NOT INITIAL.
    IF PR_DIA IS BOUND.
      PR_DIA->FREE(
        EXCEPTIONS
          CNTL_ERROR        = 1
          CNTL_SYSTEM_ERROR = 2
          OTHERS            = 3
      ).
    ENDIF.

    FREE PR_DIA.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_DIALOGBOX
*&---------------------------------------------------------------------*
FORM CREATE_DIALOGBOX USING PV_WIDTH
                            PV_HEIGHT
                            PV_CAPTION.

  CREATE OBJECT GR_CON_DIALOG
    EXPORTING
      WIDTH   = PV_WIDTH            " Width of This Container
      HEIGHT  = PV_HEIGHT           " Height of This Container
      TOP     = 50                  " Top Position of Dialog Box
      LEFT    = 50                  " Left Position of Dialog Box
      CAPTION = PV_CAPTION          " Dialog Box Caption
    EXCEPTIONS
      CNTL_ERROR                  = 1 " CNTL_ERROR
      CNTL_SYSTEM_ERROR           = 2 " CNTL_SYSTEM_ERROR
      CREATE_ERROR                = 3 " CREATE_ERROR
      LIFETIME_ERROR              = 4 " LIFETIME_ERROR
      LIFETIME_DYNPRO_DYNPRO_LINK = 5 " LIFETIME_DYNPRO_DYNPRO_LINK
      EVENT_ALREADY_REGISTERED    = 6 " Event Already Registered
      ERROR_REGIST_EVENT          = 7 " Error While Registering Event
      OTHERS                      = 8.

  CHECK SY-SUBRC EQ 0.

  IF GR_EVENT_RECEIVER IS INITIAL.
    CREATE OBJECT GR_EVENT_RECEIVER.
  ENDIF.

  SET HANDLER GR_EVENT_RECEIVER->ON_CLOSE FOR GR_CON_DIALOG.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_DIALOGALV
*&---------------------------------------------------------------------*
FORM CREATE_DIALOGALV USING PV_TABNAME.

  CREATE OBJECT GR_ALV_DIALOG
    EXPORTING
      I_CONTAINER = GR_CON_DIALOG.

  GR_ALV_DIALOG->SET_FIELD_CATALOG(
    EXPORTING
      " ABAP Dictionary 의 Table/View/Structure
      I_TABNAME               = PV_TABNAME
    EXCEPTIONS
      INVALID_INPUT_PARAMETER = 1
      EMPTY_FIELD_CATALOG     = 2
      OTHERS                  = 3
  ).

  LOOP AT GR_ALV_DIALOG->MT_FIELDCAT INTO DATA(LS_FIELDCAT).

    CASE LS_FIELDCAT-FIELDNAME.
      WHEN 'WW120' OR 'BEZEK'.
        LS_FIELDCAT-EMPHASIZE = 'C300'.
    ENDCASE.

    MODIFY GR_ALV_DIALOG->MT_FIELDCAT FROM LS_FIELDCAT.
  ENDLOOP.

  GR_ALV_DIALOG->SET_LAYOUT( I_TYPE = GC_A ).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SHOW_DIALOGALV
*&---------------------------------------------------------------------*
FORM SHOW_DIALOGALV TABLES PT_OUTTAB.

  GR_ALV_DIALOG->DISPLAY(
    CHANGING
      T_OUTTAB = PT_OUTTAB[]
  ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_CLOSE
*&---------------------------------------------------------------------*
FORM HANDLE_CLOSE
  USING PR_SENDER TYPE REF TO CL_GUI_DIALOGBOX_CONTAINER.

  PR_SENDER->FREE(
    EXCEPTIONS
      CNTL_ERROR        = 1                " CNTL_ERROR
      CNTL_SYSTEM_ERROR = 2                " CNTL_SYSTEM_ERROR
      OTHERS            = 3
  ).

  FREE PR_SENDER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SHOW_WBS_ATTR_MAPPING
*&---------------------------------------------------------------------*
FORM SHOW_WBS_ATTR_MAPPING .

  PERFORM SELECT_1310.

  PERFORM FREE_DIALOGBOX   USING GR_CON_DIALOG
                                 GR_ALV_DIALOG.

  PERFORM CREATE_DIALOGBOX USING 1600 400 'BU 매핑조회(속성)'.
  PERFORM CREATE_DIALOGALV USING 'GS_1310'.

  GR_ALV_DIALOG->SET_SORT( IT_FIELD =
    VALUE #( ( 'BUKRS' )
             ( 'BUTXT' )
             ( 'ZZBGU' )
             ( 'ZZBGUTX' )
             ( 'ZZBGD' )
             ( 'ZZBGDTX' )
             ( 'ZZPRG' )
             ( 'ZZPRGTX' ) ) ).

  PERFORM SHOW_DIALOGALV   TABLES GT_1310.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_1310
*&---------------------------------------------------------------------*
FORM SELECT_1310 .

  SELECT A~BUKRS,
         B~BUTXT,
         A~ZZBGU,
         C~ZZBGUTX,
         A~ZZBGD,
         D~ZZBGDTX,
         A~ZZPRG,
         E~ZZPRGTX,
         A~WW120,
         F~BEZEK,
         A~AEDAT,
         A~AEZET,
         A~AENAM
    FROM ZCOT1310 AS A LEFT JOIN T001       AS B ON B~BUKRS EQ A~BUKRS
                       LEFT JOIN ZCOT1040T  AS C ON C~SPRAS EQ @SY-LANGU
                                                AND C~ZZBGU EQ A~ZZBGU
                       LEFT JOIN ZCOT1050T  AS D ON D~SPRAS EQ @SY-LANGU
                                                AND D~ZZBGU EQ A~ZZBGU
                                                AND D~ZZBGD EQ A~ZZBGD
                       LEFT JOIN ZCOT1100T  AS E ON E~SPRAS EQ @SY-LANGU
                                                AND E~ZZPRG EQ A~ZZPRG
                       LEFT JOIN T25A1      AS F ON F~SPRAS EQ @SY-LANGU
                                                AND F~WW120 EQ A~WW120

    INTO CORRESPONDING FIELDS OF TABLE @GT_1310.


  SORT GT_1310 BY BUKRS
                  ZZBGU
                  ZZBGD
                  ZZPRG
                  WW120.

ENDFORM.
