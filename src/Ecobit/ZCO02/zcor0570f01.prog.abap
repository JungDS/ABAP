*&---------------------------------------------------------------------*
*& Include          ZCOR0570F01
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

  DATA LV_DATUM TYPE SY-DATUM.

  LV_DATUM = SY-DATUM.
  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      DATE      = LV_DATUM
      DAYS      = '00'
      MONTHS    = '01'
      SIGNUM    = '-'
      YEARS     = '00'
    IMPORTING
      CALC_DATE = LV_DATUM.

  " 기본값 : 년도/월
  P_GJAHR     = LV_DATUM(4).
  S_POPER-LOW = LV_DATUM+4(2).
  APPEND S_POPER.

  " 기본값 : 회사코드
*  S_BUKRS      = 'IEQ'.
  S_BUKRS-LOW  = ZCL_CO_COMMON=>GET_DEFAULT_BUKRS( ).
  IF S_BUKRS-LOW IS INITIAL.
    S_BUKRS-LOW = '1100'.
  ENDIF.
  APPEND S_BUKRS.

  " Selection Screen 텍스트
  TEXT_S01 = '실행조건'(S01).
  TEXT_S03 = '실적/계획 선택'(S03).
  TEXT_S04 = '조회조건'(S04).
  TEXT_S06 = '계획'(S06).
  TEXT_S07 = '계획버젼'(S07).


  " 기본값 : 조회월 범위
  GV_MONTH_MIN = '999'.
  GV_MONTH_MAX = '000'.

  P_NOZERO = GC_X.

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
      WHEN 'P_VERSN'.
        IF P_PLN IS INITIAL.
          SCREEN-INPUT = '0'.
        ENDIF.
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
    WHEN 'UC_VT'.

      IF P_ACT EQ GC_X.
        CLEAR P_VERSN.
      ENDIF.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_BLOCK_B03
*&---------------------------------------------------------------------*
FORM CHECK_BLOCK_B03.

  CHECK SSCRFIELDS-UCOMM EQ 'ONLI'.
  CHECK P_PLN EQ GC_X.


  IF P_VERSN IS INITIAL.
    " & 필드는 필수입니다.
    MESSAGE E026 WITH '계획버젼'(S07).
  ENDIF.


  SELECT COUNT(*)
    FROM TKA09
   WHERE KOKRS    EQ @P_KOKRS
     AND VERSN    EQ @P_VERSN
     AND PLANNING EQ @GC_X.

  IF SY-SUBRC NE 0.
    " & 필드값이 유효하지 않습니다.
    MESSAGE E027 WITH '계획버젼'(S07).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_DATA_RTN
*&---------------------------------------------------------------------*
FORM SELECTED_DATA_RTN .


  ZCL_CO_COMMON=>SET_KOKRS( P_KOKRS ).

  PERFORM CLEAR_ITAB.
  PERFORM SELECT_TKA01.
  PERFORM SELECT_CSKS.
  PERFORM SELECT_COSX USING 'COSP'.
  PERFORM SELECT_COSX USING 'COSS'.
  PERFORM SELECT_HIERARCHY.

  PERFORM MAKE_DISPLAY_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CLEAR_ITAB
*&---------------------------------------------------------------------*
FORM CLEAR_ITAB .

  REFRESH: GT_DATA,
           GT_DISPLAY,
           GT_COSP,
           GT_COSS.

  CLEAR GS_TKA01.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_TKA01
*&---------------------------------------------------------------------*
FORM SELECT_TKA01 .

  CLEAR GS_TKA01.

  SELECT SINGLE
         KOKRS,
         BEZEI,
         WAERS
    FROM TKA01
   WHERE KOKRS EQ @P_KOKRS
    INTO CORRESPONDING FIELDS OF @GS_TKA01.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_CSKS
*&---------------------------------------------------------------------*
FORM SELECT_CSKS .

  SELECT A~KOSTL,
         A~DATBI,
         A~DATAB,
         A~BUKRS,
         A~OBJNR,
         B~KTEXT,
         C~BUTXT
    FROM CSKS AS A LEFT JOIN CSKT AS B
                   ON  B~SPRAS EQ @SY-LANGU
                   AND B~KOKRS EQ A~KOKRS
                   AND B~KOSTL EQ A~KOSTL
                   AND B~DATBI EQ A~DATBI
                   LEFT JOIN T001 AS C
                   ON  C~BUKRS EQ A~BUKRS
   WHERE A~KOKRS EQ @P_KOKRS
     AND A~BUKRS IN @S_BUKRS
    INTO TABLE @GT_DATA.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_COSP
*&---------------------------------------------------------------------*
FORM SELECT_COSX USING VALUE(PV_TABNAME) TYPE TABNAME.

  DATA: LT_SELECT_FIELDS TYPE TABLE OF FIELDNAME,
        LR_OBJNR         TYPE RANGE OF COSP-OBJNR WITH HEADER LINE,
        LR_WRTTP         TYPE RANGE OF COSP-WRTTP,
        LR_VERSN         TYPE RANGE OF COSP-VERSN,
        LR_KSTAR         TYPE RANGE OF COSP-KSTAR,
        LV_TABNAME       TYPE TABNAME.

  FIELD-SYMBOLS <FS_COSX> TYPE STANDARD TABLE.
  FIELD-SYMBOLS <FS_LINE> TYPE FIELDNAME.

  CASE PV_TABNAME.
    WHEN 'COSP'.  ASSIGN GT_COSP TO <FS_COSX>.  CHECK SY-SUBRC EQ 0.
    WHEN 'COSS'.  ASSIGN GT_COSS TO <FS_COSX>.  CHECK SY-SUBRC EQ 0.
    WHEN OTHERS. EXIT.
  ENDCASE.

  CHECK GT_DATA[] IS NOT INITIAL.
  LOOP AT GT_DATA INTO GS_DATA.
    LR_OBJNR  = VALUE #(
      SIGN    = 'I'
      OPTION  = 'EQ'
      LOW     = GS_DATA-OBJNR
    ).

    APPEND LR_OBJNR.
  ENDLOOP.


  CASE GC_X.
    WHEN P_ACT.   LR_WRTTP = VALUE #( ( CONV #( 'IEQ04' ) ) ).
    WHEN P_PLN.   LR_WRTTP = VALUE #( ( CONV #( 'IEQ01' ) ) ).
                  LR_VERSN = VALUE #( ( CONV #( 'IEQ' && P_VERSN ) ) ).
  ENDCASE.


  SELECT RIGHT( FIELDNAME, 3 ) AS MONTH,
         FIELDNAME
    FROM DD03L
   WHERE TABNAME   EQ @PV_TABNAME
     AND FIELDNAME LIKE 'WKG0__'
   ORDER BY MONTH
    INTO TABLE @DATA(LT_DD03L).

  CHECK SY-SUBRC EQ 0.

  APPEND ' OBJNR' TO LT_SELECT_FIELDS.
  APPEND ',VRGNG' TO LT_SELECT_FIELDS.

  " 특정월만 조회대상 필드로 선정
  APPEND ', SUM( CAST( ( ' TO LT_SELECT_FIELDS.
  LOOP AT LT_DD03L INTO DATA(LS_DD03L) WHERE MONTH IN S_POPER.
    CONCATENATE LS_DD03L-FIELDNAME '+'
           INTO LS_DD03L-FIELDNAME SEPARATED BY SPACE.

    APPEND LS_DD03L-FIELDNAME TO LT_SELECT_FIELDS ASSIGNING <FS_LINE>.

    IF GV_MONTH_MIN > LS_DD03L-MONTH.
      GV_MONTH_MIN = LS_DD03L-MONTH.
    ENDIF.

    IF GV_MONTH_MAX < LS_DD03L-MONTH.
      GV_MONTH_MAX = LS_DD03L-MONTH.
    ENDIF.
  ENDLOOP.

  CHECK <FS_LINE> IS ASSIGNED.
  REPLACE ' +' IN <FS_LINE> WITH SPACE.
  APPEND ' ) AS DEC( 31, 2 ) ) )' TO LT_SELECT_FIELDS.
  APPEND ' AS WKGSUM' TO LT_SELECT_FIELDS.



  IF P_EXCLDM EQ GC_X.
    LR_KSTAR = VALUE #(
      ( SIGN = 'E' OPTION = 'EQ' LOW = '0504101003' ) " 구매가격차이-원재료
      ( SIGN = 'E' OPTION = 'EQ' LOW = '0504101013' ) " 가격변경차이-원재료
      ( SIGN = 'E' OPTION = 'BT' LOW = '0505000000' HIGH = '0505999999' ) " 제조 관련 계정
    ).
  ENDIF.


  SELECT (LT_SELECT_FIELDS)
    FROM (PV_TABNAME)
   WHERE LEDNR EQ '00'
     AND OBJNR IN @LR_OBJNR
     AND GJAHR EQ @P_GJAHR
     AND WRTTP IN @LR_WRTTP
     AND VERSN IN @LR_VERSN
     AND KSTAR IN @LR_KSTAR
     AND VRGNG NE 'SDOR'
   GROUP BY OBJNR, VRGNG
    INTO CORRESPONDING FIELDS OF TABLE @<FS_COSX>.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM MAKE_DISPLAY_DATA .

  SORT GT_DATA BY BUKRS KOSTL.
  SORT GT_COSP BY OBJNR VRGNG.
  SORT GT_COSS BY OBJNR VRGNG.

  LOOP AT GT_DATA INTO GS_DATA.

    GS_DISPLAY = CORRESPONDING #( GS_DATA ).
    GS_DISPLAY-STATUS = ICON_LED_GREEN.

    PERFORM SET_AMOUNT TABLES GT_COSP.
    PERFORM SET_AMOUNT TABLES GT_COSS.

    IF GS_DISPLAY-AMT_B NE 0.
      GS_DISPLAY-STATUS   = ICON_LED_RED.
      GS_DISPLAY-MESSAGE  = '남은 잔고가 존재합니다.'(E01).
    ELSEIF P_NOZERO EQ 'X' OR ( GS_DISPLAY-AMT_P EQ 0
                            AND GS_DISPLAY-AMT_D EQ 0
*                            AND GS_DISPLAY-AMT_S EQ 0
                            AND GS_DISPLAY-AMT_B EQ 0 ).
      CONTINUE.
    ENDIF.


    PERFORM SET_HIERARCHY_INFO.


    APPEND GS_DISPLAY TO GT_DISPLAY.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_AMOUNT
*&---------------------------------------------------------------------*
FORM SET_AMOUNT  TABLES PT_COSX .

  FIELD-SYMBOLS: <FS_WA>,
                 <FS_OBJNR>,
                 <FS_VRGNG>,
                 <FS_WKGSUM>.


  READ TABLE PT_COSX ASSIGNING <FS_WA>
                     WITH KEY ('OBJNR') = GS_DATA-OBJNR
                              BINARY SEARCH.
  CHECK SY-SUBRC EQ 0.


  " 필드 점검
  ASSIGN COMPONENT 'VRGNG'  OF STRUCTURE <FS_WA> TO <FS_VRGNG>.
  CHECK SY-SUBRC EQ 0.
  UNASSIGN <FS_VRGNG>.

  ASSIGN COMPONENT 'WKGSUM' OF STRUCTURE <FS_WA> TO <FS_WKGSUM>.
  CHECK SY-SUBRC EQ 0.
  UNASSIGN <FS_WKGSUM>.


  LOOP AT PT_COSX ASSIGNING <FS_WA> FROM SY-TABIX.

    ASSIGN COMPONENT 'OBJNR' OF STRUCTURE <FS_WA> TO <FS_OBJNR>.
    IF <FS_OBJNR> EQ GS_DATA-OBJNR.
      UNASSIGN <FS_OBJNR>.
    ELSE.
      EXIT.
    ENDIF.

    ASSIGN COMPONENT 'VRGNG'  OF STRUCTURE <FS_WA> TO <FS_VRGNG>.
    ASSIGN COMPONENT 'WKGSUM' OF STRUCTURE <FS_WA> TO <FS_WKGSUM>.

    ADD <FS_WKGSUM> TO GS_DISPLAY-AMT_B.
    ADD <FS_WKGSUM> TO GS_DISPLAY-AMT_P.

    CASE <FS_VRGNG>.
*      WHEN 'KOAO'
*        OR 'KOAP'. " 정산
*        ADD       <FS_WKGSUM> TO   GS_DISPLAY-AMT_S.
*        SUBTRACT  <FS_WKGSUM> FROM GS_DISPLAY-AMT_P.

      WHEN 'RKIV'
        OR 'RKPV'
        OR 'KSPA'
        OR 'KSPB'. " 배부

        ADD       <FS_WKGSUM> TO   GS_DISPLAY-AMT_D.
        SUBTRACT  <FS_WKGSUM> FROM GS_DISPLAY-AMT_P.
      WHEN OTHERS.
    ENDCASE.

    UNASSIGN <FS_VRGNG>.
    UNASSIGN <FS_WKGSUM>.
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

*--------------------------------------------------------------------*
  LV_TEXT1 = '관리회계영역 : '(L01).
  LV_TEXT2 = P_KOKRS.

  LR_TABLE->NEW_ROW( ).
  LR_COLUMN1->ADD_TEXT( TEXT = LV_TEXT1 SAP_EMPHASIS = CL_DD_AREA=>STRONG ).
  LR_COLUMN2->ADD_TEXT( TEXT = LV_TEXT2 ).
*--------------------------------------------------------------------*
  LV_TEXT1 = '기간 : '(L02).
  LV_TEXT2 = SPACE.

  LOOP AT S_POPER.
    IF SY-TABIX NE 1.
      CONCATENATE LV_TEXT2 ',' INTO LV_TEXT2 SEPARATED BY SPACE.
    ENDIF.

    IF S_POPER-HIGH IS INITIAL.
      LV_TEXT2 = |{ LV_TEXT2 } { P_GJAHR }.{ S_POPER-LOW+1(2) }|.
    ELSE.
      LV_TEXT2 = |{ LV_TEXT2 } { P_GJAHR }.{ S_POPER-LOW+1(2) } ~ { P_GJAHR }.{ S_POPER-HIGH+1(2) }|.
    ENDIF.

  ENDLOOP.

  CONDENSE LV_TEXT2.

  LR_TABLE->NEW_ROW( ).
  LR_COLUMN1->ADD_TEXT( TEXT = LV_TEXT1 SAP_EMPHASIS = CL_DD_AREA=>STRONG ).
  LR_COLUMN2->ADD_TEXT( TEXT = LV_TEXT2 ).
*--------------------------------------------------------------------*
  LV_TEXT1 = '옵션 : '.
  LV_TEXT2 = SPACE.

  CASE GC_X.
    WHEN P_ACT. LV_TEXT2 = |실적|.
    WHEN P_PLN. LV_TEXT2 = |계획 / 계획버젼: { P_VERSN }|.
  ENDCASE.

  IF P_NOZERO EQ GC_X.
    LV_TEXT2 = |{ LV_TEXT2 } / 잔고 '0'인 WBS 제외|.
  ENDIF.

  IF P_EXCLDM EQ GC_X.
    LV_TEXT2 = |{ LV_TEXT2 } / 제조 제외|.
  ENDIF.

  LR_TABLE->NEW_ROW( ).
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

  GR_ALV->SET_LAYOUT(
    I_TYPE       = 'B'
    I_STYLEFNAME = 'STYLE'
    I_CTAB_FNAME = 'COLOR'
  ).

  GR_ALV->SET_SORT( IT_FIELD = VALUE #( ( 'BUKRS' )
                                        ( 'BUTXT' )
                                        ( 'KOSTL' )
                                        ( 'KTEXT' ) ) ).

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
    CLEAR LS_FIELDCAT-COL_OPT.

*-- Field 속성
    CASE LS_FIELDCAT-FIELDNAME.
      WHEN 'STATUS'.
        LS_FIELDCAT-OUTPUTLEN = 4.
        LS_FIELDCAT-ICON      = GC_X.
*        LS_FIELDCAT-EMPHASIZE = 'C100'.
        LS_FIELDCAT-JUST      = GC_C.
        LS_FIELDCAT-FIX_COLUMN = GC_X.

      WHEN 'BUKRS'.
        LS_FIELDCAT-OUTPUTLEN = 5.
*        LS_FIELDCAT-KEY       = GC_X.
        LS_FIELDCAT-JUST      = GC_C.
        LS_FIELDCAT-FIX_COLUMN = GC_X.

      WHEN 'BUTXT'.
        LS_FIELDCAT-OUTPUTLEN = 25.
*        LS_FIELDCAT-EMPHASIZE = 'C500'.
        LS_FIELDCAT-FIX_COLUMN = GC_X.
        LS_FIELDCAT-NO_OUT     = GC_X.

      WHEN 'KOSTL'.
        LS_FIELDCAT-OUTPUTLEN = 10.
*        LS_FIELDCAT-KEY       = GC_X.
        LS_FIELDCAT-HOTSPOT   = GC_X.
        LS_FIELDCAT-FIX_COLUMN = GC_X.

      WHEN 'KTEXT'.
        LS_FIELDCAT-COL_OPT    = GC_X.
*        LS_FIELDCAT-OUTPUTLEN = 30.
*        LS_FIELDCAT-EMPHASIZE = 'C500'.
        LS_FIELDCAT-FIX_COLUMN = GC_X.

      WHEN 'SETID1'.
        LS_FIELDCAT-COL_OPT    = GC_X.
        LS_FIELDCAT-HOTSPOT    = GC_X.
      WHEN 'SETTX1'.
        LS_FIELDCAT-COL_OPT    = GC_X.
      WHEN 'SETID2'.
        LS_FIELDCAT-COL_OPT    = GC_X.
      WHEN 'SETTX2'.
        LS_FIELDCAT-COL_OPT    = GC_X.

      WHEN 'AMT_P'.
        LS_FIELDCAT-OUTPUTLEN = 14.
        LS_FIELDCAT-CURRENCY  = GS_TKA01-WAERS.
        LS_FIELDCAT-NO_ZERO   = GC_X.

      WHEN 'AMT_D'.
        LS_FIELDCAT-OUTPUTLEN = 14.
        LS_FIELDCAT-CURRENCY  = GS_TKA01-WAERS.
        LS_FIELDCAT-NO_ZERO   = GC_X.

*      WHEN 'AMT_S'.
*        LS_FIELDCAT-OUTPUTLEN = 14.
*        LS_FIELDCAT-CURRENCY  = GS_TKA01-WAERS.
*        LS_FIELDCAT-NO_ZERO   = GC_X.

      WHEN 'AMT_B'.
        LS_FIELDCAT-OUTPUTLEN = 14.
        LS_FIELDCAT-EMPHASIZE = 'C300'.
        LS_FIELDCAT-HOTSPOT   = GC_X.
        LS_FIELDCAT-CURRENCY  = GS_TKA01-WAERS.
        LS_FIELDCAT-NO_ZERO   = GC_X.

      WHEN 'MESSAGE'.
        LS_FIELDCAT-OUTPUTLEN = 20.

      WHEN 'STYLE'
        OR 'COLOR'.
        LS_FIELDCAT-TECH      = GC_X.

      WHEN OTHERS.
*        LS_FIELDCAT-TECH = GC_X.

    ENDCASE.

*-- Field 텍스트
    CASE LS_FIELDCAT-FIELDNAME.
      WHEN 'STATUS'.       LV_TEXT = '구분'.
      WHEN 'BUKRS'.        LV_TEXT = '회사'.
      WHEN 'BUTXT'.        LV_TEXT = '회사명'.
      WHEN 'KOSTL'.        LV_TEXT = '코스트센터'.
      WHEN 'KTEXT'.        LV_TEXT = '코스트센터이름'.
      WHEN 'SETID1'.        LV_TEXT = '상위그룹'.
      WHEN 'SETTX1'.        LV_TEXT = '상위그룹내역'.
      WHEN 'SETID2'.        LV_TEXT = '하위그룹'.
      WHEN 'SETTX2'.        LV_TEXT = '하위그룹내역'.
      WHEN 'AMT_P'.        LV_TEXT = '기표금액'.
      WHEN 'AMT_D'.        LV_TEXT = '배부금액'.
*      WHEN 'AMT_S'.        LV_TEXT = '정산금액'.
      WHEN 'AMT_B'.        LV_TEXT = '잔고금액'.
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

  SET HANDLER GR_EVENT_RECEIVER->ON_HOTSPOT_CLICK FOR GR_ALV->MR_ALV_GRID.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM HANDLE_HOTSPOT_CLICK  USING PS_ROW_ID     TYPE LVC_S_ROW
                                 PS_COLUMN_ID  TYPE LVC_S_COL
                                 PS_ROW_NO     TYPE LVC_S_ROID
                                 PR_SENDER     TYPE REF TO CL_GUI_ALV_GRID.

  CASE PR_SENDER.
    WHEN GR_ALV->MR_ALV_GRID.

      READ TABLE GT_DISPLAY INTO GS_DISPLAY INDEX PS_ROW_ID-INDEX.
      CHECK SY-SUBRC EQ 0.

      CASE PS_COLUMN_ID-FIELDNAME.
        WHEN 'KOSTL'.

          SET PARAMETER ID 'KOS' FIELD GS_DISPLAY-KOSTL.
          CALL TRANSACTION 'KS03' AND SKIP FIRST SCREEN.

        WHEN 'SETID1'.

          SET PARAMETER ID 'HNA' FIELD 'ZBU'.
          CALL TRANSACTION 'KSH3' AND SKIP FIRST SCREEN.

        WHEN 'AMT_B'.

          DATA LV_PROGNAME  TYPE PROGNAME.
          DATA LV_VERSN     TYPE COSP-VERSN.
          DATA LR_KOSTL     TYPE RANGE OF CSKS-KOSTL.
          DATA LR_KSTAR     TYPE RANGE OF COSP-KSTAR.


          ZCL_CO_COMMON=>GET_REPORT_PROGNAME(
            EXPORTING
              I_RGJNR     = '1SIP'
              I_VERSN     = '00000001'
              I_PROGCLASS = 'RW_EXECUTION'
            IMPORTING
              E_PROGNAME  = LV_PROGNAME
            EXCEPTIONS
              REPORT_PROGRAM_NOT_FOUND = 1
              OTHERS                   = 2
          ).

          CHECK LV_PROGNAME IS NOT INITIAL
            AND SY-SUBRC    EQ 0.



          IF P_VERSN IS INITIAL.
            LV_VERSN = '000'.
          ELSE.
            LV_VERSN = P_VERSN.
          ENDIF.

          LR_KOSTL = VALUE #( ( SIGN    = 'I'
                                OPTION  = 'EQ'
                                LOW     = GS_DISPLAY-KOSTL ) ).


          SET PARAMETER ID 'ZPROG' FIELD SY-CPROG.

          CASE SY-MANDT.
            WHEN '100'.
              SUBMIT GP4QLQKH46VZJ3P1A40A23S3HJG100
                WITH $1KOKRE  EQ P_KOKRS
                WITH $1GJAHR  EQ P_GJAHR
                WITH $1PERIV  EQ GV_MONTH_MIN
                WITH $1PERIB  EQ GV_MONTH_MAX
                WITH $1VERP   EQ LV_VERSN
                WITH $1KOSET  EQ SPACE
                WITH _1KOSET  IN LR_KOSTL
                WITH $1KSTAR  EQ SPACE
                WITH _1KSTAR  IN LR_KSTAR
                AND RETURN .
            WHEN '200'.
              SUBMIT GP4QLQKH46VZJ3P1A40A23S3HJG200
                WITH $1KOKRE  EQ P_KOKRS
                WITH $1GJAHR  EQ P_GJAHR
                WITH $1PERIV  EQ GV_MONTH_MIN
                WITH $1PERIB  EQ GV_MONTH_MAX
                WITH $1VERP   EQ LV_VERSN
                WITH $1KOSET  EQ SPACE
                WITH _1KOSET  IN LR_KOSTL
                WITH $1KSTAR  EQ SPACE
                WITH _1KSTAR  IN LR_KSTAR
                AND RETURN .
            WHEN '400'.
              SUBMIT GP4QLQKH46VZJ3P1A40A23S3HJG400
                WITH $1KOKRE  EQ P_KOKRS
                WITH $1GJAHR  EQ P_GJAHR
                WITH $1PERIV  EQ GV_MONTH_MIN
                WITH $1PERIB  EQ GV_MONTH_MAX
                WITH $1VERP   EQ LV_VERSN
                WITH $1KOSET  EQ SPACE
                WITH _1KOSET  IN LR_KOSTL
                WITH $1KSTAR  EQ SPACE
                WITH _1KSTAR  IN LR_KSTAR
                AND RETURN .
            WHEN OTHERS.
              SUBMIT (LV_PROGNAME)
                WITH $1KOKRE  EQ P_KOKRS
                WITH $1GJAHR  EQ P_GJAHR
                WITH $1PERIV  EQ GV_MONTH_MIN
                WITH $1PERIB  EQ GV_MONTH_MAX
                WITH $1VERP   EQ LV_VERSN
                WITH $1KOSET  EQ SPACE
                WITH _1KOSET  IN LR_KOSTL
                WITH $1KSTAR  EQ SPACE
                WITH _1KSTAR  IN LR_KSTAR
                AND RETURN.
          ENDCASE.
*
*          SET PARAMETER ID 'CAC' FIELD P_KOKRS.
*          SET PARAMETER ID 'GJR' FIELD P_GJAHR.
*          SET PARAMETER ID 'KVS' FIELD LV_VERSN.
*          SET PARAMETER ID 'KSG' FIELD SPACE.
*          SET PARAMETER ID 'KOS' FIELD GS_DISPLAY-KOSTL.
*          SET PARAMETER ID 'KAG' FIELD SPACE.
*          CALL TRANSACTION 'S_ALR_87013611'.

        WHEN OTHERS.
      ENDCASE.


  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_HIERARCHY
*&---------------------------------------------------------------------*
FORM SELECT_HIERARCHY .

  REFRESH : GT_SETH_NODE,
            GT_SETH_VAL.

  CALL METHOD ZCL_CO_COMMON=>GET_HIERARCHY_LIST
    EXPORTING
      I_CLASS        = '0101'
      I_KOKRS        = P_KOKRS
      I_SETNAME      = 'ZBU'
    IMPORTING
      ET_NODES       = GT_SETH_NODE
      ET_VALUES      = GT_SETH_VAL
    EXCEPTIONS
      NO_INPUT_CLASS = 1
      OTHERS         = 2.

  CHECK SY-SUBRC EQ 0.

  SORT GT_SETH_NODE BY SETID.
  SORT GT_SETH_VAL  BY SETID.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_HIERARCHY_INFO
*&---------------------------------------------------------------------*
FORM SET_HIERARCHY_INFO .

  DATA LV_TABIX TYPE SY-TABIX.


  LOOP AT GT_SETH_VAL INTO DATA(LS_VALUE).
    CHECK GS_DISPLAY-KOSTL BETWEEN LS_VALUE-VFROM
                               AND LS_VALUE-VTO.

    GS_DISPLAY-SETID2 = LS_VALUE-SETID+8.

    READ TABLE GT_SETH_NODE INTO DATA(LS_NODE)
                            WITH KEY SETID = LS_VALUE-SETID
                                     BINARY SEARCH.

    CHECK SY-SUBRC EQ 0.
    LV_TABIX = SY-TABIX.
    GS_DISPLAY-SETTX2 = LS_NODE-DESCRIPT.

    DO.
      IF LV_TABIX EQ 1.
        EXIT.
      ENDIF.

      LV_TABIX = LV_TABIX - 1.
      READ TABLE GT_SETH_NODE INTO DATA(LS_SUPER) INDEX LV_TABIX.

      CHECK LS_SUPER-HLEVEL < LS_NODE-HLEVEL.

      GS_DISPLAY-SETID1 = LS_SUPER-SETID+8.
      GS_DISPLAY-SETTX1 = LS_SUPER-DESCRIPT.

      EXIT.

    ENDDO.

    EXIT.

  ENDLOOP.

ENDFORM.
