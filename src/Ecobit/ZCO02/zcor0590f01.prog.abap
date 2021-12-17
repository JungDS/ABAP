*&---------------------------------------------------------------------*
*& Include          ZCOR0590F01
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

  " Selection Screen 텍스트
  TEXT_S01 = '실행조건'(S01).
  TEXT_S03 = '실적/계획 선택'(S02).

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
  GS_FUNTXT-TEXT = '코스트센터그룹(ZBU)'.

  SSCRFIELDS-FUNCTXT_02 = GS_FUNTXT.

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
    WHEN 'FC02'.
      ZCL_CO_COMMON=>SET_KOKRS( P_KOKRS ).
      SET PARAMETER ID 'CAC' FIELD P_KOKRS.
      SET PARAMETER ID 'HNA' FIELD 'ZBU'.
      CALL TRANSACTION 'KSH2'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_DATA_RTN
*&---------------------------------------------------------------------*
FORM SELECTED_DATA_RTN .

  ZCL_CO_COMMON=>SET_KOKRS( P_KOKRS ).

  PERFORM CLEAR_ITAB.
  PERFORM SET_RANGES_CYCLE.
  PERFORM SELECT_T811.
  PERFORM SELECT_TEXT.

  PERFORM MAKE_DISPLAY_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CLEAR_ITAB
*&---------------------------------------------------------------------*
FORM CLEAR_ITAB .

  REFRESH: GT_T811C,
           GT_T811S,
           GT_T811K,
           GT_T811L,
           GT_DISPLAY,
           GT_SETHDRT,
           GT_CSKT,
           GT_CSKU,
           GT_T001,
           GT_T25A1,
           GT_TGSBT,
           GT_1090T,
           GT_DD04T,
           GT_TKCTK,
           GT_TKB9B,
           GT_PRPS,
           GT_PROJ.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_T811
*&---------------------------------------------------------------------*
FORM SELECT_T811.

  PERFORM SELECT_T811C.
  PERFORM SELECT_T811S.
  PERFORM SELECT_T811K.
  PERFORM SELECT_T811L.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_T811C
*&---------------------------------------------------------------------*
FORM SELECT_T811C .

  DATA LV_IPKNZ LIKE T811C-IPKNZ.

  IF P_ACT EQ GC_X.
    LV_IPKNZ = GC_I.  " 실적
  ELSE.
    LV_IPKNZ = GC_P.  " 계획
  ENDIF.

  REFRESH GT_T811C.

  SELECT *
    FROM T811C
   WHERE TAB    EQ @GC_TAB_CE71000
     AND CYCLE  IN @R_CYCLE
     AND IPKNZ  EQ @LV_IPKNZ
    INTO CORRESPONDING FIELDS OF TABLE @GT_T811C.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_T811C
*&---------------------------------------------------------------------*
FORM SELECT_T811S .

  CHECK GT_T811C[] IS NOT INITIAL.

  SELECT *
    FROM T811S
   WHERE TAB   EQ @GC_TAB_CE71000
     AND CYCLE IN @R_CYCLE
    INTO CORRESPONDING FIELDS OF TABLE @GT_T811S.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_T811C
*&---------------------------------------------------------------------*
FORM SELECT_T811K .

  CHECK GT_T811C[] IS NOT INITIAL.

  SELECT *
    FROM T811K
   WHERE TAB   EQ @GC_TAB_CE71000
     AND CYCLE IN @R_CYCLE
    INTO CORRESPONDING FIELDS OF TABLE @GT_T811K.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_T811C
*&---------------------------------------------------------------------*
FORM SELECT_T811L .

  CHECK GT_T811C[] IS NOT INITIAL.

  SELECT *
    FROM T811L
   WHERE LANGU EQ @SY-LANGU
     AND TAB   EQ @GC_TAB_CE71000
     AND CYCLE IN @R_CYCLE
    INTO CORRESPONDING FIELDS OF TABLE @GT_T811L.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM MAKE_DISPLAY_DATA .


  SORT GT_T811C BY CYCLE SDATE.
  SORT GT_T811S BY CYCLE SDATE SEQNR.
  SORT GT_T811K BY CYCLE SDATE SEQNR FIELD POS.
  SORT GT_T811L BY CYCLE SDATE SEQNR.


  LOOP AT GT_T811C INTO GS_T811C.

    GS_DISPLAY = CORRESPONDING #( GS_T811C ).
    GS_DISPLAY-CATEG = GS_T811C-CYCLE+4(2).
    GS_DISPLAY-CYCLE_OUT = GS_T811C-CYCLE+4.

    CLEAR GS_T811L.
    READ TABLE GT_T811L INTO GS_T811L
                        WITH KEY CYCLE = GS_T811C-CYCLE
                                 SDATE = GS_T811C-SDATE
                                 SEQNR = '0000'
                                 BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GS_DISPLAY-CYCLE_TXT = GS_T811L-TXT.
    ENDIF.


    PERFORM MAKE_DISPLAY_DATA_T811S CHANGING SY-SUBRC.

    IF SY-SUBRC NE 0.
      APPEND GS_DISPLAY TO GT_DISPLAY.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_DISPLAY_DATA_T811S
*&---------------------------------------------------------------------*
FORM MAKE_DISPLAY_DATA_T811S CHANGING VALUE(PV_SUBRC).

  READ TABLE GT_T811S TRANSPORTING NO FIELDS
                      WITH KEY CYCLE = GS_T811C-CYCLE
                               SDATE = GS_T811C-SDATE
                               BINARY SEARCH.

  PV_SUBRC = SY-SUBRC. CHECK PV_SUBRC EQ 0.


  DATA(LS_DISPLAY) = GS_DISPLAY.

  LOOP AT GT_T811S INTO GS_T811S FROM SY-TABIX.
    IF NOT ( GS_T811S-CYCLE EQ GS_T811C-CYCLE
         AND GS_T811S-SDATE EQ GS_T811C-SDATE ).
      EXIT.
    ENDIF.

    GS_DISPLAY = CORRESPONDING #(
      BASE ( LS_DISPLAY ) GS_T811S MAPPING SEGMENT = NAME
    ).

    READ TABLE GT_T811L INTO GS_T811L
                        WITH KEY CYCLE = GS_T811S-CYCLE
                                 SDATE = GS_T811S-SDATE
                                 SEQNR = GS_T811S-SEQNR
                                 BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GS_DISPLAY-SEGMENT_TXT = GS_T811L-TXT.
    ENDIF.

    PERFORM SET_TEXT_RCDATA USING GS_DISPLAY-RCDATA
                                  GS_DISPLAY-RCDATA_TXT.
    PERFORM SET_TEXT_ERSCH  USING GS_DISPLAY-ERSCH
                                  GS_DISPLAY-ERSCH_TXT.


    PERFORM MAKE_DISPLAY_DATA_T811K.

    APPEND GS_DISPLAY TO GT_DISPLAY.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_DISPLAY_DATA_T811K
*&---------------------------------------------------------------------*
FORM MAKE_DISPLAY_DATA_T811K.


  READ TABLE GT_T811K TRANSPORTING NO FIELDS
                      WITH KEY CYCLE = GS_T811S-CYCLE
                               SDATE = GS_T811S-SDATE
                               SEQNR = GS_T811S-SEQNR
                               BINARY SEARCH.
  CHECK SY-SUBRC EQ 0.


  LOOP AT GT_T811K INTO GS_T811K FROM SY-TABIX.
    IF NOT ( GS_T811K-CYCLE EQ GS_T811S-CYCLE
         AND GS_T811K-SDATE EQ GS_T811S-SDATE
         AND GS_T811K-SEQNR EQ GS_T811S-SEQNR ).
      EXIT.
    ENDIF.

    PERFORM SET_FIELD_COND.

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
  PERFORM ADD_COLUMN USING LR_TABLE LR_COL_L '450px'.
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
  LV_LABEL = '관리회계영역'(L01).
  LV_CONDI = P_KOKRS.
  PR_COL_I->ADD_ICON( 'ICON_PARAMETER' ).
  PR_COL_L->ADD_TEXT( TEXT = LV_LABEL ).
  PR_COL_C->ADD_TEXT( TEXT = LV_CONDI ).
*--------------------------------------------------------------------*
  LV_LABEL = '배부사이클'(L02).
  __ADD_SELOPT_TEXT LV_CONDI S_CYCLE.

  PR_TABLE->NEW_ROW( ).
  PR_COL_I->ADD_ICON( COND #( LET  LINES  = LINES( S_CYCLE ) IN
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
  LV_LABEL = '실적'(L03).

  PR_COL_I->ADD_ICON( COND #( WHEN P_ACT EQ GC_X
                              THEN 'ICON_WD_RADIO_BUTTON'
                              ELSE 'ICON_WD_RADIO_BUTTON_EMPTY' ) ).
  PR_COL_L->ADD_TEXT( TEXT = LV_LABEL ).
*--------------------------------------------------------------------*
  LV_LABEL = '계획'(L04).

  PR_TABLE->NEW_ROW( ).
  PR_COL_I->ADD_ICON( COND #( WHEN P_PLN EQ GC_X
                              THEN 'ICON_WD_RADIO_BUTTON'
                              ELSE 'ICON_WD_RADIO_BUTTON_EMPTY' ) ).
  PR_COL_L->ADD_TEXT( TEXT = LV_LABEL ).
*--------------------------------------------------------------------*

*  PR_TABLE->NEW_ROW( ).
*  PR_TABLE->NEW_ROW( ).
*  PR_TABLE->NEW_ROW( ).

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
    I_TYPE       = 'A'
    I_STYLEFNAME = 'STYLE'
    I_CTAB_FNAME = 'COLOR'
  ).

  GR_ALV->SET_SORT( IT_FIELD = VALUE #( ( 'CATEG' )
                                        ( 'CYCLE' )
                                        ( 'CYCLE_OUT' )
                                        ( 'CYCLE_TXT' )
                                        ( 'SEQNR' )
                                        ( 'SEGMENT' )
                                        ( 'SEGMENT_TXT' )
                                        ( 'SDATE' )
                                        ( 'EDATE' )
                                        ) ).

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
  DATA LV_EMPHASIZE   TYPE LVC_S_FCAT-EMPHASIZE.
  DATA LV_FIX         TYPE C.

*  LV_EMPHASIZE = 'C110'.
  LV_FIX = GC_X.

  LOOP AT GR_ALV->MT_FIELDCAT INTO DATA(LS_FIELDCAT).

    CLEAR LV_TEXT.
    CLEAR LV_TOOLTIP.
    CLEAR LS_FIELDCAT-KEY.
    CLEAR LS_FIELDCAT-COL_OPT.

*-- 열고정
    LS_FIELDCAT-EMPHASIZE  = LV_EMPHASIZE.
    LS_FIELDCAT-FIX_COLUMN = LV_FIX.

    " 처음 ~ S.CC명 까지 틀고정
    IF LS_FIELDCAT-FIELDNAME EQ 'S_CC_TXT'.
      CLEAR LV_EMPHASIZE.
      CLEAR LV_FIX.
    ENDIF.


*-- Field 속성
    CASE LS_FIELDCAT-FIELDNAME.

      WHEN 'CYCLE'.
        LS_FIELDCAT-NO_OUT = GC_X.

      WHEN 'CYCLE_OUT'.
        LS_FIELDCAT-HOTSPOT = GC_X.

      WHEN 'S_CC_F' OR 'S_CC_T' OR 'S_CC_GRP'
        OR 'S_CE_F' OR 'S_CE_T' OR 'S_CE_GRP'

        OR 'R_BU_GRP'    " OR 'R_BU_F' OR 'R_BU_T'
        OR 'R_CD_GRP'    " OR 'R_CD_F' OR 'R_CD_T'
        OR 'R_BA_GRP'    " OR 'R_BA_F' OR 'R_BA_T'
        OR 'R_RG_GRP'    " OR 'R_RG_F' OR 'R_RG_T'
        OR 'R_WB_F' OR 'R_WB_T' OR 'R_WB_GRP'.

        LS_FIELDCAT-HOTSPOT = GC_X.
        LS_FIELDCAT-NO_ZERO = GC_X.


      WHEN 'S_CC_G'
        OR 'S_CE_G'
        OR 'R_BU_G'
        OR 'R_CD_G'
        OR 'R_BA_G'
        OR 'R_RG_G'
        OR 'R_WB_G'.

        LS_FIELDCAT-NO_OUT = GC_X.

      WHEN 'RCDATA'.
        LS_FIELDCAT-HOTSPOT = GC_X.

      WHEN 'STYLE'
        OR 'COLOR'.
        LS_FIELDCAT-TECH = GC_X.



    ENDCASE.

    CASE LS_FIELDCAT-FIELDNAME(2).
      WHEN 'S_'.
        LS_FIELDCAT-EMPHASIZE = 'C500'.
      WHEN 'R_'.
        LS_FIELDCAT-EMPHASIZE = 'C300'.
    ENDCASE.

*-- Field 텍스트
    CASE LS_FIELDCAT-FIELDNAME.

      WHEN 'CATEG'.       LV_TEXT = '구분'.
      WHEN 'CYCLE'.       LV_TEXT = '사이클'.
      WHEN 'CYCLE_TXT'.   LV_TEXT = '사이클명'.
      WHEN 'SEQNR'.       LV_TEXT = 'Seg.'.
      WHEN 'SEGMENT'.     LV_TEXT = '세그먼트'.
      WHEN 'SEGMENT_TXT'. LV_TEXT = '세그먼트명'.

      WHEN 'S_CC_F'.      LV_TEXT = 'S.CC_F'.
      WHEN 'S_CC_T'.      LV_TEXT = 'S.CC_T'.
      WHEN 'S_CC_GRP'.    LV_TEXT = 'S.CC_G'.
      WHEN 'S_CC_TXT'.    LV_TEXT = 'S.CC명'.
                          LV_TOOLTIP = 'Sender Cost Center or Group 이름'.

      WHEN 'S_CE_F'.      LV_TEXT = 'S.CE_F'.
      WHEN 'S_CE_T'.      LV_TEXT = 'S.CE_T'.
      WHEN 'S_CE_GRP'.    LV_TEXT = 'S.CE_G'.
      WHEN 'S_CE_TXT'.    LV_TEXT = 'S.CE명'.
                          LV_TOOLTIP = 'Sender Cost Element or Group 이름'.

      WHEN 'R_BU_F'.      LV_TEXT = 'R.BU_F'.
      WHEN 'R_BU_T'.      LV_TEXT = 'R.BU_T'.
      WHEN 'R_BU_GRP'.    LV_TEXT = 'R.BU_G'.
      WHEN 'R_BU_TXT'.    LV_TEXT = 'R.BU명'.
                          LV_TOOLTIP = 'Receiver BU구분 or Group 이름'.

      WHEN 'R_CD_F'.      LV_TEXT = 'R.CD_F'.
      WHEN 'R_CD_T'.      LV_TEXT = 'R.CD_T'.
      WHEN 'R_CD_GRP'.    LV_TEXT = 'R.CD_G'.
      WHEN 'R_CD_TXT'.    LV_TEXT = 'R.CD명'.
                          LV_TOOLTIP = 'Receiver 회사코드 or Group 이름'.

      WHEN 'R_BA_F'.      LV_TEXT = 'R.BA_F'.
      WHEN 'R_BA_T'.      LV_TEXT = 'R.BA_T'.
      WHEN 'R_BA_GRP'.    LV_TEXT = 'R.BA_G'.
      WHEN 'R_BA_TXT'.    LV_TEXT = 'R.BA명'.
                          LV_TOOLTIP = 'Receiver 사업영역 or Group 이름'.

      WHEN 'R_RG_F'.      LV_TEXT = 'R.RG_F'.
      WHEN 'R_RG_T'.      LV_TEXT = 'R.RG_T'.
      WHEN 'R_RG_GRP'.    LV_TEXT = 'R.RG_G'.
      WHEN 'R_RG_TXT'.    LV_TEXT = 'R.RG명'.
                          LV_TOOLTIP = 'Receiver 행정구역 or Group 이름'.

      WHEN 'R_WB_F'.      LV_TEXT = 'R.WBS_F'.
      WHEN 'R_WB_T'.      LV_TEXT = 'R.WBS_T'.
      WHEN 'R_WB_GRP'.    LV_TEXT = 'R.WBS_G'.
      WHEN 'R_WB_TXT'.    LV_TEXT = 'R.WBS명'.
                          LV_TOOLTIP = 'Receiver WBS or Group 이름'.

      WHEN 'RCDATA'.      LV_TEXT = '배부기준'.
      WHEN 'RCDATA_TXT'.  LV_TEXT = '배부기준명'.
      WHEN 'VERSN'.       LV_TEXT = '버전'.
      WHEN 'SPERCENT'.    LV_TEXT = '센더%'.
      WHEN 'ABSCH'.       LV_TEXT = '배부구조'.
      WHEN 'ERSCH'.       LV_TEXT = 'PA전송'.
      WHEN 'ERSCH_TXT'.   LV_TEXT = 'PA전송구조명'.
      WHEN 'SDATE'.       LV_TEXT = '시작일'.
      WHEN 'EDATE'.       LV_TEXT = '종료일'.
      WHEN 'CRDATE'.      LV_TEXT = '생성일'.
      WHEN 'MODDATE'.     LV_TEXT = '최종변경일'.
      WHEN 'MODUSER'.     LV_TEXT = '최종변경자'.
      WHEN 'LASTEXEC'.    LV_TEXT = '최종수행일'.
      WHEN 'PROC_GROUP'.  LV_TEXT = '실행그룹'.

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

      ASSIGN COMPONENT PS_COLUMN_ID-FIELDNAME
          OF STRUCTURE GS_DISPLAY
          TO FIELD-SYMBOL(<FS_VALUE>).

      CHECK SY-SUBRC EQ 0 AND <FS_VALUE> IS NOT INITIAL.

      CASE PS_COLUMN_ID-FIELDNAME(5).
        WHEN 'CYCLE'.
          SET PARAMETER ID 'ERB' FIELD P_KOKRS.
          SET PARAMETER ID 'KCY' FIELD <FS_VALUE>.

          IF P_ACT EQ GC_X.
            " 실적변경
            CALL TRANSACTION 'KEU2' AND SKIP FIRST SCREEN.
          ELSE.
            " 계획변경
            CALL TRANSACTION 'KEU8' AND SKIP FIRST SCREEN.
          ENDIF.

        WHEN 'S_CC_'.
          IF PS_COLUMN_ID-FIELDNAME+5(3) EQ 'GRP'.
            SET PARAMETER ID 'CAC' FIELD P_KOKRS.
            SET PARAMETER ID 'HNA' FIELD <FS_VALUE>.
            CALL TRANSACTION 'KSH3' AND SKIP FIRST SCREEN.
          ELSE.
            SET PARAMETER ID 'KOS' FIELD <FS_VALUE>.
            CALL TRANSACTION 'KS03' AND SKIP FIRST SCREEN.
          ENDIF.

        WHEN 'S_CE_'.
          IF PS_COLUMN_ID-FIELDNAME+5(3) EQ 'GRP'.
            SET PARAMETER ID 'HNA' FIELD <FS_VALUE>.
            CALL TRANSACTION 'KAH3' AND SKIP FIRST SCREEN.
          ELSE.
            SET PARAMETER ID 'BUK' FIELD GS_DISPLAY-PROC_GROUP.
            SET PARAMETER ID 'SAK' FIELD <FS_VALUE>.
            CALL TRANSACTION 'FS00' AND SKIP FIRST SCREEN.
          ENDIF.

        WHEN 'R_WB_'.
          IF PS_COLUMN_ID-FIELDNAME+5(3) EQ 'GRP'.
            SET PARAMETER ID 'GSE' FIELD <FS_VALUE>.
            CALL TRANSACTION 'GS03' AND SKIP FIRST SCREEN.
          ELSE.
            SET PARAMETER ID 'PSP' FIELD SPACE.
            SET PARAMETER ID 'PRO' FIELD <FS_VALUE>.
            CALL TRANSACTION 'CJ03' AND SKIP FIRST SCREEN.
          ENDIF.

        WHEN 'RCDAT'.

          CHECK <FS_VALUE>(1) EQ 'Z'.

          SET PARAMETER ID 'KYR' FIELD <FS_VALUE>.
          CALL TRANSACTION 'KEUH' AND SKIP FIRST SCREEN.


        WHEN OTHERS.
          IF PS_COLUMN_ID-FIELDNAME+5(3) EQ 'GRP'.
            SET PARAMETER ID 'GSE' FIELD <FS_VALUE>.
            CALL TRANSACTION 'GS03' AND SKIP FIRST SCREEN.
          ENDIF.
      ENDCASE.



  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FIELD_COND
*&---------------------------------------------------------------------*
FORM SET_FIELD_COND .

  DATA: LV_SETCLASS LIKE SETHEADERT-SETCLASS,
        LV_SUBCLASS LIKE SETHEADERT-SUBCLASS.


  FIELD-SYMBOLS: <FS_F>   TYPE T811K-VALMIN,
                 <FS_T>   TYPE T811K-VALMAX,
                 <FS_G>   TYPE T811K-SETID,
                 <FS_GRP> TYPE SETHEADERT-SETNAME,
                 <FS_TXT> TYPE SETHEADERT-DESCRIPT.

  UNASSIGN: <FS_F>,
            <FS_T>,
            <FS_G>,
            <FS_TXT>.


  CASE GS_T811K-SETKIND.
    WHEN '2'. " Sender
      CASE GS_T811K-FIELD.
        WHEN 'KOSTL'.
          ASSIGN GS_DISPLAY-S_CC_F    TO <FS_F>.
          ASSIGN GS_DISPLAY-S_CC_T    TO <FS_T>.
          ASSIGN GS_DISPLAY-S_CC_G    TO <FS_G>.
          ASSIGN GS_DISPLAY-S_CC_GRP  TO <FS_GRP>.

          ASSIGN GS_DISPLAY-S_CC_TXT  TO <FS_TXT>.

          IF <FS_TXT> IS ASSIGNED AND GS_T811K-VALMIN IS NOT INITIAL.
            READ TABLE GT_CSKT TRANSPORTING NO FIELDS
                               WITH KEY KOSTL = GS_T811K-VALMIN
                                        BINARY SEARCH.
            IF SY-SUBRC EQ 0.
              LOOP AT GT_CSKT INTO GS_CSKT FROM SY-TABIX.
                IF GS_CSKT-KOSTL NE GS_T811K-VALMIN.
                  EXIT.
                ENDIF.

                CHECK GS_CSKT-DATBI > GS_DISPLAY-SDATE.
                <FS_TXT> = GS_CSKT-KTEXT.
                EXIT.
              ENDLOOP.
            ENDIF.
          ENDIF.

        WHEN 'KSTAR'.
          ASSIGN GS_DISPLAY-S_CE_F    TO <FS_F>.
          ASSIGN GS_DISPLAY-S_CE_T    TO <FS_T>.
          ASSIGN GS_DISPLAY-S_CE_G    TO <FS_G>.
          ASSIGN GS_DISPLAY-S_CE_GRP  TO <FS_GRP>.

          ASSIGN GS_DISPLAY-S_CE_TXT  TO <FS_TXT>.

          IF <FS_TXT> IS ASSIGNED AND GS_T811K-VALMIN IS NOT INITIAL.
            READ TABLE GT_CSKU INTO GS_CSKU
                               WITH KEY KSTAR = GS_T811K-VALMIN
                                        BINARY SEARCH.
            IF SY-SUBRC EQ 0.
              <FS_TXT> = GS_CSKU-KTEXT.
            ENDIF.
          ENDIF.

      ENDCASE.

    WHEN '3'. " Receiver

      CASE GS_T811K-FIELD.
        WHEN 'WW120'.
          ASSIGN GS_DISPLAY-R_BU_F    TO <FS_F>.
          ASSIGN GS_DISPLAY-R_BU_T    TO <FS_T>.
          ASSIGN GS_DISPLAY-R_BU_G    TO <FS_G>.
          ASSIGN GS_DISPLAY-R_BU_GRP  TO <FS_GRP>.

          ASSIGN GS_DISPLAY-R_BU_TXT  TO <FS_TXT>.

          IF <FS_TXT> IS ASSIGNED AND GS_T811K-VALMIN IS NOT INITIAL.
            READ TABLE GT_T25A1 INTO GS_T25A1
                                WITH KEY WW120 = GS_T811K-VALMIN
                                         BINARY SEARCH.
            IF SY-SUBRC EQ 0.
              <FS_TXT> = GS_T25A1-BEZEK.
            ENDIF.
          ENDIF.

        WHEN 'BUKRS'.
          ASSIGN GS_DISPLAY-R_CD_F    TO <FS_F>.
          ASSIGN GS_DISPLAY-R_CD_T    TO <FS_T>.
          ASSIGN GS_DISPLAY-R_CD_G    TO <FS_G>.
          ASSIGN GS_DISPLAY-R_CD_GRP  TO <FS_GRP>.

          ASSIGN GS_DISPLAY-R_CD_TXT  TO <FS_TXT>.

          IF <FS_TXT> IS ASSIGNED AND GS_T811K-VALMIN IS NOT INITIAL.
            READ TABLE GT_T001 INTO GS_T001
                                WITH KEY BUKRS = GS_T811K-VALMIN
                                         BINARY SEARCH.
            IF SY-SUBRC EQ 0.
              <FS_TXT> = GS_T001-BUTXT.
            ENDIF.
          ENDIF.

        WHEN 'GSBER'.
          ASSIGN GS_DISPLAY-R_BA_F    TO <FS_F>.
          ASSIGN GS_DISPLAY-R_BA_T    TO <FS_T>.
          ASSIGN GS_DISPLAY-R_BA_G    TO <FS_G>.
          ASSIGN GS_DISPLAY-R_BA_GRP  TO <FS_GRP>.

          ASSIGN GS_DISPLAY-R_BA_TXT  TO <FS_TXT>.

          IF <FS_TXT> IS ASSIGNED AND GS_T811K-VALMIN IS NOT INITIAL.
            READ TABLE GT_TGSBT INTO GS_TGSBT
                                WITH KEY GSBER = GS_T811K-VALMIN
                                         BINARY SEARCH.
            IF SY-SUBRC EQ 0.
              <FS_TXT> = GS_TGSBT-GTEXT.
            ENDIF.
          ENDIF.

        WHEN 'WW090'.
          ASSIGN GS_DISPLAY-R_RG_F    TO <FS_F>.
          ASSIGN GS_DISPLAY-R_RG_T    TO <FS_T>.
          ASSIGN GS_DISPLAY-R_RG_G    TO <FS_G>.
          ASSIGN GS_DISPLAY-R_RG_GRP  TO <FS_GRP>.

          ASSIGN GS_DISPLAY-R_RG_TXT  TO <FS_TXT>.

          IF <FS_TXT> IS ASSIGNED AND GS_T811K-VALMIN IS NOT INITIAL.
            READ TABLE GT_1090T INTO GS_1090T
                                WITH KEY ZZADT = GS_T811K-VALMIN
                                         BINARY SEARCH.
            IF SY-SUBRC EQ 0.
              <FS_TXT> = GS_1090T-ZZADTTX.
            ENDIF.
          ENDIF.

        WHEN 'COPA_POSID'.
          ASSIGN GS_DISPLAY-R_WB_F    TO <FS_F>.
          ASSIGN GS_DISPLAY-R_WB_T    TO <FS_T>.
          ASSIGN GS_DISPLAY-R_WB_G    TO <FS_G>.
          ASSIGN GS_DISPLAY-R_WB_GRP  TO <FS_GRP>.

          ASSIGN GS_DISPLAY-R_WB_TXT  TO <FS_TXT>.

          IF <FS_TXT> IS ASSIGNED AND GS_T811K-VALMIN IS NOT INITIAL.
            READ TABLE GT_PRPS INTO GS_PRPS
                                WITH KEY POSID = GS_T811K-VALMIN
                                         BINARY SEARCH.
            IF SY-SUBRC EQ 0.
              <FS_TXT> = GS_PRPS-POST1.
            ELSE.
              READ TABLE GT_PROJ INTO GS_PROJ
                                WITH KEY PSPID = GS_T811K-VALMIN
                                         BINARY SEARCH.
              IF SY-SUBRC EQ 0.
                <FS_TXT> = GS_PROJ-POST1.
              ENDIF.
            ENDIF.
          ENDIF.

      ENDCASE.

    WHEN OTHERS.

      CASE GS_T811K-FIELD.
        WHEN 'VERSI'.
          ASSIGN GS_DISPLAY-VERSN TO <FS_F>.
      ENDCASE.
  ENDCASE.


  " 그룹에 대한 내용
  IF <FS_G> IS ASSIGNED AND GS_T811K-SETID IS NOT INITIAL.
    <FS_G> = GS_T811K-SETID.
    IF <FS_GRP> IS ASSIGNED.
      CALL FUNCTION 'G_SET_GET_KEYS_FROM_SETID'
        EXPORTING
          IM_SETID    = GS_T811K-SETID
        IMPORTING
          EX_SETCLASS = LV_SETCLASS " Set Class
          EX_SUBCLASS = LV_SUBCLASS " Organizational Unit as Set Subclass
          EX_SETNAME  = <FS_GRP>.   " Set Name

      IF <FS_TXT> IS ASSIGNED.
        READ TABLE GT_SETHDRT INTO GS_SETHDRT
                              WITH KEY SETCLASS = LV_SETCLASS
                                       SUBCLASS = LV_SUBCLASS
                                       SETNAME  = <FS_GRP>
                                       BINARY SEARCH.
        IF SY-SUBRC EQ 0.
          <FS_TXT> = GS_SETHDRT-DESCRIPT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  " 단일 값에 대한 내용
  IF <FS_F> IS ASSIGNED. <FS_F> = GS_T811K-VALMIN. ENDIF.
  IF <FS_T> IS ASSIGNED. <FS_T> = GS_T811K-VALMAX. ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_TEXT
*&---------------------------------------------------------------------*
FORM SELECT_TEXT .

  SELECT SINGLE *
    FROM TKA01
   WHERE KOKRS EQ @P_KOKRS
    INTO @DATA(LS_TKA01).

*-- 그룹명
  SELECT *
    FROM SETHEADERT
   WHERE LANGU EQ @SY-LANGU
    INTO CORRESPONDING FIELDS OF TABLE @GT_SETHDRT.

*-- 코스트센터명
  SELECT *
    FROM CSKT
   WHERE SPRAS EQ @SY-LANGU
     AND KOKRS EQ @P_KOKRS
    INTO CORRESPONDING FIELDS OF TABLE @GT_CSKT.

*-- 원가요소명
  SELECT *
    FROM CSKU
   WHERE SPRAS EQ @SY-LANGU
     AND KTOPL EQ @LS_TKA01-KTOPL
    INTO CORRESPONDING FIELDS OF TABLE @GT_CSKU.

*-- 회사코드명
  SELECT *
    FROM T001
   WHERE SPRAS EQ @SY-LANGU
    INTO CORRESPONDING FIELDS OF TABLE @GT_T001.

*-- BU구분명
  SELECT *
    FROM T25A1
   WHERE SPRAS EQ @SY-LANGU
    INTO CORRESPONDING FIELDS OF TABLE @GT_T25A1.

*-- 사업영역명
  SELECT *
    FROM TGSBT
   WHERE SPRAS EQ @SY-LANGU
    INTO CORRESPONDING FIELDS OF TABLE @GT_TGSBT.

*-- 행정구역명
  SELECT *
    FROM ZCOT1090T
   WHERE SPRAS EQ @SY-LANGU
    INTO CORRESPONDING FIELDS OF TABLE @GT_1090T.

*-- 배부기준명(VV:DD04T에서 추출, VV가 아니면 TKCTK에서 추출
  SELECT ROLLNAME, REPTEXT
    FROM DD04T
   WHERE ROLLNAME   LIKE 'RKE2_%'
     AND DDLANGUAGE EQ   @SY-LANGU
     AND AS4LOCAL   EQ   @GC_A
    INTO TABLE @GT_DD04T.

  SELECT KEYRA, TXT_M
    FROM TKCTK
   WHERE APPLC EQ 'KEP'
     AND LANGU EQ @SY-LANGU
    INTO TABLE @GT_TKCTK.

*-- PA전송구조명
  SELECT ERSCH,
         STEXT
    FROM TKB9B
   WHERE SPRAS EQ @SY-LANGU
    INTO TABLE @GT_TKB9B.

*-- Project 설명
  SELECT PSPID,
         POST1
    FROM PROJ
    INTO TABLE @GT_PRPS.

*-- WBS Element 설명
  SELECT POSID,
         POST1
    FROM PRPS
    INTO TABLE @GT_PRPS.


  SORT GT_SETHDRT BY SETCLASS SUBCLASS SETNAME.

  SORT GT_CSKT    BY KOSTL DATBI.
  SORT GT_CSKU    BY KSTAR.
  SORT GT_T001    BY BUKRS.
  SORT GT_T25A1   BY WW120.
  SORT GT_TGSBT   BY GSBER.
  SORT GT_1090T   BY ZZADT.
  SORT GT_DD04T   BY ROLLNAME.
  SORT GT_TKCTK   BY KEYRA.
  SORT GT_TKB9B   BY ERSCH.
  SORT GT_PROJ    BY PSPID.
  SORT GT_PRPS    BY POSID.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TEXT_RCDATA
*&---------------------------------------------------------------------*
FORM SET_TEXT_RCDATA  USING PV_RCDATA
                            PV_RCDATA_TXT.

  DATA LV_ROLLNAME TYPE DD04T-ROLLNAME.

  CLEAR PV_RCDATA_TXT.
  CHECK PV_RCDATA IS NOT INITIAL.

  IF PV_RCDATA(2) EQ 'VV'.
    LV_ROLLNAME = 'RKE2_' && PV_RCDATA.

    READ TABLE GT_DD04T INTO GS_DD04T
                        WITH KEY ROLLNAME = LV_ROLLNAME
                                 BINARY SEARCH.
    CHECK SY-SUBRC EQ 0.
    PV_RCDATA_TXT = GS_DD04T-REPTEXT.
  ELSE.
    READ TABLE GT_TKCTK INTO GS_TKCTK
                        WITH KEY KEYRA = PV_RCDATA
                                 BINARY SEARCH.

    CHECK SY-SUBRC EQ 0.
    PV_RCDATA_TXT = GS_TKCTK-TXT_M.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TEXT_ERSCH
*&---------------------------------------------------------------------*
FORM SET_TEXT_ERSCH  USING PV_ERSCH
                           PV_ERSCH_TXT.

  CLEAR PV_ERSCH_TXT.
  CHECK PV_ERSCH IS NOT INITIAL.

  READ TABLE GT_TKB9B INTO GS_TKB9B
                      WITH KEY ERSCH = PV_ERSCH
                               BINARY SEARCH.
  CHECK SY-SUBRC EQ 0.

  PV_ERSCH_TXT = GS_TKB9B-STEXT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F4_CYCLE
*&---------------------------------------------------------------------*
FORM F4_CYCLE  USING PV_CYCLE LIKE GV_CYCLE.

  P_KOKRS = ZCL_CO_COMMON=>GET_DYNP_VALUE( 'P_KOKRS' ).

  CALL FUNCTION 'RK_F4_ALLOCATION_CYCLE'
    EXPORTING
      APPL             = GC_E
      TABLE            = GC_TAB_CE71000
      GROUP            = P_KOKRS
      CHOOSEN          = 'SELECTION_CYCLE'
    IMPORTING
      EX_CYCLE         = PV_CYCLE
    EXCEPTIONS
      NO_CYCLE         = 2
      NOTHING_SELECTED = 4.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_RANGES_CYCLE
*&---------------------------------------------------------------------*
FORM SET_RANGES_CYCLE .

  CLEAR: R_CYCLE, R_CYCLE[].

  LOOP AT S_CYCLE.

    R_CYCLE = CORRESPONDING #( S_CYCLE ).

    IF R_CYCLE-LOW IS NOT INITIAL.
      R_CYCLE-LOW = P_KOKRS && R_CYCLE-LOW.
    ENDIF.

    IF R_CYCLE-HIGH IS NOT INITIAL.
      R_CYCLE-HIGH = P_KOKRS && R_CYCLE-HIGH.
    ENDIF.

    APPEND R_CYCLE.

  ENDLOOP.


ENDFORM.
