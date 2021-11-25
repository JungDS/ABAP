*&---------------------------------------------------------------------*
*& Include          ZCOR0200F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITAIL
*&---------------------------------------------------------------------*
FORM INITAIL .

*  DATA LV_GJAHR TYPE GJAHR.
*
*  GV_REPID = SY-REPID.
*
*  SELECT SINGLE BEZEI, WAERS INTO (@PA_KTXT, @GV_WAERS)
*    FROM TKA01
*   WHERE KOKRS = @PA_KOKRS.
*
*  SELECT SINGLE VTEXT INTO @PA_VTXT
*    FROM TKVS AS A
*    LEFT JOIN TKVST AS B
*      ON A~VERSI = B~VERSI
*     AND B~SPRAS = @SY-LANGU
*   WHERE A~VERSI = 'B1'.
*
*  LV_GJAHR = SY-DATUM(4) - '1'.
*  PA_JSPER = LV_GJAHR && '001'.
*
*  LV_GJAHR = SY-DATUM(4) + '1'.
*  PA_JEPER = LV_GJAHR && '012'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SCREEN
*&---------------------------------------------------------------------*
FORM SET_SCREEN .

  LOOP AT SCREEN.
    IF SCREEN-NAME = 'PA_KOKRS' OR SCREEN-NAME = 'PA_VERSN'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_DATA_RTN
*&---------------------------------------------------------------------*
FORM SELECTED_DATA_RTN .

  PERFORM AUTHORITY_CHECK.
  PERFORM SELECTED_MAIN_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM AUTHORITY_CHECK .

  DATA: LV_TYPE    TYPE BAPI_MTYPE,
        LV_MESSAGE TYPE BAPI_MSG.

  DATA: LT_0070  LIKE TABLE OF ZCAS0070,
        LS_0070  LIKE ZCAS0070,
        LV_CLASS TYPE ZCAT0031-CD_CLASS,
        LV_CODE  TYPE ZCAT0031-CD_CODE.


  LV_CLASS = 'CASUSR'.
  LV_CODE  = SY-UNAME.

  "__ SUPER USER ID 체크
  PERFORM CALL_F4_VALUES(ZCAR9000) TABLES LT_0070
                                    USING LV_CLASS LV_CODE LS_0070.
  IF LT_0070[] IS NOT INITIAL.
    EXIT.
  ELSE.
    LV_CLASS = 'CASUCO'.
    LV_CODE  = SY-UNAME.

    "__ SUPER USER ID 체크
    PERFORM CALL_F4_VALUES(ZCAR9000) TABLES LT_0070
                                      USING LV_CLASS LV_CODE LS_0070.
    IF LT_0070[] IS NOT INITIAL.
      EXIT.
    ENDIF.
  ENDIF.

  IF SO_BUKRS[] IS INITIAL.

    MESSAGE S000 WITH TEXT-E01 DISPLAY LIKE 'E'.
    STOP.

  ENDIF.

  CALL FUNCTION 'ZCA_PRG_AUTH_CHECK'
    EXPORTING
      I_MODULE    = 'CO'
    IMPORTING
      E_TYPE      = LV_TYPE
      E_MESSAGE   = LV_MESSAGE
    TABLES
      IT_BUKRS_CO = SO_BUKRS[].

  IF LV_TYPE = 'E'.
    MESSAGE S000 WITH LV_MESSAGE DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_MAIN_DATA
*&---------------------------------------------------------------------*
FORM SELECTED_MAIN_DATA .
  DATA(LV_FISCYEARPER) = PA_GJAHR && PA_PERBL.
  DATA LT_TEMP LIKE TABLE OF GS_DISPLAY.

  MOVE: 'I'   TO R_RACCT-SIGN,
        'BT'  TO R_RACCT-OPTION,
        '04*' TO R_RACCT-LOW,
        '09*' TO R_RACCT-HIGH.
  APPEND  R_RACCT.

  FIELD-SYMBOLS: <FS> TYPE ANY.

  SELECT A~RBUKRS AS BUKRS, A~GJAHR, A~BELNR, C~KOSTL, D~KTEXT AS KOTXT,
         E~POSID, E~POST1, A~RACCT, F~KTEXT AS KSTXT, A~RHCUR, A~HSL
    INTO TABLE @GT_DISPLAY
    FROM ACDOCA AS A JOIN ZFIT0480 AS B
                       ON A~RBUKRS = B~BUKRS
                      AND A~GJAHR  = B~GJAHR
                      AND A~BELNR  = B~BELNR
          LEFT OUTER JOIN CSKS AS C
                       ON A~OBJNR  = C~OBJNR
                      AND C~DATBI >= @SY-DATUM
                      AND C~DATAB <= @SY-DATUM
                LEFT JOIN CSKT AS D
                       ON C~KOKRS = D~KOKRS
                      AND C~DATBI = D~DATBI
                      AND C~KOSTL = D~KOSTL
                      AND D~SPRAS = @SY-LANGU
          LEFT OUTER JOIN PRPS AS E
                       ON A~OBJNR  = E~OBJNR
          LEFT OUTER JOIN CSKU AS F
                       ON F~SPRAS  = @SY-LANGU
                      AND A~KTOPL  = F~KTOPL
                      AND A~RACCT  = F~KSTAR
   WHERE A~RLDNR        = '0L'
     AND A~RBUKRS      IN @SO_BUKRS
     AND A~GJAHR        = @PA_GJAHR
     AND A~KOKRS        = @PA_KOKRS
     AND A~RACCT       IN @R_RACCT
     AND A~FISCYEARPER  = @LV_FISCYEARPER.

  SORT GT_DISPLAY BY BELNR.

*  LOOP AT GT_DISPLAY INTO GS_DISPLAY WHERE KSTAR CP '04*'
*                                        OR KSTAR CP '0701*'
*                                        OR KSTAR CP '0703*'
*                                        OR KSTAR CP '0705*'.
*    GS_DISPLAY-HSL = GS_DISPLAY-HSL * -1.
*    MODIFY GT_DISPLAY FROM GS_DISPLAY.
*  ENDLOOP.

  LT_TEMP[] = GT_DISPLAY[].

  DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING BELNR.

  WRITE LINES( LT_TEMP ) TO GV_COUNT LEFT-JUSTIFIED.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREATE_INSTANCE_0100
*&---------------------------------------------------------------------*
FORM CREATE_INSTANCE_0100 .
*-- 1. customer container

*  CREATE OBJECT GR_CON1
*    EXPORTING
*      CONTAINER_NAME = GV_CONTAINER. "USER가 정의한 CONTAINER
*
*  CREATE OBJECT GR_GRID1
*    EXPORTING
*      I_PARENT = GR_CON1.

*-- 2. full screen
  CREATE OBJECT GR_SPLITTER1
    EXPORTING
      ROWS    = 2
      COLUMNS = 1
      PARENT  = CL_GUI_SPLITTER_CONTAINER=>SCREEN0.

*== get container instance
*-- 1. top of page
  GR_PARENT_HTML = GR_SPLITTER1->GET_CONTAINER(
      ROW       = 1
      COLUMN    = 1 ).

  GR_DATA_CONTAINER = GR_SPLITTER1->GET_CONTAINER(
      ROW       = 2
      COLUMN    = 1 ).

  CALL METHOD GR_SPLITTER1->SET_ROW_HEIGHT
    EXPORTING
      ID     = 1
      HEIGHT = 7.

  CALL METHOD GR_SPLITTER1->SET_ROW_HEIGHT
    EXPORTING
      ID     = 2
      HEIGHT = 50.

  CREATE OBJECT GR_GRID1
    EXPORTING
      I_PARENT = GR_DATA_CONTAINER.

ENDFORM.                    " CREATE_INSTANCE_0100
*&---------------------------------------------------------------------*
*&      Form  INIT_LAYOUT_0100
*&---------------------------------------------------------------------*
FORM INIT_LAYOUT_0100.

  CLEAR GS_LAYOUT.

*  GS_LAYOUT-EDIT_MODE  = ABAP_TRUE.
  GS_LAYOUT-ZEBRA      = ABAP_TRUE.
*  GS_LAYOUT-CWIDTH_OPT = ABAP_TRUE.
  GS_LAYOUT-SEL_MODE   = SPACE.     "B:단일,C:복수,D:셀,A:행/열
  GS_LAYOUT-BOX_FNAME  = SPACE.
  GS_LAYOUT-NO_ROWMARK = SPACE.

*  GS_LAYOUT-STYLEFNAME = 'STYLE'.
*  GS_LAYOUT-CTAB_FNAME = 'COLOR'.
*  GS_LAYOUT-INFO_FNAME = 'INFO'.

**  "alv title
**  GS_LAYOUT-GRID_TITLE = TEXT-GT1.

ENDFORM.                    " INIT_LAYOUT_0100
*&---------------------------------------------------------------------*
*&      Form  SET_GRID_EXCLUDE_0100
*&---------------------------------------------------------------------*
FORM SET_GRID_EXCLUDE_0100 .

  DATA: LS_EXCLUDE LIKE LINE OF GT_EXCLUDE.
  REFRESH: GT_EXCLUDE.

  "-- DEFINE _SET_EX
  DEFINE _SET_EX.
    CLEAR: ls_exclude.
    ls_exclude = &1.
    APPEND ls_exclude TO gt_exclude.
  END-OF-DEFINITION.


  _SET_EX:
*   CL_GUI_ALV_GRID=>MC_FC_FIND,
*   CL_GUI_ALV_GRID=>MC_FC_SORT_ASC,
*   CL_GUI_ALV_GRID=>MC_FC_SORT_DSC,
*   CL_GUI_ALV_GRID=>MC_MB_SUBTOT,
*   CL_GUI_ALV_GRID=>MC_MB_SUM,

    CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_COPY,
    CL_GUI_ALV_GRID=>MC_FC_LOC_CUT,
    CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE,
    CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO,
    CL_GUI_ALV_GRID=>MC_FC_CHECK,

*   CL_GUI_ALV_GRID=>MC_FC_DETAIL,
*   CL_GUI_ALV_GRID=>MC_FC_FILTER,
    CL_GUI_ALV_GRID=>MC_FC_GRAPH,
    CL_GUI_ALV_GRID=>MC_FC_HTML,
    CL_GUI_ALV_GRID=>MC_FC_INFO,
    CL_GUI_ALV_GRID=>MC_FC_REFRESH,

*   CL_GUI_ALV_GRID=>MC_FC_VIEWS,
*   CL_GUI_ALV_GRID=>MC_FC_LOAD_VARIANT,
*   CL_GUI_ALV_GRID=>MC_FC_PRINT,
*   CL_GUI_ALV_GRID=>MC_MB_VARIANT,
*   CL_GUI_ALV_GRID=>MC_MB_EXPORT,

    CL_GUI_ALV_GRID=>MC_FC_VIEW_CRYSTAL,
    CL_GUI_ALV_GRID=>MC_FC_VIEW_EXCEL,
    CL_GUI_ALV_GRID=>MC_FC_VIEW_GRID,
    CL_GUI_ALV_GRID=>MC_FC_VIEW_LOTUS,
    CL_GUI_ALV_GRID=>MC_FC_EXPCRDATA,
    CL_GUI_ALV_GRID=>MC_FC_EXPCRDESIG,
    CL_GUI_ALV_GRID=>MC_FC_EXPCRTEMPL,
    CL_GUI_ALV_GRID=>MC_FC_CALL_ABC,
    CL_GUI_ALV_GRID=>MC_FC_CALL_CRBATCH.

ENDFORM. " SET_GRID_EXCLUDE_0100
*&---------------------------------------------------------------------*
*&      Form  ALV_SORT_0100
*&---------------------------------------------------------------------*
FORM ALV_SORT_0100 .

  CLEAR: GS_SORT, GT_SORT.
  REFRESH: GT_SORT.

ENDFORM.                    " ALV_SORT_0100
*&---------------------------------------------------------------------*
*&      Form  APPEND_FIELDCAT_0100
*&---------------------------------------------------------------------*
FORM APPEND_FIELDCAT_0100 .

  "-- field catalog data
  "   field catalog merge or set fieldcatalog를 사용할 수 있음.

  "{ FIELDCATLOG MERGE 사용
  PERFORM GET_FIELDCATLOG_DATA.

  PERFORM MODIFY_FIELDCATLOG_DATA.
  "}

  "{ SET FIELDCATLOG 사용
*  PERFORM SET_FIELDCATLOG_DATA.
  "}

ENDFORM.                    " APPEND_FIELDCAT_0100
*&---------------------------------------------------------------------*
*&      Form  GET_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
FORM GET_FIELDCATLOG_DATA .

  DATA: LT_FIELDCAT TYPE KKBLO_T_FIELDCAT.

  CALL FUNCTION 'K_KKB_FIELDCAT_MERGE'
    EXPORTING
      I_CALLBACK_PROGRAM     = SY-REPID
      I_STRUCNAME            = 'ZCOS0200' "ABAP DIC. 정의된 STRUCTURE
      I_BYPASSING_BUFFER     = ABAP_TRUE
      I_INCLNAME             = SY-REPID
    CHANGING
      CT_FIELDCAT            = LT_FIELDCAT[]
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      OTHERS                 = 2.

  IF SY-SUBRC EQ 0.

    "-- Trasnfer LVC.
    CALL FUNCTION 'LVC_TRANSFER_FROM_KKBLO'
      EXPORTING
        IT_FIELDCAT_KKBLO = LT_FIELDCAT[]
      IMPORTING
        ET_FIELDCAT_LVC   = GT_FIELDCAT[]
      EXCEPTIONS
        IT_DATA_MISSING   = 1.
  ELSE.

    MESSAGE E020.

  ENDIF.

ENDFORM.                    " GET_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
FORM MODIFY_FIELDCATLOG_DATA .

  DATA:  LV_TEXT(50).

  LOOP AT GT_FIELDCAT INTO GS_FIELDCAT.

    CLEAR: LV_TEXT.

    CASE GS_FIELDCAT-FIELDNAME.
      WHEN 'BUKRS'.
        LV_TEXT               = TEXT-C09.
        GS_FIELDCAT-EMPHASIZE = 'C112'.

      WHEN 'GJAHR'.
        LV_TEXT               = TEXT-C10.
        GS_FIELDCAT-EMPHASIZE = 'C112'.

      WHEN 'BELNR'.
        LV_TEXT               = TEXT-C01.
        GS_FIELDCAT-EMPHASIZE = 'C112'.
        GS_FIELDCAT-HOTSPOT   = ABAP_TRUE.

      WHEN 'KOSTL'.
        LV_TEXT = TEXT-C02.

      WHEN 'KOTXT'.
        LV_TEXT = TEXT-C03.

      WHEN 'POSID'.
        LV_TEXT = TEXT-C04.

      WHEN 'POST1'.
        LV_TEXT = TEXT-C05.

      WHEN 'KSTAR'.
        LV_TEXT = TEXT-C06.

      WHEN 'KSTXT'.
        LV_TEXT = TEXT-C07.

      WHEN 'HSL'.
        LV_TEXT = TEXT-C08.

      WHEN OTHERS.
        GS_FIELDCAT-NO_OUT = 'X'.
    ENDCASE.

    "-- Common attribute
    IF LV_TEXT IS NOT INITIAL.
      GS_FIELDCAT-COLTEXT   = LV_TEXT.
      GS_FIELDCAT-SCRTEXT_L = LV_TEXT.
      GS_FIELDCAT-SCRTEXT_M = LV_TEXT.
      GS_FIELDCAT-SCRTEXT_S = LV_TEXT.
    ENDIF.

    MODIFY GT_FIELDCAT FROM GS_FIELDCAT.

  ENDLOOP.

ENDFORM.                    " MODIFY_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
*&      Form  SET_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
FORM SET_FIELDCATLOG_DATA.

  CLEAR GT_FIELDCAT[].

  PERFORM FILL_FIELD_CATEGORY USING :
        'S' 'FIELDNAME'   'GJAHR',
        ' ' 'OUTPUTLEN'   '4',
        'E' 'COLTEXT'     '적용연도',

        'S' 'FIELDNAME'   'CARRID',
        ' ' 'OUTPUTLEN'   '3',
        ' ' 'EDIT'        'X',
        'E' 'COLTEXT'     '항공사 ID'.

ENDFORM.                    " SET_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
*&      Form  fill_field_category
*&---------------------------------------------------------------------*
FORM FILL_FIELD_CATEGORY USING PV_GUB PV_FNAME PV_CON.

  IF PV_GUB = 'S'.
    CLEAR GS_FIELDCAT.
  ENDIF.

* 속성 MOVE
  DATA LV_COL(40).
  FIELD-SYMBOLS <FS>.
  CONCATENATE 'GS_FIELDCAT-' PV_FNAME  INTO LV_COL.
  ASSIGN      (LV_COL)       TO        <FS>.
  MOVE         PV_CON        TO        <FS>.

  IF PV_GUB = 'E'.
    APPEND GS_FIELDCAT TO GT_FIELDCAT.
  ENDIF.
ENDFORM. " fill_field_category
*&---------------------------------------------------------------------*
*& Form TOP_OF_PAGE_CREATE_OBJECT_0100
*&---------------------------------------------------------------------*
FORM TOP_OF_PAGE_CREATE_OBJECT_0100 .

* Create TOP-Document
  CREATE OBJECT GR_TOP_DOCUMENT
    EXPORTING
      STYLE = 'ALV_GRID'.

* Initialize
  CALL METHOD GR_TOP_DOCUMENT->INITIALIZE_DOCUMENT( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_TOP_OF_PAGE_DATA_0100
*&---------------------------------------------------------------------*
FORM MAKE_TOP_OF_PAGE_DATA_0100 .

  DATA: LT_TEXTS TYPE SDYDO_TEXT_TABLE,
        LV_TEXT  TYPE SDYDO_TEXT_ELEMENT.

  CONCATENATE TEXT-001 ':' PA_KOKRS
        INTO LV_TEXT SEPARATED BY SPACE.

  CALL METHOD GR_TOP_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_COLOR    = CL_DD_DOCUMENT=>LIST_HEADING_INT
      SAP_EMPHASIS = CL_DD_AREA=>KEY
      STYLE_CLASS  = SPACE.

  CALL METHOD GR_TOP_DOCUMENT->NEW_LINE.

  IF SO_BUKRS-HIGH IS INITIAL.
    CONCATENATE TEXT-002 ':' SO_BUKRS-LOW
          INTO LV_TEXT SEPARATED BY SPACE.
  ELSE.
    CONCATENATE TEXT-002 ':' SO_BUKRS-LOW ` ~ ` SO_BUKRS-HIGH
          INTO LV_TEXT SEPARATED BY SPACE.
  ENDIF.


  CALL METHOD GR_TOP_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_COLOR    = CL_DD_DOCUMENT=>LIST_HEADING_INT
      SAP_EMPHASIS = CL_DD_AREA=>KEY
      STYLE_CLASS  = SPACE.

  CALL METHOD GR_TOP_DOCUMENT->NEW_LINE.

  CONCATENATE TEXT-003 ':' PA_GJAHR
        INTO LV_TEXT SEPARATED BY SPACE.

  CALL METHOD GR_TOP_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_COLOR    = CL_DD_DOCUMENT=>LIST_HEADING_INT
      SAP_EMPHASIS = CL_DD_AREA=>KEY
      STYLE_CLASS  = SPACE.

  CALL METHOD GR_TOP_DOCUMENT->NEW_LINE.

  CONCATENATE TEXT-004 ':' PA_PERBL
        INTO LV_TEXT SEPARATED BY SPACE.

  CALL METHOD GR_TOP_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_COLOR    = CL_DD_DOCUMENT=>LIST_HEADING_INT
      SAP_EMPHASIS = CL_DD_AREA=>KEY
      STYLE_CLASS  = SPACE.

  CALL METHOD GR_TOP_DOCUMENT->NEW_LINE.

  LV_TEXT = GV_COUNT.

  CONCATENATE TEXT-005 ':' LV_TEXT
        INTO LV_TEXT SEPARATED BY SPACE.

  CALL METHOD GR_TOP_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_COLOR    = CL_DD_DOCUMENT=>LIST_HEADING_INT
      SAP_EMPHASIS = CL_DD_AREA=>KEY
      STYLE_CLASS  = SPACE.

  CALL METHOD GR_TOP_DOCUMENT->NEW_LINE
    EXPORTING
      REPEAT = 1.

*  CALL METHOD GR_TOP_DOCUMENT->ADD_GAP
*    EXPORTING
*      WIDTH = 20.

  " Get Ready
  CALL METHOD GR_TOP_DOCUMENT->MERGE_DOCUMENT.

*" Display TOP document
  CALL METHOD GR_TOP_DOCUMENT->DISPLAY_DOCUMENT
    EXPORTING
      REUSE_CONTROL      = 'X'
      PARENT             = GR_PARENT_HTML
    EXCEPTIONS
      HTML_DISPLAY_ERROR = 1.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  REGIST_ALV_EVENT_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM REGIST_ALV_EVENT_0100 USING PR_GRID TYPE REF TO CL_GUI_ALV_GRID.

* REGISTER EVENT
  CALL METHOD PR_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
*
  CALL METHOD PR_GRID->SET_READY_FOR_INPUT
    EXPORTING
      I_READY_FOR_INPUT = 1.

*-- GR_EVENT_RECEIVER
  IF GR_EVENT_RECEIVER IS INITIAL.
    CREATE OBJECT GR_EVENT_RECEIVER.
  ENDIF.

* Handler Event
  SET HANDLER:
*    GR_EVENT_RECEIVER->HANDLE_TOOLBAR       FOR ALL INSTANCES,
*    GR_EVENT_RECEIVER->HANDLE_DATA_CHANGED  FOR ALL INSTANCES,
*    GR_EVENT_RECEIVER->HANDLE_DATA_CHANGED_FINISHED
*      FOR ALL INSTANCES,
*    GR_EVENT_RECEIVER->HANDLE_USER_COMMAND  FOR ALL INSTANCES,
    GR_EVENT_RECEIVER->HANDLE_HOTSPOT_CLICK FOR ALL INSTANCES.
*    GR_EVENT_RECEIVER->HANDLE_DOUBLE_CLICK  FOR ALL INSTANCES,
*    GR_EVENT_RECEIVER->HANDLE_ONF4          FOR ALL INSTANCES.

ENDFORM.                    " REGIST_ALV_EVENT_0100
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_TITLE_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DISPLAY_ALV_TITLE_0100 .

  DATA: LV_TITLE TYPE LVC_TITLE.

  LV_TITLE = TEXT-GT1.

  CALL METHOD GR_GRID1->SET_GRIDTITLE
    EXPORTING
      I_GRIDTITLE = LV_TITLE.

ENDFORM.                    " DISPLAY_ALV_TITLE_0100
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_GRID_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DISPLAY_ALV_GRID_0100 .

  GS_VARIANT-REPORT = SY-REPID.

  GV_SAVE = 'A'.

  "*-- Build field catalog for the alv control
  CALL METHOD GR_GRID1->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      I_DEFAULT                     = ABAP_TRUE
      IS_LAYOUT                     = GS_LAYOUT
      IS_VARIANT                    = GS_VARIANT
      I_SAVE                        = GV_SAVE
      IT_TOOLBAR_EXCLUDING          = GT_EXCLUDE
    CHANGING
      IT_FIELDCATALOG               = GT_FIELDCAT
      IT_SORT                       = GT_SORT
      IT_OUTTAB                     = GT_DISPLAY[]
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3.

  IF SY-SUBRC NE 0.
    MESSAGE E000(0K) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " DISPLAY_ALV_GRID_0100
*&---------------------------------------------------------------------*
*&      Form  REFRESH_GRID_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM REFRESH_GRID_0100 .

  GS_STABLE-ROW = ABAP_TRUE. "Row
  GS_STABLE-COL = ABAP_TRUE. "column

  CALL METHOD GR_GRID1->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = GS_STABLE
      I_SOFT_REFRESH = SPACE.

  CALL METHOD CL_GUI_CFW=>FLUSH.

ENDFORM.                    " REFRESH_GRID_0100
*&---------------------------------------------------------------------*
*&      Form  EVENT_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM EVENT_HOTSPOT_CLICK USING PS_ROW_ID    TYPE LVC_S_ROW
                               PS_COLUMN_ID TYPE LVC_S_COL
                               PS_ROW_NO    TYPE LVC_S_ROID
                               PR_SENDER   TYPE REF TO CL_GUI_ALV_GRID.


* - Prefix 정의 - Form Parameter
*   1. PT_  : Internal Table or Ranges
*   2. PS_  : Structure
*   3. PV_  : Variables
*   4. PR_ : Reference Variables

  DATA: LT_SPAGPA TYPE TABLE OF RFC_SPAGPA WITH HEADER LINE.

  CASE PR_SENDER.
    WHEN GR_GRID1.

      CASE PS_COLUMN_ID-FIELDNAME.
        WHEN 'BELNR'.
          READ TABLE GT_DISPLAY ASSIGNING FIELD-SYMBOL(<FS_DISPLAY>) INDEX PS_ROW_NO-ROW_ID.
          CHECK SY-SUBRC EQ 0.

          CLEAR: LT_SPAGPA, LT_SPAGPA[].
          LT_SPAGPA-PARID  = 'BLN'.
          LT_SPAGPA-PARVAL = <FS_DISPLAY>-BELNR.
          APPEND LT_SPAGPA. CLEAR LT_SPAGPA.

          LT_SPAGPA-PARID  = 'BUK'.
          LT_SPAGPA-PARVAL = <FS_DISPLAY>-BUKRS.
          APPEND LT_SPAGPA. CLEAR LT_SPAGPA.

          LT_SPAGPA-PARID  = 'GJR'.
          LT_SPAGPA-PARVAL = <FS_DISPLAY>-GJAHR.
          APPEND LT_SPAGPA. CLEAR LT_SPAGPA.

          CALL FUNCTION 'ABAP4_CALL_TRANSACTION' STARTING NEW TASK ''
            EXPORTING
              TCODE       = 'FB03'
              SKIP_SCREEN = 'X'
            TABLES
              SPAGPA_TAB  = LT_SPAGPA.

      ENDCASE.

    WHEN OTHERS.
  ENDCASE.





ENDFORM.                    " EVENT_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*& Form SET_INIT_HELP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_INIT_HELP .

  "__ Function Key
  DATA: LS_FUNTXT TYPE SMP_DYNTXT.

  LS_FUNTXT-ICON_ID   = ICON_INFORMATION.
  LS_FUNTXT-QUICKINFO = 'Program Help'.

  SSCRFIELDS-FUNCTXT_01 = LS_FUNTXT.

  "__ 20191223 BSGSM_FCM ADD default cac
  SET PARAMETER ID 'CAC' FIELD PA_KOKRS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCR_USER_COMMAND_HELP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SCR_USER_COMMAND_HELP .

  CASE SSCRFIELDS-UCOMM.
    WHEN 'FC01'.
      PERFORM CALL_POPUP_HELP(ZCAR9000) USING SY-REPID SY-DYNNR SY-LANGU ''.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
