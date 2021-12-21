*&---------------------------------------------------------------------*
*& Include          ZCOR0190F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form SET_SCREEN
*&---------------------------------------------------------------------*
FORM SET_SCREEN .

  LOOP AT SCREEN.
    IF SCREEN-NAME = 'PA_KOKRS' ."OR SCREEN-NAME = 'PA_VERSN'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_DATA_RTN
*&---------------------------------------------------------------------*
FORM SELECTED_DATA_RTN .

*--------------------------------------------------------------------*
* [ESG_CO] DEV_ESG 기존PGM 고도화 #4, 2021.12.06 08:55:57
*--------------------------------------------------------------------*
* 기존 설계된 전자세금계산서 - 전표집계 관련 함수로 인해
* 버전 조회 시 '000' 이 아닌 '0' 으로 개발된 내용이 있음.
* 이로 인해 ALPHA_INPUT 이 아닌 ALPHA_OUTPUT 으로 강제적용함.
*--------------------------------------------------------------------*

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      INPUT  = PA_VERSN
    IMPORTING
      OUTPUT = PA_VERSN.

  GV_CHANGE = ABAP_OFF.

  IF PA_BUKRS EQ '*' OR PA_BUKRS IS INITIAL.
    PERFORM AUTHORITY_CHECK_2.
    PERFORM SELECTED_MAIN_DATA_2.

*  ELSEIF PA_BUKRS(1) EQ '9'.
*    " 입력하신 회사코드는 대상이 아닙니다.
*    MESSAGE S000 WITH TEXT-E02 DISPLAY LIKE 'E'.
*    STOP.

  ELSE.

    GV_CHANGE = ABAP_ON.

    PERFORM AUTHORITY_CHECK.
    PERFORM SELECTED_MAIN_DATA.

  ENDIF.

  " 변경자명 TEXT 붙임
  PERFORM MAKE_DISPLAY_DATA.

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

  IF PA_BUKRS IS INITIAL.

    MESSAGE S000 WITH TEXT-E01 DISPLAY LIKE 'E'.
    STOP.

  ENDIF.

  CALL FUNCTION 'ZCA_PRG_AUTH_CHECK'
    EXPORTING
      I_MODULE   = 'CO'
      I_BUKRS_CO = PA_BUKRS
    IMPORTING
      E_TYPE     = LV_TYPE
      E_MESSAGE  = LV_MESSAGE.

  IF LV_TYPE = 'E'.
    MESSAGE S000 WITH LV_MESSAGE DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_MAIN_DATA
*&---------------------------------------------------------------------*
FORM SELECTED_MAIN_DATA .
  CLEAR: GS_DISPLAY, GT_DISPLAY, GT_DISPLAY[],
         GT_DISPLAY_LOG, GT_DISPLAY_LOG[].

  SELECT A~KOKRS, A~BUKRS, B~BUTXT, A~GJAHR, A~VERSN, A~MON01,
         A~MON02, A~MON03, A~MON04, A~MON05, A~MON06, A~MON07,
         A~MON08, A~MON09, A~MON10, A~MON11, A~MON12,
         A~AEDAT, A~AEZET, A~AENAM
    INTO TABLE @GT_DISPLAY
    FROM ZCOT0190 AS A JOIN T001 AS B
                         ON A~BUKRS = B~BUKRS
   WHERE A~KOKRS = @PA_KOKRS
     AND A~BUKRS = @PA_BUKRS
     AND A~GJAHR = @PA_GJAHR
     AND A~VERSN = @PA_VERSN.
  IF SY-SUBRC <> 0.

    GS_DISPLAY-KOKRS = PA_KOKRS.
    GS_DISPLAY-BUKRS = PA_BUKRS.

    SELECT SINGLE BUTXT INTO @GS_DISPLAY-BUTXT
      FROM T001
     WHERE BUKRS = @PA_BUKRS.

    GS_DISPLAY-GJAHR = PA_GJAHR.
    GS_DISPLAY-VERSN = PA_VERSN.
    APPEND GS_DISPLAY TO GT_DISPLAY.

  ENDIF.

  GT_DISPLAY_LOG[] = GT_DISPLAY[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_CHANGE
*&---------------------------------------------------------------------*
FORM CHECK_CHANGE  CHANGING P_GV_VALID.

  CHECK GV_CHANGE EQ ABAP_ON.

  IF GT_DISPLAY_LOG[] = GT_DISPLAY[].
    CLEAR P_GV_VALID.
  ELSE.
    P_GV_VALID = ABAP_TRUE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
FORM POPUP_TO_CONFIRM USING PV_TITLE
                            PV_QUEST.

  "-- call popup
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR       = PV_TITLE                "TEXT-PT1
*     DIAGNOSE_OBJECT             = ' '
      TEXT_QUESTION  = PV_QUEST                "TEXT-QT1
*     TEXT_BUTTON_1  = 'Ja'(001)
*     ICON_BUTTON_1  = ' '
*     TEXT_BUTTON_2  = 'Nein'(002)
*     ICON_BUTTON_2  = ' '
*     DEFAULT_BUTTON = '1'
*     DISPLAY_CANCEL_BUTTON       = 'X'
*     USERDEFINED_F1_HELP         = ' '
*     START_COLUMN   = 25
*     START_ROW      = 6
*     POPUP_TYPE     =
*     IV_QUICKINFO_BUTTON_1       = ' '
*     IV_QUICKINFO_BUTTON_2       = ' '
    IMPORTING
      ANSWER         = GV_ANSWER
*   TABLES
*     PARAMETER      =
    EXCEPTIONS
      TEXT_NOT_FOUND = 1
      OTHERS         = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " POPUP_TO_CONFIRM
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
      HEIGHT = 5.

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
      I_STRUCNAME            = 'ZCOS0220' "ABAP DIC. 정의된 STRUCTURE
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
        LV_TEXT = TEXT-C02.
        GS_FIELDCAT-OUTPUTLEN = '8'.
        GS_FIELDCAT-EMPHASIZE = 'C112'.

      WHEN 'BUTXT'.
        LV_TEXT = TEXT-C03.
        GS_FIELDCAT-EMPHASIZE = 'C112'.

      WHEN 'GJAHR'.
        LV_TEXT = TEXT-C01.
        GS_FIELDCAT-OUTPUTLEN = '8'.
        GS_FIELDCAT-EMPHASIZE = 'C112'.

      WHEN 'AEDAT'.
*        LV_TEXT = TEXT-C01.

      WHEN 'AEZET'.
*        LV_TEXT = TEXT-C01.

      WHEN 'AENAM'.
        GS_FIELDCAT-COL_OPT = ABAP_ON.
*        LV_TEXT = TEXT-C01.

      WHEN 'AENAMTEXT'.
        LV_TEXT = TEXT-C04.
        GS_FIELDCAT-COL_OPT = ABAP_ON.


      WHEN OTHERS.
        GS_FIELDCAT-NO_OUT = 'X'.
    ENDCASE.

    IF GS_FIELDCAT-FIELDNAME CP 'MON*'.

      CONCATENATE GS_FIELDCAT-FIELDNAME+3(2) '월' INTO LV_TEXT.
      GS_FIELDCAT-NO_OUT    = SPACE.
      GS_FIELDCAT-OUTPUTLEN = 10.
      GS_FIELDCAT-EDIT      = ABAP_TRUE.
      GS_FIELDCAT-CHECKBOX  = 'X'.

    ENDIF.

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

  CONCATENATE TEXT-C02 ':' PA_BUKRS
        INTO LV_TEXT SEPARATED BY SPACE.

  CALL METHOD GR_TOP_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_COLOR    = CL_DD_DOCUMENT=>LIST_HEADING_INT
      SAP_EMPHASIS = CL_DD_AREA=>KEY
      STYLE_CLASS  = SPACE.

  CALL METHOD GR_TOP_DOCUMENT->NEW_LINE.

  CONCATENATE TEXT-002 ':' PA_GJAHR
        INTO LV_TEXT SEPARATED BY SPACE.

  CALL METHOD GR_TOP_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_COLOR    = CL_DD_DOCUMENT=>LIST_HEADING_INT
      SAP_EMPHASIS = CL_DD_AREA=>KEY
      STYLE_CLASS  = SPACE.

  CALL METHOD GR_TOP_DOCUMENT->NEW_LINE.

  CONCATENATE TEXT-003 ':' PA_VERSN
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
  IF GV_CHANGE EQ ABAP_ON.
    CALL METHOD PR_GRID->SET_READY_FOR_INPUT
      EXPORTING
        I_READY_FOR_INPUT = 1.
  ELSE.
    CALL METHOD PR_GRID->SET_READY_FOR_INPUT
      EXPORTING
        I_READY_FOR_INPUT = 0.
  ENDIF.

*-- GR_EVENT_RECEIVER
  IF GR_EVENT_RECEIVER IS INITIAL.
    CREATE OBJECT GR_EVENT_RECEIVER.
  ENDIF.

* Handler Event
*  SET HANDLER:
*    GR_EVENT_RECEIVER->HANDLE_TOOLBAR       FOR ALL INSTANCES,
*    GR_EVENT_RECEIVER->HANDLE_DATA_CHANGED  FOR ALL INSTANCES,
*    GR_EVENT_RECEIVER->HANDLE_DATA_CHANGED_FINISHED
*      FOR ALL INSTANCES,
*    GR_EVENT_RECEIVER->HANDLE_USER_COMMAND  FOR ALL INSTANCES,
*    GR_EVENT_RECEIVER->HANDLE_HOTSPOT_CLICK FOR ALL INSTANCES,
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
*& Form SAVE_DATA_RTN
*&---------------------------------------------------------------------*
FORM SAVE_DATA_RTN .
  DATA: LS_ZCOT0190 TYPE ZCOT0190.

  READ TABLE GT_DISPLAY INTO GS_DISPLAY INDEX 1.
  IF SY-SUBRC = 0.

    MOVE-CORRESPONDING GS_DISPLAY TO LS_ZCOT0190.

    LS_ZCOT0190-ERDAT = SY-DATUM.
    LS_ZCOT0190-ERZET = SY-UZEIT.
    LS_ZCOT0190-ERNAM = SY-UNAME.
    LS_ZCOT0190-AEDAT = SY-DATUM.
    LS_ZCOT0190-AEZET = SY-UZEIT.
    LS_ZCOT0190-AENAM = SY-UNAME.

  ENDIF.

  SELECT SINGLE * INTO @DATA(ZCOT0190)
    FROM ZCOT0190
   WHERE KOKRS = @PA_KOKRS
     AND GJAHR = @PA_GJAHR
     AND VERSN = @PA_VERSN.
  IF SY-SUBRC EQ 0.

    LS_ZCOT0190-ERDAT = ZCOT0190-ERDAT.
    LS_ZCOT0190-ERZET = ZCOT0190-ERZET.
    LS_ZCOT0190-ERNAM = ZCOT0190-ERNAM.

    MODIFY ZCOT0190 FROM LS_ZCOT0190.

  ELSE.

    INSERT ZCOT0190 FROM LS_ZCOT0190.

  ENDIF.

*  GT_DISPLAY_LOG[] = GT_DISPLAY[].

  IF SY-SUBRC EQ 0.
    COMMIT WORK AND WAIT.
    " 성공적으로 저장하였습니다.
    MESSAGE S007.
    PERFORM SELECTED_DATA_RTN.
  ELSE.
    ROLLBACK WORK.
    " 저장에 실패하였습니다.
    MESSAGE S008 DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
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

*--------------------------------------------------------------------*
* [ESG_CO] DEV_ESG 기존PGM 고도화 #7, 2021.12.09 10:02:56, MDP_06
*--------------------------------------------------------------------*
* 계정별 잠금 조회버튼 추가
*--------------------------------------------------------------------*
  LS_FUNTXT = VALUE #( TEXT = '계정별 잠금조회' ).
  SSCRFIELDS-FUNCTXT_02 = LS_FUNTXT.

  LS_FUNTXT = VALUE #( TEXT = 'CO기간 잠금조회' ).
  SSCRFIELDS-FUNCTXT_03 = LS_FUNTXT.

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

*--------------------------------------------------------------------*
* [ESG_CO] DEV_ESG 기존PGM 고도화 #7, 2021.12.09 10:02:56, MDP_06
*--------------------------------------------------------------------*
    WHEN 'FC02'.
      " 파라메터 변경 없이 그대로 실행( 테스트모드로 수행되어 변경안됨 )
      CALL TRANSACTION 'OB52B' WITHOUT AUTHORITY-CHECK
                               AND SKIP FIRST SCREEN.
    WHEN 'FC03'.
      " 파라메터 변경 없이 그대로 실행( 테스트모드로 수행되어 변경안됨 )
      CALL TRANSACTION 'OKP2'  WITH AUTHORITY-CHECK
                               AND SKIP FIRST SCREEN.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form AUTHORITY_CHECK_2
*&---------------------------------------------------------------------*
FORM AUTHORITY_CHECK_2 .


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

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_MAIN_DATA_2
*&---------------------------------------------------------------------*
FORM SELECTED_MAIN_DATA_2 .
  CLEAR: GS_DISPLAY, GT_DISPLAY, GT_DISPLAY[],
         GT_DISPLAY_LOG, GT_DISPLAY_LOG[].

  SELECT A~KOKRS,
         A~BUKRS,
         B~BUTXT,
         @PA_GJAHR AS GJAHR,
         @PA_VERSN AS VERSN,
         C~MON01, C~MON02, C~MON03, C~MON04, C~MON05, C~MON06,
         C~MON07, C~MON08, C~MON09, C~MON10, C~MON11, C~MON12,
         C~AEDAT, C~AEZET, C~AENAM
    FROM TKA02 AS A INNER JOIN T001     AS B ON B~BUKRS EQ A~BUKRS
                    LEFT  JOIN ZCOT0190 AS C ON C~KOKRS EQ A~KOKRS
                                            AND C~BUKRS EQ A~BUKRS
                                            AND C~GJAHR EQ @PA_GJAHR
                                            AND C~VERSN EQ @PA_VERSN
   WHERE A~KOKRS EQ @PA_KOKRS
     AND A~BUKRS NOT LIKE '9%'
    INTO CORRESPONDING FIELDS OF TABLE @GT_DISPLAY.

  SORT GT_DISPLAY BY KOKRS BUKRS.

  GT_DISPLAY_LOG[] = GT_DISPLAY[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM MAKE_DISPLAY_DATA .

  CHECK GT_DISPLAY[] IS NOT INITIAL.


  RANGES LR_AENAM FOR GS_DISPLAY-AENAM.

  LOOP AT GT_DISPLAY INTO GS_DISPLAY WHERE AENAM IS NOT INITIAL.

    LR_AENAM = VALUE #( SIGN    = 'I'
                        OPTION  = 'EQ'
                        LOW     = GS_DISPLAY-AENAM ).

    APPEND LR_AENAM.

  ENDLOOP.

  CHECK LR_AENAM[] IS NOT INITIAL.
  SORT LR_AENAM BY LOW.
  DELETE ADJACENT DUPLICATES FROM LR_AENAM COMPARING LOW.

  SELECT A~BNAME,
         B~DATE_FROM,
         B~NAME_TEXT
    FROM USR21 AS A
    JOIN ADRP  AS B ON A~PERSNUMBER EQ B~PERSNUMBER
   WHERE A~BNAME IN @LR_AENAM
    INTO TABLE @DATA(LT_ADRP).


  SORT LT_ADRP BY BNAME DATE_FROM DESCENDING.

  LOOP AT GT_DISPLAY INTO GS_DISPLAY WHERE AENAM IS NOT INITIAL.

    READ TABLE LT_ADRP TRANSPORTING NO FIELDS
                       WITH KEY BNAME = GS_DISPLAY-AENAM
                                BINARY SEARCH.
    CHECK SY-SUBRC EQ 0.

    LOOP AT LT_ADRP INTO DATA(LS_ADRP) FROM SY-TABIX.
      IF LS_ADRP-BNAME NE GS_DISPLAY-AENAM.
        EXIT.
      ENDIF.

      GS_DISPLAY-AENAMTEXT = LS_ADRP-NAME_TEXT.

      IF LS_ADRP-DATE_FROM LE SY-DATUM.
        EXIT.
      ENDIF.
    ENDLOOP.

    MODIFY GT_DISPLAY FROM GS_DISPLAY TRANSPORTING AENAMTEXT.

  ENDLOOP.

  GT_DISPLAY_LOG[] = GT_DISPLAY[].

ENDFORM.
