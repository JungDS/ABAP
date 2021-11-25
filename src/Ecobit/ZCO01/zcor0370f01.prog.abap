*&---------------------------------------------------------------------*
*& Include          ZCOR0370F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITIAL_SET
*&---------------------------------------------------------------------*
FORM INITIAL_SET .

  CASE SY-TCODE.
    WHEN 'ZCOR0371'.
      GV_MODE = 'S'.
    WHEN OTHERS.
      GV_MODE = 'E'.
  ENDCASE.

  GV_REPID = SY-REPID.
  PA_KOKRS = '1000'.

  SELECT SINGLE BEZEI, WAERS INTO (@PA_KTXT, @GV_WAERS)
    FROM TKA01
   WHERE KOKRS = @PA_KOKRS.

  SELECT SINGLE VTEXT INTO @PA_VTXT
    FROM TKVS AS A
    LEFT JOIN TKVST AS B
      ON A~VERSI = B~VERSI
     AND B~SPRAS = @SY-LANGU
   WHERE A~VERSI = 'P0'.

  "__ 20191223 BSGSM_FCM ADD default cac
  SET PARAMETER ID 'CAC' FIELD PA_KOKRS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_CONTROLLING_AREA
*&---------------------------------------------------------------------*
FORM CHECK_CONTROLLING_AREA .

  SELECT SINGLE VTEXT INTO @PA_VTXT
    FROM TKVS AS A
    LEFT JOIN TKVST AS B
      ON A~VERSI = B~VERSI
     AND B~SPRAS = @SY-LANGU
   WHERE A~VERSI = @PA_VERSN.

  IF SY-SUBRC <> 0.
    SET CURSOR FIELD 'PA_VERSN'.
    MESSAGE E027  WITH TEXT-002.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_DATA_RTN
*&---------------------------------------------------------------------*
FORM SELECTED_DATA_RTN .

  CLEAR: R_POSID, R_POSID[].

  R_POSID-LOW    = 'C*****C'.
  R_POSID-SIGN   = 'I'.
  R_POSID-OPTION = 'CP'.
  APPEND R_POSID.

  CLEAR: GT_DISPLAY, GT_DISPLAY[].

  SELECT A~BUKRS, B~BUTXT, C~POSID, C~POST1,
         A~SAKNR, D~TXT20, A~WAERS, A~OBJNR,
         A~HSL01, A~HSL02, A~HSL03, A~HSL04, A~HSL05,
         A~HSL06, A~HSL07, A~HSL08, A~HSL09,
         A~HSL10, A~HSL11, A~HSL12
    INTO TABLE @GT_DISPLAY
    FROM ZFIT0621 AS A
    LEFT JOIN T001 AS B
      ON A~BUKRS = B~BUKRS
    LEFT JOIN PRPS AS C
      ON A~OBJNR = C~OBJNR
    LEFT JOIN SKAT AS D
      ON A~SAKNR = D~SAKNR
     AND D~KTOPL = @GC_KTOPL
     AND D~SPRAS = @SY-LANGU
   WHERE A~KOKRS = @PA_KOKRS
     AND A~RYEAR = @PA_GJAHR
     AND A~RVERS = @PA_VERSN.

  IF SY-SUBRC <> 0.

    CASE GV_MODE.

      WHEN 'S'.
        MESSAGE S004 DISPLAY LIKE 'E'.
        STOP.

      WHEN OTHERS.

        PERFORM POPUP_TO_CONFIRM USING TEXT-PT1
                                       TEXT-QT2.

        CASE GV_ANSWER.

          WHEN '1'.

            SELECT A~PBUKR AS BUKRS, B~BUTXT ,
                   A~POSID, A~POST1, C~SAKNR, D~TXT20,
                   B~WAERS AS WAERS, A~OBJNR
              INTO CORRESPONDING FIELDS OF TABLE @GT_DISPLAY
              FROM PRPS AS A
              INNER JOIN T001 AS B
                ON A~PBUKR = B~BUKRS
              INNER JOIN SKB1 AS C
                ON B~BUKRS = C~BUKRS
              LEFT JOIN SKAT AS D
                ON C~SAKNR = D~SAKNR
               AND D~KTOPL = @GC_KTOPL
             WHERE A~POSID IN @R_POSID
               AND B~BUKRS BETWEEN '1100' AND '9040'
               AND C~SAKNR BETWEEN '0984000100' AND
                                   '0984000999'
              ORDER BY A~PBUKR, A~POSID, C~SAKNR.

          WHEN OTHERS.
            MESSAGE S004 DISPLAY LIKE 'E'.
            STOP.
        ENDCASE.

    ENDCASE.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_INSTANCE_0100
*&---------------------------------------------------------------------*
FORM CREATE_INSTANCE_0100 .

  CREATE OBJECT GR_SPLITTER1
    EXPORTING
      ROWS    = 2
      COLUMNS = 1
      PARENT  = CL_GUI_SPLITTER_CONTAINER=>SCREEN0.

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

ENDFORM.
*&---------------------------------------------------------------------*
*& Form INIT_LAYOUT_0100
*&---------------------------------------------------------------------*
FORM INIT_LAYOUT_0100 .

  CLEAR GS_LAYOUT.

  GS_LAYOUT-EDIT_MODE  = ABAP_TRUE.
  GS_LAYOUT-ZEBRA      = ABAP_TRUE.
*  GS_LAYOUT-CWIDTH_OPT = ABAP_TRUE.
  GS_LAYOUT-SEL_MODE   = SPACE.     "B:단일,C:복수,D:셀,A:행/열
  GS_LAYOUT-BOX_FNAME  = SPACE.
  GS_LAYOUT-NO_ROWMARK = SPACE.

ENDFORM.
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

*
  _SET_EX:

    CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW,


    CL_GUI_ALV_GRID=>MC_FC_LOC_COPY,
    CL_GUI_ALV_GRID=>MC_FC_LOC_CUT,
    CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE,
    CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
  "-- end

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

  PERFORM GET_FIELDCATLOG_DATA.
  PERFORM MODIFY_FIELDCATLOG_DATA.

ENDFORM.                    " APPEND_FIELDCAT_0100
*&---------------------------------------------------------------------*
*&      Form  GET_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
FORM GET_FIELDCATLOG_DATA .

  DATA: LT_FIELDCAT TYPE KKBLO_T_FIELDCAT.

  CALL FUNCTION 'K_KKB_FIELDCAT_MERGE'
    EXPORTING
      I_CALLBACK_PROGRAM     = SY-REPID
      I_STRUCNAME            = 'ZCOS0350'
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
        GS_FIELDCAT-OUTPUTLEN = 8.

      WHEN 'BUTXT'.
        GS_FIELDCAT-OUTPUTLEN = 18.

      WHEN 'POSID'.
        GS_FIELDCAT-OUTPUTLEN = 10.

      WHEN 'POST1'.
        GS_FIELDCAT-OUTPUTLEN = 25.

      WHEN 'SAKNR'.
        GS_FIELDCAT-OUTPUTLEN = 12.

      WHEN 'TXT20'.
        GS_FIELDCAT-OUTPUTLEN = 30.

      WHEN 'OBJNR'.
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.

    ENDCASE.

    IF GS_FIELDCAT-FIELDNAME CP 'HSL*'.
      LV_TEXT = GS_FIELDCAT-FIELDNAME+3(2) &&
                TEXT-C01.
      GS_FIELDCAT-EDIT      = ABAP_TRUE.
      GS_FIELDCAT-NO_ZERO   = ABAP_TRUE.
      GS_FIELDCAT-OUTPUTLEN = 18.
    ELSE.
      GS_FIELDCAT-KEY = ABAP_TRUE.
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
*&      Form  REGIST_ALV_EVENT_0100
*&---------------------------------------------------------------------*
FORM REGIST_ALV_EVENT_0100 USING PR_GRID TYPE REF TO CL_GUI_ALV_GRID.

  CASE GV_MODE.

    WHEN 'S'.

      CALL METHOD PR_GRID->SET_READY_FOR_INPUT
        EXPORTING
          I_READY_FOR_INPUT = 0.

    WHEN 'E'.

* REGISTER EVENT
      CALL METHOD PR_GRID->REGISTER_EDIT_EVENT
        EXPORTING
          I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
*
      CALL METHOD PR_GRID->SET_READY_FOR_INPUT
        EXPORTING
          I_READY_FOR_INPUT = 1.

  ENDCASE.

*-- GR_EVENT_RECEIVER
  IF GR_EVENT_RECEIVER IS INITIAL.
    CREATE OBJECT GR_EVENT_RECEIVER.
  ENDIF.

* Handler Event
  SET HANDLER:
    GR_EVENT_RECEIVER->HANDLE_DATA_CHANGED  FOR ALL INSTANCES,
    GR_EVENT_RECEIVER->HANDLE_USER_COMMAND  FOR ALL INSTANCES,
    GR_EVENT_RECEIVER->HANDLE_DOUBLE_CLICK  FOR ALL INSTANCES.

ENDFORM.                    " REGIST_ALV_EVENT_0100
*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV_GRID_0100
*&---------------------------------------------------------------------*
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

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM EVENT_DATA_CHANGED
       USING PR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
             PV_ONF4          TYPE CHAR01
             PV_ONF4_BEFORE   TYPE CHAR01
             PV_ONF4_AFTER    TYPE CHAR01
             PV_UCOMM         TYPE SY-UCOMM
             PR_SENDER       TYPE REF TO CL_GUI_ALV_GRID.

*--- Begin or Example
  DATA: LS_MOD_CELLS TYPE LVC_S_MODI,
        LS_INS_CELLS TYPE LVC_S_MOCE,
        LS_DEL_CELLS TYPE LVC_S_MOCE.

  DATA LV_POST1 TYPE PS_POST1.
  DATA LV_KTEXT TYPE KTEXT.
  DATA LV_POSID TYPE PS_POSID.
  DATA LV_KOSTL TYPE KOSTL.


  DATA LV_TABIX TYPE SY-TABIX.

  DEFINE _MODIFY_CELL.
    CALL METHOD pr_data_changed->modify_cell
      EXPORTING
        i_fieldname = &1
        i_row_id    = &2
        i_value     = &3.

  END-OF-DEFINITION.

  DEFINE _GET_CELL_VALUE.
    CALL METHOD pr_data_changed->get_cell_value
      EXPORTING
        i_fieldname = &1
        i_row_id    = &2
      IMPORTING
        e_value     = &3.
  END-OF-DEFINITION.

  DEFINE _ADD_PROTOCOL.
    CALL METHOD pr_data_changed->add_protocol_entry
      EXPORTING
        i_fieldname = &1
        i_row_id    = &2
        i_msgid     = 'ZCO01'
        i_msgty     = &3
        i_msgno     = &4
        i_msgv1     = &5
        i_msgv2     = &6
        i_msgv3     = &7
        i_msgv4     = &8.
  END-OF-DEFINITION.

  DEFINE _MODIFY_STYLE.
    CALL METHOD pr_data_changed->modify_style
      EXPORTING
        i_fieldname = &1
        i_row_id    = &2
        i_style     = &3.
  END-OF-DEFINITION.
*--- End of Example

  CLEAR GV_EXIT.

  CASE PR_SENDER.

    WHEN GR_GRID1.

  ENDCASE.

  CHECK GV_EXIT IS INITIAL.

  CALL METHOD PR_DATA_CHANGED->DISPLAY_PROTOCOL.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EVENT_DATA_CHANGED_FINISHED
*&---------------------------------------------------------------------*
FORM EVENT_DATA_CHANGED_FINISHED
       USING P_MODIFIED    TYPE CHAR01
             PT_GOOD_CELLS TYPE LVC_T_MODI
             PR_SENDER    TYPE REF TO CL_GUI_ALV_GRID.


* - Prefix 정의 - Form Parameter
*   1. PT_  : Internal Table or Ranges
*   2. PS_  : Structure
*   3. PV_  : Variables
*   4. PR_ : Reference Variables


ENDFORM.                    " EVENT_DATA_CHANGED_FINISHED
*&---------------------------------------------------------------------*
*&      Form  EVENT_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM EVENT_HOTSPOT_CLICK USING PS_ROW_ID    TYPE LVC_S_ROW
                               PS_COLUMN_ID TYPE LVC_S_COL
                               PS_ROW_NO    TYPE LVC_S_ROID
                               PR_SENDER   TYPE REF TO CL_GUI_ALV_GRID.


* - Prefix 정의 - Form Parameter
*   1. PT_  : Internal Table or Ranges
*   2. PS_  : Structure
*   3. PV_  : Variables
*   4. PR_ : Reference Variables

*
*  CASE PR_SENDER.
*    WHEN GR_GRID1.
*
*      CASE PS_COLUMN_ID-FIELDNAME.
*        WHEN 'CONNID'.
*
*      ENDCASE.
*
*    WHEN OTHERS.
*  ENDCASE.


ENDFORM.                    " EVENT_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*&      Form  EVENT_DOUBLE_CLICK
*&---------------------------------------------------------------------*
FORM EVENT_DOUBLE_CLICK  USING PS_ROW     TYPE LVC_S_ROW
                               PS_COLUMN  TYPE LVC_S_COL
                               PS_ROW_NO  TYPE LVC_S_ROID
                               PR_SENDER TYPE REF TO CL_GUI_ALV_GRID.


* - Prefix 정의 - Form Parameter
*   1. PT_  : Internal Table or Ranges
*   2. PS_  : Structure
*   3. PV_  : Variables
*   4. PR_ : Reference Variables


*  CASE PR_SENDER.
*    WHEN GR_GRID1.
*
*    WHEN OTHERS.
*  ENDCASE.

ENDFORM.                    " EVENT_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*&      Form  EVENT_HELP_ON_F4
*&---------------------------------------------------------------------*
FORM EVENT_HELP_ON_F4
       USING PV_FIELDNAME   TYPE LVC_FNAME
             PV_FIELDVALUE  TYPE LVC_VALUE
             PS_ROW_NO      TYPE LVC_S_ROID
             PR_EVENT_DATA TYPE REF TO CL_ALV_EVENT_DATA
             PT_BAD_CELLS   TYPE LVC_T_MODI
             PV_DISPLAY     TYPE CHAR01
             PR_SENDER     TYPE REF TO CL_GUI_ALV_GRID.


*  DATA :  IS_MODI TYPE LVC_S_MODI.
*  FIELD-SYMBOLS <F4TAB> TYPE LVC_T_MODI.
*  ASSIGN PR_EVENT_DATA->M_DATA->* TO <F4TAB>.
*
*  DATA LV_KAGRU TYPE KAGRU.

*  CASE PR_SENDER.
*
*    WHEN GR_GRID1.
*
*      CASE PV_FIELDNAME.
*
*        WHEN 'KAGRU'.
*
**          CALL FUNCTION 'K_GROUP_SELECT'
**            EXPORTING
**              FIELD_NAME    = 'KSTAR'
**              KOKRS         = PA_KOKRS
**              KTOPL         = GC_KTOPL
**            IMPORTING
**              SET_NAME      = LV_KAGRU
**            EXCEPTIONS
**              NO_SET_PICKED = 1
**              OTHERS        = 2.
**
**          IF SY-SUBRC <> 0.
**            EXIT.
**          ENDIF.
**
**          IF PV_DISPLAY IS INITIAL AND LV_KAGRU IS NOT INITIAL.
**            IS_MODI-ROW_ID    = PS_ROW_NO-ROW_ID.
**            IS_MODI-FIELDNAME = PV_FIELDNAME.
**            IS_MODI-VALUE     = LV_KAGRU.
**            APPEND IS_MODI TO <F4TAB>.
**          ENDIF.
**
**          PR_EVENT_DATA->M_EVENT_HANDLED = 'X'.
*
*      ENDCASE.
*
*    WHEN OTHERS.
*
*  ENDCASE.

ENDFORM.                    " EVENT_HELP_ON_F4
*&---------------------------------------------------------------------*
*&      Form  EVENT_TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM EVENT_TOP_OF_PAGE USING PR_DD         TYPE REF TO CL_DD_DOCUMENT
                             PV_TABLE_INDEX TYPE SYINDEX
                             PR_SENDER     TYPE REF TO CL_GUI_ALV_GRID.

* - Prefix 정의 - Form Parameter
*   1. PT_  : Internal Table or Ranges
*   2. PS_  : Structure
*   3. PV_  : Variables
*   4. PR_ : Reference Variables


*  CASE PR_SENDER.
*    WHEN GR_GRID1.
*    WHEN OTHERS.
*  ENDCASE.
ENDFORM.                    " EVENT_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  EVENT_END_OF_LIST
*&---------------------------------------------------------------------*
FORM EVENT_END_OF_LIST USING PR_DD     TYPE REF TO CL_DD_DOCUMENT
                             PR_SENDER TYPE REF TO CL_GUI_ALV_GRID.

* - Prefix 정의 - Form Parameter
*   1. PT_  : Internal Table or Ranges
*   2. PS_  : Structure
*   3. PV_  : Variables
*   4. PR_ : Reference Variables


*  CASE PR_SENDER.
*    WHEN GR_GRID1.
*    WHEN OTHERS.
*  ENDCASE.


ENDFORM.                    " EVENT_END_OF_LIST
*&---------------------------------------------------------------------*
*&      Form  EVENT_TOOLBAR
*&---------------------------------------------------------------------*
FORM EVENT_TOOLBAR
       USING PR_OBJECT     TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET
             PV_INTERACTIVE TYPE CHAR01
             PR_SENDER     TYPE REF TO CL_GUI_ALV_GRID.

  CASE PR_SENDER.

    WHEN GR_GRID1.

*      "ADD_BUTTON : OBJECT, BTYPE, FUNC, ICON, INFO, TEXT, DISABLE
*      PERFORM ADD_BUTTON
*        USING: PR_OBJECT '3' SPACE SPACE SPACE SPACE SPACE, "분리자
*
*               PR_OBJECT '0' '&APD' ICON_CREATE TEXT-BT1
*                 TEXT-BT1 SPACE,
*
*               PR_OBJECT '0' '&INS' ICON_INSERT_ROW TEXT-BT2
*                 TEXT-BT2 SPACE,
*
*               PR_OBJECT '0' '&DEL' ICON_DELETE_ROW TEXT-BT3
*                 TEXT-BT3 SPACE.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " EVENT_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form ADD_BUTTON
*&---------------------------------------------------------------------*
FORM ADD_BUTTON USING PR_OBJECT TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET
                    PV_BTYPE
                    PV_FUNC
                    PV_ICON
                    PV_INFO
                    PV_TEXT
                    PV_DISA.

  DATA: LS_BUTTON TYPE STB_BUTTON,
        LS_BTNMNU TYPE STB_BTNMNU,

        LT_BUTTON TYPE TTB_BUTTON,
        LT_BTNMNU TYPE TTB_BTNMNU.

  CLEAR LS_BUTTON.
  LS_BUTTON-BUTN_TYPE = PV_BTYPE.
  LS_BUTTON-FUNCTION  = PV_FUNC.
  LS_BUTTON-ICON      = PV_ICON.
  LS_BUTTON-QUICKINFO = PV_INFO.

  LS_BUTTON-TEXT      = PV_TEXT.
  LS_BUTTON-DISABLED  = PV_DISA.

  APPEND LS_BUTTON TO PR_OBJECT->MT_TOOLBAR.

ENDFORM.                   " ADD_BUTTON
*&---------------------------------------------------------------------*
*&      Form  EVENT_USER_COMMAND
*&---------------------------------------------------------------------*
FORM EVENT_USER_COMMAND  USING PV_UCOMM   TYPE SY-UCOMM
                               PR_SENDER TYPE REF TO CL_GUI_ALV_GRID.

  DATA: LV_ROW TYPE I,
        LV_COL TYPE I.

  CLEAR: GT_ROWS, GT_ROWS[].

  "선택 ROW가져오기
  CALL METHOD PR_SENDER->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GT_ROWS[].

  "GRID에 따라 처리.
  CASE PR_SENDER.
    WHEN GR_GRID1.
      CASE PV_UCOMM.
        WHEN '&APD'.   "-- 행 추가

      ENDCASE.

    WHEN OTHERS.
  ENDCASE.


ENDFORM.                    " EVENT_USER_COMMAND
*&---------------------------------------------------------------------*
*& Form SET_SCREEN
*&---------------------------------------------------------------------*
FORM SET_SCREEN .

  LOOP AT SCREEN.
    IF SCREEN-NAME = 'PA_KOKRS'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DELETE_DATA_RTN
*&---------------------------------------------------------------------*
FORM DELETE_DATA_RTN .

  DATA: LV_MESSAGE TYPE STRING.

  TRY .

      DELETE FROM ZFIT0621 WHERE KOKRS  = @PA_KOKRS
                             AND RVERS  = @PA_VERSN
                             AND RYEAR  = @PA_GJAHR.

      COMMIT WORK.

      MESSAGE S009.

      CLEAR: GT_DISPLAY, GT_DISPLAY[].

      SELECT A~PBUKR AS BUKRS, B~BUTXT ,
             A~POSID, A~POST1, C~SAKNR, D~TXT20,
             B~WAERS AS WAERS
        INTO CORRESPONDING FIELDS OF TABLE @GT_DISPLAY
        FROM PRPS AS A
        INNER JOIN T001 AS B
          ON A~PBUKR = B~BUKRS
        INNER JOIN SKB1 AS C
          ON B~BUKRS = C~BUKRS
        LEFT JOIN SKAT AS D
          ON C~SAKNR = D~SAKNR
         AND D~KTOPL = @GC_KTOPL
       WHERE A~POSID IN @R_POSID
         AND B~BUKRS NOT LIKE '9%'
         AND C~SAKNR BETWEEN '0984000100' AND
                             '0984000999'
        ORDER BY A~PBUKR, A~POSID, C~SAKNR.

    CATCH CX_SY_SQL_ERROR INTO DATA(LR_ERROR).

      ROLLBACK WORK.

      LV_MESSAGE = LR_ERROR->GET_TEXT( ).
      MESSAGE S001 WITH LV_MESSAGE DISPLAY LIKE 'E'.

  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCRFIELDS_FUNCTXT
*&---------------------------------------------------------------------*
FORM SCRFIELDS_FUNCTXT .

  GS_FUNTXT-ICON_ID   = ICON_INFORMATION.
  GS_FUNTXT-QUICKINFO = 'Program Help'.

  SSCRFIELDS-FUNCTXT_01 = GS_FUNTXT.

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
