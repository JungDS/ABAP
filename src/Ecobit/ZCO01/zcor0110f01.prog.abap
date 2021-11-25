*&---------------------------------------------------------------------*
*& Include          ZCOR0110F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITAIL
*&---------------------------------------------------------------------*
FORM INITAIL .

  GV_REPID = SY-REPID.

  SELECT SINGLE BEZEI, WAERS INTO (@PA_KTXT, @GV_WAERS)
    FROM TKA01
   WHERE KOKRS = @PA_KOKRS.

  SELECT SINGLE VTEXT INTO @PA_VTXT
    FROM TKVS AS A
    LEFT JOIN TKVST AS B
      ON A~VERSI = B~VERSI
     AND B~SPRAS = @SY-LANGU
   WHERE A~VERSI = 'B1'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SCREEN
*&---------------------------------------------------------------------*
FORM SET_SCREEN .

  LOOP AT SCREEN.

    IF SCREEN-NAME = 'PA_KOKRS' OR
       SCREEN-NAME = 'PA_VERSN'.

      SCREEN-INPUT = 0.
      MODIFY SCREEN.

    ENDIF.

    CASE ABAP_TRUE.
      WHEN PA_RAD1.
        IF SCREEN-GROUP1 = 'WBS'.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ENDIF.
      WHEN PA_RAD2.
        IF SCREEN-GROUP1 = 'KOS'.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ENDIF.
    ENDCASE.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_B1
*&---------------------------------------------------------------------*
FORM CHECK_B1.

  IF PA_SPERL > PA_EPERL.
    SET CURSOR FIELD 'PA_SPERL'.
    MESSAGE E018  WITH TEXT-E03.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DATA_GET
*&---------------------------------------------------------------------*
FORM DATA_GET .

  DATA  LV_COUNT TYPE I.
  DATA: LV_FIELDNAME TYPE FIELDNAME,
        LV_MONTH     TYPE N LENGTH 3.

  FIELD-SYMBOLS: <FS_HSL> TYPE ANY,
                 <FS_SUM> TYPE ANY,
                 <FS_ALL> TYPE ANY.

  CLEAR: GT_OUTTAB.

  LV_COUNT = PA_EPERL - PA_SPERL + 1.

  SELECT A~KOSTL, B~KTEXT
    INTO TABLE @DATA(LT_KOSTL)
    FROM CSKS AS A
    LEFT JOIN CSKT AS B
      ON A~KOKRS = B~KOKRS
     AND A~KOSTL = B~KOSTL
     AND A~DATBI = B~DATBI
     AND B~SPRAS = @SY-LANGU
   WHERE A~KOKRS = @PA_KOKRS.

  SELECT OBJNR, PSPNR, POSID, POST1 INTO TABLE @DATA(LT_PRPS)
    FROM PRPS.

  SELECT A~RVERS, A~ROBJNR, A~RKSTAR, B~KTEXT, A~PROCESS_9, A~RTCUR,
         SUM( A~HSL01 ) AS HSL01, SUM( A~HSL02 ) AS HSL02,
         SUM( A~HSL03 ) AS HSL03, SUM( A~HSL04 ) AS HSL04,
         SUM( A~HSL05 ) AS HSL05, SUM( A~HSL06 ) AS HSL06,
         SUM( A~HSL07 ) AS HSL07, SUM( A~HSL08 ) AS HSL08,
         SUM( A~HSL09 ) AS HSL09, SUM( A~HSL10 ) AS HSL10,
         SUM( A~HSL11 ) AS HSL11, SUM( A~HSL12 ) AS HSL12
    INTO TABLE @DATA(LT_ZCOT0040)
  FROM ZCOT0040 AS A
  LEFT JOIN CSKU AS B
    ON A~RKSTAR = B~KSTAR
   AND B~KTOPL = @GV_KTOPL
   AND B~SPRAS = @SY-LANGU
 WHERE A~RLDNR = '00'
   AND A~RRCTY = '1'
   AND A~RVERS = 'B1'
   AND A~RYEAR = @PA_GJAHR
   AND A~ROBJNR IN @R_OBJNR
   AND A~RKOKRS = @PA_KOKRS
   AND A~RKSTAR IN @R_KSTAR
  GROUP BY A~RVERS, A~ROBJNR, A~RKSTAR, B~KTEXT, A~PROCESS_9, A~RTCUR.

  IF SY-SUBRC <> 0.
    MESSAGE S004 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  CASE ABAP_TRUE.

    WHEN PA_RAD1.
      DELETE LT_ZCOT0040 WHERE ROBJNR NP 'KS*'.

    WHEN PA_RAD2.
      DELETE LT_ZCOT0040 WHERE ROBJNR NP 'PR*'.

  ENDCASE.

  LOOP AT LT_ZCOT0040 ASSIGNING FIELD-SYMBOL(<LS_ZCOT0040>).

    CASE <LS_ZCOT0040>-ROBJNR(2).

      WHEN 'KS'.
        MOVE <LS_ZCOT0040>-ROBJNR+6 TO GS_OUTTAB-KOSTL.

        READ TABLE LT_KOSTL ASSIGNING FIELD-SYMBOL(<LS_KOSTL>)
                     WITH KEY KOSTL = GS_OUTTAB-KOSTL.

        IF SY-SUBRC = 0.
          MOVE <LS_KOSTL>-KTEXT TO GS_OUTTAB-KTEXT.
        ENDIF.

      WHEN 'PR'.
        READ TABLE LT_PRPS ASSIGNING FIELD-SYMBOL(<LS_PRPS>)
           WITH KEY OBJNR = <LS_ZCOT0040>-ROBJNR.

        IF SY-SUBRC = 0.

          MOVE:  <LS_PRPS>-POSID TO GS_OUTTAB-POSID,
                 <LS_PRPS>-POST1 TO GS_OUTTAB-POST1.

        ENDIF.

    ENDCASE.

    MOVE: <LS_ZCOT0040>-RKSTAR TO GS_OUTTAB-KSTAR,
          <LS_ZCOT0040>-KTEXT TO GS_OUTTAB-KSTXT,
          <LS_ZCOT0040>-RTCUR TO GS_OUTTAB-WAERS.

    LV_MONTH = PA_SPERL.

    DO LV_COUNT TIMES.

      IF LV_MONTH > PA_EPERL.
        EXIT.
      ENDIF.

      LV_FIELDNAME = '<LS_ZCOT0040>-HSL' && LV_MONTH+1(2).

      ASSIGN (LV_FIELDNAME) TO <FS_HSL>.

      ASSIGN COMPONENT  <LS_ZCOT0040>-PROCESS_9
             OF STRUCTURE GS_OUTTAB TO <FS_SUM>.

      ASSIGN COMPONENT  'ALL' OF STRUCTURE GS_OUTTAB TO <FS_ALL>.

      <FS_SUM> = <FS_SUM> + <FS_HSL>.
      <FS_ALL> = <FS_ALL> + <FS_HSL>.

      ADD 1 TO LV_MONTH.

    ENDDO.

    COLLECT GS_OUTTAB INTO GT_OUTTAB.
    CLEAR  GS_OUTTAB.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SALV_CALL
*&---------------------------------------------------------------------*
FORM SALV_CALL .

  TRY.
      CL_SALV_TABLE=>FACTORY(
                IMPORTING
                  R_SALV_TABLE = GO_ALV
                CHANGING
                  T_TABLE      = GT_OUTTAB ).
    CATCH CX_SALV_MSG.
  ENDTRY.

  PERFORM SET_PF_STATUS.
  PERFORM SET_LAYOUT.

  PERFORM SET_TOP_OF_PAGE.

  PERFORM SET_EVENT.
  PERFORM SET_TABLE_SETTINGS.

  GO_ALV->DISPLAY( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_PF_STATUS
*&---------------------------------------------------------------------*
FORM SET_PF_STATUS .

  DATA: LT_FUNC_LIST TYPE SALV_T_UI_FUNC,
        LS_FUNC_LIST TYPE SALV_S_UI_FUNC.

  DATA: L_TITLE TYPE LVC_TITLE,
        L_LINES TYPE C LENGTH 100,
        L_COUNT TYPE I.

  DATA LV_TEXT TYPE CHAR100.
  DATA LV_GUBUN  TYPE CHAR40.


  GO_ALV->SET_SCREEN_STATUS(
    PFSTATUS      =  'STANDARD'
    REPORT        =  GV_REPID
    SET_FUNCTIONS = GO_ALV->C_FUNCTIONS_ALL ).

  L_COUNT = LINES( GT_OUTTAB ).

  CASE ABAP_TRUE.

    WHEN PA_RAD1.
      CONCATENATE TEXT-T00  ':' TEXT-005
            INTO LV_TEXT SEPARATED BY SPACE.

    WHEN PA_RAD2.
      CONCATENATE TEXT-T00  ':' TEXT-006
            INTO LV_TEXT SEPARATED BY SPACE.

  ENDCASE.

  WRITE L_COUNT TO L_LINES.
  CONDENSE L_LINES.

  CONCATENATE LV_TEXT '(' 'Selected entries :' L_LINES ')'
        INTO L_TITLE SEPARATED BY SPACE.

* set output control : ZEBRA
  GO_DSPSET = GO_ALV->GET_DISPLAY_SETTINGS( ).
  GO_DSPSET->SET_STRIPED_PATTERN( ABAP_TRUE ).
  GO_DSPSET->SET_LIST_HEADER( L_TITLE ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LAYOUT
*&---------------------------------------------------------------------*
FORM SET_LAYOUT .

  GS_SALV_LAYOUT-REPID = SY-REPID.
  GS_SALV_LAYOUT-DEFAULT = 'X'.
  GS_SALV_LAYOUT-LAYOUT = '/STANDARD'.
  GS_KEY-REPORT = GV_REPID.

  GO_LAYOUT = GO_ALV->GET_LAYOUT( ).
  GO_LAYOUT->SET_KEY( GS_KEY ).

  GS_SALV_LAYOUT-RESTRICT = IF_SALV_C_LAYOUT=>RESTRICT_NONE.
  GO_LAYOUT->SET_DEFAULT( GS_SALV_LAYOUT-DEFAULT ).
  GO_LAYOUT->SET_SAVE_RESTRICTION( GS_SALV_LAYOUT-RESTRICT ).
  GO_LAYOUT->SET_INITIAL_LAYOUT( GS_SALV_LAYOUT-LAYOUT ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_EVENT
*&---------------------------------------------------------------------*
FORM SET_EVENT .

*-- EVENT
  DATA: LR_EVENTS TYPE REF TO CL_SALV_EVENTS_TABLE.

  GO_FUNCTIONS = GO_ALV->GET_FUNCTIONS( ).
  GO_FUNCTIONS->SET_ALL( ).

  LR_EVENTS = GO_ALV->GET_EVENT( ).

  CREATE OBJECT G_EVENT_RECEIVER.
  SET HANDLER G_EVENT_RECEIVER->ON_USER_COMMAND FOR LR_EVENTS.
  SET HANDLER G_EVENT_RECEIVER->TOP_OF_PAGE     FOR LR_EVENTS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TABLE_SETTINGS
*&---------------------------------------------------------------------*
FORM SET_TABLE_SETTINGS .

  DATA: LR_COLUMNS  TYPE REF TO CL_SALV_COLUMNS_TABLE.
  DATA: LO_AGGRS    TYPE REF TO CL_SALV_AGGREGATIONS.
  DATA: LO_SORT     TYPE REF TO CL_SALV_SORTS.

*-- set column
  TRY.
      LR_COLUMNS = GO_ALV->GET_COLUMNS( ).
      LO_AGGRS   = GO_ALV->GET_AGGREGATIONS( ).
      LO_SORT = GO_ALV->GET_SORTS( ).

*      LR_COLUMNS->SET_OPTIMIZE( ABAP_TRUE ).
      LR_COLUMNS->SET_CELL_TYPE_COLUMN( 'CELLTYPE' ).
    CATCH CX_SALV_DATA_ERROR.
  ENDTRY.

  PERFORM SET_COLUMNS_TECHNICAL USING LR_COLUMNS
                                      LO_AGGRS
                                      LO_SORT.

  TRY.
      LR_COLUMNS->SET_KEY_FIXATION( ABAP_TRUE ).
    CATCH CX_SALV_DATA_ERROR.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_COLUMNS_TECHNICAL
*&---------------------------------------------------------------------*
FORM SET_COLUMNS_TECHNICAL USING IR_COLUMNS TYPE REF TO
                                            CL_SALV_COLUMNS_TABLE
                                  IR_AGG TYPE REF TO
                                            CL_SALV_AGGREGATIONS
                                  IR_SORT TYPE REF TO
                                            CL_SALV_SORTS.

  DATA LR_COLUMN  TYPE REF TO CL_SALV_COLUMN_TABLE.

  DATA: LR_SELECTIONS TYPE REF TO CL_SALV_SELECTIONS,
        LT_COLUMN     TYPE SALV_T_COLUMN.

  DATA LV_LTEXT TYPE SCRTEXT_L.
  DATA LV_MTEXT TYPE SCRTEXT_M.
  DATA LV_STEXT TYPE SCRTEXT_S.

  FIELD-SYMBOLS:
    <COLUMN_REF> LIKE LINE OF GT_COLUMN_REF.

  GT_COLUMN_REF = IR_COLUMNS->GET( ).

  TRY.
      LOOP AT GT_COLUMN_REF
        ASSIGNING <COLUMN_REF>.
        GR_COLUMN ?= IR_COLUMNS->GET_COLUMN( <COLUMN_REF>-COLUMNNAME ).

        PERFORM SET_COLUMN_TEXT USING    <COLUMN_REF>-COLUMNNAME
                                CHANGING GV_COLUMN_TEXT.

        IF GV_COLUMN_TEXT IS NOT INITIAL.

          GV_SCRTEXT_S = GV_COLUMN_TEXT.
          GV_SCRTEXT_M = GV_COLUMN_TEXT.
          GV_SCRTEXT_L = GV_COLUMN_TEXT.

          GR_COLUMN->SET_SHORT_TEXT( GV_SCRTEXT_S ).
          GR_COLUMN->SET_MEDIUM_TEXT( GV_SCRTEXT_M ).
          GR_COLUMN->SET_LONG_TEXT( GV_SCRTEXT_L ).

        ENDIF.

        IF ( <COLUMN_REF>-COLUMNNAME = 'ENTR' OR
            <COLUMN_REF>-COLUMNNAME = 'ALL'  OR
            <COLUMN_REF>-COLUMNNAME = 'SUPL' OR
            <COLUMN_REF>-COLUMNNAME = 'RENT' OR
            <COLUMN_REF>-COLUMNNAME = 'SEND' OR
            <COLUMN_REF>-COLUMNNAME = 'RECV' OR
            <COLUMN_REF>-COLUMNNAME = 'DIFF' ).

          CALL METHOD IR_AGG->ADD_AGGREGATION
            EXPORTING
              COLUMNNAME  = <COLUMN_REF>-COLUMNNAME
              AGGREGATION = IF_SALV_C_AGGREGATION=>TOTAL.

        ELSE.

          CALL METHOD IR_SORT->ADD_SORT
            EXPORTING
              COLUMNNAME = <COLUMN_REF>-COLUMNNAME
              SUBTOTAL   = IF_SALV_C_BOOL_SAP=>FALSE.

        ENDIF.

      ENDLOOP.

    CATCH CX_SALV_NOT_FOUND.
    CATCH CX_SALV_DATA_ERROR.
    CATCH CX_SALV_EXISTING.

  ENDTRY.

  CASE ABAP_TRUE.

    WHEN PA_RAD1.

      TRY.
          LR_COLUMN ?= IR_COLUMNS->GET_COLUMN( 'POSID' ).
          LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
          LR_COLUMN ?= IR_COLUMNS->GET_COLUMN( 'POST1' ).
          LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
        CATCH CX_SALV_NOT_FOUND.
      ENDTRY.

    WHEN PA_RAD2.

      TRY.
          LR_COLUMN ?= IR_COLUMNS->GET_COLUMN( 'KOSTL' ).
          LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
          LR_COLUMN ?= IR_COLUMNS->GET_COLUMN( 'KTEXT' ).
          LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
        CATCH CX_SALV_NOT_FOUND.
      ENDTRY.

  ENDCASE.

*-- SELECT FIELD 추가
  LR_SELECTIONS = GO_ALV->GET_SELECTIONS( ).
  LR_SELECTIONS->SET_SELECTION_MODE( IF_SALV_C_SELECTION_MODE=>CELL ).

  IR_AGG->SET_AGGREGATION_BEFORE_ITEMS( ).

ENDFORM. " SET_COLUMNS_TECHNICAL
*&---------------------------------------------------------------------*
*& Form HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
FORM HANDLE_USER_COMMAND USING P_UCOMM TYPE SALV_DE_FUNCTION.

  DATA L_DUMMY TYPE C LENGTH 100.

  CASE P_UCOMM.

    WHEN '&HELP'.
      PERFORM CALL_POPUP_HELP(ZCAR9000)
               USING SY-REPID SY-DYNNR SY-LANGU ''.

    WHEN '&RESH'.
      PERFORM DATA_GET.
      CALL METHOD GO_ALV->REFRESH
        EXPORTING
          REFRESH_MODE = IF_SALV_C_REFRESH=>FULL.

      MESSAGE S040.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
FORM POPUP_TO_CONFIRM USING PV_TITLE
                            PV_QUEST.
  "-- call popup
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR       = PV_TITLE
      TEXT_QUESTION  = PV_QUEST
    IMPORTING
      ANSWER         = GV_ANSWER
    EXCEPTIONS
      TEXT_NOT_FOUND = 1
      OTHERS         = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_COLUMN_TEXT
*&---------------------------------------------------------------------*
FORM SET_COLUMN_TEXT USING P_COLUMNNAME
                      CHANGING P_COLUMN_TEXT.

  DATA L_FIELD TYPE LVC_CFNAME VALUE 'WAERS'.

  TRY.

      CLEAR P_COLUMN_TEXT.

      CASE P_COLUMNNAME.

        WHEN 'KOSTL'.
          P_COLUMN_TEXT = TEXT-C01.
          GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 10 ).

        WHEN 'KTEXT'.
          P_COLUMN_TEXT = TEXT-C02.
          GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).

        WHEN 'POSID'.
          P_COLUMN_TEXT = TEXT-C03.
          GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 10 ).

        WHEN 'POST1'.
          P_COLUMN_TEXT = TEXT-C04.
          GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 20 ).

        WHEN 'KSTAR'.
          P_COLUMN_TEXT = TEXT-C05.
          GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).

        WHEN 'KSTXT'.
          P_COLUMN_TEXT = TEXT-C06.
          GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).

        WHEN 'WAERS'.
          P_COLUMN_TEXT = TEXT-C07.

        WHEN 'ENTR'.
          P_COLUMN_TEXT = TEXT-C08.

          GR_COLUMN->SET_CURRENCY_COLUMN( L_FIELD ).

          _SET_COLOR '3' '0' '0'.
          GR_COLUMN->SET_COLOR( GS_COLOR ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 18 ).

        WHEN 'ALL'.
          P_COLUMN_TEXT = TEXT-C09.

          GR_COLUMN->SET_CURRENCY_COLUMN( L_FIELD ).

          _SET_COLOR '3' '0' '0'.
          GR_COLUMN->SET_COLOR( GS_COLOR ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 18 ).

        WHEN 'SUPL'.
          P_COLUMN_TEXT = TEXT-C10.

          GR_COLUMN->SET_CURRENCY_COLUMN( L_FIELD ).

          _SET_COLOR '5' '0' '0'.
          GR_COLUMN->SET_COLOR( GS_COLOR ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 18 ).

        WHEN 'RENT'.
          P_COLUMN_TEXT = TEXT-C11.

          GR_COLUMN->SET_CURRENCY_COLUMN( L_FIELD ).

          _SET_COLOR '5' '0' '0'.
          GR_COLUMN->SET_COLOR( GS_COLOR ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 18 ).

        WHEN 'SEND'.
          P_COLUMN_TEXT = TEXT-C12.

          GR_COLUMN->SET_CURRENCY_COLUMN( L_FIELD ).

          _SET_COLOR '6' '0' '0'.
          GR_COLUMN->SET_COLOR( GS_COLOR ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 18 ).

        WHEN 'RECV'.
          P_COLUMN_TEXT = TEXT-C13.

          GR_COLUMN->SET_CURRENCY_COLUMN( L_FIELD ).

          _SET_COLOR '6' '0' '0'.
          GR_COLUMN->SET_COLOR( GS_COLOR ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 18 ).

        WHEN 'DIFF'.
          P_COLUMN_TEXT = TEXT-C14.

          GR_COLUMN->SET_CURRENCY_COLUMN( L_FIELD ).
          GR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).

          _SET_COLOR '1' '0' '0'.
          GR_COLUMN->SET_COLOR( GS_COLOR ).
          GR_COLUMN->SET_OUTPUT_LENGTH( VALUE = 18 ).

      ENDCASE.

    CATCH CX_SALV_NOT_FOUND.
    CATCH CX_SALV_DATA_ERROR.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM SET_TOP_OF_PAGE .

  DATA: LR_CONTENT TYPE REF TO CL_SALV_FORM_ELEMENT.
  PERFORM BUILT_HEADER CHANGING LR_CONTENT.
  GO_ALV->SET_TOP_OF_LIST( LR_CONTENT ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F4_KAGRU
*&---------------------------------------------------------------------*
FORM F4_KAGRU  CHANGING P_KAGRU.

  DATA LV_KAGRU TYPE KAGRU.

  CALL FUNCTION 'K_GROUP_SELECT'
    EXPORTING
      CLASS         = '0102'
      FIELD_NAME    = 'KSTAR'
      KOKRS         = PA_KOKRS
      KTOPL         = GC_KTOPL
    IMPORTING
      SET_NAME      = LV_KAGRU
    EXCEPTIONS
      NO_SET_PICKED = 1
      OTHERS        = 2.

  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.

  IF LV_KAGRU IS NOT INITIAL.
    P_KAGRU  = LV_KAGRU.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F4_KSGRU
*&---------------------------------------------------------------------*
FORM F4_KSGRU  CHANGING P_KSGRU.

  DATA: HELP_SETNR     LIKE RGSMH-SETNR,
        HELP_SEARCHFLD LIKE RGSMH-SEARCHFLD,
        HELP_SET       LIKE RGSBS-SETNR,
        HELP_SETCLASS  LIKE RGSMH-CLASS.

  MOVE PA_KOKRS TO HELP_SEARCHFLD.

  CALL FUNCTION 'K_GROUP_SELECT'
    EXPORTING
      CLASS           = '0101'
      FIELD_NAME      = 'KOSTL'
      SEARCHFLD       = HELP_SEARCHFLD
      SEARCHFLD_INPUT = ' '
      SET             = HELP_SET
    IMPORTING
      SET_NAME        = HELP_SETNR
    EXCEPTIONS
      NO_SET_PICKED   = 1.

  IF SY-SUBRC = 0.
    P_KSGRU = HELP_SETNR.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F4_PDGR
*&---------------------------------------------------------------------*
FORM F4_PDGR  CHANGING P_PDGR.

  DATA: HELP_SETNR     LIKE RGSMH-SETNR,
        HELP_SEARCHFLD LIKE RGSMH-SEARCHFLD,
        HELP_SET       LIKE RGSBS-SETNR,
        HELP_SETCLASS  LIKE RGSMH-CLASS.

  MOVE PA_KOKRS TO HELP_SEARCHFLD.

  CALL FUNCTION 'K_GROUP_SELECT'
    EXPORTING
      CLASS           = '0110'
      FIELD_NAME      = 'POSID'
      SEARCHFLD       = HELP_SEARCHFLD
      SEARCHFLD_INPUT = ' '
      SET             = HELP_SET
    IMPORTING
      SET_NAME        = HELP_SETNR
    EXCEPTIONS
      NO_SET_PICKED   = 1.

  IF SY-SUBRC = 0.
    P_PDGR = HELP_SETNR.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_B2
*&---------------------------------------------------------------------*
FORM CHECK_B2 .

  DATA LS_RETURN TYPE BAPIRET2.

  DATA LS_SETLEAF TYPE SETLEAF.

  CASE ABAP_TRUE.

    WHEN PA_RAD1.

      IF SO_KOSTL[] IS NOT INITIAL AND
         PA_KSGRU IS NOT INITIAL.
        SET CURSOR FIELD 'SO_KOSTL-LOW'.
        MESSAGE E000  WITH TEXT-E01.
      ENDIF.

      IF SO_KOSTL[] IS NOT INITIAL.

        SELECT SINGLE * FROM CSKS
          INTO @DATA(LS_CSKS)
         WHERE KOKRS = @PA_KOKRS
           AND KOSTL IN @SO_KOSTL
           AND DATBI >= @SY-DATUM
           AND DATAB <= @SY-DATUM.

        IF SY-SUBRC <> 0.
          SET CURSOR FIELD 'SO_KOSTL-LOW'.
          MESSAGE E027  WITH TEXT-E05.
        ENDIF.

      ENDIF.

      IF PA_KSGRU IS NOT INITIAL.

        PERFORM GET_CHECK_GROUP USING '0101'
                                      PA_KSGRU
                                CHANGING LS_RETURN.

        IF LS_RETURN-TYPE = 'E'.
          SET CURSOR FIELD 'PA_KSGRU'.
          MESSAGE E027  WITH TEXT-E06.
        ENDIF.

      ENDIF.

    WHEN PA_RAD2.

      IF SO_POSID[] IS NOT INITIAL AND
         PA_PDGR IS NOT INITIAL.
        SET CURSOR FIELD 'SO_POSID-LOW'.
        MESSAGE E026  WITH TEXT-E04.
      ENDIF.

      IF SO_POSID[] IS NOT INITIAL.

        SELECT SINGLE * FROM PRPS
          INTO @DATA(LS_PRPS)
         WHERE POSID IN @SO_POSID.

        IF SY-SUBRC <> 0.
          SET CURSOR FIELD 'SO_POSID-LOW'.
          MESSAGE E027  WITH TEXT-E07.
        ENDIF.

      ENDIF.

      IF PA_PDGR IS NOT INITIAL.

        PERFORM GET_CHECK_GROUP USING '0110'
                                      PA_PDGR
                                CHANGING LS_RETURN.

        IF LS_RETURN-TYPE = 'E'.
          SET CURSOR FIELD 'PA_PDGR'.
          MESSAGE E027  WITH TEXT-E08.
        ENDIF.

      ENDIF.

  ENDCASE.

  IF SO_KSTAR[] IS NOT INITIAL AND
     PA_KAGRU IS NOT INITIAL.

    SET CURSOR FIELD 'SO_KSTAR-LOW'.
    MESSAGE E026  WITH TEXT-E02.

  ENDIF.

  IF SO_KSTAR[] IS NOT INITIAL.

    SELECT SINGLE * FROM CSKA
      INTO @DATA(LS_CSKA)
     WHERE KTOPL = @GV_KTOPL
       AND KSTAR IN @SO_KSTAR.

    IF SY-SUBRC <> 0.
      SET CURSOR FIELD 'SO_KSTAR-LOW'.
      MESSAGE E027  WITH TEXT-E09.
    ENDIF.

  ENDIF.

  IF PA_KAGRU IS NOT INITIAL.

    PERFORM GET_CHECK_GROUP USING '0102'
                                  PA_KAGRU
                            CHANGING LS_RETURN.


    IF LS_RETURN-TYPE = 'E'.
      SET CURSOR FIELD 'PA_KAGRU'.
      MESSAGE E027  WITH TEXT-E10.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form READ_HIERARCHY_TABLES
*&---------------------------------------------------------------------*
FORM READ_HIERARCHY_TABLES TABLES PT_VALUES STRUCTURE GRPVALUES
                            USING PV_CLASS TYPE SETCLASS
                                  PV_SETID.

  DATA: LV_SETID     LIKE SETHIER-SETID,
        LV_OVERWRITE LIKE SY-DATAR,
        LT_INFO      LIKE GRPHINFO OCCURS 0 WITH HEADER LINE,
        LT_NODES     LIKE GRPOBJECTS OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'G_SET_ENCRYPT_SETID'
    EXPORTING
      SETCLASS  = PV_CLASS
      SHORTNAME = PV_SETID  "코스트센터그룹
    IMPORTING
      SETID     = LV_SETID.

  IF SY-SUBRC = 0.

    CALL FUNCTION 'K_HIERARCHY_TABLES_READ'
      EXPORTING
        E_CLASS                     = PV_CLASS
        E_SETID                     = LV_SETID
        E_KOKRS                     = PA_KOKRS
      TABLES
        T_NODES                     = LT_NODES
        T_VALUES                    = PT_VALUES
      CHANGING
        C_INFO                      = LT_INFO
        C_OVERWRITE                 = LV_OVERWRITE
      EXCEPTIONS
        NO_CONTROLLING_AREA         = 1
        NO_CHART_OF_ACCOUNT         = 2
        DIFFERENT_CONTROLLING_AREAS = 3
        DIFFERENT_CHART_OF_ACCOUNTS = 4
        SET_NOT_FOUND               = 5
        ILLEGAL_FIELD_REPLACEMENT   = 6
        ILLEGAL_TABLE_REPLACEMENT   = 7
        FM_RAISE                    = 8
        CONVERT_ERROR               = 9
        NO_OVERWRITE_STANDARD_HIER  = 10
        NO_BUKRS_FOR_KOKRS          = 11
        OTHERS                      = 12.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_RANGES_OBJNR
*&---------------------------------------------------------------------*
FORM SET_RANGES_OBJNR .

  CLEAR: GT_VALUES, GT_VALUES[],
         R_OBJNR,   R_OBJNR[],
         R_KSTAR,   R_KSTAR[].

  RANGES R_KOSTL FOR CSKS-KOSTL.

  CASE ABAP_TRUE.

    WHEN PA_RAD1.

      IF SO_KOSTL[] IS NOT INITIAL.

        R_KOSTL[] = SO_KOSTL[].

      ENDIF.

      IF PA_KSGRU IS NOT INITIAL.

        PERFORM READ_HIERARCHY_TABLES TABLES GT_VALUES
                                      USING '0101'
                                            PA_KSGRU.  "코스트센터 그룹

        LOOP AT GT_VALUES.

          MOVE: 'I'             TO R_KOSTL-SIGN,
                'BT'            TO R_KOSTL-OPTION,
                GT_VALUES-VFROM TO R_KOSTL-LOW,
                GT_VALUES-VTO   TO R_KOSTL-HIGH.

          COLLECT R_KOSTL.
          CLEAR   R_KOSTL.

        ENDLOOP.

      ENDIF.

      SELECT KOSTL FROM CSKS
        INTO TABLE @DATA(LT_CSKS)
       WHERE KOSTL IN @R_KOSTL
         AND KOKRS = @PA_KOKRS
         AND PRCTR IN @R_PRCTR2
         AND DATBI >= @SY-DATUM
         AND DATAB <= @SY-DATUM.

      LOOP AT LT_CSKS INTO DATA(LS_CSKS).

        MOVE: 'I'  TO R_OBJNR-SIGN,
              'EQ' TO R_OBJNR-OPTION.

        R_OBJNR-LOW  = 'KS' && PA_KOKRS && LS_CSKS-KOSTL.

        COLLECT R_OBJNR.
        CLEAR   R_OBJNR.

      ENDLOOP.

    WHEN PA_RAD2.

      IF SO_POSID[] IS NOT INITIAL.

        LOOP AT SO_POSID.

          MOVE: SO_POSID-SIGN   TO R_OBJNR-SIGN,
                SO_POSID-OPTION TO R_OBJNR-OPTION.

          CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
            EXPORTING
              INPUT     = SO_POSID-LOW
            IMPORTING
              OUTPUT    = R_OBJNR-LOW
            EXCEPTIONS
              NOT_FOUND = 1
              OTHERS    = 2.

          R_OBJNR-LOW  = 'PR' && R_OBJNR-LOW.

          IF SO_POSID-HIGH IS NOT INITIAL.

            CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
              EXPORTING
                INPUT     = SO_POSID-HIGH
              IMPORTING
                OUTPUT    = R_OBJNR-HIGH
              EXCEPTIONS
                NOT_FOUND = 1
                OTHERS    = 2.

            R_OBJNR-HIGH  = 'PR' && R_OBJNR-HIGH.

          ENDIF.

          COLLECT R_OBJNR.
          CLEAR   R_OBJNR.

        ENDLOOP.

      ENDIF.

      IF PA_PDGR IS NOT INITIAL.

        PERFORM READ_HIERARCHY_TABLES TABLES GT_VALUES
                                      USING '0110'
                                            PA_PDGR.   "WBS 요소 그룹

        LOOP AT GT_VALUES.

          MOVE: 'I'   TO R_OBJNR-SIGN,
                'BT'  TO R_OBJNR-OPTION.

          CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
            EXPORTING
              INPUT     = GT_VALUES-VFROM
            IMPORTING
              OUTPUT    = R_OBJNR-LOW
            EXCEPTIONS
              NOT_FOUND = 1
              OTHERS    = 2.


          CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
            EXPORTING
              INPUT     = GT_VALUES-VTO
            IMPORTING
              OUTPUT    = R_OBJNR-HIGH
            EXCEPTIONS
              NOT_FOUND = 1
              OTHERS    = 2.

          R_OBJNR-HIGH  = 'PR' && R_OBJNR-HIGH.
          R_OBJNR-HIGH  = 'PR' && R_OBJNR-HIGH.

          COLLECT R_OBJNR.
          CLEAR   R_OBJNR.

        ENDLOOP.

      ENDIF.

      SELECT A~OBJNR, A~PSPNR, A~POSID,
             A~POST1, A~ZZCYP
        INTO TABLE @GT_PRPS
        FROM PRPS AS A
       INNER JOIN PROJ AS B
          ON A~PSPHI = B~PSPNR
       WHERE A~PKOKR = @PA_KOKRS
         AND A~PBUKR IN @SO_BUKRS
         AND A~PRCTR IN @SO_PRCTR
         AND A~OBJNR IN @R_OBJNR
         AND B~PSPID IN @SO_PSPID
         AND A~PBUKR IN @R_BUKRS
         AND A~PRCTR IN @R_PRCTR1
         AND B~PSPID IN @R_PSPID.

      CLEAR: R_OBJNR, R_OBJNR[].

      LOOP AT GT_PRPS.
        MOVE: 'I'            TO R_OBJNR-SIGN,
              'EQ'           TO R_OBJNR-OPTION,
              GT_PRPS-OBJNR  TO R_OBJNR-LOW.
        COLLECT R_OBJNR.
        CLEAR   R_OBJNR.
      ENDLOOP.

  ENDCASE.

*-- 원가요소
  IF PA_KAGRU IS NOT INITIAL.

    PERFORM READ_HIERARCHY_TABLES TABLES GT_VALUES
                                  USING '0102'
                                        PA_KAGRU.  "원가요소 그룹

    LOOP AT GT_VALUES.

      MOVE: 'I'               TO R_KSTAR-SIGN,
             'BT'             TO R_KSTAR-OPTION,
             GT_VALUES-VFROM  TO R_KSTAR-LOW,
             GT_VALUES-VTO    TO R_KSTAR-HIGH.

      COLLECT R_KSTAR.
      CLEAR   R_KSTAR.
    ENDLOOP.

  ENDIF.

  IF SO_KSTAR[] IS NOT INITIAL.
    R_KSTAR[] = SO_KSTAR[].
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILT_HEADER
*&---------------------------------------------------------------------*
FORM BUILT_HEADER  CHANGING CR_CONTENT TYPE REF TO CL_SALV_FORM_ELEMENT.

  DATA: LR_GRID   TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
        LR_GRID_1 TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
        LR_LABEL  TYPE REF TO CL_SALV_FORM_LABEL,
        LR_TEXT   TYPE REF TO CL_SALV_FORM_TEXT,
        LV_TEXT   TYPE STRING,
        LV_M1     TYPE C LENGTH 10,
        LV_M2     TYPE C LENGTH 10.

  CREATE OBJECT LR_GRID.

  LR_GRID_1 = LR_GRID->CREATE_GRID(
                ROW    = 2
                COLUMN = 1 ).

  LR_LABEL = LR_GRID_1->CREATE_LABEL(
    ROW    = 1
    COLUMN = 1
    TEXT    = TEXT-E03
    TOOLTIP = TEXT-E03 ).

  IF PA_EPERL IS NOT INITIAL.

    LV_M1 = PA_GJAHR && '.' && PA_SPERL+1(2).
    LV_M2 = PA_GJAHR && '.' && PA_EPERL+1(2).
    CONCATENATE LV_M1 '~' LV_M2
           INTO LV_TEXT SEPARATED BY SPACE.

  ELSE.
    LV_M1 = PA_GJAHR && '.' && PA_SPERL+1(2).
    LV_TEXT = LV_M1.

  ENDIF.

  LR_TEXT = LR_GRID_1->CREATE_TEXT(
    ROW    = 1
    COLUMN = 2
    TEXT    = LV_TEXT
    TOOLTIP = LV_TEXT ).

  LR_LABEL->SET_LABEL_FOR( LR_TEXT ).

  LR_LABEL = LR_GRID_1->CREATE_LABEL(
    ROW    = 2
    COLUMN = 1
    TEXT    = TEXT-T03
    TOOLTIP = TEXT-T03 ).

  CONCATENATE PA_VERSN '(' PA_VTXT ')' INTO LV_TEXT
  SEPARATED BY SPACE.

  LR_TEXT = LR_GRID_1->CREATE_TEXT(
    ROW    = 2
    COLUMN = 2
    TEXT    = LV_TEXT
    TOOLTIP = LV_TEXT ).

  LR_LABEL->SET_LABEL_FOR( LR_TEXT ).

  CR_CONTENT = LR_GRID.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM AUTHORITY_CHECK .

  DATA LV_TYPE TYPE C.
  DATA LV_MESSAGE TYPE BAPI_MSG.

  DATA: LT_0070  LIKE TABLE OF ZCAS0070,
        LS_0070  LIKE ZCAS0070,
        LV_CLASS TYPE ZCAT0031-CD_CLASS,
        LV_CODE  TYPE ZCAT0031-CD_CODE.

  CLEAR: R_PRCTR1, R_PRCTR1[],
         R_BUKRS,  R_BUKRS[],
         R_PSPID,  R_PSPID[],
         R_PRCTR2, R_PRCTR2[].

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

  CASE ABAP_TRUE.

    WHEN PA_RAD1.

      IF SO_KOSTL[] IS INITIAL AND PA_KSGRU IS INITIAL.
        MESSAGE S000 WITH TEXT-E13 DISPLAY LIKE 'E'.
        STOP.
      ENDIF.

      CALL FUNCTION 'ZCA_PRG_AUTH_CHECK'
        EXPORTING
          I_MODULE    = 'CO'
          I_KSTGR_CO  = PA_KSGRU
        IMPORTING
          E_TYPE      = LV_TYPE
          E_MESSAGE   = LV_MESSAGE
        TABLES
          IT_KOSTL_CO = SO_KOSTL[].

    WHEN PA_RAD2.

      IF SO_BUKRS[] IS INITIAL AND
         SO_PRCTR[] IS INITIAL AND
         SO_PSPID[] IS INITIAL AND
         ( SO_POSID[] IS INITIAL AND PA_PDGR IS INITIAL ).
        MESSAGE S000 WITH TEXT-E12 DISPLAY LIKE 'E'.
        STOP.
      ENDIF.

      CALL FUNCTION 'ZCA_PRG_AUTH_CHECK'
        EXPORTING
          I_MODULE     = 'CO'
          I_POSIDGR_CO = PA_PDGR
        IMPORTING
          E_TYPE       = LV_TYPE
          E_MESSAGE    = LV_MESSAGE
        TABLES
          IT_PRCTR_CO  = SO_PRCTR[]
          IT_BUKRS_CO  = SO_BUKRS[]
          IT_PSPID_CO  = SO_PSPID[]
          IT_POSID_CO  = SO_POSID[].

  ENDCASE.

  IF LV_TYPE = 'E'.
    MESSAGE S000 WITH LV_MESSAGE DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  SELECT * FROM ZCOT0320
    INTO TABLE @DATA(LT_ZCOT0320)
    WHERE BNAME = @SY-UNAME.

  LOOP AT LT_ZCOT0320 INTO DATA(LS_ZCOT0320).

    IF LS_ZCOT0320-PRCTR1 IS NOT INITIAL.

      MOVE: LS_ZCOT0320-PRCTR1 TO R_PRCTR1-LOW,
            'I'                TO R_PRCTR1-SIGN,
            'EQ'               TO R_PRCTR1-OPTION.

      COLLECT R_PRCTR1.
      CLEAR   R_PRCTR1.

    ENDIF.

    IF LS_ZCOT0320-BUKRS IS NOT INITIAL.

      MOVE: LS_ZCOT0320-BUKRS TO R_BUKRS-LOW,
            'I'                TO R_BUKRS-SIGN,
            'EQ'               TO R_BUKRS-OPTION.

      COLLECT R_BUKRS.
      CLEAR   R_BUKRS.

    ENDIF.

    IF LS_ZCOT0320-PSPID IS NOT INITIAL.

      MOVE: LS_ZCOT0320-PSPID TO R_PSPID-LOW,
            'I'                TO R_PSPID-SIGN,
            'EQ'               TO R_PSPID-OPTION.

      COLLECT R_PSPID.
      CLEAR   R_PSPID.

    ENDIF.

    IF LS_ZCOT0320-PRCTR2 IS NOT INITIAL.

      MOVE: LS_ZCOT0320-PRCTR2 TO R_PRCTR2-LOW,
            'I'                TO R_PRCTR2-SIGN,
            'EQ'               TO R_PRCTR2-OPTION.

      COLLECT R_PRCTR2.
      CLEAR   R_PRCTR2.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_CHECK_GROUP
*&---------------------------------------------------------------------*
FORM GET_CHECK_GROUP  USING    VALUE(P_SETCLASS)
                                        P_GROUP
*                                        P_CHRT_ACCTS
                               CHANGING PS_RETURN STRUCTURE BAPIRET2.

  CLEAR PS_RETURN.

  DATA LV_GROUPNAME TYPE GRPNAME.
  DATA LV_SETCLASS  TYPE SETCLASS.
  DATA LT_SETHIER TYPE TABLE OF SETHIER_CO WITH HEADER LINE.

  MOVE: P_GROUP     TO LV_GROUPNAME,
        P_SETCLASS  TO LV_SETCLASS.

  CALL FUNCTION 'K_GROUP_REMOTE_READ'
    EXPORTING
      SETCLASS   = LV_SETCLASS
      CO_AREA    = PA_KOKRS
      CHRT_ACCTS = GC_KTOPL
      GROUPNAME  = LV_GROUPNAME
    IMPORTING
      RETURN     = PS_RETURN
    TABLES
      ET_SETHIER = LT_SETHIER.

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
