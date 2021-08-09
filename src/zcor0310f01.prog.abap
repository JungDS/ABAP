*&---------------------------------------------------------------------*
*& Include          ZCOR0310F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITAIL
*&---------------------------------------------------------------------*
FORM INITAIL .

  GV_REPID = SY-REPID.
  GET PARAMETER ID 'CAC' FIELD PA_KOKRS.

  IF PA_KOKRS IS INITIAL.
    PA_KOKRS = '1000'.
  ENDIF.

  SELECT SINGLE BEZEI INTO @PA_KTXT
    FROM TKA01
   WHERE KOKRS = '1000'.

  SELECT SINGLE VTEXT INTO @PA_VTXT
    FROM TKVS AS A
    LEFT JOIN TKVST AS B
      ON A~VERSI = B~VERSI
     AND B~SPRAS = @SY-LANGU
   WHERE A~VERSI = '000'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCRFIELDS_FUNCTXT
*&---------------------------------------------------------------------*
FORM SCRFIELDS_FUNCTXT .

  GS_FUNTXT-ICON_ID   = ICON_INFORMATION.
  GS_FUNTXT-QUICKINFO = 'Program Help'.

  SSCRFIELDS-FUNCTXT_01 = GS_FUNTXT.

  GS_FUNTXT-ICON_ID = ICON_IMPORT.
  GS_FUNTXT-QUICKINFO = 'Excel Format Download'.
  GS_FUNTXT-ICON_TEXT = 'Excel Format Download'.

  SSCRFIELDS-FUNCTXT_02 = GS_FUNTXT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F4_FILE
*&---------------------------------------------------------------------*
FORM F4_FILE  CHANGING  PV_FILE.

  DATA: LT_FILE_TABLE TYPE FILETABLE,
        LS_FIEL_TABLE TYPE FILE_TABLE.

  DATA: LV_SUBRC  TYPE I,
        LV_ACTION TYPE I.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      WINDOW_TITLE            = 'File Path'
      DEFAULT_FILENAME        = ''
      FILE_FILTER             = 'Excel files (*.XLSX)|*.XLSX|Excel files (*.XLS)|*.XLS|'
      INITIAL_DIRECTORY       = 'C:\'
      MULTISELECTION          = ' '
    CHANGING
      FILE_TABLE              = LT_FILE_TABLE
      RC                      = LV_SUBRC
      USER_ACTION             = LV_ACTION
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3.

  CALL METHOD CL_GUI_CFW=>FLUSH.

  CASE LV_ACTION.

    WHEN  CL_GUI_FRONTEND_SERVICES=>ACTION_OK.

      READ TABLE LT_FILE_TABLE INDEX 1 INTO LS_FIEL_TABLE.
      PV_FILE = LS_FIEL_TABLE.

    WHEN CL_GUI_FRONTEND_SERVICES=>ACTION_CANCEL.
      EXIT.
  ENDCASE.

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
      PERFORM EXCELFORM_DOWNLOAD_V1 USING 'ZCOR0310'.

    WHEN OTHERS.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXCELFORM_DOWNLOAD_V1
*&---------------------------------------------------------------------*
FORM EXCELFORM_DOWNLOAD_V1  USING PV_OBJID TYPE WWWDATATAB-OBJID.

  DATA: LT_MIME      LIKE TABLE OF W3MIME WITH HEADER LINE.
  DATA: LS_WWWDATA   TYPE WWWDATATAB.
  DATA: LV_FILENAME     TYPE STRING,
        LV_PATH         TYPE STRING,
        LV_FULLPATH     TYPE STRING,
        LV_FILESIZE(10),
        LV_SIZE         TYPE I.

  SELECT SINGLE * FROM  WWWDATA
    INTO CORRESPONDING FIELDS OF LS_WWWDATA
   WHERE OBJID EQ PV_OBJID.   "SMW0 Object 명

  CHECK SY-SUBRC = 0 .

  CALL FUNCTION 'WWWDATA_IMPORT'
    EXPORTING
      KEY               = LS_WWWDATA
    TABLES
      MIME              = LT_MIME
    EXCEPTIONS
      WRONG_OBJECT_TYPE = 1
      IMPORT_ERROR      = 2
      OTHERS            = 3.

  CALL FUNCTION 'WWWPARAMS_READ'
    EXPORTING
      RELID = LS_WWWDATA-RELID
      OBJID = LS_WWWDATA-OBJID
      NAME  = 'filesize'
    IMPORTING
      VALUE = LV_FILESIZE.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
    EXPORTING
      DEFAULT_EXTENSION = 'XLSX'  "저장할 때 XLSX 확장자로 저장
    CHANGING
      FILENAME          = LV_FILENAME
      PATH              = LV_PATH
      FULLPATH          = LV_FULLPATH.

  LV_SIZE = LV_FILESIZE.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      FILENAME     = LV_FULLPATH
      FILETYPE     = 'BIN'
      BIN_FILESIZE = LV_SIZE
    TABLES
      DATA_TAB     = LT_MIME.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXCEL_UPLOAD_EXEC
*&---------------------------------------------------------------------*
FORM EXCEL_UPLOAD_EXEC .

  TYPE-POOLS TRUXS.

  FIELD-SYMBOLS: <FS_COL>.

  DATA LV_FILE TYPE STRING.
  DATA LV_RESULT.

  LV_FILE = PA_FILE.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_EXIST
    EXPORTING
      FILE                 = LV_FILE
    RECEIVING
      RESULT               = LV_RESULT
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      WRONG_PARAMETER      = 3
      NOT_SUPPORTED_BY_GUI = 4
      OTHERS               = 5.

  IF  LV_RESULT IS INITIAL.
    MESSAGE S000 DISPLAY LIKE 'E' WITH TEXT-M02.
    STOP.
  ENDIF.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = PA_FILE
      I_BEGIN_COL             = 1         "Excel file start col
      I_BEGIN_ROW             = 2         "Excel file start row
      I_END_COL               = 19
      I_END_ROW               = 50000
    TABLES
      INTERN                  = GT_INTERN
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  DESCRIBE TABLE GT_INTERN LINES SY-TFILL.

  IF SY-TFILL EQ 0.
    MESSAGE S001 WITH TEXT-E16.
    STOP.
  ENDIF.

  LOOP AT GT_INTERN.

    ASSIGN COMPONENT GT_INTERN-COL OF STRUCTURE
                                   GT_EXCEL TO <FS_COL>.
    <FS_COL> = GT_INTERN-VALUE.

    AT END OF ROW.
      APPEND GT_EXCEL.
      CLEAR  GT_EXCEL.
    ENDAT.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DATA_CONVERT
*&---------------------------------------------------------------------*
FORM DATA_CONVERT .

  DATA LV_FNAME TYPE STFNA.
  DATA LV_MONTH TYPE N LENGTH 2.

  DATA: LT_CELLTYPE TYPE SALV_T_INT4_COLUMN.
  DATA: LS_CELLTYPE LIKE LINE OF LT_CELLTYPE.

  LOOP AT GT_EXCEL.

    REFRESH LT_CELLTYPE.

    MOVE: GT_EXCEL-PBUKR   TO GS_OUTTAB-PBUKR,
          GT_EXCEL-POSID   TO GS_OUTTAB-POSID,
          GT_EXCEL-RKSTAR  TO GS_OUTTAB-RKSTAR,
          GT_EXCEL-RTCUR   TO GS_OUTTAB-RTCUR,
          GT_EXCEL-RYEAR   TO GS_OUTTAB-RYEAR.

    CLEAR LV_MONTH.

    DO.

      ADD 1 TO LV_MONTH.

      IF LV_MONTH > 12.
        EXIT.
      ENDIF.

      LV_FNAME = 'GT_EXCEL-HSL' && LV_MONTH.
      ASSIGN (LV_FNAME) TO FIELD-SYMBOL(<FS1>).

      PERFORM CONV_CHAR_OF_DEC CHANGING <FS1>.

      IF GS_OUTTAB-RTCUR = 'KRW'.
        _CONV_KRW: <FS1>.
      ENDIF.

      LV_FNAME = 'HSL' && LV_MONTH.

      ASSIGN COMPONENT LV_FNAME OF STRUCTURE GS_OUTTAB
         TO FIELD-SYMBOL(<FS2>).

      <FS2> = <FS1>.

      GS_OUTTAB-SUM = GS_OUTTAB-SUM + <FS1>.

    ENDDO.

    PERFORM CHECK_DATA.

    IF GS_OUTTAB-STATUS = 'E'.
      MOVE ICON_RED_LIGHT TO GS_OUTTAB-ICON.
    ELSEIF GS_OUTTAB-STATUS = 'W'.
      MOVE ICON_YELLOW_LIGHT TO GS_OUTTAB-ICON.
    ENDIF.

    LS_CELLTYPE-COLUMNNAME = 'ICON'.
    LS_CELLTYPE-VALUE      = IF_SALV_C_CELL_TYPE=>BUTTON.
    APPEND LS_CELLTYPE TO LT_CELLTYPE.

    APPEND LINES OF LT_CELLTYPE TO GS_OUTTAB-CELLTYPE.

    APPEND GS_OUTTAB TO GT_OUTTAB.
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

  PERFORM SET_EVENT.
  PERFORM SET_TABLE_SETTINGS.

  GO_ALV->DISPLAY( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
FORM HANDLE_USER_COMMAND  USING P_UCOMM TYPE SALV_DE_FUNCTION.

  DATA L_DUMMY TYPE C LENGTH 100.

  CASE P_UCOMM.

    WHEN 'CREATE'.

      PERFORM ERROR_CHECK.
      CHECK GV_EXIT IS INITIAL.
      PERFORM POPUP_TO_CONFIRM USING TEXT-PT1
                                     TEXT-QT1.
      CHECK GV_ANSWER EQ '1'.
      PERFORM CREATE_EXECUTE.

    WHEN '&HELP'.
      PERFORM CALL_POPUP_HELP(ZCAR9000)
               USING SY-REPID SY-DYNNR SY-LANGU ''.

  ENDCASE.

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

  GO_ALV->SET_SCREEN_STATUS(
    PFSTATUS      =  'STANDARD'
    REPORT        =  GV_REPID
    SET_FUNCTIONS = GO_ALV->C_FUNCTIONS_ALL ).

  L_COUNT = LINES( GT_OUTTAB ).
  LV_TEXT = TEXT-T00.

  WRITE L_COUNT TO L_LINES.
  CONDENSE L_LINES.

  CONCATENATE LV_TEXT '(' 'Upload entries :' L_LINES ')'
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

*-- set column
  TRY.
      LR_COLUMNS = GO_ALV->GET_COLUMNS( ).
      LO_AGGRS   = GO_ALV->GET_AGGREGATIONS( ).
      LR_COLUMNS->SET_OPTIMIZE( ABAP_TRUE ).
      LR_COLUMNS->SET_CELL_TYPE_COLUMN( 'CELLTYPE' ).
    CATCH CX_SALV_DATA_ERROR.
  ENDTRY.

  PERFORM SET_COLUMNS_TECHNICAL USING LR_COLUMNS
                                      LO_AGGRS.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_COLUMNS_TECHNICAL
*&---------------------------------------------------------------------*
FORM SET_COLUMNS_TECHNICAL USING IR_COLUMNS TYPE REF TO
                                            CL_SALV_COLUMNS_TABLE
                                  IR_AGG TYPE REF TO
                                            CL_SALV_AGGREGATIONS.

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

        IF ( <COLUMN_REF>-COLUMNNAME CP 'HSL*' OR
            <COLUMN_REF>-COLUMNNAME = 'SUM' ).

          CALL METHOD IR_AGG->ADD_AGGREGATION
            EXPORTING
              COLUMNNAME  = <COLUMN_REF>-COLUMNNAME
              AGGREGATION = IF_SALV_C_AGGREGATION=>TOTAL.

        ENDIF.

      ENDLOOP.

    CATCH CX_SALV_NOT_FOUND.
    CATCH CX_SALV_DATA_ERROR.
    CATCH CX_SALV_EXISTING.

  ENDTRY.

  TRY.
      LR_COLUMN ?= IR_COLUMNS->GET_COLUMN( 'STATUS' ).
      LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
    CATCH CX_SALV_NOT_FOUND.
  ENDTRY.

  TRY.
      LR_COLUMN ?= IR_COLUMNS->GET_COLUMN( 'ROBJNR' ).
      LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
    CATCH CX_SALV_NOT_FOUND.
  ENDTRY.

*-- SELECT FIELD 추가
  LR_SELECTIONS = GO_ALV->GET_SELECTIONS( ).
  LR_SELECTIONS->SET_SELECTION_MODE( IF_SALV_C_SELECTION_MODE=>CELL ).

  IR_AGG->SET_AGGREGATION_BEFORE_ITEMS( ).

ENDFORM. " SET_COLUMNS_TECHNICAL
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

  DATA: LV_SMONTH TYPE N LENGTH 2,
        LV_EMONTH TYPE N LENGTH 2,
        LV_MONTH  TYPE N LENGTH 2.

  DATA L_FIELD TYPE LVC_CFNAME VALUE 'RTCUR'.

  TRY.

      CLEAR P_COLUMN_TEXT.

      CASE P_COLUMNNAME.

        WHEN 'ICON'.
          P_COLUMN_TEXT = TEXT-C01.

          GR_COLUMN->SET_ICON( IF_SALV_C_BOOL_SAP=>TRUE ).
          GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).

        WHEN 'PBUKR'.
          P_COLUMN_TEXT = TEXT-C08.


        WHEN 'POSID'.
          P_COLUMN_TEXT = TEXT-C02.

        WHEN 'POST1'.
          P_COLUMN_TEXT = TEXT-C03.

        WHEN 'RKSTAR'.
          P_COLUMN_TEXT = TEXT-C04.

        WHEN 'KTEXT'.
          P_COLUMN_TEXT = TEXT-C05.

        WHEN 'KTEXT'.
          P_COLUMN_TEXT = TEXT-C05.

        WHEN 'RTCUR'.
          P_COLUMN_TEXT = TEXT-C10.

        WHEN 'SUM'.
          P_COLUMN_TEXT = TEXT-C06.
          GR_COLUMN->SET_CURRENCY_COLUMN( L_FIELD ).

          _SET_COLOR '3' '0' '0'.
          GR_COLUMN->SET_COLOR( GS_COLOR ).

        WHEN 'MESSAGE'.
          P_COLUMN_TEXT = TEXT-C07.

      ENDCASE.

      IF P_COLUMNNAME CP 'HSL*'.

        P_COLUMN_TEXT = P_COLUMNNAME+3(2).

        SHIFT P_COLUMN_TEXT LEFT DELETING LEADING '0'.
        CONCATENATE P_COLUMN_TEXT TEXT-M01 INTO P_COLUMN_TEXT.

        GR_COLUMN->SET_CURRENCY_COLUMN( L_FIELD ).

      ENDIF.

    CATCH CX_SALV_NOT_FOUND.
    CATCH CX_SALV_DATA_ERROR.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_EXECUTE
*&---------------------------------------------------------------------*
FORM CREATE_EXECUTE .

  DATA LT_ZCOT1180 TYPE TABLE OF ZCOT1180 WITH HEADER LINE.

  LOOP AT GT_OUTTAB INTO GS_OUTTAB.

    MOVE-CORRESPONDING GS_OUTTAB TO LT_ZCOT1180.

    MOVE: SY-DATUM TO LT_ZCOT1180-CPUDT,
          SY-UZEIT TO LT_ZCOT1180-CPUTM,
          SY-UNAME TO LT_ZCOT1180-USNAM,
          PA_VERSN TO LT_ZCOT1180-RVERS,
          '04'     TO LT_ZCOT1180-WRTTP,   "실제
          PA_KOKRS TO LT_ZCOT1180-RKOKRS.

    IF PA_GJAHR = '2019'.
      CLEAR: LT_ZCOT1180-HSL10, LT_ZCOT1180-HSL11,
             LT_ZCOT1180-HSL12, LT_ZCOT1180-HSL13,
             LT_ZCOT1180-HSL14, LT_ZCOT1180-HSL15,
             LT_ZCOT1180-HSL16.
    ENDIF.

    APPEND LT_ZCOT1180.
    CLEAR  LT_ZCOT1180.

  ENDLOOP.

  TRY .

      DELETE FROM ZCOT1180 WHERE RYEAR = PA_GJAHR
                            AND RKOKRS = PA_KOKRS
                            AND PBUKR  = PA_BUKRS.

      INSERT  ZCOT1180 FROM TABLE LT_ZCOT1180.

      COMMIT WORK.

      MESSAGE S007.

    CATCH CX_SY_SQL_ERROR INTO DATA(LR_ERROR).

      ROLLBACK WORK.

      DATA(LV_MESSAGE) = LR_ERROR->GET_TEXT( ).
      MESSAGE S001 WITH LV_MESSAGE DISPLAY LIKE 'E'.

  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ERROR_CHECK
*&---------------------------------------------------------------------*
FORM ERROR_CHECK .

  CLEAR GV_EXIT.

  READ TABLE GT_OUTTAB TRANSPORTING NO FIELDS
        WITH KEY STATUS = 'E'.

  IF SY-SUBRC = 0.
    MESSAGE S000 WITH TEXT-E05 DISPLAY LIKE 'E'.
    GV_EXIT = ABAP_TRUE.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CONV_CHAR_OF_DEC
*&---------------------------------------------------------------------*
FORM CONV_CHAR_OF_DEC CHANGING PV_VALUE.

  _STRING_REPLACE PV_VALUE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_CONTROLLING_AREA
*&---------------------------------------------------------------------*
FORM CHECK_CONTROLLING_AREA .

  SELECT SINGLE BEZEI INTO @PA_KTXT
    FROM TKA01
   WHERE KOKRS = @PA_KOKRS.

  IF SY-SUBRC <> 0.
    SET CURSOR FIELD 'PA_KOKRS'.
    MESSAGE E027  WITH PA_KOKRS.
  ENDIF.

  SELECT SINGLE BUTXT INTO @PA_BTXT
    FROM T001
   WHERE BUKRS = @PA_BUKRS.

  IF SY-SUBRC <> 0.
    SET CURSOR FIELD 'PA_BUKRS'.
    MESSAGE E027  WITH TEXT-003.
  ENDIF.

  IF PA_GJAHR > '2019'.
    SET CURSOR FIELD 'PA_GJAHR'.
    MESSAGE E000  WITH TEXT-E06.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_DATA
*&---------------------------------------------------------------------*
FORM CHECK_DATA .

  DATA : LV_OBJNR LIKE COSS-OBJNR.

  DATA: LV_DATUM TYPE DATUM.

  CLEAR GV_MESSAGE.

  CHECK GS_OUTTAB-STATUS IS INITIAL.

  "-- 필수 값 체크
  _INITIAL_CHK : GS_OUTTAB-POSID   TEXT-C02,
                 GS_OUTTAB-RKSTAR  TEXT-C04,
                 GS_OUTTAB-RTCUR   TEXT-C10,
                 GS_OUTTAB-PBUKR   TEXT-003.

  IF GS_OUTTAB-STATUS = 'E'.
    MESSAGE S026 WITH GV_MESSAGE
         INTO  GS_OUTTAB-MESSAGE.
    EXIT.
  ENDIF.

  _CONVERSION_WBS_IN GS_OUTTAB-POSID.

  "-- 1) WBS
  SELECT SINGLE *
    INTO @DATA(LS_PRPS)
    FROM PRPS
   WHERE PKOKR = @PA_KOKRS
     AND POSID = @GS_OUTTAB-POSID.

  IF SY-SUBRC <> 0 .
    GS_OUTTAB-STATUS = 'E'.
    MESSAGE S023 INTO GS_OUTTAB-MESSAGE
         WITH GS_OUTTAB-POSID.
    EXIT.
  ELSE.


    MOVE: LS_PRPS-POST1 TO GS_OUTTAB-POST1,
          LS_PRPS-OBJNR TO GS_OUTTAB-ROBJNR.

    IF GS_OUTTAB-PBUKR <> LS_PRPS-PBUKR.

      GS_OUTTAB-STATUS = 'E'.
      MESSAGE S000 INTO GS_OUTTAB-MESSAGE
           WITH TEXT-E03.
      EXIT.

    ENDIF.

  ENDIF.

  _CONVERSION_IN GS_OUTTAB-RKSTAR.

  "-- 2) 원가요소
  SELECT SINGLE B~KTEXT
    INTO @GS_OUTTAB-KTEXT
    FROM CSKA AS A
    LEFT JOIN CSKU AS B
      ON A~KTOPL = B~KTOPL
     AND A~KSTAR = B~KSTAR
     AND B~SPRAS = @SY-LANGU
   WHERE A~KTOPL = @GV_KTOPL
     AND A~KSTAR = @GS_OUTTAB-RKSTAR.

  IF SY-SUBRC <> 0 .
    GS_OUTTAB-STATUS = 'E'.
    MESSAGE S023 INTO GS_OUTTAB-MESSAGE
           WITH GS_OUTTAB-RKSTAR.
    EXIT.
  ENDIF.

  IF GS_OUTTAB-PBUKR <> PA_BUKRS.
    GS_OUTTAB-STATUS = 'E'.
    MESSAGE S000  WITH TEXT-E01 INTO GS_OUTTAB-MESSAGE.
    EXIT.
  ENDIF.

  IF GS_OUTTAB-RYEAR <> PA_GJAHR.
    GS_OUTTAB-STATUS = 'E'.
    MESSAGE S000 WITH TEXT-E02 INTO GS_OUTTAB-MESSAGE.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SCREEN
*&---------------------------------------------------------------------*
FORM SET_SCREEN .

  LOOP AT SCREEN INTO DATA(SCREEN_WA).
    IF SCREEN_WA-GROUP1 = 'MG1'.
      SCREEN_WA-INPUT = '0'.
      MODIFY SCREEN FROM SCREEN_WA.
    ENDIF.
  ENDLOOP.

ENDFORM.
