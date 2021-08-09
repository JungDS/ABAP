*&---------------------------------------------------------------------*
*& Include          ZCOR0010F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITAIL
*&---------------------------------------------------------------------*
FORM INITAIL .

  GV_REPID = SY-REPID.
  GET PARAMETER ID 'CAC' FIELD PA_KOKRS.

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
      PERFORM EXCELFORM_DOWNLOAD_V1 USING 'ZCOR0030'.
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
      I_END_COL               = 18
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
  DATA LV_SMONTH TYPE N LENGTH 2.
  DATA LV_EMONTH TYPE N LENGTH 2.

  DATA: LT_CELLTYPE TYPE SALV_T_INT4_COLUMN.
  DATA: LS_CELLTYPE LIKE LINE OF LT_CELLTYPE.

  DATA LV_PLUS TYPE C.

  FIELD-SYMBOLS: <FS1> TYPE ANY,
                 <FS2> TYPE ANY.

  LV_EMONTH = PA_EPERL+1(2).

  LOOP AT GT_EXCEL.

    CLEAR LV_PLUS.

    REFRESH LT_CELLTYPE.

    LV_SMONTH = PA_SPERL+1(2).

    MOVE:
          GT_EXCEL-POSID TO GS_OUTTAB-POSID,
          GT_EXCEL-KSTAR TO GS_OUTTAB-KSTAR,
          GT_EXCEL-WAERS TO GS_OUTTAB-WAERS.
    DO.

      LV_FNAME = 'GT_EXCEL-M' && LV_SMONTH.
      ASSIGN (LV_FNAME) TO <FS1>.

      PERFORM CONV_CHAR_OF_DEC CHANGING <FS1>.

      LV_FNAME = 'GS_OUTTAB-M' && LV_SMONTH.
      ASSIGN (LV_FNAME) TO <FS2>.

      <FS2> = <FS1>.

      IF <FS2> > 0.
        LV_PLUS = ABAP_TRUE.
      ENDIF.

      IF LV_SMONTH = LV_EMONTH.
        EXIT.
      ELSE.
        ADD 1 TO LV_SMONTH.
      ENDIF.

    ENDDO.

    PERFORM CHECK_DATA USING LV_PLUS.

    IF GS_OUTTAB-STATUS = 'E'.
      MOVE ICON_RED_LIGHT TO GS_OUTTAB-ICON.
    ELSEIF GS_OUTTAB-STATUS = 'W'.
      MOVE ICON_YELLOW_LIGHT TO GS_OUTTAB-ICON.
    ENDIF.

    LV_SMONTH = PA_SPERL+1(2).

    DO.

      LV_FNAME = 'GS_OUTTAB-M' && LV_SMONTH.

      ASSIGN (LV_FNAME) TO <FS1>.

      IF GS_OUTTAB-WAERS = 'KRW'.
        _CONV_KRW: <FS1>.
      ENDIF.

      GS_OUTTAB-SUM = GS_OUTTAB-SUM + <FS1>.

      IF LV_SMONTH = LV_EMONTH.
        EXIT.
      ELSE.
        ADD 1 TO LV_SMONTH.
      ENDIF.

    ENDDO.

    PERFORM SET_BAPI_CHECKPRIMCOST.

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

  PERFORM SET_TOP_OF_PAGE.

  PERFORM SET_EVENT.
  PERFORM SET_TABLE_SETTINGS.

  GO_ALV->DISPLAY( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_DATA
*&---------------------------------------------------------------------*
FORM CHECK_DATA  USING VALUE(P_PLUS).

  DATA : LV_OBJNR LIKE COSS-OBJNR.

  DATA: LV_DATUM TYPE DATUM.

  CLEAR GV_MESSAGE.

  "-- 필수 값 체크
  _INITIAL_CHK : GS_OUTTAB-POSID  TEXT-C02,
                 GS_OUTTAB-KSTAR  TEXT-C04,
                 GS_OUTTAB-WAERS  TEXT-C10.

  IF GS_OUTTAB-STATUS = 'E'.
    MESSAGE S026 INTO GS_OUTTAB-MESSAGE
                 WITH GS_OUTTAB-MESSAGE.
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
    MESSAGE S023 INTO GS_OUTTAB-MESSAGE WITH GS_OUTTAB-POSID.
    EXIT.
  ELSE.

    MOVE LS_PRPS-POST1 TO GS_OUTTAB-POST1.

  ENDIF.

  _CONVERSION_IN GS_OUTTAB-KSTAR.

  "-- 2) 원가요소
  SELECT SINGLE B~KTEXT
    INTO @GS_OUTTAB-KTEXT2
    FROM CSKA AS A
    LEFT JOIN CSKU AS B
      ON A~KTOPL = B~KTOPL
     AND A~KSTAR = B~KSTAR
     AND B~SPRAS = @SY-LANGU
   WHERE A~KTOPL = @GV_KTOPL
     AND A~KSTAR = @GS_OUTTAB-KSTAR.

  IF SY-SUBRC <> 0 .
    GS_OUTTAB-STATUS = 'E'.
    MESSAGE S023 INTO GS_OUTTAB-MESSAGE WITH GS_OUTTAB-KSTAR.
    EXIT.
  ENDIF.

*-- 매출 계정 마이너스 확인.
  IF  ( GS_OUTTAB-KSTAR CP '04*' OR
        GS_OUTTAB-KSTAR CP '0701*' OR
        GS_OUTTAB-KSTAR CP '0703*' OR
        GS_OUTTAB-KSTAR CP '0705*' )
     AND P_PLUS = ABAP_TRUE.
    GS_OUTTAB-STATUS = 'W'.
    MESSAGE S000 INTO GS_OUTTAB-MESSAGE WITH TEXT-E02.
  ENDIF.

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

        IF ( <COLUMN_REF>-COLUMNNAME CP 'M*' OR
            <COLUMN_REF>-COLUMNNAME = 'SUM' ) AND
            <COLUMN_REF>-COLUMNNAME <> 'MESSAGE'.

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

  DATA L_FIELD TYPE LVC_CFNAME VALUE 'WAERS'.

  TRY.

      LV_SMONTH = PA_SPERL+1(2).
      LV_EMONTH = PA_EPERL+1(2).

      CLEAR P_COLUMN_TEXT.

      CASE P_COLUMNNAME.

        WHEN 'ICON'.
          P_COLUMN_TEXT = TEXT-C01.

          GR_COLUMN->SET_ICON( IF_SALV_C_BOOL_SAP=>TRUE ).
          GR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).

        WHEN 'BUKRS'.
          P_COLUMN_TEXT = TEXT-C08.

        WHEN 'PRCTR'.
          P_COLUMN_TEXT = TEXT-C09.

        WHEN 'POSID'.
          P_COLUMN_TEXT = TEXT-C02.

        WHEN 'POST1'.
          P_COLUMN_TEXT = TEXT-C03.

        WHEN 'KSTAR'.
          P_COLUMN_TEXT = TEXT-C04.

        WHEN 'KTEXT2'.
          P_COLUMN_TEXT = TEXT-C05.

        WHEN 'WAERS'.
          P_COLUMN_TEXT = TEXT-C10.

        WHEN 'SUM'.
          P_COLUMN_TEXT = TEXT-C06.
          GR_COLUMN->SET_CURRENCY_COLUMN( L_FIELD ).

          _SET_COLOR '3' '0' '0'.
          GR_COLUMN->SET_COLOR( GS_COLOR ).

        WHEN 'MESSAGE'.
          P_COLUMN_TEXT = TEXT-C07.

      ENDCASE.

      IF P_COLUMNNAME CP 'M*' AND P_COLUMNNAME <> 'MESSAGE'.

        P_COLUMN_TEXT = P_COLUMNNAME+1(2).

        SHIFT P_COLUMN_TEXT LEFT DELETING LEADING '0'.
        CONCATENATE P_COLUMN_TEXT TEXT-M01 INTO P_COLUMN_TEXT.

        LV_MONTH = P_COLUMNNAME+1(2).

        IF LV_SMONTH > LV_MONTH OR
           LV_EMONTH < LV_MONTH.

          GR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
        ENDIF.

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

  DATA LV_TLINE  TYPE I.
  DATA LV_SLINE  TYPE I.
  DATA LV_ELINE  TYPE I.
  DATA LV_LN     TYPE I.

  DATA LV_FNAME  TYPE STFNA.
  DATA LV_SMONTH TYPE N LENGTH 2.
  DATA LV_EMONTH TYPE N LENGTH 2.

  FIELD-SYMBOLS: <FS1> TYPE ANY,
                 <FS2> TYPE ANY.

  CLEAR: GV_ERROR, GV_SUCESS, GV_TOTAL.

  DATA : LS_HEADERINFO  LIKE  BAPIPLNHDR.

  DATA : LT_INDEXSTRUCTURE LIKE BAPIACPSTRU OCCURS 0 WITH HEADER LINE,
         LT_COOBJECT       LIKE BAPIPCPOBJ OCCURS 0 WITH HEADER LINE,
         LT_PERVALUE       LIKE BAPIPCPVAL OCCURS 0 WITH HEADER LINE.

  LV_TLINE = LINES( GT_OUTTAB ).

  LOOP AT GT_OUTTAB INTO GS_OUTTAB.

    CLEAR: GT_RETURN, GT_RETURN[].

    CLEAR: LS_HEADERINFO, LT_INDEXSTRUCTURE,
                          LT_INDEXSTRUCTURE[],
                          LT_COOBJECT,
                          LT_COOBJECT[],
                          LT_PERVALUE,
                          LT_PERVALUE[].

*-- Header Data
    LS_HEADERINFO-CO_AREA     = PA_KOKRS.     "관리 회계영역
    LS_HEADERINFO-FISC_YEAR   = PA_GJAHR.     "회계연도
    LS_HEADERINFO-PERIOD_FROM = PA_SPERL.     "기간 시작
    LS_HEADERINFO-PERIOD_TO   = PA_EPERL.     "기간 종료
    LS_HEADERINFO-VERSION     = PA_VERSN.     "버전

*  CONCATENATE PA_GJAHR '년 사업계획' INTO LS_HEADERINFO-DOC_HDR_TX.

*-- 전표 헤더 텍스트
    LS_HEADERINFO-PLAN_CURRTYPE = 'C'. "통화

*-- CO-계획: 액티비티투입 & 주요지표 계획 BAPIs
    LT_INDEXSTRUCTURE-OBJECT_INDEX = 1.
    LT_INDEXSTRUCTURE-VALUE_INDEX  = 1.
    APPEND LT_INDEXSTRUCTURE.

*-- CO 계획: 1차 원가 BAPI에 대한 오브젝트
    LT_COOBJECT-OBJECT_INDEX  = 1.
    LT_COOBJECT-WBS_ELEMENT   = GS_OUTTAB-POSID.
    APPEND LT_COOBJECT.

*-- CO 계획: 1차 원가 BAPI에 대한 값
    LT_PERVALUE-VALUE_INDEX  = 1.
    LT_PERVALUE-COST_ELEM    = GS_OUTTAB-KSTAR.   "원가요소
    LT_PERVALUE-TRANS_CURR   = GS_OUTTAB-WAERS.

    LV_SMONTH = PA_SPERL+1(2).

    DO.

      LV_FNAME = 'GS_OUTTAB-M' && LV_SMONTH.
      ASSIGN (LV_FNAME) TO <FS1>.

      LV_FNAME = 'LT_PERVALUE-FIX_VAL_PER' && LV_SMONTH.
      ASSIGN (LV_FNAME) TO <FS2>.

      IF GS_OUTTAB-WAERS = 'KRW'.
        <FS2> = <FS1>  * 100.
      ELSE.
        <FS2> = <FS1>.
      ENDIF.

      IF LV_SMONTH = LV_EMONTH.
        EXIT.
      ELSE.
        ADD 1 TO LV_SMONTH.
      ENDIF.

    ENDDO.

    APPEND LT_PERVALUE.

    CALL FUNCTION 'BAPI_COSTACTPLN_POSTPRIMCOST'
      EXPORTING
        HEADERINFO     = LS_HEADERINFO
      TABLES
        INDEXSTRUCTURE = LT_INDEXSTRUCTURE
        COOBJECT       = LT_COOBJECT
        PERVALUE       = LT_PERVALUE
        RETURN         = GT_RETURN.

    READ TABLE GT_RETURN WITH KEY TYPE = 'E'.

    IF SY-SUBRC EQ 0 .

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      ADD 1 TO LV_ELINE.

      DESCRIBE TABLE GT_RETURN LINES LV_LN.
      READ TABLE GT_RETURN     INDEX LV_LN.

      PERFORM BUILD_MESSAGE USING    GT_RETURN
                            CHANGING GS_OUTTAB-MESSAGE.

      GS_OUTTAB-ICON = ICON_RED_LIGHT.

    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      GS_OUTTAB-MESSAGE = TEXT-S01.
      GS_OUTTAB-ICON    = ICON_GREEN_LIGHT.

      ADD 1 TO LV_SLINE.

    ENDIF.

    MODIFY GT_OUTTAB FROM GS_OUTTAB.

  ENDLOOP.

  WRITE LV_TLINE TO GV_TOTAL  LEFT-JUSTIFIED.
  WRITE LV_SLINE TO GV_SUCESS LEFT-JUSTIFIED.
  WRITE LV_ELINE TO GV_ERROR  LEFT-JUSTIFIED.

  PERFORM SET_TOP_OF_PAGE.

  MESSAGE S000 WITH TEXT-S02 .

  CALL METHOD GO_ALV->REFRESH
    EXPORTING
      REFRESH_MODE = IF_SALV_C_REFRESH=>SOFT.

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
*& Form SET_TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM SET_TOP_OF_PAGE .

  DATA: LR_CONTENT TYPE REF TO CL_SALV_FORM_ELEMENT.
  PERFORM BUILT_HEADER CHANGING LR_CONTENT.
  GO_ALV->SET_TOP_OF_LIST( LR_CONTENT ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILT_HEADER
*&---------------------------------------------------------------------*
FORM BUILT_HEADER CHANGING CR_CONTENT TYPE REF TO CL_SALV_FORM_ELEMENT.

  DATA: LR_GRID   TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
        LR_GRID_1 TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
        LR_LABEL  TYPE REF TO CL_SALV_FORM_LABEL,
        LR_TEXT   TYPE REF TO CL_SALV_FORM_TEXT,
        L_TEXT    TYPE STRING,
        L_TOTAL   TYPE STRING,
        L_SUCESS  TYPE STRING,
        L_ERROR   TYPE STRING.

  IF GV_TOTAL IS INITIAL.
    GV_TOTAL = 0.
  ENDIF.

  IF GV_SUCESS IS INITIAL.
    GV_SUCESS = 0.
  ENDIF.

  IF GV_ERROR IS INITIAL.
    GV_ERROR = 0.
  ENDIF.

  CREATE OBJECT LR_GRID.

  LR_GRID_1 = LR_GRID->CREATE_GRID(
                ROW    = 1
                COLUMN = 1 ).

  CONCATENATE TEXT-005 ':' PA_KOKRS '(' PA_KTXT ')'
      INTO L_TEXT SEPARATED BY SPACE.

  LR_LABEL = LR_GRID_1->CREATE_LABEL(
   ROW    = 1
   COLUMN = 1
   TEXT    = L_TEXT
   TOOLTIP = L_TEXT ).

  CONCATENATE TEXT-004 ':' PA_VERSN '(' PA_VTXT ')'
      INTO L_TEXT SEPARATED BY SPACE.

  LR_LABEL = LR_GRID_1->CREATE_LABEL(
   ROW    = 2
   COLUMN = 1
   TEXT    = L_TEXT
   TOOLTIP = L_TEXT ).

  CONCATENATE TEXT-007 ':' PA_GJAHR
      INTO L_TEXT SEPARATED BY SPACE.

  LR_LABEL = LR_GRID_1->CREATE_LABEL(
   ROW    = 3
   COLUMN = 1
   TEXT    = L_TEXT
   TOOLTIP = L_TEXT ).

  CONCATENATE TEXT-008 ':' PA_SPERL '~' PA_EPERL
      INTO L_TEXT SEPARATED BY SPACE.

  LR_LABEL = LR_GRID_1->CREATE_LABEL(
   ROW    = 4
   COLUMN = 1
   TEXT    = L_TEXT
   TOOLTIP = L_TEXT ).

  CLEAR L_TEXT.

  LR_LABEL = LR_GRID_1->CREATE_LABEL(
   ROW     = 5
   COLUMN  = 1
   TEXT    = L_TEXT
   TOOLTIP = L_TEXT ).

  CONCATENATE TEXT-T03 GV_TOTAL
              TEXT-T04 GV_SUCESS
              TEXT-T05 GV_ERROR
       INTO L_TEXT
       SEPARATED BY SPACE.

  LR_LABEL = LR_GRID_1->CREATE_LABEL(
   ROW     = 6
   COLUMN  = 1
   TEXT    = L_TEXT
   TOOLTIP = L_TEXT ).

  CR_CONTENT = LR_GRID.

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

  IF PA_SPERL > PA_EPERL.
    SET CURSOR FIELD 'PA_SPERL'.
    MESSAGE E018  WITH TEXT-E03.
  ENDIF.

  SELECT SINGLE VTEXT INTO @PA_VTXT
    FROM TKVS AS A
    LEFT JOIN TKVST AS B
      ON A~VERSI = B~VERSI
     AND B~SPRAS = @SY-LANGU
   WHERE A~VERSI = @PA_VERSN.

  IF SY-SUBRC <> 0.
    SET CURSOR FIELD 'PA_VERSN'.
    MESSAGE E027  WITH TEXT-004.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_BAPI_CHECKPRIMCOST
*&---------------------------------------------------------------------*
FORM SET_BAPI_CHECKPRIMCOST .

  DATA : LS_HEADERINFO  LIKE  BAPIPLNHDR.

  DATA : LT_INDEXSTRUCTURE LIKE BAPIACPSTRU OCCURS 0 WITH HEADER LINE,
         LT_COOBJECT       LIKE BAPIPCPOBJ OCCURS 0 WITH HEADER LINE,
         LT_PERVALUE       LIKE BAPIPCPVAL OCCURS 0 WITH HEADER LINE.

  DATA LV_FNAME  TYPE STFNA.
  DATA LV_SMONTH TYPE N LENGTH 2.
  DATA LV_EMONTH TYPE N LENGTH 2.

  FIELD-SYMBOLS: <FS1> TYPE ANY,
                 <FS2> TYPE ANY.

  CHECK GS_OUTTAB-STATUS <> 'E'.

  CLEAR : GT_RETURN , GT_RETURN[].

*-- Header Data
  LS_HEADERINFO-CO_AREA     = PA_KOKRS.     "관리 회계영역
  LS_HEADERINFO-FISC_YEAR   = PA_GJAHR.     "회계연도
  LS_HEADERINFO-PERIOD_FROM = PA_SPERL.     "기간 시작
  LS_HEADERINFO-PERIOD_TO   = PA_EPERL.     "기간 종료
  LS_HEADERINFO-VERSION     = PA_VERSN.     "버전

*  CONCATENATE PA_GJAHR '년 사업계획' INTO LS_HEADERINFO-DOC_HDR_TX.

*-- 전표 헤더 텍스트
  LS_HEADERINFO-PLAN_CURRTYPE = 'C'. "통화

*-- CO-계획: 액티비티투입 & 주요지표 계획 BAPIs
  LT_INDEXSTRUCTURE-OBJECT_INDEX = 1.
  LT_INDEXSTRUCTURE-VALUE_INDEX  = 1.
  APPEND LT_INDEXSTRUCTURE.

*-- CO 계획: 1차 원가 BAPI에 대한 오브젝트
  LT_COOBJECT-OBJECT_INDEX = 1.
  LT_COOBJECT-WBS_ELEMENT  = GS_OUTTAB-POSID.
  APPEND LT_COOBJECT.

*-- CO 계획: 1차 원가 BAPI에 대한 값

  LT_PERVALUE-VALUE_INDEX  = 1.
  LT_PERVALUE-COST_ELEM    = GS_OUTTAB-KSTAR.   "원가요소
  LT_PERVALUE-TRANS_CURR   = GS_OUTTAB-WAERS.

  LV_SMONTH = PA_SPERL+1(2).

  DO.

    LV_FNAME = 'GS_OUTTAB-M' && LV_SMONTH.
    ASSIGN (LV_FNAME) TO <FS1>.

    LV_FNAME = 'LT_PERVALUE-FIX_VAL_PER' && LV_SMONTH.
    ASSIGN (LV_FNAME) TO <FS2>.

    IF GS_OUTTAB-WAERS = 'KRW'.
      <FS2> = <FS1>  * 100.
    ELSE.
      <FS2> = <FS1>.
    ENDIF.

    IF LV_SMONTH = LV_EMONTH.
      EXIT.
    ELSE.
      ADD 1 TO LV_SMONTH.
    ENDIF.

  ENDDO.

  APPEND LT_PERVALUE.

*-- 이상 없이 기표가 가능한지 점검
  CALL FUNCTION 'BAPI_COSTACTPLN_CHECKPRIMCOST'
    EXPORTING
      HEADERINFO     = LS_HEADERINFO
    TABLES
      INDEXSTRUCTURE = LT_INDEXSTRUCTURE
      COOBJECT       = LT_COOBJECT
      PERVALUE       = LT_PERVALUE
      RETURN         = GT_RETURN.

  READ TABLE GT_RETURN WITH KEY TYPE = 'E'.

  IF SY-SUBRC EQ 0 .

    DESCRIBE TABLE GT_RETURN LINES DATA(LV_LN).
    READ TABLE GT_RETURN     INDEX LV_LN.

    PERFORM BUILD_MESSAGE USING    GT_RETURN
                          CHANGING GS_OUTTAB-MESSAGE.

    GS_OUTTAB-ICON = ICON_RED_LIGHT.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_MESSAGE
*&---------------------------------------------------------------------*
FORM BUILD_MESSAGE  USING    PS_MESSAGE STRUCTURE BAPIRET2
                     CHANGING PV_TEXT.

  CLEAR PV_TEXT.

  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      MSGID               = PS_MESSAGE-ID
      MSGNR               = PS_MESSAGE-NUMBER
      MSGV1               = PS_MESSAGE-MESSAGE_V1
      MSGV2               = PS_MESSAGE-MESSAGE_V2
      MSGV3               = PS_MESSAGE-MESSAGE_V3
      MSGV4               = PS_MESSAGE-MESSAGE_V4
    IMPORTING
      MESSAGE_TEXT_OUTPUT = PV_TEXT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SCREEN
*&---------------------------------------------------------------------*
FORM SET_SCREEN .

  LOOP AT SCREEN INTO DATA(SCREEN_WA).
    IF SCREEN_WA-GROUP1 = 'MG1'.
      SCREEN_WA-INTENSIFIED = '1'.
      MODIFY SCREEN FROM SCREEN_WA.
    ENDIF.
  ENDLOOP.

ENDFORM.
