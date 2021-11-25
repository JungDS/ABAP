*&---------------------------------------------------------------------*
*& Include          ZCOR0010F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITAIL
*&---------------------------------------------------------------------*
FORM INITAIL .

  GV_REPID = SY-REPID.
*  GET PARAMETER ID 'CAC' FIELD PA_KOKRS.

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
      FILE_FILTER             = 'Excel files (*.XLS)|*.XLS|Excel files (*.XLSX)|*.XLSX|'
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
      PERFORM EXCELFORM_DOWNLOAD_V1 USING 'ZCOR0010'.

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
      I_END_COL               = 10
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

  DATA: LT_CELLTYPE TYPE SALV_T_INT4_COLUMN.
  DATA: LS_CELLTYPE LIKE LINE OF LT_CELLTYPE.

  LOOP AT GT_EXCEL.

    REFRESH LT_CELLTYPE.

    PERFORM CONV_CHAR_OF_DATE CHANGING GT_EXCEL-DATAB.
    PERFORM CONV_CHAR_OF_DATE CHANGING GT_EXCEL-DATBI.

    MOVE-CORRESPONDING GT_EXCEL TO GS_OUTTAB.
    GS_OUTTAB-KTEXT = GS_OUTTAB-LTEXT.
    PERFORM CHECK_DATA.

    IF GS_OUTTAB-STATUS = 'E'.
      MOVE ICON_RED_LIGHT TO GS_OUTTAB-ICON.
    ENDIF.

    LS_CELLTYPE-COLUMNNAME = 'ICON'.
    LS_CELLTYPE-VALUE      = IF_SALV_C_CELL_TYPE=>BUTTON.
    APPEND LS_CELLTYPE TO LT_CELLTYPE.

    APPEND LINES OF LT_CELLTYPE TO GS_OUTTAB-I_CELLTYPE.

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
*& Form LIST_BOX_SET
*&---------------------------------------------------------------------*
FORM LIST_BOX_SET .

  TYPE-POOLS : VRM.
  DATA: L_NAME     TYPE VRM_ID,
        L_LIST     TYPE VRM_VALUES,
        L_VALUE    LIKE LINE OF L_LIST,
        L_TEMP(20).

  DEFINE CATALOG1.
    L_VALUE-KEY  = &1 .
    L_VALUE-TEXT = &2 .
    APPEND L_VALUE TO L_LIST.
    CLEAR: L_VALUE.
  END-OF-DEFINITION.

  L_NAME = 'PA_MODE'.

  CLEAR L_LIST.
  CATALOG1: 'N' 'Background',
            'E' 'Error Check',
            'A' 'Foreground'.
  SORT L_LIST.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID     = L_NAME
      VALUES = L_LIST.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_DATA
*&---------------------------------------------------------------------*
FORM CHECK_DATA .

  DATA: LV_DATUM TYPE DATUM.

  CLEAR GV_MESSAGE.

  "-- 필수 값 체크
  _INITIAL_CHK : GS_OUTTAB-BUKRS  '회사코드',
                 GS_OUTTAB-KOSTL  '코스트센터',
                 GS_OUTTAB-DATBI  '효력시작일',
                 GS_OUTTAB-DATAB  '종료일',
                 GS_OUTTAB-KTEXT  '코스트센터명',
                 GS_OUTTAB-KOSAR  '코스트센터범주',
                 GS_OUTTAB-KHINR  '계층구조영역',
                 GS_OUTTAB-GSBER  '사업영역',
                 GS_OUTTAB-WAERS  '통화',
                 GS_OUTTAB-PRCTR  '손익센터'.

  IF GS_OUTTAB-STATUS = 'E'.
    MESSAGE S026 INTO GS_OUTTAB-MESSAGE
                 WITH GS_OUTTAB-MESSAGE.
    EXIT.
  ENDIF.

  LV_DATUM = GT_EXCEL-DATBI.

*  IF GS_OUTTAB-BUKRS <> PA_BUKRS.
*    GS_OUTTAB-STATUS        = 'E'.
*    MESSAGE S038 INTO GS_OUTTAB-MESSAGE WITH TEXT-C12.
*    EXIT.
*  ENDIF.

  _CONVERSION_IN GS_OUTTAB-KOSTL.

  "-- 1) 코스트 센터
  SELECT SINGLE *
    INTO @DATA(LS_CSKS)
    FROM CSKS
   WHERE KOKRS = @PA_KOKRS
     AND KOSTL = @GS_OUTTAB-KOSTL
     AND DATBI >= @LV_DATUM
     AND DATAB <= @LV_DATUM.

  IF SY-SUBRC EQ 0 .
    GS_OUTTAB-STATUS        = 'E'.
    GS_OUTTAB-MESSAGE       = TEXT-E01.
    EXIT.
  ENDIF.

  "-- 2) 코스트 센터 범주_ TKA05-KOSAR
  SELECT SINGLE KOSAR INTO @DATA(LV_KOSAR)
   FROM TKA05
   WHERE KOSAR = @GS_OUTTAB-KOSAR.

  IF SY-SUBRC <> 0 .
    GS_OUTTAB-STATUS        = 'E'.
    MESSAGE S038 INTO GS_OUTTAB-MESSAGE WITH TEXT-C06.
    EXIT.
  ENDIF.

  "-- 3)통화 __TCURC-WAERS
  SELECT SINGLE WAERS INTO @DATA(LV_WAERS)
   FROM TCURC
   WHERE WAERS = @GS_OUTTAB-WAERS.

  IF SY-SUBRC <> 0 .
    GS_OUTTAB-STATUS        = 'E'.
    MESSAGE S038 INTO GS_OUTTAB-MESSAGE WITH TEXT-C08.
    EXIT.
  ENDIF.

  "-- 4)손익센터 __CEPC-PRCTR
  SELECT SINGLE * INTO @DATA(LS_CEPC)
    FROM CEPC
   WHERE PRCTR = @GS_OUTTAB-PRCTR
     AND KOKRS = @PA_KOKRS
     AND DATBI >= @LV_DATUM
     AND DATAB <= @LV_DATUM.

  IF SY-SUBRC NE 0 .
    GS_OUTTAB-STATUS        = 'E'.
    MESSAGE S038 INTO GS_OUTTAB-MESSAGE WITH TEXT-C09.
    EXIT.

  ELSE.

    "-- 4)손익센터 __CEPC-PRCTR __ 회사코드 체크
    SELECT SINGLE * INTO @DATA(LS_CEPC_BUKRS)
      FROM CEPC_BUKRS
     WHERE KOKRS = @PA_KOKRS
       AND PRCTR = @GS_OUTTAB-PRCTR
       AND BUKRS = @GS_OUTTAB-BUKRS.

    IF SY-SUBRC NE 0.
      GS_OUTTAB-STATUS        = 'E'.
      MESSAGE S041 INTO GS_OUTTAB-MESSAGE WITH GS_OUTTAB-BUKRS GS_OUTTAB-PRCTR.
      EXIT.
    ENDIF.

  ENDIF.

  "-- 5)사업영역 __CEPC-PRCTR
  SELECT SINGLE GSBER INTO @DATA(LV_GSBER)
    FROM TGSB
   WHERE GSBER = @GS_OUTTAB-GSBER.

  IF SY-SUBRC NE 0 .
    GS_OUTTAB-STATUS        = 'E'.
    MESSAGE S038 INTO GS_OUTTAB-MESSAGE WITH TEXT-C11.
    EXIT.
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

*-- set column
  TRY.
      LR_COLUMNS = GO_ALV->GET_COLUMNS( ).
      LR_COLUMNS->SET_OPTIMIZE( ABAP_TRUE ).
      LR_COLUMNS->SET_CELL_TYPE_COLUMN( 'I_CELLTYPE' ).
    CATCH CX_SALV_DATA_ERROR.
  ENDTRY.

  PERFORM SET_COLUMNS_TECHNICAL USING LR_COLUMNS.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_COLUMNS_TECHNICAL
*&---------------------------------------------------------------------*
FORM SET_COLUMNS_TECHNICAL USING IR_COLUMNS TYPE REF TO
                                            CL_SALV_COLUMNS_TABLE.

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
      ENDLOOP.
    CATCH CX_SALV_NOT_FOUND.
  ENDTRY.

  TRY.
      LR_COLUMN ?= IR_COLUMNS->GET_COLUMN( 'ICON' ).
      LR_COLUMN->SET_ICON( IF_SALV_C_BOOL_SAP=>TRUE ).
      LR_COLUMN->SET_KEY( IF_SALV_C_BOOL_SAP=>TRUE ).
    CATCH CX_SALV_NOT_FOUND.
  ENDTRY.

  TRY.
      LR_COLUMN ?= IR_COLUMNS->GET_COLUMN( 'STATUS' ).
      LR_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
    CATCH CX_SALV_NOT_FOUND.
  ENDTRY.

*-- SELECT FIELD 추가
  LR_SELECTIONS = GO_ALV->GET_SELECTIONS( ).
  LR_SELECTIONS->SET_SELECTION_MODE( IF_SALV_C_SELECTION_MODE=>CELL ).

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

  CLEAR P_COLUMN_TEXT.

  CASE P_COLUMNNAME.

    WHEN 'ICON'.
      P_COLUMN_TEXT = TEXT-C01.

    WHEN 'BUKRS'.
      P_COLUMN_TEXT = TEXT-C12.

    WHEN 'KOSTL'.
      P_COLUMN_TEXT = TEXT-C02.

    WHEN 'KTEXT'.
      P_COLUMN_TEXT = TEXT-C03.

    WHEN 'DATAB'.
      P_COLUMN_TEXT = TEXT-C04.

    WHEN 'DATBI'.
      P_COLUMN_TEXT = TEXT-C05.

    WHEN 'KOSAR'.
      P_COLUMN_TEXT = TEXT-C06.

    WHEN 'KHINR'.
      P_COLUMN_TEXT = TEXT-C07.

    WHEN 'GSBER'.
      P_COLUMN_TEXT = TEXT-C11.

    WHEN 'WAERS'.
      P_COLUMN_TEXT = TEXT-C08.

    WHEN 'PRCTR'.
      P_COLUMN_TEXT = TEXT-C09.

    WHEN 'MESSAGE'.
      P_COLUMN_TEXT = TEXT-C10.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BDCPARAMETER_SET
*&---------------------------------------------------------------------*
FORM BDCPARAMETER_SET USING G_CTU_PARAMS_S LIKE CTU_PARAMS
                            P_MODE.

  G_CTU_PARAMS_S-DEFSIZE = 'X'.
  G_CTU_PARAMS_S-UPDMODE = 'S'.
  G_CTU_PARAMS_S-DISMODE = P_MODE.
  G_CTU_PARAMS_S-RACOMMIT = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_EXECUTE
*&---------------------------------------------------------------------*
FORM CREATE_EXECUTE .

  DATA LR_SELECTIONS TYPE REF TO CL_SALV_SELECTIONS.

  DATA LV_TLINE  TYPE I.
  DATA LV_SLINE  TYPE I.
  DATA LV_ELINE  TYPE I.

  CLEAR: GT_ROWS.

  CLEAR: GV_ERROR, GV_SUCESS, GV_TOTAL.

  LR_SELECTIONS = GO_ALV->GET_SELECTIONS( ).
  GT_ROWS = LR_SELECTIONS->GET_SELECTED_ROWS( ).

  IF GT_ROWS IS INITIAL.
    MESSAGE S015 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  PERFORM K_KOKRS_SET.

  LV_TLINE = LINES( GT_ROWS ).

  PERFORM BDCPARAMETER_SET USING G_CTU_PARAMS_S
                                 PA_MODE.

  LOOP AT GT_ROWS INTO GS_ROW.

    READ TABLE GT_OUTTAB INTO GS_OUTTAB INDEX GS_ROW.

    IF SY-SUBRC = 0.

      CLEAR: BDCDATA,     BDCDATA[],
             G_MESSTAB_T, G_MESSTAB_T[].

      PERFORM DYNPRO USING :
          'X'  'SAPLKMA1'                '0200',
          ' '  'BDC_OKCODE'              '/00',
          ' ' 'CSKSZ-KOKRS'             PA_KOKRS,
          ' ' 'CSKSZ-KOSTL'             GS_OUTTAB-KOSTL,
          ' ' 'CSKSZ-DATAB_ANFO'        GS_OUTTAB-DATAB,
          ' ' 'CSKSZ-DATBI_ANFO'        GS_OUTTAB-DATBI.

      PERFORM DYNPRO USING :
          'X' 'SAPLKMA1'                 '0299',
          ' ' 'BDC_OKCODE'               '/00',
          ' ' 'CSKSZ-KTEXT'              GS_OUTTAB-KTEXT, "이름
          ' ' 'CSKSZ-LTEXT'              GS_OUTTAB-LTEXT, "내역
          ' ' 'CSKSZ-VERAK'              TEXT-V01,
          ' ' 'CSKSZ-KOSAR'              GS_OUTTAB-KOSAR,
          ' ' 'CSKSZ-KHINR'              GS_OUTTAB-KHINR,
          ' ' 'CSKSZ-GSBER'              GS_OUTTAB-GSBER,
          ' ' 'CSKSZ-WAERS'              GS_OUTTAB-WAERS,
*          ' ' 'CSKSZ-BUKRS'              PA_BUKRS,
          ' ' 'CSKSZ-BUKRS'              GS_OUTTAB-BUKRS,
          ' ' 'CSKSZ-PRCTR'              GS_OUTTAB-PRCTR.

      PERFORM DYNPRO USING :
         'X' 'SAPLKMA1'                  '0299',
         ' ' 'BDC_OKCODE'                '=BU'.

      CALL TRANSACTION 'KS01'     USING         BDCDATA
                                  MESSAGES INTO G_MESSTAB_T
                                  OPTIONS  FROM G_CTU_PARAMS_S.

      READ TABLE G_MESSTAB_T WITH KEY MSGTYP = 'E'.

      IF SY-SUBRC <> 0.

        COMMIT WORK .
        READ TABLE G_MESSTAB_T WITH KEY MSGTYP = 'S'.
        MOVE ICON_GREEN_LIGHT TO GS_OUTTAB-ICON.

        ADD 1 TO LV_SLINE.

      ELSE.
        ROLLBACK WORK.
        MOVE ICON_RED_LIGHT TO GS_OUTTAB-ICON.

        ADD 1 TO LV_ELINE.

      ENDIF.

      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          MSGID               = G_MESSTAB_T-MSGID
          MSGNR               = G_MESSTAB_T-MSGNR
          MSGV1               = G_MESSTAB_T-MSGV1
          MSGV2               = G_MESSTAB_T-MSGV2
          MSGV3               = G_MESSTAB_T-MSGV3
          MSGV4               = G_MESSTAB_T-MSGV4
        IMPORTING
          MESSAGE_TEXT_OUTPUT = GS_OUTTAB-MESSAGE.

    ENDIF.

    MODIFY GT_OUTTAB FROM GS_OUTTAB INDEX GS_ROW.

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

  DATA LR_SELECTIONS TYPE REF TO CL_SALV_SELECTIONS.
  CLEAR: GT_ROWS.
  CLEAR GV_EXIT.

  LR_SELECTIONS = GO_ALV->GET_SELECTIONS( ).
  GT_ROWS = LR_SELECTIONS->GET_SELECTED_ROWS( ).

  IF GT_ROWS IS INITIAL.
    MESSAGE S015 DISPLAY LIKE 'E'.
    GV_EXIT = ABAP_TRUE.
    EXIT.
  ENDIF.

  LOOP AT GT_ROWS INTO GS_ROW.

    READ TABLE GT_OUTTAB INTO GS_OUTTAB INDEX GS_ROW.

    IF GS_OUTTAB-STATUS = 'E' AND SY-SUBRC = 0 .
      MESSAGE S000 WITH TEXT-E05 DISPLAY LIKE 'E'.
      GV_EXIT = ABAP_TRUE.
      EXIT.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CONV_CHAR_OF_DATE
*&---------------------------------------------------------------------*
FORM CONV_CHAR_OF_DATE  CHANGING PV_DATE.

  CHECK PV_DATE IS NOT INITIAL.

  CONDENSE PV_DATE NO-GAPS.
  REPLACE ALL OCCURRENCES OF '.' IN PV_DATE WITH ''.
  REPLACE ALL OCCURRENCES OF '-' IN PV_DATE WITH ''.
  REPLACE ALL OCCURRENCES OF '/' IN PV_DATE WITH ''.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form K_KOKRS_SET
*&---------------------------------------------------------------------*
FORM K_KOKRS_SET.

  CALL FUNCTION 'K_KOKRS_SET'
    EXPORTING
      I_KOKRS   = PA_KOKRS
      POPUP     = '0'
    EXCEPTIONS
      NOT_FOUND = 1
      OTHERS    = 2.

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
*& Form DYNPRO
*&---------------------------------------------------------------------*
FORM DYNPRO USING DYNBEGIN NAME VALUE.

  IF DYNBEGIN = 'X'.
    CLEAR BDCDATA.
    MOVE: NAME  TO BDCDATA-PROGRAM,
          VALUE TO BDCDATA-DYNPRO,
            'X' TO BDCDATA-DYNBEGIN.
    APPEND BDCDATA.
  ELSE.
    CLEAR BDCDATA.
    MOVE: NAME  TO BDCDATA-FNAM,
          VALUE TO BDCDATA-FVAL.
    APPEND BDCDATA.
  ENDIF.

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

  CREATE OBJECT LR_GRID.

  L_TEXT = TEXT-T02.

  LR_GRID->CREATE_HEADER_INFORMATION(
    ROW    = 1
    COLUMN = 1
    TEXT    = L_TEXT
    TOOLTIP = L_TEXT ).

  LR_GRID->ADD_ROW( ).

  LR_GRID_1 = LR_GRID->CREATE_GRID(
                ROW    = 3
                COLUMN = 1 ).

  LR_LABEL = LR_GRID_1->CREATE_LABEL(
    ROW     = 1
    COLUMN  = 1
    TEXT    = TEXT-T03
    TOOLTIP = TEXT-T03 ).

  LR_TEXT = LR_GRID_1->CREATE_TEXT(
    ROW     = 1
    COLUMN  = 2
    TEXT    = GV_TOTAL
    TOOLTIP = GV_TOTAL ).

  LR_LABEL->SET_LABEL_FOR( LR_TEXT ).

  LR_LABEL = LR_GRID_1->CREATE_LABEL(
    ROW    = 2
    COLUMN = 1
    TEXT    = TEXT-T04
    TOOLTIP = TEXT-T04 ).

  LR_TEXT = LR_GRID_1->CREATE_TEXT(
    ROW    = 2
    COLUMN = 2
    TEXT    = GV_SUCESS
    TOOLTIP = GV_SUCESS ).

  LR_LABEL->SET_LABEL_FOR( LR_TEXT ).

  LR_LABEL = LR_GRID_1->CREATE_LABEL(
    ROW    = 3
    COLUMN = 1
    TEXT    = TEXT-T05
    TOOLTIP = TEXT-T05 ).

  LR_TEXT = LR_GRID_1->CREATE_TEXT(
    ROW    = 3
    COLUMN = 2
    TEXT    = GV_ERROR
    TOOLTIP = GV_ERROR ).

  LR_LABEL->SET_LABEL_FOR( LR_TEXT ).

  CR_CONTENT = LR_GRID.

ENDFORM.
**&---------------------------------------------------------------------*
**& Form CHECK_BUKRS_WITH_TKA01
**&---------------------------------------------------------------------*
*FORM CHECK_BUKRS_WITH_TKA01 CHANGING PS_TKA02 LIKE TKA02
*                                     PV_BUTXT.
*
*  DATA : L_DYNP_VALUE_TAB LIKE TABLE OF DYNPREAD WITH HEADER LINE.
*
*  CALL FUNCTION 'DYNP_VALUES_READ'
*    EXPORTING
*      DYNAME     = SY-REPID
*      DYNUMB     = SY-DYNNR
*    TABLES
*      DYNPFIELDS = L_DYNP_VALUE_TAB.
*
*  READ TABLE L_DYNP_VALUE_TAB WITH KEY FIELDNAME = 'PA_BUKRS'.
*
*  IF SY-SUBRC = 0.
*    MOVE L_DYNP_VALUE_TAB-FIELDVALUE TO PA_BUKRS.
*  ENDIF.
*
*  CHECK PA_BUKRS IS NOT INITIAL.
*
*  SELECT SINGLE BUTXT
*   FROM T001
*   INTO PV_BUTXT
*  WHERE BUKRS = PA_BUKRS.
*
*  IF SY-SUBRC NE 0.
*    MESSAGE E027  WITH PA_BUKRS.
*  ELSE.
*
*    CLEAR PS_TKA02.
*    SELECT SINGLE B~*
*    FROM TKA01 AS A JOIN TKA02 AS B
*    ON B~KOKRS = A~KOKRS
*    WHERE B~BUKRS = @PA_BUKRS
*      AND B~GSBER = @SPACE
*      AND A~KOKRS = @PA_KOKRS
*    INTO @PS_TKA02.
*
*    IF SY-SUBRC <> 0.
*      MESSAGE E027  WITH PA_BUKRS.
*    ENDIF.
*
*  ENDIF.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCREEN_SET
*&---------------------------------------------------------------------*
FORM SCREEN_SET .

  LOOP AT SCREEN.
    IF SCREEN-NAME = 'PA_KOKRS'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.
