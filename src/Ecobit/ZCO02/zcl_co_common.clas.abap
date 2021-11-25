class ZCL_CO_COMMON definition
  public
  create public .

public section.

  types:
    BEGIN OF TS_CELL,
           VALUE TYPE TEXT4096,
         END OF TS_CELL .
  types:
    TT_CELL TYPE TABLE OF TS_CELL .
  types:
    BEGIN OF TS_EXCEL_TABLINE ,
            ROW   TYPE I,
            COL   TYPE I,
            VALUE TYPE TEXT4096,
          END OF TS_EXCEL_TABLINE .
  types:
    TT_EXCEL_TABLINE TYPE TABLE OF TS_EXCEL_TABLINE .
  types:
    TT_DDSHRETVAL TYPE TABLE OF DDSHRETVAL .

  constants C_PROFID type TCNT-PROFID value '000000000001' ##NO_TEXT.
  constants C_ERKRS type ERKRS value '1000' ##NO_TEXT.
  constants C_FILETYPE_EXCEL type C value 'E' ##NO_TEXT.
  constants C_FILETYPE_TEXT type C value 'T' ##NO_TEXT.
  constants C_LINE_END type C value %_NEWLINE ##NO_TEXT.

  class-methods GET_REPORT_PROGNAME
    importing
      !I_RGJNR type T803VP-RGJNR
      !I_VERSN type T803VP-VERSN
      !I_PROGCLASS type T803VP-PROGCLASS
    exporting
      !E_PROGNAME type PROGNAME
    exceptions
      REPORT_PROGRAM_NOT_FOUND .
  class-methods CALL_TRANSACTION
    importing
      !I_TCODE type SY-TCODE
      !I_SKIP_SCREEN type SY-FTYPE optional
      !I_NEW_SESSION type SY-FTYPE optional
      !IT_SPAGPA type RFC_T_SPAGPA optional .
  class-methods F4_BUKRS
    importing
      !I_KOKRS type TKA02-KOKRS default '1000'
      !I_REPID type SY-REPID default SY-CPROG
      !I_DYNNR type SY-DYNNR default SY-DYNNR
    returning
      value(R_BUKRS) type TKA02-BUKRS .
  class-methods F4_FILE
    importing
      !I_PATH type LOCALFILE default 'C:\'
      !I_MULTI type ABAP_BOOL optional
      !I_FILE_TYPE type C optional
    exporting
      !ET_FILE type FILETABLE
    returning
      value(E_FILE) type LOCALFILE .
  class-methods F4_GROUP
    importing
      !I_CLASS type RGSBS-CLASS
      !I_FIELD type RGSBS-FIELD
      !I_KOKRS type TKA01-KOKRS optional
    returning
      value(R_SETNAME) type SETHEADER-SETNAME
    exceptions
      NO_INPUT_PARAMETER .
  class-methods F4_KOKRS
    importing
      !I_REPID type SY-REPID default SY-CPROG
      !I_DYNNR type SY-DYNNR default SY-DYNNR
    returning
      value(R_KOKRS) type KOKRS .
  class-methods F4_KSGRU
    importing
      !I_KOKRS type TKA01-KOKRS optional
    returning
      value(R_KSGRU) type KSGRU .
  class-methods F4_PDGR
    importing
      !I_KOKRS type TKA01-KOKRS optional
    returning
      value(R_PDGR) type POSIDGR .
  class-methods FILE_DOWNLOAD
    importing
      !I_OBJID type WWWDATATAB-OBJID default SY-CPROG
      !I_EXECUTE type XFELD default ABAP_ON
      !I_INPUT_START_ROW type I optional
      !I_INPUT_START_COL type I optional
      !IT_INPUT_DATA type TT_EXCEL_TABLINE optional
    exceptions
      NOT_EXIST_OBJECT_ID
      NO_DATA_LENGTH
      FILE_DOWNLOAD_ERROR .
  class-methods GET_CONTAINER_01
    exporting
      !ER_SPLIT type ref to CL_GUI_SPLITTER_CONTAINER
      !ER_CON_TOP type ref to CL_GUI_CONTAINER
      !ER_CON_MAIN type ref to CL_GUI_CONTAINER .
  class-methods GET_DEFAULT_BUKRS
    returning
      value(R_BUKRS) type BUKRS .
  class-methods GET_DEFAULT_KOKRS
    returning
      value(R_KOKRS) type TKA01-KOKRS .
  class-methods GET_DYNP_VALUE
    importing
      !I_FIELD type DYNPREAD-FIELDNAME
      !I_REPID type SY-REPID default SY-CPROG
      !I_DYNNR type D020S-DNUM default SY-DYNNR
    returning
      value(E_VALUE) type STRING .
  class-methods GET_EXCEL_CONTENTS
    importing
      !I_FILENAME type RLGRAP-FILENAME
      !I_BEGIN_ROW type I
      !I_BEGIN_COL type I
      !I_END_ROW type I optional
      !I_END_COL type I
    exporting
      value(ET_INTERN) type TT_EXCEL_TABLINE
    exceptions
      NO_INPUT_EXCEL_FILE
      INCONSISTENT_PARAMETERS .
  class-methods GET_HIERARCHY_LIST
    importing
      !I_TABLE type SETHIER-TABNAME optional
      !I_CLASS type SETHIER-SETCLASS
      !I_KOKRS type SETHIER-KOKRS default '1000'
      !I_SETNAME type SETLEAF-SETNAME
    exporting
      !ET_NODES type GSETH_NODE_TAB
      !ET_VALUES type GSETH_VAL_TAB
    exceptions
      NO_INPUT_CLASS .
  class-methods GET_TEXT_BUKRS
    importing
      !I_BUKRS type T001-BUKRS
    returning
      value(R_BUTXT) type T001-BUTXT .
  class-methods GET_TEXT_KOKRS
    importing
      !I_KOKRS type TKA01-KOKRS
    returning
      value(R_BEZEI) type TKA01-BEZEI .
  class-methods POPUP_CONFIRM
    importing
      !I_TITLEBAR type TEXT60 optional
      !I_QUESTION type STRING
      !I_CANCEL type C optional
      !I_POPUP_TYPE type ICON-NAME optional
    returning
      value(R_ANSWER) type ABAP_BOOL .
  class-methods POPUP_VALUE_REQUEST
    importing
      !I_RETFIELD type DFIES-FIELDNAME
      !I_REPID type SY-REPID default SPACE
      !I_DYNNR type SY-DYNNR default SPACE
      !I_FIELD type HELP_INFO-DYNPROFLD default SPACE
      !IT_VALUE type STANDARD TABLE
    exporting
      !ET_RETURN type TT_DDSHRETVAL
    returning
      value(R_FIELDVAL) type SHVALUE_D .
  class-methods SET_DB_PROFILE
    importing
      !I_PROFID type TCNT-PROFID
    exceptions
      NOT_EXIST_PROFID .
  class-methods SET_DYNP_VALUE
    importing
      !I_FIELD type DYNPREAD-FIELDNAME
      !I_REPID type SY-REPID default SY-CPROG
      !I_DYNNR type D020S-DNUM default SY-DYNNR
      value(I_VALUE) type CLIKE .
  class-methods SET_KOKRS
    importing
      !I_KOKRS type TKA01-KOKRS
      !I_POPUP type C default '0' .
protected section.
private section.

  class-methods EXCEL_FILE_CLOSE
    changing
      !APPLICATION type OLE2_OBJECT
      !WORKBOOKS type OLE2_OBJECT
      !WORKBOOK type OLE2_OBJECT
      !WORKSHEET type OLE2_OBJECT .
  class-methods EXCEL_FILE_INPUT
    importing
      !I_WORKBOOK type OLE2_OBJECT
      !I_WORKSHEET type OLE2_OBJECT
      !I_BEGIN_ROW type I default 1
      !I_BEGIN_COL type I default 1
      !IT_DATA type TT_EXCEL_TABLINE
    exceptions
      WORKBOOK_IS_INITIAL
      WORKSHEET_IS_INITIAL .
  class-methods EXCEL_FILE_OPEN
    importing
      !I_FILENAME type RLGRAP-FILENAME
    exporting
      !E_APPLICATION type OLE2_OBJECT
      !E_WORKBOOKS type OLE2_OBJECT
      !E_WORKBOOK type OLE2_OBJECT
      !E_WORKSHEET type OLE2_OBJECT
      !E_MAX_ROW type I
      !E_MAX_COL type I
    exceptions
      FILE_OPEN .
  class-methods EXCEL_FILE_READ
    importing
      !I_WORKSHEET type OLE2_OBJECT
      !I_ROW_FROM type I
      !I_ROW_TO type I
      !I_COL type I
    exporting
      value(ET_CELL) type TT_CELL
    exceptions
      CLIPBOARD_IMPORT
      WORKSHEET_IS_INITIAL .
  class-methods EXCEL_FILE_SHOW
    importing
      !I_APPLICATION type OLE2_OBJECT
      !I_WORKBOOKS type OLE2_OBJECT
      !I_WORKBOOK type OLE2_OBJECT
      !I_WORKSHEET type OLE2_OBJECT
    exceptions
      APPLICATION_IS_INITIAL .
  class-methods SHOW_MESSAGE
    importing
      !I_MESSAGE type STRING
      !I_MESSAGE_TYPE type C default 'S'
      !I_DISPLAY_TYPE type C default SPACE .
ENDCLASS.



CLASS ZCL_CO_COMMON IMPLEMENTATION.


  method CALL_TRANSACTION.

    IF I_NEW_SESSION IS INITIAL.

      CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
        EXPORTING
          TCODE       = I_TCODE
          SKIP_SCREEN = I_SKIP_SCREEN
        TABLES
          SPAGPA_TAB  = IT_SPAGPA.


    ELSE.

      CALL FUNCTION 'ABAP4_CALL_TRANSACTION' STARTING NEW TASK ''
        EXPORTING
          TCODE       = I_TCODE
          SKIP_SCREEN = I_SKIP_SCREEN
        TABLES
          SPAGPA_TAB  = IT_SPAGPA.


    ENDIF.

  endmethod.


method EXCEL_FILE_CLOSE.

  DATA LV_COUNT TYPE I.


  GET PROPERTY OF WORKBOOKS    'Count' = LV_COUNT.

* quit Excel and free ABAP Object - unfortunately, this does not kill the Excel process
  IF NOT ( WORKBOOK-HANDLE EQ '-1' OR WORKBOOK-HANDLE IS INITIAL ).
    CALL METHOD OF WORKBOOK    'Close' EXPORTING #1 = 0.
  ENDIF.


  GET PROPERTY OF WORKBOOKS    'Count' = LV_COUNT.

  DO LV_COUNT TIMES.
    FREE OBJECT WORKBOOK.     __MESSAGE.
    CALL METHOD OF WORKBOOKS   'Item' = WORKBOOK  EXPORTING #1 = SY-INDEX.
    CALL METHOD OF WORKBOOK    'Close' EXPORTING #1 = 0.
  ENDDO.


  CALL METHOD OF APPLICATION   'Quit'.

  FREE OBJECT WORKSHEET.    __MESSAGE.
  FREE OBJECT WORKBOOK.     __MESSAGE.
  FREE OBJECT WORKBOOKS.    __MESSAGE.
  FREE OBJECT APPLICATION.  __MESSAGE.

endmethod.


  METHOD EXCEL_FILE_INPUT.

    DEFINE __MESSAGE.
      IF SY-SUBRC EQ 0  OR
         SY-MSGID EQ '' OR
         SY-MSGTY EQ ''.
      ELSE.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        CLEAR: SY-MSGID, SY-MSGTY, SY-MSGNO,
               SY-MSGV1, SY-MSGV2, SY-MSGV3, SY-MSGV4.
      ENDIF.
    END-OF-DEFINITION.


    DATA: L_SHEET       TYPE OLE2_OBJECT,
          L_CELL        TYPE OLE2_OBJECT.

    DATA: LV_ROW        TYPE I,
          LV_COL        TYPE I,
          LV_BEGIN_COL  TYPE I.


*   Check paramenter
    IF I_WORKBOOK  IS INITIAL.
      RAISE WORKBOOK_IS_INITIAL.
    ENDIF.

    IF I_WORKSHEET IS INITIAL.
      RAISE WORKSHEET_IS_INITIAL.
    ELSE.
      L_SHEET = I_WORKSHEET.
    ENDIF.

    IF I_BEGIN_ROW EQ 0.
      LV_ROW = 1.
    ELSE.
      LV_ROW = I_BEGIN_ROW.
    ENDIF.

    IF I_BEGIN_COL EQ 0.
      LV_COL = LV_BEGIN_COL = 1.
    ELSE.
      LV_COL = LV_BEGIN_COL = I_BEGIN_COL.
    ENDIF.

    LOOP AT IT_DATA INTO DATA(LS_DATA).

      " Get Cell
      CALL METHOD OF L_SHEET 'CELLS' = L_CELL
           EXPORTING #1 = LV_ROW
                     #2 = LV_COL.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.

      " Update Value of Cell
      SET PROPERTY OF L_CELL 'VALUE' = LS_DATA-VALUE.  " NO FLUSH
      FREE OBJECT     L_CELL. __MESSAGE.
      LV_COL = LV_COL + 1.

      AT END OF ROW.
        LV_ROW = LV_ROW + 1.
        LV_COL = LV_BEGIN_COL.
      ENDAT.
    ENDLOOP.

    CALL METHOD OF I_WORKBOOK 'Save'.

  ENDMETHOD.


METHOD EXCEL_FILE_OPEN.

  DEFINE __MESSAGE.
    CASE SY-SUBRC.
      WHEN 0.
      WHEN 1.
        IF SY-MSGID IS NOT INITIAL AND
           SY-MSGTY IS NOT INITIAL.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

          CLEAR: SY-MSGID,
                 SY-MSGTY,
                 SY-MSGNO,
                 SY-MSGV1,
                 SY-MSGV2,
                 SY-MSGV3,
                 SY-MSGV4.
        ENDIF.
      WHEN OTHERS. RAISE FILE_OPEN.
    ENDCASE.
  END-OF-DEFINITION.


  DATA: L_APPLICATION TYPE OLE2_OBJECT,
        L_WORKBOOKS   TYPE OLE2_OBJECT,
        L_WORKBOOK    TYPE OLE2_OBJECT,
        L_WORKSHEET   TYPE OLE2_OBJECT.

* open file in Excel
  CREATE OBJECT   L_APPLICATION 'Excel.Application'.
  __MESSAGE.
  CALL METHOD OF  L_APPLICATION 'Workbooks'   = L_WORKBOOKS.
  __MESSAGE.
  CALL METHOD OF  L_WORKBOOKS   'Open'        = L_WORKBOOK EXPORTING #1 = I_FILENAME
                                                                     #3 = 1.
  __MESSAGE.
  GET PROPERTY OF L_WORKBOOK    'ActiveSheet' = L_WORKSHEET.
  __MESSAGE.
  " set property of application 'Visible' = 1.

  DATA: L_RANGE   TYPE OLE2_OBJECT,
        L_ROWS    TYPE OLE2_OBJECT,
        L_COLUMNS TYPE OLE2_OBJECT,
        L_MAX_ROW TYPE I,
        L_MAX_COL TYPE I.


  GET PROPERTY OF L_WORKSHEET   'UsedRange' = L_RANGE.
  GET PROPERTY OF L_RANGE:      'Rows'      = L_ROWS,
                                'Columns'   = L_COLUMNS.
  GET PROPERTY OF L_ROWS        'Count'     = L_MAX_ROW.
  GET PROPERTY OF L_COLUMNS     'Count'     = L_MAX_COL.

  E_APPLICATION = L_APPLICATION.
  E_WORKBOOKS   = L_WORKBOOKS.
  E_WORKBOOK    = L_WORKBOOK.
  E_WORKSHEET   = L_WORKSHEET.
  E_MAX_ROW     = L_MAX_ROW.
  E_MAX_COL     = L_MAX_COL.


  FREE OBJECT L_ROWS.
  FREE OBJECT L_COLUMNS.
  FREE OBJECT L_RANGE.

ENDMETHOD.


METHOD EXCEL_FILE_READ.

  DEFINE __MESSAGE.
    IF SY-SUBRC EQ 0  OR
       SY-MSGID EQ '' OR
       SY-MSGTY EQ ''.
    ELSE.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      CLEAR: SY-MSGID, SY-MSGTY, SY-MSGNO,
             SY-MSGV1, SY-MSGV2, SY-MSGV3, SY-MSGV4.
    ENDIF.
  END-OF-DEFINITION.


  DATA: LT_CELL   TYPE TT_CELL.
  DATA: LT_CELL2  TYPE TT_CELL.
  DATA: LT_CLEAR  TYPE TT_CELL.

  DATA: L_SHEET   TYPE OLE2_OBJECT,
        L_CELL_1  TYPE OLE2_OBJECT,
        L_CELL_2  TYPE OLE2_OBJECT,
        L_RANGE   TYPE OLE2_OBJECT.

  DATA: LV_RC      TYPE  I.


* Check paramenter
  IF I_WORKSHEET IS INITIAL.
    RAISE WORKSHEET_IS_INITIAL.
  ELSE.
    L_SHEET = I_WORKSHEET.
  ENDIF.


* mark whole spread sheet
  CALL METHOD OF L_SHEET 'CELLS' = L_CELL_1 EXPORTING #1 = I_ROW_FROM #2 = I_COL.      __MESSAGE.
  CALL METHOD OF L_SHEET 'CELLS' = L_CELL_2 EXPORTING #1 = I_ROW_TO   #2 = I_COL.      __MESSAGE.
  CALL METHOD OF L_SHEET 'RANGE' = L_RANGE  EXPORTING #1 = L_CELL_1   #2 = L_CELL_2.   __MESSAGE.
  CALL METHOD OF L_RANGE 'SELECT'. __MESSAGE.

  DO.
*   copy marked area (whole spread sheet) into Clippboard
    CALL METHOD OF L_RANGE 'COPY'.   __MESSAGE.

*   read clipboard into ABAP
    CALL METHOD CL_GUI_FRONTEND_SERVICES=>CLIPBOARD_IMPORT
      IMPORTING
        DATA                 = LT_CELL
      EXCEPTIONS
        CNTL_ERROR           = 1
        ERROR_NO_GUI         = 2
        NOT_SUPPORTED_BY_GUI = 3
        OTHERS               = 4.

    IF LINES( LT_CELL[] ) EQ I_ROW_TO - I_ROW_FROM + 1.
      EXIT.
    ELSE.
      REFRESH: LT_CELL.
    ENDIF.
  ENDDO.

  ET_CELL[] = LT_CELL[].


  " Clipboard 경고창 무시하기 위해 추가
  CALL METHOD OF L_CELL_1 'COPY'.

* >>>>> Begin of change note 575877
* to kill the Excel process it's necessary to free all used objects
  FREE OBJECT L_RANGE.  __MESSAGE.
  FREE OBJECT L_CELL_1. __MESSAGE.
  FREE OBJECT L_CELL_2. __MESSAGE.
* <<<<< End of change note 575877

* clear clipboard
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>CLIPBOARD_EXPORT
     IMPORTING
        DATA                 = LT_CLEAR
     CHANGING
        RC                   = LV_RC
     EXCEPTIONS
        CNTL_ERROR           = 1
        ERROR_NO_GUI         = 2
        NOT_SUPPORTED_BY_GUI = 3
        OTHERS               = 4.


ENDMETHOD.


METHOD EXCEL_FILE_SHOW.

  DATA: L_APPLICATION TYPE OLE2_OBJECT,
        L_WORKBOOKS   TYPE OLE2_OBJECT,
        L_WORKBOOK    TYPE OLE2_OBJECT,
        L_WORKSHEET   TYPE OLE2_OBJECT.


* Check paramenter
  IF I_APPLICATION IS INITIAL.
    RAISE APPLICATION_IS_INITIAL.
  ELSE.
    L_APPLICATION = I_APPLICATION.
  ENDIF.


  " 화면에 display 여부 (1: Display)
  SET PROPERTY OF L_APPLICATION 'visible' = 1.


 " EXCEL 과 연결된 Object 들 Release
  FREE OBJECT: L_WORKSHEET,
               L_WORKBOOK,
               L_WORKBOOKS,
               L_APPLICATION.

ENDMETHOD.


  METHOD F4_BUKRS.

    DATA LR_KOKRS   TYPE RANGE OF TKA02-KOKRS.

    IF I_KOKRS IS NOT INITIAL.
      LR_KOKRS = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = I_KOKRS ) ).
    ENDIF.

    SELECT A~KOKRS,
           A~BUKRS,
           B~BUTXT,
           A~GSBER,
           B~ORT01,
           B~LAND1,
           B~WAERS,
           B~SPRAS,
           B~KTOPL
      FROM TKA02 AS A INNER JOIN T001 AS B ON B~BUKRS EQ A~BUKRS
     WHERE A~KOKRS IN @LR_KOKRS
     ORDER BY A~KOKRS, A~BUKRS
      INTO TABLE @DATA(LT_DATA).

    R_BUKRS = POPUP_VALUE_REQUEST( I_RETFIELD = 'BUKRS'
                                   I_REPID    = I_REPID
                                   I_DYNNR    = I_DYNNR
                                   IT_VALUE   = LT_DATA ).

  ENDMETHOD.


  method F4_FILE.

**********************************************************************

  DATA : LV_TITLE   TYPE STRING,
         LV_FILTER  TYPE STRING,
*         LV_DIR     TYPE STRING,
         LV_SUBRC   TYPE I,
         LV_ACTION  TYPE I,
         LV_RESULT  TYPE ABAP_BOOL.


  CASE I_FILE_TYPE.
    WHEN C_FILETYPE_EXCEL.
      LV_FILTER = CL_GUI_FRONTEND_SERVICES=>FILETYPE_EXCEL && '|'.
    WHEN C_FILETYPE_TEXT.
      LV_FILTER = CL_GUI_FRONTEND_SERVICES=>FILETYPE_TEXT && '|'.
    WHEN OTHERS.
      CONCATENATE CL_GUI_FRONTEND_SERVICES=>FILETYPE_ALL
                  CL_GUI_FRONTEND_SERVICES=>FILETYPE_EXCEL
                  CL_GUI_FRONTEND_SERVICES=>FILETYPE_WORD
                  '|'
             INTO LV_FILTER.
  ENDCASE.


*  IF I_PATH IS NOT INITIAL.
*
*    LV_DIR = I_PATH.
*
*    CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_EXIST
*      EXPORTING
*        DIRECTORY            = LV_DIR           " Directory name
*      RECEIVING
*        RESULT               = LV_RESULT        " Result
*      EXCEPTIONS
*        CNTL_ERROR           = 1                " Control error
*        ERROR_NO_GUI         = 2                " No GUI available
*        WRONG_PARAMETER      = 3                " Incorrect parameter
*        NOT_SUPPORTED_BY_GUI = 4                " GUI does not support this
*        OTHERS               = 5.
*
*    IF LV_RESULT EQ ABAP_FALSE.
*      CLEAR LV_DIR.
*    ENDIF.
*  ENDIF.


  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      WINDOW_TITLE            = LV_TITLE         " Title Of File Open Dialog
      FILE_FILTER             = LV_FILTER        " File Extension Filter String
      MULTISELECTION          = I_MULTI          " Multiple selections poss.
*      INITIAL_DIRECTORY       = LV_DIR           " Initial Directory
*      WITH_ENCODING           =                  " File Encoding
    CHANGING
      FILE_TABLE              = ET_FILE          " Table Holding Selected Files
      RC                      = LV_SUBRC         " Return Code, Number of Files or -1 If Error Occurred
      USER_ACTION             = LV_ACTION        " User Action (See Class Constants ACTION_OK, ACTION_CANCEL)
*      FILE_ENCODING           =
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1                " "Open File" dialog failed
      CNTL_ERROR              = 2                " Control error
      ERROR_NO_GUI            = 3                " No GUI available
      NOT_SUPPORTED_BY_GUI    = 4                " GUI does not support this
      OTHERS                  = 5.

  CASE LV_ACTION.
    WHEN CL_GUI_FRONTEND_SERVICES=>ACTION_OK.
      MESSAGE '파일이 선택되었습니다.' TYPE 'S'.
      E_FILE = ET_FILE[ 1 ]-FILENAME.
    WHEN CL_GUI_FRONTEND_SERVICES=>ACTION_CANCEL.
      MESSAGE '취소되었습니다.' TYPE 'S' DISPLAY LIKE 'W'.
  ENDCASE.

**********************************************************************


  endmethod.


  method F4_GROUP.

    IF I_CLASS IS INITIAL OR
       I_FIELD IS INITIAL.
      RAISE NO_INPUT_PARAMETER.
    ENDIF.

    CALL FUNCTION 'K_GROUP_SELECT'
      EXPORTING
        CLASS              = I_CLASS          " Class, for cost centers, '0H'
        FIELD_NAME         = I_FIELD          " Data base field name
        KOKRS              = I_KOKRS          " Controlling Area
        SEARCHFLD_INPUT    = ' '              " Is search field ready for input?

*        BUTTONS            = 'X'              " Can group type be selected? 'X'-->
*        CRUSER             = '*'              " Creating user
*        SEARCHFLD          = '    '           " Search field known so far
*        SEARCHFLD_REQUIRED = 'X'              " Search field a required field? 'X'
*        SET                = '*'              " Selected group
*        START_COLUMN       = 10               " Start Column
*        START_ROW          = 5                " Start Line
*        TABLE              = 'CCSS'           " Table Names
*        TYPELIST           = 'BS'             " Defaults for the node type
*        UPDUSER            = '*'              " Change by
*        KTOPL              =                  " Chart of Accounts
*        FIKRS              =                  " Financial Management Area
      IMPORTING
*        CLASS_NAME         =                  " Found object class
        SET_NAME           = R_SETNAME         " Found cost-object group
*        SET_TITLE          =                  " Found object group name
*        TABLE_NAME         =                  " Found table to which the cost-obje
*        SETID              =
      EXCEPTIONS
        NO_SET_PICKED      = 1                " No group found or chosen
        OTHERS             = 2.

  endmethod.


  METHOD F4_KOKRS.

    SELECT KOKRS,
           BEZEI,
           WAERS,
           KTOPL,
           LMONA,
           KOKFI,
           KHINR,
           ERKRS,
           PHINR,
           FIKRS
      FROM TKA01
     WHERE ERKRS = @C_ERKRS
      INTO TABLE @DATA(LT_DATA).

    R_KOKRS = POPUP_VALUE_REQUEST( I_RETFIELD = 'KOKRS'
                                   I_REPID    = I_REPID
                                   I_DYNNR    = I_DYNNR
                                   IT_VALUE   = LT_DATA ).

  ENDMETHOD.


  METHOD F4_KSGRU.

    DATA LV_KOKRS TYPE TKA01-KOKRS.

    IF I_KOKRS IS SUPPLIED.
      LV_KOKRS = I_KOKRS.
    ENDIF.

    R_KSGRU = F4_GROUP( I_CLASS = '0101'
                        I_FIELD = 'KOSTL'
                        I_KOKRS = LV_KOKRS ).


  ENDMETHOD.


  METHOD F4_PDGR.

    DATA LV_KOKRS TYPE TKA01-KOKRS.

    IF I_KOKRS IS SUPPLIED.
      LV_KOKRS = I_KOKRS.
    ENDIF.

    R_PDGR = F4_GROUP( I_CLASS = '0110'
                       I_FIELD = 'POSID'
                       I_KOKRS = LV_KOKRS ).


  ENDMETHOD.


METHOD FILE_DOWNLOAD.

  DATA: L_APPLICATION   TYPE OLE2_OBJECT,
        L_WORKBOOKS     TYPE OLE2_OBJECT,
        L_WORKBOOK      TYPE OLE2_OBJECT,
        L_WORKSHEET     TYPE OLE2_OBJECT.

  DATA: LT_MIME         TYPE TABLE OF W3MIME.
  DATA: LS_WWWDATA      TYPE WWWDATATAB.
  DATA: LV_FILENAME     TYPE STRING,
        LV_PATH         TYPE STRING,
        LV_FULLPATH     TYPE STRING,
        LV_FILESIZE     TYPE W3_QVALUE,
        LV_FILE_EXT     TYPE W3_QVALUE,
        LV_DEFAULT_EXT  TYPE STRING,
        LV_SIZE         TYPE I,
        LV_RESULT       TYPE C,
        LV_ACTION       TYPE I.



*--------------------------------------------------------------------*
* SAP 웹저장소 등록정보 조회
*--------------------------------------------------------------------*

  SELECT SINGLE *
    FROM WWWDATA
   WHERE OBJID EQ @I_OBJID      "SMW0 Object 명
    INTO CORRESPONDING FIELDS OF @LS_WWWDATA.

  IF SY-SUBRC NE 0 .
    SHOW_MESSAGE( I_MESSAGE      = 'SAP 웹저장소에 등록되지 않은 Object 입니다.'
                  I_DISPLAY_TYPE = 'E' ).

    RAISE NOT_EXIST_OBJECT_ID.
  ENDIF.


  " 파일 크기
  CALL FUNCTION 'WWWPARAMS_READ'
    EXPORTING
      RELID            = LS_WWWDATA-RELID
      OBJID            = LS_WWWDATA-OBJID
      NAME             = 'filesize'
    IMPORTING
      VALUE            = LV_FILESIZE
    EXCEPTIONS
      ENTRY_NOT_EXISTS = 1
      OTHERS           = 2.

  LV_SIZE = LV_FILESIZE.

  IF SY-SUBRC NE 0 OR LV_SIZE EQ 0.
    SHOW_MESSAGE( I_MESSAGE      = 'SAP 웹저장소 Object 의 내용이 없습니다.'
                  I_DISPLAY_TYPE = 'E' ).

    RAISE NO_DATA_LENGTH.
  ENDIF.


  " 파일 확장자
  CALL FUNCTION 'WWWPARAMS_READ'
    EXPORTING
      RELID            = LS_WWWDATA-RELID
      OBJID            = LS_WWWDATA-OBJID
      NAME             = 'fileextension'
    IMPORTING
      VALUE            = LV_FILE_EXT
    EXCEPTIONS
      ENTRY_NOT_EXISTS = 1
      OTHERS           = 2.

*--------------------------------------------------------------------*
* 파일명 Rule
* - 웹저장소 Object 명 + 현재날짜 + 현재시간
* - 웹저장소의 확장자로 설정
*--------------------------------------------------------------------*
  CONCATENATE LS_WWWDATA-TEXT SY-DATUM SY-UZEIT
         INTO LV_FILENAME
    SEPARATED BY '_'.

  " 파일확장자 조합
  IF LV_FILE_EXT IS NOT INITIAL.
    CONCATENATE LV_FILENAME LV_FILE_EXT INTO LV_FILENAME.

    IF TO_UPPER( LV_FILE_EXT ) CP '*XLS*'.
      LV_DEFAULT_EXT = 'XLSX'.
    ELSE.
      CLEAR LV_DEFAULT_EXT.
    ENDIF.

  ENDIF.


*--------------------------------------------------------------------*
* 다운로드 저장위치 지정
*--------------------------------------------------------------------*
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
    EXPORTING
*      WINDOW_TITLE              =                  " Window Title
      DEFAULT_EXTENSION         = LV_DEFAULT_EXT   " Default Extension
      DEFAULT_FILE_NAME         = LV_FILENAME      " Default File Name
*      INITIAL_DIRECTORY         =                  " Initial Directory
*      PROMPT_ON_OVERWRITE       = 'X'
    CHANGING
      FILENAME                  = LV_FILENAME      " File Name to Save
      PATH                      = LV_PATH          " Path to File
      FULLPATH                  = LV_FULLPATH      " Path + File Name
      USER_ACTION               = LV_ACTION        " User Action (C Class Const ACTION_OK, ACTION_OVERWRITE etc)
    EXCEPTIONS
      CNTL_ERROR                = 1                " Control error
      ERROR_NO_GUI              = 2                " No GUI available
      NOT_SUPPORTED_BY_GUI      = 3                " GUI does not support this
      INVALID_DEFAULT_FILE_NAME = 4                " Invalid default file name
      OTHERS                    = 5.


  IF LV_ACTION EQ CL_GUI_FRONTEND_SERVICES=>ACTION_CANCEL.
    " 사용자가 파일저장위치를 지정하지 않고 창을 닫아버린 경우
    SHOW_MESSAGE( I_MESSAGE      = '작업이 취소되었습니다'
                  I_DISPLAY_TYPE = 'W' ).
    EXIT.
  ENDIF.


*--------------------------------------------------------------------*
* SAP 웹저장소에서 내려받을 파일내용 수집
*--------------------------------------------------------------------*
  CALL FUNCTION 'WWWDATA_IMPORT'
    EXPORTING
      KEY               = LS_WWWDATA
    TABLES
      MIME              = LT_MIME
    EXCEPTIONS
      WRONG_OBJECT_TYPE = 1
      IMPORT_ERROR      = 2
      OTHERS            = 3.


*--------------------------------------------------------------------*
* Local PC 로 데이터 전송
*--------------------------------------------------------------------*
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      BIN_FILESIZE              = LV_SIZE
      FILENAME                  = LV_FULLPATH
      FILETYPE                  = 'BIN'
    TABLES
      DATA_TAB                  = LT_MIME              " Transfer table
    EXCEPTIONS
      FILE_WRITE_ERROR          = 1                    " Cannot write to file
      NO_BATCH                  = 2                    " Cannot execute front-end function in background
      GUI_REFUSE_FILETRANSFER   = 3                    " Incorrect Front End
      INVALID_TYPE              = 4                    " Invalid value for parameter FILETYPE
      NO_AUTHORITY              = 5                    " No Download Authorization
      UNKNOWN_ERROR             = 6
      HEADER_NOT_ALLOWED        = 7                    " Invalid header
      SEPARATOR_NOT_ALLOWED     = 8                    " Invalid separator
      FILESIZE_NOT_ALLOWED      = 9                    " Invalid file size
      HEADER_TOO_LONG           = 10                   " The header information is limited to 1023 bytes at present
      DP_ERROR_CREATE           = 11                   " Cannot Create Data Provider
      DP_ERROR_SEND             = 12                   " Error Sending Data with DataProvider
      DP_ERROR_WRITE            = 13                   " Error Writing Data with DataProvider
      UNKNOWN_DP_ERROR          = 14                   " Error when calling data provider
      ACCESS_DENIED             = 15                   " Access to file denied.
      DP_OUT_OF_MEMORY          = 16                   " Not enough memory in data provider
      DISK_FULL                 = 17                   " Storage medium is full.
      DP_TIMEOUT                = 18                   " Data provider timeout
      FILE_NOT_FOUND            = 19                   " Could not find file
      DATAPROVIDER_EXCEPTION    = 20                   " General Exception Error in Data Provider
      CONTROL_FLUSH_ERROR       = 21                   " Error in Control Framework
      OTHERS                    = 22.


  IF SY-SUBRC NE 0.
    SHOW_MESSAGE( I_MESSAGE      = '파일 다운로드 중 오류가 발생했습니다'
                  I_DISPLAY_TYPE = 'E' ).
    RAISE FILE_DOWNLOAD_ERROR.
  ENDIF.


  CHECK I_EXECUTE EQ ABAP_ON.


  IF IT_INPUT_DATA[]    IS NOT INITIAL  AND
     I_INPUT_START_ROW GT 0            AND
     I_INPUT_START_COL GT 0.


    " 엑셀 Application 실행 및 엑셀파일 열기
    EXCEL_FILE_OPEN(
      EXPORTING
        I_FILENAME    = CONV #( LV_FULLPATH )
      IMPORTING
        E_APPLICATION = L_APPLICATION
        E_WORKBOOKS   = L_WORKBOOKS
        E_WORKBOOK    = L_WORKBOOK
        E_WORKSHEET   = L_WORKSHEET
      EXCEPTIONS
        FILE_OPEN     = 1
        OTHERS        = 2 ).

    " 엑셀 데이터 입력
    EXCEL_FILE_INPUT(
      EXPORTING
        I_WORKBOOK      = L_WORKBOOK
        I_WORKSHEET     = L_WORKSHEET
        I_BEGIN_ROW     = I_INPUT_START_ROW
        I_BEGIN_COL     = I_INPUT_START_COL
        IT_DATA         = IT_INPUT_DATA
      EXCEPTIONS
        WORKSHEET_IS_INITIAL = 1
        OTHERS               = 2 ).

    EXCEL_FILE_SHOW(
      EXPORTING
        I_APPLICATION          = L_APPLICATION
        I_WORKBOOKS            = L_WORKBOOKS
        I_WORKBOOK             = L_WORKBOOK
        I_WORKSHEET            = L_WORKSHEET
      EXCEPTIONS
        APPLICATION_IS_INITIAL = 1
        OTHERS                 = 2 ).

  ELSE.


    " 다운로드한 파일 열기
    CALL METHOD CL_GUI_FRONTEND_SERVICES=>EXECUTE
      EXPORTING
        DOCUMENT               = LV_FULLPATH       " Path+Name to Document
      EXCEPTIONS
        CNTL_ERROR             = 1                " Control error
        ERROR_NO_GUI           = 2                " No GUI available
        BAD_PARAMETER          = 3                " Incorrect parameter combination
        FILE_NOT_FOUND         = 4                " File not found
        PATH_NOT_FOUND         = 5                " Path not found
        FILE_EXTENSION_UNKNOWN = 6                " Could not find application for specified extension
        ERROR_EXECUTE_FAILED   = 7                " Could not execute application or document
        SYNCHRONOUS_FAILED     = 8                " Cannot Call Application Synchronously
        NOT_SUPPORTED_BY_GUI   = 9                " GUI does not support this
        OTHERS                 = 10.


  ENDIF.


ENDMETHOD.


  METHOD GET_CONTAINER_01.

    DATA LR_PARENT  TYPE REF TO CL_GUI_CONTAINER.
    DATA LR_SPLIT   TYPE REF TO CL_GUI_SPLITTER_CONTAINER.
    DATA LR_CON1    TYPE REF TO CL_GUI_CONTAINER.
    DATA LR_CON2    TYPE REF TO CL_GUI_CONTAINER.

*    LR_PARENT = CL_GUI_CONTAINER=>DEFAULT_SCREEN.
    LR_PARENT = CL_GUI_CONTAINER=>SCREEN0.
*    LR_PARENT = CL_GUI_CONTAINER=>DESKTOP.

    CREATE OBJECT LR_SPLIT
      EXPORTING
        ROWS                    = 2                  " Number of Rows to be displayed
        COLUMNS                 = 1                  " Number of Columns to be Displayed
        PARENT                  = LR_PARENT          " Parent Container
      EXCEPTIONS
        CNTL_ERROR              = 1                  " See Superclass
        CNTL_SYSTEM_ERROR       = 2                  " See Superclass
        OTHERS                  = 3.

    LR_CON1 = LR_SPLIT->GET_CONTAINER( ROW = 1 COLUMN = 1 ).
    LR_CON2 = LR_SPLIT->GET_CONTAINER( ROW = 2 COLUMN = 1 ).

    LR_SPLIT->SET_ROW_MODE(
      EXPORTING
        MODE              = LR_SPLIT->MODE_ABSOLUTE " Row Mode
      EXCEPTIONS
        CNTL_ERROR        = 1
        CNTL_SYSTEM_ERROR = 2
        OTHERS            = 3
    ).

    LR_SPLIT->SET_ROW_HEIGHT(
      EXPORTING
        ID                = 1                " Row ID
        HEIGHT            = 100              " Height
      EXCEPTIONS
        CNTL_ERROR        = 1                " See CL_GUI_CONTROL
        CNTL_SYSTEM_ERROR = 2                " See CL_GUI_CONTROL
        OTHERS            = 3
    ).

    ER_SPLIT     = LR_SPLIT.
    ER_CON_TOP   = LR_CON1.
    ER_CON_MAIN  = LR_CON2.

  ENDMETHOD.


  method GET_DEFAULT_BUKRS.

* 1. Sap Memory 기준
* 2. User Parameter 기준

    CLEAR R_BUKRS.

    GET PARAMETER ID 'BUK' FIELD R_BUKRS.

    IF R_BUKRS IS INITIAL.

      CALL FUNCTION 'TPM_THX_USER_PARAMETER_GET'
        EXPORTING
          I_PARAM_ID    = 'BUK'
        IMPORTING
          E_PARAM_VALUE = R_BUKRS.

    ENDIF.

  endmethod.


  method GET_DEFAULT_KOKRS.

* 1. Sap Memory 기준
* 2. User Parameter 기준
* 3. 회사코드와 Assigned 된 관리회계영역

    CLEAR R_KOKRS.

    GET PARAMETER ID 'CAC' FIELD R_KOKRS.

    IF R_KOKRS IS INITIAL.

      CALL FUNCTION 'TPM_THX_USER_PARAMETER_GET'
        EXPORTING
          I_PARAM_ID    = 'CAC'
        IMPORTING
          E_PARAM_VALUE = R_KOKRS.

      IF R_KOKRS IS INITIAL.

        DATA(LV_BUKRS) = GET_DEFAULT_BUKRS( ).

        SELECT SINGLE KOKRS
          FROM TKA02
         WHERE BUKRS EQ @LV_BUKRS
          INTO @R_KOKRS.

      ENDIF.

    ENDIF.

  endmethod.


  method GET_DYNP_VALUE.

  CALL FUNCTION 'GET_DYNP_VALUE'
    EXPORTING
      I_FIELD       = I_FIELD
      I_REPID       = I_REPID
      I_DYNNR       = I_DYNNR
    CHANGING
      O_VALUE       = E_VALUE.

  endmethod.


METHOD GET_EXCEL_CONTENTS.
**********************************************************************
* 개발자 : 정훈영
*
* 주어진 범위로 엑셀파일의 내용을 가져와 조회한다.
* 기존 Standard Function 의 경우 엑셀 셀값에 Tab 문자가 있을 경우
* 열 구분이 제대로 이뤄지지 않는 문제가 존재하였으나,
* 본 방식에서는 특정 라인에 대해서 열단위로 내용을 가져와
* 결과 테이블에 적재하므로, 해당 문제가 발생하지 않는다.
**********************************************************************



**********************************************************************
* Definition
**********************************************************************
  CONSTANTS C_MAX_ROW TYPE I VALUE 5000.  " 한번씩 조회할 최대 라인수


* DATA  LT_INTERN     TYPE TABLE OF ALSMEX_TABLINE.
  DATA  LT_INTERN     TYPE TT_EXCEL_TABLINE.
  DATA  LS_INTERN     TYPE TS_EXCEL_TABLINE.
  DATA  LT_CELL       TYPE TT_CELL.

  DATA: LV_BEG_ROW    TYPE I,  " 조회시작행
        LV_END_ROW    TYPE I,  " 조회종료행
        LV_END_COL    TYPE I,
        LV_COL        TYPE I.  " 조회대상열


  DATA: LV_MAX_ROW    TYPE I,
        LV_MAX_COL    TYPE I.

  DATA: L_APPLICATION TYPE OLE2_OBJECT,
        L_WORKBOOKS   TYPE OLE2_OBJECT,
        L_WORKBOOK    TYPE OLE2_OBJECT,
        L_WORKSHEET   TYPE OLE2_OBJECT.

  DATA  LV_LINE       TYPE STRING.


**********************************************************************
* Check Parameters
**********************************************************************
  IF I_FILENAME IS INITIAL.
    RAISE NO_INPUT_EXCEL_FILE.
  ENDIF.

  IF I_BEGIN_ROW > I_END_ROW AND I_END_ROW NE 0.
    RAISE INCONSISTENT_PARAMETERS.
  ENDIF.

  IF I_BEGIN_COL > I_END_COL.
    RAISE INCONSISTENT_PARAMETERS.
  ENDIF.

**********************************************************************
* Main Routine
* CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
**********************************************************************

**********************************************************************
* 엑셀 Application 실행 및 엑셀파일 열기
**********************************************************************
  EXCEL_FILE_OPEN(
    EXPORTING
      I_FILENAME    = I_FILENAME  " Excel 파일명(경로포함)
    IMPORTING
      E_APPLICATION = L_APPLICATION
      E_WORKBOOKS   = L_WORKBOOKS
      E_WORKBOOK    = L_WORKBOOK
      E_WORKSHEET   = L_WORKSHEET
      E_MAX_ROW     = LV_MAX_ROW
      E_MAX_COL     = LV_MAX_COL
    EXCEPTIONS
      FILE_OPEN     = 1
      OTHERS        = 2
  ).

**********************************************************************
* Setup
**********************************************************************
  IF I_BEGIN_ROW > LV_MAX_ROW OR I_BEGIN_COL > LV_MAX_COL.
    " 조회시작 위치가 이미 Excel 사용범위를 벗어나면 Method 종료
    EXCEL_FILE_CLOSE(
      CHANGING
        APPLICATION = L_APPLICATION
        WORKBOOKS   = L_WORKBOOKS
        WORKBOOK    = L_WORKBOOK
        WORKSHEET   = L_WORKSHEET
    ).
    RETURN.
  ENDIF.

  LV_BEG_ROW = I_BEGIN_ROW.
  LV_END_ROW = I_BEGIN_ROW + C_MAX_ROW - 1.
  LV_END_COL = I_END_COL.

  " 조회기준 단위보다 작은 종료행번호
  IF LV_END_ROW > I_END_ROW AND I_END_ROW NE 0.
    LV_END_ROW = I_END_ROW.
  ENDIF.

  " 엑셀에서 사용중인 범위보다 큰 범위면 엑셀 사용범위로 제한
  IF LV_END_ROW > LV_MAX_ROW. LV_END_ROW = LV_MAX_ROW. ENDIF.
  IF LV_END_COL > LV_MAX_COL. LV_END_COL = LV_MAX_COL. ENDIF.



  DO.

**********************************************************************
* 엑셀시트에서 특정 범위를 열단위로 읽어온다.
* 엑셀 셀안에 구분자(Tab문자)가 있는 경우를 해결하기 위함.
**********************************************************************

    REFRESH LT_INTERN.

    LV_COL = I_BEGIN_COL.

    WHILE LV_END_COL >= LV_COL.

      REFRESH LT_CELL.

      EXCEL_FILE_READ(
        EXPORTING
          I_WORKSHEET          = L_WORKSHEET  " Excel Sheet
          I_ROW_FROM           = LV_BEG_ROW   " 조회범위 시작 행번호
          I_ROW_TO             = LV_END_ROW   " 조회범위 종료 행번호
          I_COL                = LV_COL       " 조회대상 열
        IMPORTING
          ET_CELL              = LT_CELL      " Excel 범위 데이터
        EXCEPTIONS
          CLIPBOARD_IMPORT     = 1
          WORKSHEET_IS_INITIAL = 2
          OTHERS               = 3
      ).

      LOOP AT LT_CELL INTO DATA(LS_CELL).
        AT FIRST.
          LS_INTERN-ROW = LV_BEG_ROW - 1.
        ENDAT.
        LS_INTERN-ROW   = LS_INTERN-ROW + 1.
        LS_INTERN-COL   = LV_COL.
        LS_INTERN-VALUE = LS_CELL-VALUE.
        APPEND LS_INTERN TO LT_INTERN.
      ENDLOOP.

      LV_COL = LV_COL + 1.
    ENDWHILE.

    SORT LT_INTERN BY ROW COL.
    APPEND LINES OF LT_INTERN TO ET_INTERN.


    IF LV_END_ROW EQ LV_MAX_ROW.
      " 최대라인까지 조회하면 더이상 반복하지 않는다.
      EXIT.
    ELSE.

      " 지정한 범위까지 조회가 되었는지 점검
      READ TABLE LT_INTERN TRANSPORTING NO FIELDS
                           WITH KEY ROW = LV_END_ROW BINARY SEARCH.
      IF SY-SUBRC EQ 0.

        " 지정한 범위까지 조회된 경우 내용이 있는지 점검한다.
        CLEAR LV_LINE.

        LOOP AT LT_INTERN INTO LS_INTERN FROM SY-TABIX.
          IF LS_INTERN-ROW NE LV_END_ROW.
            EXIT.
          ENDIF.
          LV_LINE = LV_LINE && LS_INTERN-VALUE.
        ENDLOOP.

        IF LV_LINE IS INITIAL.
          " 지정한 범위까지 조회는 되었으나, 내용이 없다면 반복 중단.
          EXIT.
        ELSE.

          " 지정한 범위까지 조회는 되고 내용이 있다면 반복해서 내용 조회

          LV_BEG_ROW = LV_END_ROW + 1.
          LV_END_ROW = LV_END_ROW + C_MAX_ROW.

          " 최대 범위를 벗어나지 않도록 조정한다.
          IF LV_END_ROW > LV_MAX_ROW.
            LV_END_ROW = LV_MAX_ROW.
          ENDIF.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDIF.
  ENDDO.

  EXCEL_FILE_CLOSE(
    CHANGING
      APPLICATION = L_APPLICATION
      WORKBOOKS   = L_WORKBOOKS
      WORKBOOK    = L_WORKBOOK
      WORKSHEET   = L_WORKSHEET
  ).

ENDMETHOD.


  METHOD GET_HIERARCHY_LIST.

    DATA: LV_CLASS        TYPE SETHIER-SETCLASS,
          LV_SETID        TYPE SETHIER-SETID,
          LV_KOKRS        TYPE SETHIER-KOKRS,
          LV_TABLE        TYPE SETHIER-TABNAME,
          LV_FIELD        TYPE SETHIER-FIELDNAME.

    DATA: LT_NODES       TYPE GSETH_NODE_TAB,
          LT_VALUES      TYPE GSETH_VAL_TAB,
          LT_FIELD_INFO  TYPE GSETH_INFO_TAB.

    DATA: LS_INFO        TYPE GRPHINFO.
    DATA: LV_OVERWRITE   TYPE SYDATAR.


    IF I_TABLE IS SUPPLIED AND I_TABLE IS NOT INITIAL.
      CASE I_TABLE.
        WHEN 'CSKS'.    " 코스트 센터 그룹
          LV_FIELD = 'KSGRU'.
          LV_CLASS = '0101'.
        WHEN 'CSKA'.    " 원가 요소 그룹
          LV_FIELD = 'KAGRU'.
          LV_CLASS = '0102'.
        WHEN 'AUFK'.    " 오더 그룹
          LV_FIELD = 'AUFGR'.
          LV_CLASS = '0103'.
        WHEN 'TKA03'.   " 통계 주요 지표 그룹
          LV_FIELD = 'STGRU'.
          LV_CLASS = '0104'.
        WHEN 'CSLA'.    " 액티비티 유형 그룹
          LV_FIELD = 'LAGRU'.
          LV_CLASS = '0105'.
        WHEN 'CEPC'.    " 손익 센터 그룹
          LV_FIELD = 'NAME_COALL'.
          LV_CLASS = '0106'.
        WHEN 'SKA1'.    " 계정 그룹 계정
          LV_FIELD = 'NAME_COALL'.
          LV_CLASS = '0109'.
        WHEN 'PRPS'.    " WBS 요소 그룹
          LV_FIELD = 'POSIDGR'.
          LV_CLASS = '0110'.
        WHEN 'FMFCTR'.  " 펀드 그룹
          LV_FIELD = 'NAME_COALL'.
          LV_CLASS = '0111'.
        WHEN 'TFKB'.    " 기능 영역 그룹
          LV_FIELD = 'NAME_COALL'.
          LV_CLASS = '0112'.
      ENDCASE.
    ENDIF.


    IF I_CLASS IS INITIAL AND LV_CLASS IS INITIAL.
      RAISE NO_INPUT_CLASS.
    ELSE.
      LV_CLASS = I_CLASS.
    ENDIF.


    CASE LV_CLASS.
      WHEN '0101'.  " 코스트 센터 그룹
        LV_SETID = LV_CLASS && I_KOKRS && I_SETNAME.
      WHEN '0102'.  " 원가 요소 그룹
        SELECT SINGLE KTOPL
          FROM TKA01
         WHERE KOKRS EQ @I_KOKRS
          INTO @DATA(LV_KTOPL).
        LV_SETID = LV_CLASS && LV_KTOPL && I_SETNAME.
      WHEN '0103'.  " 오더 그룹
        LV_SETID = LV_CLASS && I_SETNAME.
      WHEN '0104'.  " 통계 주요 지표 그룹
        LV_SETID = LV_CLASS && I_KOKRS && I_SETNAME.
      WHEN '0105'.  " 액티비티 유형 그룹
        LV_SETID = LV_CLASS && I_KOKRS && I_SETNAME.
      WHEN '0106'.  " 손익 센터 그룹
        LV_SETID = LV_CLASS && I_KOKRS && I_SETNAME.
      WHEN '0109'.  " 계정 그룹 계정
        SELECT SINGLE KTOPL
          FROM TKA01
         WHERE KOKRS EQ @I_KOKRS
          INTO @LV_KTOPL.
        LV_SETID = LV_CLASS && LV_KTOPL && I_SETNAME.
      WHEN '0110'.  " WBS 요소 그룹
        LV_SETID = LV_CLASS && I_SETNAME.
      WHEN '0111'.  " 펀드 그룹
        SELECT SINGLE FIKRS
          FROM TKA01
         WHERE KOKRS EQ @I_KOKRS
          INTO @DATA(LV_FIKRS).
        LV_SETID = LV_CLASS && LV_FIKRS && I_SETNAME.
      WHEN '0112'.  " 기능 영역 그룹
        LV_SETID = LV_CLASS && I_SETNAME.

    ENDCASE.



    CALL FUNCTION 'K_HIERARCHY_TABLES_READ'
      EXPORTING
        E_CLASS                     = LV_CLASS         " Set Class
        E_SETID                     = LV_SETID
        E_KOKRS                     = I_KOKRS          " Controlling Area
*        E_MANDT                     =                  " Client
*        E_MASTER_DATA               =
*        E_STRUCTURE                 =
*        E_REPLACE_CLASS             =
*        E_REPLACE_UNIT              =
*        E_REPLACE_TABLE             = SPACE
*        E_REPLACE_FIELD             = SPACE
*        E_SUFFIX                    =
*        E_OLD_LINE_LEVEL            = 0
*        E_CO_REN_FLG                =
*      IMPORTING
*        I_DOUBLE_CHECK              =
*        I_MASTER_DATA               =
      TABLES
        T_NODES                     = LT_NODES   " Hierarchy Node
        T_VALUES                    = LT_VALUES
      CHANGING
        C_INFO                      = LS_INFO
        C_OVERWRITE                 = LV_OVERWRITE
      EXCEPTIONS
        NO_CONTROLLING_AREA         = 1                " Controlling area does not exist
        NO_CHART_OF_ACCOUNT         = 2
        DIFFERENT_CONTROLLING_AREAS = 3
        DIFFERENT_CHART_OF_ACCOUNTS = 4
        SET_NOT_FOUND               = 5
        ILLEGAL_FIELD_REPLACEMENT   = 6                " Invalid Change of Reference Field
        ILLEGAL_TABLE_REPLACEMENT   = 7                " Invalid Change of Reference Table
        FM_RAISE                    = 8
        CONVERT_ERROR               = 9
        NO_OVERWRITE_STANDARD_HIER  = 10
        NO_BUKRS_FOR_KOKRS          = 11
        OTHERS                      = 12.

    IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    IF ET_NODES IS REQUESTED.
      ET_NODES  = LT_NODES.
    ENDIF.

    IF ET_VALUES IS REQUESTED.
      ET_VALUES = LT_VALUES.
    ENDIF.


  ENDMETHOD.


  METHOD GET_REPORT_PROGNAME.

    SELECT SINGLE UNI_IDC25 FROM T803VP
      INTO @DATA(LV_IDC25)
      WHERE RGJNR     = @I_RGJNR       " '1SIP'
        AND VERSN     = @I_VERSN       " '00000001'
        AND PROGCLASS = @I_PROGCLASS.  " 'RW_EXECUTION'.

    IF SY-SUBRC NE 0.
      RAISE REPORT_PROGRAM_NOT_FOUND.
    ENDIF.

    " GP && 4QLQKH46VZJ3P1A40A23S3HJG && 100
    E_PROGNAME = 'GP' && LV_IDC25 && SY-MANDT.

    SELECT COUNT(*)
      FROM TRDIR
     WHERE NAME = @E_PROGNAME.

    IF SY-SUBRC NE 0.
      CLEAR E_PROGNAME.
      RAISE REPORT_PROGRAM_NOT_FOUND.
    ENDIF.


  ENDMETHOD.


  method GET_TEXT_BUKRS.

    CLEAR R_BUTXT.

    SELECT SINGLE BUTXT
      FROM T001
     WHERE BUKRS EQ @I_BUKRS
      INTO @R_BUTXT.

  endmethod.


  method GET_TEXT_KOKRS.

    CLEAR R_BEZEI.

    SELECT SINGLE BEZEI
      FROM TKA01
     WHERE KOKRS EQ @I_KOKRS
      INTO @R_BEZEI.

  endmethod.


  METHOD POPUP_CONFIRM.

*"     VALUE(TITLEBAR) DEFAULT SPACE
*"     VALUE(DIAGNOSE_OBJECT) LIKE  DOKHL-OBJECT DEFAULT SPACE
*"     VALUE(TEXT_QUESTION)
*"     VALUE(TEXT_BUTTON_1) DEFAULT 'Ja'(001)
*"     VALUE(ICON_BUTTON_1) LIKE  ICON-NAME DEFAULT SPACE
*"     VALUE(TEXT_BUTTON_2) DEFAULT 'Nein'(002)
*"     VALUE(ICON_BUTTON_2) LIKE  ICON-NAME DEFAULT SPACE
*"     VALUE(DEFAULT_BUTTON) DEFAULT '1'
*"     VALUE(DISPLAY_CANCEL_BUTTON) DEFAULT 'X'
*"     VALUE(USERDEFINED_F1_HELP) LIKE  DOKHL-OBJECT DEFAULT SPACE
*"     VALUE(START_COLUMN) LIKE  SY-CUCOL DEFAULT 25
*"     VALUE(START_ROW) LIKE  SY-CUROW DEFAULT 6
*"     VALUE(POPUP_TYPE) LIKE  ICON-NAME OPTIONAL
*"     REFERENCE(IV_QUICKINFO_BUTTON_1) TYPE  TEXT132 DEFAULT SPACE
*"     REFERENCE(IV_QUICKINFO_BUTTON_2) TYPE  TEXT132 DEFAULT SPACE

    DATA: LV_TITLEBAR TYPE TEXT100,
          LV_QUESTION TYPE TEXT200,
          LV_CANCEL   TYPE C,
          LV_ANSWER   TYPE C.

    LV_TITLEBAR        = I_TITLEBAR.
    LV_CANCEL          = I_CANCEL.

*    LV_QUESTION+00 = I_TEXT1.
*    LV_QUESTION+48 = I_TEXT2.

    LV_QUESTION = I_QUESTION.


    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        POPUP_TYPE                  = I_POPUP_TYPE
        TITLEBAR                    = LV_TITLEBAR
        TEXT_QUESTION               = LV_QUESTION
        DISPLAY_CANCEL_BUTTON       = LV_CANCEL
*       DEFAULT_BUTTON              = '1'
*       TEXT_BUTTON_1               = 'Ja'(001)
*       ICON_BUTTON_1               = ' '
*       TEXT_BUTTON_2               = 'Nein'(002)
*       ICON_BUTTON_2               = ' '
     IMPORTING
       ANSWER                      = LV_ANSWER
     EXCEPTIONS
       TEXT_NOT_FOUND              = 1
       OTHERS                      = 2.

    CLEAR R_ANSWER.
    CHECK SY-SUBRC EQ 0.
    IF LV_ANSWER EQ '1'.
      R_ANSWER = ABAP_TRUE.
    ELSE.
      R_ANSWER = ABAP_FALSE.
    ENDIF.

  ENDMETHOD.


  method POPUP_VALUE_REQUEST.

    DATA LT_RETURN TYPE TABLE OF DDSHRETVAL.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        RETFIELD         = I_RETFIELD   " Name of return field in FIELD_TAB
        DYNPPROG         = I_REPID      " Current program
        DYNPNR           = I_DYNNR      " Screen number
        DYNPROFIELD      = I_FIELD       " Name of screen field for value return
        VALUE_ORG        = 'S'          " Value return: C: cell by cell, S: structured

*        DDIC_STRUCTURE   = SPACE       " Structure of VALUE_TAB (VALUE_ORG = 'S')
*        PVALKEY          = SPACE       " Key for personal help
*        STEPL            = 0           " Steploop line of screen field
*        WINDOW_TITLE     =             " Title for the hit list
*        VALUE            = SPACE       " Field contents for F4 call
*        MULTIPLE_CHOICE  = SPACE       " Switch on multiple selection
*        DISPLAY          = SPACE       " Override readiness for input
*        CALLBACK_PROGRAM = SPACE       " Program for callback before F4 start
*        CALLBACK_FORM    = SPACE       " Form for callback before F4 start (-> long docu)
*        CALLBACK_METHOD  =             " Interface for Callback Routines
*        MARK_TAB         =             " Defaults for Selected Lines when Multiple Selection is Switched On
*      IMPORTING
*        USER_RESET       =             " Single-Character Flag

      TABLES
        VALUE_TAB        = IT_VALUE     " Table of values: entries cell by cell
*        FIELD_TAB        =             " Fields of the hit list
        RETURN_TAB       = LT_RETURN    " Return the selected value
*        DYNPFLD_MAPPING  =             " Assignment of the screen fields to the internal table
      EXCEPTIONS
        PARAMETER_ERROR  = 1            " Incorrect parameter
        NO_VALUES_FOUND  = 2            " No values found
        OTHERS           = 3.

    CHECK SY-SUBRC EQ 0 AND LT_RETURN[] IS NOT INITIAL.

    IF ET_RETURN IS SUPPLIED.
      ET_RETURN = LT_RETURN.
    ENDIF.

    R_FIELDVAL = LT_RETURN[ 1 ]-FIELDVAL.


  endmethod.


  method SET_DB_PROFILE.

*  000000000001   표준선택 (구조)
*  000000000002   표준선택 (손익센터)
*  000000000003   표준선택 (코스트센터 계층구조)
*  000000000004   표준선택 (요약)
*  000000000005   표준선택 (자본투자프로그램)
*  000000000006   표준선택 (판매뷰)
*  000000000007   표준선택 (표준구조)
*  SAPMEBPMPS     PMPS의 MEB 호출에 대한 프로젝트 선택
*  SAPMPP000000   표준노동력 계획프로파일
*  SAPPS_EV0001   표준소득분석선택

    SELECT COUNT(*)
      FROM TCNT
     WHERE PROFID EQ @I_PROFID.
    IF SY-SUBRC EQ 0.
      SET PARAMETER ID 'PDB' FIELD I_PROFID.
    ELSE.
      RAISE NOT_EXIST_PROFID.
    ENDIF.


  endmethod.


  method SET_DYNP_VALUE.

    DATA LV_VALUE TYPE DYNPREAD-FIELDVALUE.

    LV_VALUE = I_VALUE.
    CONDENSE LV_VALUE.

    CALL FUNCTION 'SET_DYNP_VALUE'
      EXPORTING
        I_FIELD = I_FIELD
        I_REPID = I_REPID
        I_DYNNR = I_DYNNR
        I_VALUE = LV_VALUE.

  endmethod.


  method SET_KOKRS.

    CALL FUNCTION 'K_KOKRS_SET'
      EXPORTING
*        DISPLAY   = SPACE            " Display Mode
        I_KOKRS   = I_KOKRS            " Vorschlag Kostenrechnungskreis
        POPUP     = I_POPUP            " Steuerung für Popup
*      IMPORTING
*        E_KOKRS   =                  " Controlling Area
*        E_ACTION  =                  " Gedrückte Taste auf Popup
      EXCEPTIONS
        NOT_FOUND = 1                " Kostenrechnungskreis nicht definiert
        OTHERS    = 2
      .
    IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  endmethod.


  method SHOW_MESSAGE.

    DATA LV_MESSAGE_TYPE TYPE C.
    DATA LV_DISPLAY_TYPE TYPE C.

    LV_MESSAGE_TYPE = I_MESSAGE_TYPE.
    LV_DISPLAY_TYPE = I_DISPLAY_TYPE.


    CASE LV_MESSAGE_TYPE.
      WHEN 'S'.
      WHEN 'W'.
      WHEN 'E'.
      WHEN 'A'.
      WHEN 'I'.
      WHEN 'X'.
      WHEN OTHERS.
        "??
        LV_MESSAGE_TYPE = 'E'.
    ENDCASE.

    CASE LV_DISPLAY_TYPE.
      WHEN 'S'.
      WHEN 'W'.
      WHEN 'E'.
      WHEN 'A'.
      WHEN 'I'.
      WHEN 'X'.
      WHEN OTHERS.
        "??
        CLEAR LV_DISPLAY_TYPE.
    ENDCASE.

    IF LV_MESSAGE_TYPE EQ LV_DISPLAY_TYPE.
      CLEAR LV_DISPLAY_TYPE.
    ENDIF.

    IF LV_DISPLAY_TYPE IS INITIAL.
      MESSAGE I_MESSAGE
         TYPE LV_MESSAGE_TYPE.
    ELSE.
      MESSAGE I_MESSAGE
         TYPE LV_MESSAGE_TYPE
      DISPLAY LIKE LV_DISPLAY_TYPE.

    ENDIF.

  endmethod.
ENDCLASS.
