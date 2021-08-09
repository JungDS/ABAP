class ZCL_CO_COMMON definition
  public
  create public .

public section.

  class-methods SET_KOKRS
    importing
      value(I_KOKRS) type TKA01-KOKRS .
  class-methods GET_DYNP_VALUE
    importing
      !I_FIELD type DYNPREAD-FIELDNAME
      !I_REPID type SY-REPID default SY-CPROG
      !I_DYNNR type D020S-DNUM default SY-DYNNR
    returning
      value(E_VALUE) type STRING .
  class-methods FILE_OPEN_DIALOG
    importing
      !I_PATH type LOCALFILE default 'C:\'
      !I_MULTI type ABAP_BOOL optional
    exporting
      !ET_FILE type FILETABLE
    returning
      value(E_FILE) type LOCALFILE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CO_COMMON IMPLEMENTATION.


  method FILE_OPEN_DIALOG.

**********************************************************************

  DATA : LV_TITLE   TYPE STRING,
         LV_FILTER  TYPE STRING,
         LV_DIR     TYPE STRING,
         LV_SUBRC   TYPE I,
         LV_ACTION  TYPE I,
         LV_RESULT  TYPE ABAP_BOOL.

  IF I_PATH IS NOT INITIAL.

    LV_DIR = I_PATH.

    CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_EXIST
      EXPORTING
        DIRECTORY            = LV_DIR           " Directory name
      RECEIVING
        RESULT               = LV_RESULT        " Result
      EXCEPTIONS
        CNTL_ERROR           = 1                " Control error
        ERROR_NO_GUI         = 2                " No GUI available
        WRONG_PARAMETER      = 3                " Incorrect parameter
        NOT_SUPPORTED_BY_GUI = 4                " GUI does not support this
        OTHERS               = 5.

    IF LV_RESULT EQ ABAP_FALSE.
      CLEAR LV_DIR.
    ENDIF.
  ENDIF.


  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      WINDOW_TITLE            = LV_TITLE         " Title Of File Open Dialog
      FILE_FILTER             = LV_FILTER        " File Extension Filter String
      MULTISELECTION          = I_MULTI          " Multiple selections poss.
      INITIAL_DIRECTORY       = LV_DIR           " Initial Directory
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


  method GET_DYNP_VALUE.

  CALL FUNCTION 'GET_DYNP_VALUE'
    EXPORTING
      I_FIELD       = I_FIELD
      I_REPID       = I_REPID
      I_DYNNR       = I_DYNNR
    CHANGING
      O_VALUE       = E_VALUE.

  endmethod.


  method SET_KOKRS.

    CALL FUNCTION 'K_KOKRS_SET'
      EXPORTING
*        DISPLAY   = SPACE            " Display Mode
        I_KOKRS   = I_KOKRS            " Vorschlag Kostenrechnungskreis
*        POPUP     = SPACE            " Steuerung für Popup
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
ENDCLASS.
