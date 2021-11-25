class ZCL_CO_ALV definition
  public
  create public .

public section.

  data MR_CONTAINER type ref to CL_GUI_CONTAINER .
  data MR_ALV_GRID type ref to CL_GUI_ALV_GRID .
  data MS_LAYOUT type LVC_S_LAYO .
  data MT_SORT type LVC_T_SORT .
  data MT_FIELDCAT type LVC_T_FCAT .
  data MT_EXCLUDED_TOOLBAR type UI_FUNCTIONS .
  data MT_ROWS type LVC_T_ROID .
  data MS_VARIANT type DISVARIANT .
  data MV_SAVE type CHAR01 .
  data MT_OUTTAB type ref to DATA .

  methods CONSTRUCTOR
    importing
      !I_CONTAINER type ref to CL_GUI_CONTAINER optional .
  methods DISPLAY
    changing
      !T_OUTTAB type STANDARD TABLE .
  methods SET_FIELD_CATALOG
    importing
      !I_REPID type SY-REPID default SY-CPROG
      !I_TABNAME type TABNAME optional
      !I_STRUCTURE type DD02L-TABNAME optional
    exceptions
      INVALID_INPUT_PARAMETER
      EMPTY_FIELD_CATALOG .
  methods SET_LAYOUT
    importing
      !I_LAYOUT type LVC_S_LAYO optional
      !I_TYPE type CHAR01 optional
      !I_STYLEFNAME type LVC_S_LAYO-STYLEFNAME optional
      !I_BOX_FNAME type LVC_S_LAYO-BOX_FNAME optional
      !I_INFO_FNAME type LVC_S_LAYO-INFO_FNAME optional
      !I_CTAB_FNAME type LVC_S_LAYO-CTAB_FNAME optional .
  methods SET_EXCLUDE_TOOLBAR
    importing
      !IT_TOOLBAR type UI_FUNCTIONS .
  methods SET_SORT
    importing
      !IT_FIELD type FIELDNAME_TAB .
  methods REFRESH
    importing
      !I_STABLE_ROW type LVC_S_STBL-ROW default 'X'
      !I_STABLE_COL type LVC_S_STBL-COL default 'X'
      !I_SOFT_REFRESH type CHAR01 optional .
  methods GET_SELECTED_ROWS
    exporting
      !ET_ROWS type LVC_T_ROID
    returning
      value(R_LINES) type I .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CO_ALV IMPLEMENTATION.


  method CONSTRUCTOR.

    IF I_CONTAINER IS SUPPLIED.
      ME->MR_CONTAINER = I_CONTAINER.
    ENDIF.


    CREATE OBJECT ME->MR_ALV_GRID
      EXPORTING
*        I_SHELLSTYLE            = 0                " Control Style
*        I_LIFETIME              =                  " Lifetime
        I_PARENT                = ME->MR_CONTAINER  " Parent Container
*        I_APPL_EVENTS           = SPACE            " Register Events as Application Events
*        I_PARENTDBG             =                  " Internal, Do not Use
*        I_APPLOGPARENT          =                  " Container for Application Log
*        I_GRAPHICSPARENT        =                  " Container for Graphics
*        I_NAME                  =                  " Name
*        I_FCAT_COMPLETE         = SPACE            " Boolean Variable (X=True, Space=False)
*        O_PREVIOUS_SRAL_HANDLER =
      EXCEPTIONS
        ERROR_CNTL_CREATE       = 1                " Error when creating the control
        ERROR_CNTL_INIT         = 2                " Error While Initializing Control
        ERROR_CNTL_LINK         = 3                " Error While Linking Control
        ERROR_DP_CREATE         = 4                " Error While Creating DataProvider Control
        OTHERS                  = 5.

  endmethod.


  method DISPLAY.

    GET REFERENCE OF T_OUTTAB INTO ME->MT_OUTTAB.


    ASSIGN ME->MT_OUTTAB->* TO FIELD-SYMBOL(<FS_TAB>).
    <FS_TAB> = T_OUTTAB[].

    ME->MR_ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY(
      EXPORTING
        I_BYPASSING_BUFFER            = 'X'              " Switch Off Buffer
        IS_VARIANT                    = ME->MS_VARIANT   " Layout
        I_SAVE                        = ME->MV_SAVE      " Save Layout
        IS_LAYOUT                     = ME->MS_LAYOUT    " Layout
        IT_TOOLBAR_EXCLUDING          = ME->MT_EXCLUDED_TOOLBAR " Excluded Toolbar Standard Functions
*        I_BUFFER_ACTIVE               =                  " Buffering Active
*        I_CONSISTENCY_CHECK           =                  " Starting Consistency Check for Interface Error Recognition
*        I_STRUCTURE_NAME              =                  " Internal Output Table Structure Name
*        I_DEFAULT                     = 'X'              " Default Display Variant
*        IS_PRINT                      =                  " Print Control
*        IT_SPECIAL_GROUPS             =                  " Field Groups
*        IT_HYPERLINK                  =                  " Hyperlinks
*        IT_ALV_GRAPHICS               =                  " Table of Structure DTC_S_TC
*        IT_EXCEPT_QINFO               =                  " Table for Exception Tooltip
*        IR_SALV_ADAPTER               =                  " Interface ALV Adapter
      CHANGING
        IT_OUTTAB                     = <FS_TAB>         " Output Table
        IT_FIELDCATALOG               = ME->MT_FIELDCAT  " Field Catalog
        IT_SORT                       = ME->MT_SORT      " Sort Criteria
*        IT_FILTER                     =                  " Filter Criteria
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1                " Wrong Parameter
        PROGRAM_ERROR                 = 2                " Program Errors
        TOO_MANY_LINES                = 3                " Too many Rows in Ready for Input Grid
        OTHERS                        = 4
    ).

  endmethod.


  method GET_SELECTED_ROWS.

    REFRESH MT_ROWS.

    CHECK MR_ALV_GRID IS BOUND.

    MR_ALV_GRID->GET_SELECTED_ROWS(
      IMPORTING
        ET_ROW_NO = MT_ROWS
    ).

    IF ET_ROWS IS REQUESTED.
      ET_ROWS[] = MT_ROWS[].
    ENDIF.

    IF R_LINES IS REQUESTED.
      R_LINES = LINES( MT_ROWS ).
    ENDIF.

  endmethod.


  method REFRESH.

    DATA LS_STABLE TYPE LVC_S_STBL.

    CHECK ME->MR_ALV_GRID IS BOUND.

    LS_STABLE = VALUE #(
      ROW = I_STABLE_ROW
      COL = I_STABLE_COL
    ).

    ME->MR_ALV_GRID->REFRESH_TABLE_DISPLAY(
      EXPORTING
        IS_STABLE      = LS_STABLE        " With Stable Rows/Columns
        I_SOFT_REFRESH = I_SOFT_REFRESH   " Without Sort, Filter, etc.
      EXCEPTIONS
        FINISHED       = 1                " Display was Ended (by Export)
        OTHERS         = 2
    ).

  endmethod.


  METHOD SET_EXCLUDE_TOOLBAR.

  CONSTANTS MC_FC_AUF                   TYPE UI_FUNC VALUE '&AUF'.
  CONSTANTS MC_FC_AVERAGE               TYPE UI_FUNC VALUE '&AVERAGE'.
  CONSTANTS MC_FC_BACK_CLASSIC          TYPE UI_FUNC VALUE '&F03'.
  CONSTANTS MC_FC_CALL_ABC              TYPE UI_FUNC VALUE '&ABC'.
  CONSTANTS MC_FC_CALL_CHAIN            TYPE UI_FUNC VALUE '&BEBN'.
  CONSTANTS MC_FC_CALL_CRBATCH          TYPE UI_FUNC VALUE '&CRBATCH'.
  CONSTANTS MC_FC_CALL_CRWEB            TYPE UI_FUNC VALUE '&CRWEB'.
  CONSTANTS MC_FC_CALL_LINEITEMS        TYPE UI_FUNC VALUE '&BEB1'.
  CONSTANTS MC_FC_CALL_MASTER_DATA      TYPE UI_FUNC VALUE '&BEB2'.
  CONSTANTS MC_FC_CALL_MORE             TYPE UI_FUNC VALUE '&BEB3'.
  CONSTANTS MC_FC_CALL_REPORT           TYPE UI_FUNC VALUE '&BEB9'.
  CONSTANTS MC_FC_CALL_XINT             TYPE UI_FUNC VALUE '&XINT'.
  CONSTANTS MC_FC_CALL_XML_EXPORT       TYPE SYUCOMM VALUE '&XML'.
  CONSTANTS MC_FC_CALL_XXL              TYPE UI_FUNC VALUE '&XXL'.
  CONSTANTS MC_FC_CHECK                 TYPE UI_FUNC VALUE '&CHECK'.
  CONSTANTS MC_FC_COL_INVISIBLE         TYPE UI_FUNC VALUE '&COL_INV'.
  CONSTANTS MC_FC_COL_OPTIMIZE          TYPE UI_FUNC VALUE '&OPTIMIZE'.
  CONSTANTS MC_FC_COUNT                 TYPE UI_FUNC VALUE '&COUNT'.
  CONSTANTS MC_FC_CURRENT_VARIANT       TYPE UI_FUNC VALUE '&COL0'.
  CONSTANTS MC_FC_DATA_SAVE             TYPE UI_FUNC VALUE '&DATA_SAVE'.
  CONSTANTS MC_FC_DELETE_FILTER         TYPE UI_FUNC VALUE '&DELETE_FILTER'.
  CONSTANTS MC_FC_DESELECT_ALL          TYPE UI_FUNC VALUE '&SAL'.
  CONSTANTS MC_FC_DETAIL                TYPE UI_FUNC VALUE '&DETAIL'.
  CONSTANTS MC_FC_EXCL_ALL              TYPE UI_FUNC VALUE '&EXCLALLFC'.
  CONSTANTS MC_FC_EXPCRDATA             TYPE UI_FUNC VALUE '&CRDATA'.
  CONSTANTS MC_FC_EXPCRDESIG            TYPE UI_FUNC VALUE '&CRDESIG'.
  CONSTANTS MC_FC_EXPCRTEMPL            TYPE UI_FUNC VALUE '&CRTEMPL'.
  CONSTANTS MC_FC_EXPMDB                TYPE UI_FUNC VALUE '&MDB'.
  CONSTANTS MC_FC_EXTEND                TYPE UI_FUNC VALUE '&EXT'.
  CONSTANTS MC_FC_F4                    TYPE UI_FUNC VALUE '&F4'.
  CONSTANTS MC_FC_FILTER                TYPE UI_FUNC VALUE '&FILTER'.
  CONSTANTS MC_FC_FIND                  TYPE UI_FUNC VALUE '&FIND'.
  CONSTANTS MC_FC_FIND_MORE             TYPE UI_FUNC VALUE '&FIND_MORE'.
  CONSTANTS MC_FC_FIX_COLUMNS           TYPE UI_FUNC VALUE '&CFI'.
  CONSTANTS MC_FC_GRAPH                 TYPE UI_FUNC VALUE '&GRAPH'.
  CONSTANTS MC_FC_HELP                  TYPE UI_FUNC VALUE '&HELP'.
  CONSTANTS MC_FC_HTML                  TYPE UI_FUNC VALUE '&HTML'.
  CONSTANTS MC_FC_INFO                  TYPE UI_FUNC VALUE '&INFO'.
  CONSTANTS MC_FC_LOAD_VARIANT          TYPE UI_FUNC VALUE '&LOAD'.
  CONSTANTS MC_FC_LOC_APPEND_ROW        TYPE UI_FUNC VALUE '&LOCAL&APPEND'.
  CONSTANTS MC_FC_LOC_COPY              TYPE UI_FUNC VALUE '&LOCAL&COPY'.
  CONSTANTS MC_FC_LOC_COPY_ROW          TYPE UI_FUNC VALUE '&LOCAL&COPY_ROW'.
  CONSTANTS MC_FC_LOC_CUT               TYPE UI_FUNC VALUE '&LOCAL&CUT'.
  CONSTANTS MC_FC_LOC_DELETE_ROW        TYPE UI_FUNC VALUE '&LOCAL&DELETE_ROW'.
  CONSTANTS MC_FC_LOC_INSERT_ROW        TYPE UI_FUNC VALUE '&LOCAL&INSERT_ROW'.
  CONSTANTS MC_FC_LOC_MOVE_ROW          TYPE UI_FUNC VALUE '&LOCAL&MOVE_ROW'.
  CONSTANTS MC_FC_LOC_PASTE             TYPE UI_FUNC VALUE '&LOCAL&PASTE'.
  CONSTANTS MC_FC_LOC_PASTE_NEW_ROW     TYPE UI_FUNC VALUE '&LOCAL&PASTE_NEW_ROW'.
  CONSTANTS MC_FC_LOC_UNDO              TYPE UI_FUNC VALUE '&LOCAL&UNDO'.
  CONSTANTS MC_FC_MAINTAIN_VARIANT      TYPE UI_FUNC VALUE '&MAINTAIN'.
  CONSTANTS MC_FC_MAXIMUM               TYPE UI_FUNC VALUE '&MAXIMUM'.
  CONSTANTS MC_FC_MINIMUM               TYPE UI_FUNC VALUE '&MINIMUM'.
  CONSTANTS MC_FC_PC_FILE               TYPE UI_FUNC VALUE '&PC'.
  CONSTANTS MC_FC_PRINT                 TYPE UI_FUNC VALUE '&PRINT'.
  CONSTANTS MC_FC_PRINT_BACK            TYPE UI_FUNC VALUE '&PRINT_BACK'.
  CONSTANTS MC_FC_PRINT_PREV            TYPE UI_FUNC VALUE '&PRINT_BACK_PREVIEW'.
  CONSTANTS MC_FC_REFRESH               TYPE UI_FUNC VALUE '&REFRESH'.
  CONSTANTS MC_FC_REPREP                TYPE UI_FUNC VALUE '&REPREP'.
  CONSTANTS MC_FC_SAVE_VARIANT          TYPE UI_FUNC VALUE '&SAVE'.
  CONSTANTS MC_FC_SELECT_ALL            TYPE UI_FUNC VALUE '&ALL'.
  CONSTANTS MC_FC_SEND                  TYPE UI_FUNC VALUE '&SEND'.
  CONSTANTS MC_FC_SEPARATOR             TYPE UI_FUNC VALUE '&&SEP'.
  CONSTANTS MC_FC_SORT                  TYPE UI_FUNC VALUE '&SORT'.
  CONSTANTS MC_FC_SORT_ASC              TYPE UI_FUNC VALUE '&SORT_ASC'.
  CONSTANTS MC_FC_SORT_DSC              TYPE UI_FUNC VALUE '&SORT_DSC'.
  CONSTANTS MC_FC_SUBTOT                TYPE UI_FUNC VALUE '&SUBTOT'.
  CONSTANTS MC_FC_SUM                   TYPE UI_FUNC VALUE '&SUMC'.
  CONSTANTS MC_FC_TO_OFFICE             TYPE UI_FUNC VALUE '&ML'.
  CONSTANTS MC_FC_TO_REP_TREE           TYPE UI_FUNC VALUE '&SERP'.
  CONSTANTS MC_FC_UNFIX_COLUMNS         TYPE UI_FUNC VALUE '&CDF'.
  CONSTANTS MC_FC_URL_COPY_TO_CLIPBOARD TYPE UI_FUNC VALUE '&URL_COPY_TO_CLIPBOARD'.
  CONSTANTS MC_FC_VARIANT_ADMIN         TYPE UI_FUNC VALUE '&VARI_ADMIN'.
  CONSTANTS MC_FC_VIEWS                 TYPE UI_FUNC VALUE '&VIEW'.
  CONSTANTS MC_FC_VIEW_CRYSTAL          TYPE UI_FUNC VALUE '&VCRYSTAL'.
  CONSTANTS MC_FC_VIEW_EXCEL            TYPE UI_FUNC VALUE '&VEXCEL'.
  CONSTANTS MC_FC_VIEW_GRID             TYPE UI_FUNC VALUE '&VGRID'.
  CONSTANTS MC_FC_VIEW_LOTUS            TYPE UI_FUNC VALUE '&VLOTUS'.
  CONSTANTS MC_FC_WORD_PROCESSOR        TYPE UI_FUNC VALUE '&AQW'.
  CONSTANTS MC_FG_EDIT                  TYPE UI_FUNC VALUE '&FG_EDIT'.
  CONSTANTS MC_FG_SORT                  TYPE UI_FUNC VALUE '&FG_SORT'.
  CONSTANTS MC_MB_EXPORT                TYPE UI_FUNC VALUE '&MB_EXPORT'.
  CONSTANTS MC_MB_FILTER                TYPE UI_FUNC VALUE '&MB_FILTER'.
  CONSTANTS MC_MB_PASTE                 TYPE UI_FUNC VALUE '&MB_PASTE'.
  CONSTANTS MC_MB_SUBTOT                TYPE UI_FUNC VALUE '&MB_SUBTOT'.
  CONSTANTS MC_MB_SUM                   TYPE UI_FUNC VALUE '&MB_SUM'.
  CONSTANTS MC_MB_VARIANT               TYPE UI_FUNC VALUE '&MB_VARIANT'.
  CONSTANTS MC_MB_VIEW                  TYPE UI_FUNC VALUE '&MB_VIEW'.

*--------------------------------------------------------------------*
* 자주 사용되는 Exclude Toolbar 1
*--------------------------------------------------------------------*
* 일반 문자열만 존재하는 ALV 에 사용되며, Sum 등의 산술 관련 버튼 제거
*--------------------------------------------------------------------*
*  IT_TOOLBAR = VALUE #(
*    ( '&SUBTOT'     )
*    ( '&SUMC'       )
*    ( '&MB_SUBTOT'  )
*    ( '&MB_SUM'     )
*  ).
*--------------------------------------------------------------------*




    FIELD-SYMBOLS <FS> TYPE UI_FUNC.

    LOOP AT IT_TOOLBAR INTO DATA(LS_TOOLBAR).

      APPEND INITIAL LINE TO ME->MT_EXCLUDED_TOOLBAR ASSIGNING <FS>.
      CHECK SY-SUBRC EQ 0.

      <FS> = LS_TOOLBAR.

    ENDLOOP.

  ENDMETHOD.


  method SET_FIELD_CATALOG.

    REFRESH ME->MT_FIELDCAT.

    DATA LT_FIELDCAT TYPE KKBLO_T_FIELDCAT.

    IF I_TABNAME IS SUPPLIED.
      CALL FUNCTION 'K_KKB_FIELDCAT_MERGE'
        EXPORTING
          I_CALLBACK_PROGRAM     = I_REPID          " Internal table declaration program
          I_INCLNAME             = I_REPID
          I_TABNAME              = I_TABNAME        " Name of table to be displayed
          I_BYPASSING_BUFFER     = 'X'              " Ignore buffer while reading
*          I_STRUCNAME            =
*          I_BUFFER_ACTIVE        =
        CHANGING
          CT_FIELDCAT            = LT_FIELDCAT      " Field Catalog with Field Descriptions
        EXCEPTIONS
          INCONSISTENT_INTERFACE = 1
          OTHERS                 = 2.

    ELSEIF I_STRUCTURE IS SUPPLIED.
      CALL FUNCTION 'K_KKB_FIELDCAT_MERGE'
        EXPORTING
          I_STRUCNAME            = I_STRUCTURE
*          I_CALLBACK_PROGRAM     =                  " Internal table declaration program
*          I_TABNAME              =                  " Name of table to be displayed
*          I_INCLNAME             =
*          I_BYPASSING_BUFFER     =                  " Ignore buffer while reading
*          I_BUFFER_ACTIVE        =
        CHANGING
          CT_FIELDCAT            = LT_FIELDCAT       " Field Catalog with Field Descriptions
        EXCEPTIONS
          INCONSISTENT_INTERFACE = 1
          OTHERS                 = 2.
    ELSE.
      RAISE INVALID_INPUT_PARAMETER.
    ENDIF.


    IF LT_FIELDCAT[] IS INITIAL.
      RAISE EMPTY_FIELD_CATALOG.
    ENDIF.

    CALL FUNCTION 'LVC_TRANSFER_FROM_KKBLO'
      EXPORTING
        IT_FIELDCAT_KKBLO         = LT_FIELDCAT
      IMPORTING
        ET_FIELDCAT_LVC           = ME->MT_FIELDCAT
      EXCEPTIONS
        IT_DATA_MISSING           = 1
        OTHERS                    = 2.

    IF ME->MT_FIELDCAT[] IS INITIAL.
      RAISE EMPTY_FIELD_CATALOG.
    ENDIF.

  endmethod.


  method SET_LAYOUT.

    CLEAR ME->MS_LAYOUT.

    IF I_LAYOUT IS SUPPLIED.
      ME->MS_LAYOUT = I_LAYOUT.
    ELSE.

      CASE I_TYPE.
        WHEN 'A'.

          ME->MS_LAYOUT = VALUE #(
            ZEBRA      = 'X'
            CWIDTH_OPT = 'X'
            TOTALS_BEF = ' '
            NO_MERGING = ' '
            NO_ROWMOVE = 'X'
            NO_ROWINS  = 'X'
            NO_ROWMARK = ' '
            NO_TOOLBAR = ' '
            SEL_MODE   = 'D'
          ).

        WHEN 'B'.

          ME->MS_LAYOUT = VALUE #(
            ZEBRA      = 'X'
            CWIDTH_OPT = ' '
            TOTALS_BEF = ' '
            NO_MERGING = ' '
            NO_ROWMOVE = 'X'
            NO_ROWINS  = 'X'
            NO_ROWMARK = ' '
            NO_TOOLBAR = ' '
            SEL_MODE   = 'D'
          ).

        WHEN 'C'.

          ME->MS_LAYOUT = VALUE #(
            ZEBRA      = 'X'
            CWIDTH_OPT = ' '
            TOTALS_BEF = ' '
            NO_MERGING = ' '
            NO_ROWMOVE = 'X'
            NO_ROWINS  = ' '
            NO_ROWMARK = ' '
            NO_TOOLBAR = ' '
            SEL_MODE   = 'D'
          ).

        WHEN 'D'.

          ME->MS_LAYOUT = VALUE #(
            ZEBRA      = 'X'
            CWIDTH_OPT = ' '
            TOTALS_BEF = ' '
            NO_MERGING = ' '
            NO_ROWMOVE = 'X'
            NO_ROWINS  = 'X'
            NO_ROWMARK = ' '
            NO_TOOLBAR = ' '
            SEL_MODE   = 'C'
          ).

        WHEN 'E'.
      ENDCASE.

    ENDIF.


    IF I_STYLEFNAME IS SUPPLIED.
      ME->MS_LAYOUT-STYLEFNAME = I_STYLEFNAME.
    ENDIF.

    IF I_BOX_FNAME  IS SUPPLIED.
      ME->MS_LAYOUT-BOX_FNAME  = I_BOX_FNAME.
      IF I_BOX_FNAME IS NOT INITIAL.
        CLEAR ME->MS_LAYOUT-NO_ROWMARK.
      ENDIF.
    ENDIF.

    IF I_INFO_FNAME IS SUPPLIED.
      ME->MS_LAYOUT-INFO_FNAME = I_INFO_FNAME.
    ENDIF.

    IF I_CTAB_FNAME  IS SUPPLIED.
      ME->MS_LAYOUT-CTAB_FNAME  = I_CTAB_FNAME.
    ENDIF.


  endmethod.


  method SET_SORT.

    FIELD-SYMBOLS <FS> TYPE LVC_S_SORT.

    LOOP AT IT_FIELD INTO DATA(LS_FIELD).

      APPEND INITIAL LINE TO ME->MT_SORT ASSIGNING <FS>.
      CHECK SY-SUBRC EQ 0.

      <FS>-SPOS = SY-TABIX.
      <FS>-FIELDNAME = LS_FIELD.
      <FS>-UP = 'X'.

    ENDLOOP.

  endmethod.
ENDCLASS.
