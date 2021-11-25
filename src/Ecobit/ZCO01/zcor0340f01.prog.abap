*----------------------------------------------------------------------*
***INCLUDE ZCOR0340F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form SET_KSTAR_RANGES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> R_KSTAR
*&      --> PA_KAGRU
*&---------------------------------------------------------------------*
FORM SET_KSTAR_RANGES  TABLES PR_KSTAR STRUCTURE R_KSTAR
                             USING P_FKAGRU.

  RANGES R_SETNAME FOR SETNODE-SETNAME.

  CLEAR R_SETNAME.
  REFRESH R_SETNAME.

  IF P_FKAGRU IS NOT INITIAL.

    MOVE: 'I'      TO R_SETNAME-SIGN,
          'EQ'     TO R_SETNAME-OPTION,
          P_FKAGRU TO R_SETNAME-LOW.

    APPEND R_SETNAME.

  ENDIF.


  SELECT * FROM SETHEADER
    INTO TABLE @DATA(LT_SETHEADER)
   WHERE SETCLASS = '0102'
     AND SUBCLASS = @PA_KOKRS
     AND SETNAME  IN @R_SETNAME.

  LOOP AT LT_SETHEADER INTO DATA(LS_SETHEADER).

    PERFORM HIERARCHY_READ USING LS_SETHEADER-SETNAME.

    LOOP  AT GT_VALUES.

      IF GT_VALUES-VFROM = GT_VALUES-VTO.

        MOVE: GT_VALUES-VFROM   TO PR_KSTAR-LOW,
              'I'               TO PR_KSTAR-SIGN,
              'EQ'              TO PR_KSTAR-OPTION.
      ELSE.

        MOVE: GT_VALUES-VFROM   TO PR_KSTAR-LOW,
              GT_VALUES-VTO     TO PR_KSTAR-HIGH,
              'I'               TO PR_KSTAR-SIGN,
              'BT'              TO PR_KSTAR-OPTION.
      ENDIF.

      COLLECT PR_KSTAR.
      CLEAR   PR_KSTAR.

    ENDLOOP.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HIERARCHY_READ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_SETHEADER_SETNAME
*&---------------------------------------------------------------------*
FORM HIERARCHY_READ  USING   P_SETID TYPE SETNAMENEW.

  CLEAR: GT_NODES,  GT_NODES[],
         GT_VALUES, GT_VALUES[],
         GT_INFO,   GT_INFO[].

  DATA: LV_SETID     LIKE SETHIER-SETID,
        LV_OVERWRITE LIKE SY-DATAR.

  CALL FUNCTION 'G_SET_ENCRYPT_SETID'
    EXPORTING
      SETCLASS  = '0102'
      SHORTNAME = P_SETID
    IMPORTING
      SETID     = LV_SETID.

  IF SY-SUBRC = 0.

    CALL FUNCTION 'K_HIERARCHY_TABLES_READ'
      EXPORTING
        E_CLASS                     = '0102'
        E_SETID                     = LV_SETID
        E_KOKRS                     = PA_KOKRS
      TABLES
        T_NODES                     = GT_NODES
        T_VALUES                    = GT_VALUES
      CHANGING
        C_INFO                      = GT_INFO
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
