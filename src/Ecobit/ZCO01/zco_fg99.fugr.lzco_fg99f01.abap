*----------------------------------------------------------------------*
***INCLUDE LZCO_FG99F01.
*----------------------------------------------------------------------*

*--------------------------------------------------------------------*
FORM BEFORE_SAVE.
*--------------------------------------------------------------------*

  DATA : LR_STRUCTURE TYPE REF TO CL_ABAP_STRUCTDESCR,
         LR_HANDLE    TYPE REF TO DATA.

  DATA : L_INDEX LIKE SY-TABIX.

  FIELD-SYMBOLS : <L_STRUCTURE> TYPE ANY,
                  <L_FIELD>     TYPE ANY,
                  <LS_CHANGE>   TYPE ANY,
                  <LV_VIEW>     TYPE ANY.

*-- Macro
DEFINE __SET_FIELD_VALUE.
  ASSIGN COMPONENT &1 OF STRUCTURE <L_STRUCTURE> TO <L_FIELD>.
  IF SY-SUBRC = 0.
    MOVE &2 TO <L_FIELD>.
    UNASSIGN <L_FIELD>.
  ENDIF.
END-OF-DEFINITION.


*-- Get data
  LR_STRUCTURE ?= CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_NAME( X_HEADER-VIEWNAME ).
  CREATE DATA LR_HANDLE TYPE HANDLE LR_STRUCTURE.
  ASSIGN LR_HANDLE->* TO <L_STRUCTURE>.


*-- Set user, time, date
  LOOP AT TOTAL.

    CHECK <ACTION> = NEUER_EINTRAG  "New Entries
       OR <ACTION> = AENDERN.       "Change/Update


    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.

    CHECK SY-SUBRC EQ 0.
    L_INDEX = SY-TABIX.

    MOVE-CORRESPONDING TOTAL TO <L_STRUCTURE>.


    IF <ACTION> EQ NEUER_EINTRAG. "New Entries
      __SET_FIELD_VALUE : 'ERDAT' SY-DATUM,
                          'ERZET' SY-UZEIT,
                          'ERNAM' SY-UNAME.
    ENDIF.

    __SET_FIELD_VALUE : 'AEDAT' SY-DATUM,
                        'AEZET' SY-UZEIT,
                        'AENAM' SY-UNAME.


    MOVE-CORRESPONDING <L_STRUCTURE> TO TOTAL.
    MODIFY TOTAL.

    EXTRACT = TOTAL.
    MODIFY EXTRACT INDEX L_INDEX.

  ENDLOOP.

ENDFORM.
