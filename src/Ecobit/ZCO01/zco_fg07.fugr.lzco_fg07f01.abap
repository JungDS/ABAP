*----------------------------------------------------------------------*
***INCLUDE LZCO_FG07F01.
*----------------------------------------------------------------------*

FORM update_rtn_zcov000.
  DATA: lv_index LIKE sy-tabix. "Index to note the lines found

  FIELD-SYMBOLS: <fs_value> TYPE any.


  DATA: lv_timestamp LIKE tzonref-tstampl.

  DATA exp        TYPE sxmsmguid.
  DATA imp        TYPE int4.
  DATA lv_stamp2  TYPE int4.


  LOOP AT total.
    IF <action> = neuer_eintrag OR <action> = aendern.

      READ TABLE extract WITH KEY <vim_xtotal_key>.
      IF sy-subrc EQ 0.
        lv_index = sy-tabix.
      ELSE.
        CLEAR lv_index.
      ENDIF.

      ASSIGN COMPONENT 'ERDAT' OF STRUCTURE total TO <fs_value>.
      IF <fs_value> IS ASSIGNED.
        IF <fs_value> IS INITIAL.
          <fs_value> = sy-datum.
        ENDIF.
      ENDIF.

      UNASSIGN <fs_value>.
      ASSIGN COMPONENT 'ERZET' OF STRUCTURE total TO <fs_value>.
      IF <fs_value> IS ASSIGNED.
        IF <fs_value> IS INITIAL OR <fs_value> EQ ''.
          <fs_value> = sy-uzeit.
        ENDIF.
      ENDIF.

      UNASSIGN <fs_value>.
      ASSIGN COMPONENT 'ERNAM' OF STRUCTURE total TO <fs_value>.
      IF <fs_value> IS ASSIGNED.
        IF <fs_value> IS INITIAL.
          <fs_value> = sy-uname.
        ENDIF.
      ENDIF.

      UNASSIGN <fs_value>.
      ASSIGN COMPONENT 'AEDAT' OF STRUCTURE total TO <fs_value>.
      IF <fs_value> IS ASSIGNED.
        <fs_value> = sy-datum.
      ENDIF.

      UNASSIGN <fs_value>.
      ASSIGN COMPONENT 'AEZET' OF STRUCTURE total TO <fs_value>.
      IF <fs_value> IS ASSIGNED.
        <fs_value> = sy-uzeit.
      ENDIF.

      UNASSIGN <fs_value>.
      ASSIGN COMPONENT 'AENAM' OF STRUCTURE total TO <fs_value>.
      IF <fs_value> IS ASSIGNED.
        <fs_value> = sy-uname.
      ENDIF.


      MODIFY total.

      CHECK lv_index GT 0.

      extract = total.

      MODIFY extract INDEX lv_index.

    ENDIF.

  ENDLOOP.

* 로그 쌓기

  CLEAR lv_timestamp.
  CALL FUNCTION 'RRBA_GET_TIME'
    IMPORTING
      e_timestampl = lv_timestamp.

  DATA  lt_log TYPE STANDARD TABLE OF zcot000log.
  DATA  ls_log LIKE LINE OF lt_log.


  LOOP AT total ASSIGNING FIELD-SYMBOL(<zz>).
    CLEAR ls_log.
    MOVE-CORRESPONDING <zz> TO ls_log.
    ls_log-tstampl = sy-datum && sy-uzeit && lv_timestamp.

    ls_log-change_flag = <action>.
    APPEND  ls_log TO lt_log.

  ENDLOOP.

  IF lt_log[]  IS NOT INITIAL.
    MODIFY zcot000log FROM TABLE lt_log.

    COMMIT WORK.

  ENDIF.


ENDFORM.
