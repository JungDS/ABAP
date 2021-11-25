*$*$----------------------------------------------------------------$*$*
*$ Correction Inst.         0020751258 0000966553                     $*
*$--------------------------------------------------------------------$*
*$ Valid for       :                                                  $*
*$ Software Component   S4CORE                                        $*
*$  Release 101          All Support Package Levels                   $*
*$  Release 102          All Support Package Levels                   $*
*$  Release 103          All Support Package Levels                   $*
*$  Release 104          All Support Package Levels                   $*
*$  Release 105          All Support Package Levels                   $*
*$  Release 106          All Support Package Levels                   $*
*$--------------------------------------------------------------------$*
*$ Changes/Objects Not Contained in Standard SAP System               $*
*$*$----------------------------------------------------------------$*$*
*&--------------------------------------------------------------------*
*& Object          REPS ZSNAPSHOT_DELETE
*& Object Header   PROG ZSNAPSHOT_DELETE
*&--------------------------------------------------------------------*
*& REPORT ZSNAPSHOT_DELETE
*&--------------------------------------------------------------------*
*>>>> START OF INSERTION <<<<
REPORT zsnapshot_delete .
TABLES: vssteu.
DATA: auth_flg TYPE c,
      text_tmp(70) TYPE c,
      answer_tmp TYPE c,
      gt_vskopfd TYPE TABLE OF vskopfd,
      g_vskopfd TYPE vskopfd,
      g_vskopf_old TYPE vskopf,
      g_objnr LIKE jsto-objnr,
      cn_tcndb LIKE tcndb.

INCLUDE lcnaud01.

PARAMETERS:
  p_vsnmr LIKE vskopf-vsnmr OBLIGATORY.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS:
  p_flg_pd TYPE c
           RADIOBUTTON GROUP sel
           USER-COMMAND sel
           DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(20) for field p_pspid.
SELECTION-SCREEN POSITION pos_low.
PARAMETERS:
  p_pspid  LIKE proj-pspid
           MEMORY ID psp
           MODIF ID psp.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS:
  p_flg_np TYPE c
           RADIOBUTTON GROUP sel.
SELECTION-SCREEN COMMENT 3(20) for field p_aufnr.
SELECTION-SCREEN POSITION pos_low.
PARAMETERS:
p_aufnr  LIKE aufk-aufnr
         MEMORY ID anr
         MODIF ID anr.
SELECTION-SCREEN END OF LINE.

INITIALIZATION.
* Berechtigung prüfen  (löschen)
  CALL FUNCTION 'CNAU_AUTHORITY_VERS'
       EXPORTING
            actvt   = cnau_delete
            msgty   = 'I'
       IMPORTING
            x_actvt = auth_flg
       EXCEPTIONS
            OTHERS  = 1.
  CHECK NOT sy-subrc IS INITIAL.
  LEAVE PROGRAM.

AT SELECTION-SCREEN OUTPUT.
  IF p_flg_pd IS INITIAL.
    LOOP AT SCREEN.
      IF screen-group1 = 'PSP'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF screen-group1 = 'ANR'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

START-OF-SELECTION.
* Sicherheitsabfrage
  CONCATENATE text-dv2 p_vsnmr INTO text_tmp SEPARATED BY space.

  CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
       EXPORTING
            defaultoption  = 'N'
            diagnosetext1  = text_tmp
            textline1      = text-dv1
            titel          = text-dv2
            cancel_display = ' '
       IMPORTING
            answer        = answer_tmp.

  IF answer_tmp = 'J'.
    IF NOT p_flg_pd IS INITIAL.
      SELECT SINGLE objnr INTO g_objnr
             FROM vsproj_cn
             WHERE vsnmr = p_vsnmr
               AND pspid = p_pspid.
      IF NOT sy-subrc IS INITIAL.
        MESSAGE e253(cj) WITH p_vsnmr p_pspid.
*   Es existiert keine Version & zu Projekt &
      ENDIF.
    ELSE.
      SELECT SINGLE objnr INTO g_objnr
             FROM vsaufk_cn
             WHERE vsnmr = p_vsnmr
               AND aufnr = p_aufnr.
      IF NOT sy-subrc IS INITIAL.
        CONCATENATE p_vsnmr '/' p_aufnr INTO text_tmp.
        MESSAGE e010(cn) WITH text_tmp.
*   Netzplan & nicht vorhanden (Bitte Eingabe prüfen)
      ENDIF.
    ENDIF.
    SELECT * FROM vskopf
           INTO CORRESPONDING FIELDS OF TABLE gt_vskopfd
           WHERE ( vsnmr = p_vsnmr
             AND vskat = '2' )
              OR ( vsnmr = p_vsnmr
             AND vskat = SPACE ).
    IF sy-subrc IS INITIAL.
      IF NOT p_flg_pd IS INITIAL.
        LOOP AT gt_vskopfd INTO g_vskopfd WHERE objnr_pd = g_objnr.
          MOVE-CORRESPONDING g_vskopfd TO g_vskopf_old.
          EXIT.
        ENDLOOP.
        IF NOT sy-subrc IS INITIAL.
          READ TABLE gt_vskopfd INTO g_vskopfd INDEX 1.
          MOVE-CORRESPONDING g_vskopfd TO g_vskopf_old.
        ENDIF.
        g_vskopfd-objnr_pd = g_objnr.
        CLEAR g_vskopfd-objnr_np.
        CLEAR cn_tcndb.
        cn_tcndb-netz_psp = cn_tcndb-incih   = cn_tcndb-incor   =
        cn_tcndb-incco    = cn_tcndb-incpp   =
        cn_tcndb-act_psp  = cn_tcndb-incihvg = cn_tcndb-incorvg =
        cn_tcndb-incppvg  = cn_tcndb-inctn    = 'X'.
        EXPORT cn_tcndb TO MEMORY ID 'SNAPSHOT_DELETE'.
      ELSE.
        LOOP AT gt_vskopfd INTO g_vskopfd WHERE objnr_np = g_objnr.
          MOVE-CORRESPONDING g_vskopfd TO g_vskopf_old.
          EXIT.
        ENDLOOP.
        IF NOT sy-subrc IS INITIAL.
          READ TABLE gt_vskopfd INTO g_vskopfd INDEX 1.
          MOVE-CORRESPONDING g_vskopfd TO g_vskopf_old.
        ENDIF.
        CLEAR g_vskopfd-objnr_pd.
        g_vskopfd-objnr_np = g_objnr.
      ENDIF.
      CALL FUNCTION 'VS_BT_VSKOPF_APPEND'
           EXPORTING
                vskopfd_imp = g_vskopfd
                vsvbkz_imp  = 'D'
           EXCEPTIONS
                OTHERS      = 1.
      CALL FUNCTION 'VS_BT_OBJECTS_DELETE'
           EXPORTING
                vsnmr_imp = p_vsnmr
           EXCEPTIONS
                OTHERS    = 1.
      PERFORM version_post(saplvsdi) USING space.

      SELECT * FROM vssteu UP TO 1 ROWS
             WHERE vsnmr = p_vsnmr.
      ENDSELECT.
      IF sy-subrc IS INITIAL.
        IF g_vskopf_old-objnr_pd = g_objnr.
          CLEAR g_vskopf_old-objnr_pd.
        ENDIF.
        IF g_vskopf_old-objnr_np = g_objnr.
          CLEAR g_vskopf_old-objnr_np.
        ENDIF.
        INSERT INTO vskopf VALUES g_vskopf_old.
      ELSE.
        DELETE FROM vskopf
               WHERE vsnmr = p_vsnmr
                 AND vskat = '2'.
      ENDIF.
      COMMIT WORK AND WAIT.
      MESSAGE s006(ov) WITH p_vsnmr.
*         S006: Projektversion & wurde gelöscht
    ENDIF.

  ENDIF.
