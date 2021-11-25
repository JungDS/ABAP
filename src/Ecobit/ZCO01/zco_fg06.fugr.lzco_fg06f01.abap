*----------------------------------------------------------------------*
***INCLUDE LZCO_FG06F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form GET_KSTAR_TEXT
*&---------------------------------------------------------------------*
FORM GET_KSTAR_TEXT  USING    P_KSTAR TYPE KSTAR
                     CHANGING P_KSTXT TYPE KTEXT.

  CLEAR:  P_KSTXT.

  SELECT SINGLE B~KTEXT
    INTO @P_KSTXT
    FROM CSKA AS A
    LEFT JOIN CSKU AS B
      ON A~KTOPL = B~KTOPL
     AND A~KSTAR = B~KSTAR
     AND B~SPRAS = @SY-LANGU
   WHERE A~KTOPL = @GC_KTOPL
     AND A~KSTAR = @P_KSTAR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_POST1_CODE_TXT
*&---------------------------------------------------------------------*
FORM GET_POST1_CODE_TXT  USING    P_ROBJNR TYPE J_OBJNR
                         CHANGING P_POSID  TYPE PS_POSID
                                  P_POST1  TYPE PS_POST1.

  SELECT SINGLE POSID, POST1 INTO (@P_POSID, @P_POST1)
    FROM PRPS
   WHERE OBJNR = @P_ROBJNR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_KOSTL_TEXT
*&---------------------------------------------------------------------*
FORM GET_KOSTL_TEXT  USING    P_KOSTL TYPE KOSTL
                              P_KOKRS TYPE KOKRS
                     CHANGING P_KTEXT TYPE KTEXT.

  SELECT SINGLE B~KTEXT INTO @P_KTEXT
    FROM CSKS AS A
    LEFT  JOIN CSKT AS B
      ON A~KOKRS = B~KOKRS
     AND A~KOSTL = B~KOSTL
     AND A~DATBI = B~DATBI
     AND B~SPRAS = @SY-LANGU
   WHERE A~KOKRS = @P_KOKRS
     AND A~KOSTL = @P_KOSTL.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_MESSAGE
*&---------------------------------------------------------------------*
FORM BUILD_MESSAGE USING    PS_MESSAGE STRUCTURE BAPIRET2
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
