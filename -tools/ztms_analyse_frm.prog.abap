*&---------------------------------------------------------------------*
*&  Include           ZACNTRANSP_FRM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init .
  DATA: lt_alv TYPE tyt_alv.

  PERFORM get_config_syst CHANGING gt_list_sys.
  IF p_datef IS INITIAL.
    p_datef = gv_datefrom.
  ENDIF.
  PERFORM extract_tms USING p_datef gv_dateto
                            gt_list_sys
                   CHANGING lt_alv.

  PERFORM extract_log CHANGING lt_alv.

  gt_alv[] = lt_alv[].
ENDFORM.                    " INIT
*&---------------------------------------------------------------------*
*&      Form  ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv .
  CALL SCREEN 100.
ENDFORM.                    " ALV
*&---------------------------------------------------------------------*
*&      Form  get_config_syst
*&---------------------------------------------------------------------*
FORM get_config_syst  CHANGING ot_list_sys TYPE tyt_list_sys.

  DATA: ls_version TYPE triwb_s_version.
  DATA: lt_system  TYPE triwb_t_system.
  DATA: lt_release TYPE triwb_t_release.
  DATA: lt_deliver TYPE triwb_t_deliver.

  DATA: ls_system  LIKE LINE OF lt_system.
  DATA: ls_release LIKE LINE OF lt_release.
  DATA: ls_deliver LIKE LINE OF lt_deliver.

  DATA: ls_list_sys TYPE ty_list_sys,
        lv_idx(1)   TYPE n VALUE 0.

  DATA: lv_length TYPE i.
  DATA: lv_fromsystem LIKE ls_deliver-fromsystem.

  CALL FUNCTION 'TRINT_TCE_READ_CONFIG'
* EXPORTING
*   IV_VERSION                    = 0
*   IV_LANGUAGE                   = ' '
*   IV_STATE                      = ' '
    IMPORTING
*     ET_VERTEXT              =
      es_version              = ls_version
      et_system               = lt_system
*     ET_TRLAYER              =
      et_release              = lt_release
      et_deliver              = lt_deliver
*     ET_SYSTEXT              =
*     ET_LAYTEXT              =
*     ET_TARTEXT              =
*     ET_TARGET               =
*     ET_CLIENTC              =
    EXCEPTIONS
      configuration_not_found = 1
      configuration_empty     = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  DELETE lt_release WHERE translayer NE p_laya.

  SORT lt_deliver.
  DELETE lt_deliver WHERE tosystem EQ '/NOD300/'.
  DELETE ADJACENT DUPLICATES FROM lt_deliver COMPARING fromsystem.

*  ls_deliver-fromsystem = 'NOJ'.
*  ls_deliver-fromclient = '900'.
*  ls_deliver-tosystem   = 'NOJ.900'.
*  INSERT ls_deliver INTO TABLE lt_deliver.

  READ TABLE lt_release INTO ls_release INDEX 1.

  CLEAR ls_list_sys.
  ls_list_sys-idx = lv_idx.
  ls_list_sys-system = 'DXE'.
*  ls_list_sys-struct_txt = gc_syst0.
  INSERT ls_list_sys INTO TABLE ot_list_sys.

  ADD 1 TO lv_idx.

  CLEAR ls_list_sys.
  ls_list_sys-idx = lv_idx.
  ls_list_sys-system = 'RXE'.
  ls_list_sys-struct_txt = gc_syst1.
  INSERT ls_list_sys INTO TABLE ot_list_sys.

*  ADD 1 TO lv_idx.
*
*  CLEAR ls_list_sys.
*  ls_list_sys-idx = lv_idx.
*  ls_list_sys-system = 'RX2'.
*  ls_list_sys-struct_txt = gc_syst2.
*  INSERT ls_list_sys INTO TABLE ot_list_sys.

  ADD 1 TO lv_idx.

  CLEAR ls_list_sys.
  ls_list_sys-idx = lv_idx.
  ls_list_sys-system = 'QXE'.
  ls_list_sys-struct_txt = gc_syst3.
  INSERT ls_list_sys INTO TABLE ot_list_sys.

  ADD 1 TO lv_idx.

  CLEAR ls_list_sys.
  ls_list_sys-idx = lv_idx.
  ls_list_sys-system = 'PXE'.
  ls_list_sys-struct_txt = gc_syst4.
  INSERT ls_list_sys INTO TABLE ot_list_sys.

*  lv_fromsystem = ls_list_sys-system.
*
*  DESCRIBE TABLE lt_deliver LINES lv_length.
*
*  WHILE lv_length NE 0.
*
*    READ TABLE lt_deliver INTO ls_deliver
*                          WITH KEY fromsystem = lv_fromsystem.
*
*    IF sy-subrc NE 0.
*      lv_length = 0.
*      CONTINUE.
*    ELSE.
*      DELETE lt_deliver INDEX sy-tabix.
*
*      ADD 1 TO lv_idx.
*
*      FIELD-SYMBOLS : <gc> TYPE any.
*      DATA: lv_var(8) TYPE c.
*
*      CONCATENATE 'GC_SYST' lv_idx INTO lv_var.
*
*      ASSIGN (lv_var) TO <gc>.
*
*      IF <gc> IS ASSIGNED.
*        CLEAR ls_list_sys.
*        ls_list_sys-idx = lv_idx.
*        ls_list_sys-system = ls_deliver-tosystem(3).
*        ls_list_sys-struct_txt = <gc>.
*        INSERT ls_list_sys INTO TABLE ot_list_sys.
*
*        lv_fromsystem = ls_list_sys-system.
*      ELSE.
*        lv_length = 0.
*        CONTINUE.
*      ENDIF.
*
*    ENDIF.
*    DESCRIBE TABLE lt_deliver LINES lv_length.
*  ENDWHILE.
*
*  ADD 1 TO lv_idx.
*
*  CONCATENATE 'GC_SYST' lv_idx INTO lv_var.
*
*  ASSIGN (lv_var) TO <gc>.
*
*  IF <gc> IS ASSIGNED.
*    CLEAR ls_list_sys.
*    ls_list_sys-idx = lv_idx.
*    ls_list_sys-system = p_syst.
*    ls_list_sys-struct_txt = <gc>.
*    IF p_rfc EQ abap_true.
*      ls_list_sys-rfcdest = p_dest.
*    ENDIF.
*    ls_list_sys-domadest = p_domad.
*
*    PERFORM start_date_rfc CHANGING ls_list_sys-datefrom.
*
*    IF p_domad IS NOT INITIAL.
*      INSERT ls_list_sys INTO TABLE ot_list_sys.
*    ENDIF.
*
*  ENDIF.
*
*  ADD 1 TO lv_idx.
*
*  CONCATENATE 'GC_SYST' lv_idx INTO lv_var.
*
*  ASSIGN (lv_var) TO <gc>.
*
*  IF <gc> IS ASSIGNED.
*    CLEAR ls_list_sys.
*    ls_list_sys-idx = lv_idx.
*    ls_list_sys-system = 'NOF'.
*    ls_list_sys-struct_txt = <gc>.
**    ls_list_sys-rfcdest = 'NOF'.
**    ls_list_sys-domadest = 'DOMAIN_NOD'.
**    ls_list_sys-datefrom = p_debut.
*    INSERT ls_list_sys INTO TABLE ot_list_sys.
*  ENDIF.
*
**
**  IF <gc> IS ASSIGNED.
**    CLEAR ls_list_sys.
**    ls_list_sys-idx = lv_idx.
**    ls_list_sys-system = 'NOJ'.
**    ls_list_sys-struct_txt = <gc>.
**    ls_list_sys-rfcdest = 'TMSSUP@NOJ.DOMAIN_NOD'.
**    ls_list_sys-domadest = 'DOMAIN_NOE'.
**    ls_list_sys-datefrom = p_debut.
**    INSERT ls_list_sys INTO TABLE ot_list_sys.
**  ENDIF.
ENDFORM.                    " get_config_syst
*&---------------------------------------------------------------------*
*&      Form  EXTRACT_TMS
*&---------------------------------------------------------------------*
FORM extract_tms  USING    iv_datefrom TYPE datum
                           iv_dateto   TYPE datum
                           ot_list_sys TYPE tyt_list_sys
                  CHANGING ot_alv      TYPE tyt_alv.

  DATA: ls_list_sys LIKE LINE OF ot_list_sys.
  DATA: lv_imp TYPE c, lv_exp TYPE c.
  DATA: lt_log TYPE tyt_log.
  DATA: lv_seq TYPE ty_seq.
  DATA: ls_extract TYPE ty_alv.

  LOOP AT ot_list_sys INTO ls_list_sys.
    CLEAR: lv_imp, lv_exp.
    CLEAR: lt_log.

    IF ls_list_sys-idx EQ 0.
      lv_exp = 'X'.
    ELSE.
      lv_imp = 'X'.
    ENDIF.

    IF ls_list_sys-domadest IS NOT INITIAL.
      PERFORM tms_get_history_rfc USING ls_list_sys
                               CHANGING lt_log.
    ELSE.
      CALL FUNCTION 'TMS_TM_GET_HISTORY'
        EXPORTING
          iv_system     = ls_list_sys-system
          iv_domain     = p_doma
*         IV_ALLCLI     = 'X'
*         IV_TRCLI      =
*         IV_TRFUNCTION =
*         IV_PROJECT    =
          iv_imports    = lv_imp
          iv_exports    = lv_exp
*         IV_ALL_STEPS  =
*         IV_ALL_ALOG_STEPS =
*         IV_TPSTAT_KEY =
*         IV_MONITOR    = 'X'
        IMPORTING
*         EV_ALOG_LINENR    =
          et_tmstpalog  = lt_log
*         ES_EXCEPTION  =
        CHANGING
          cv_start_date = iv_datefrom
*         cv_start_time = gv_timefrom
          cv_end_date   = iv_dateto
*         cv_end_time   = gv_timeto
        EXCEPTIONS
          alert         = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE 'W' NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        CONTINUE.
      ENDIF.
    ENDIF.
    CLEAR lv_seq.
    SORT lt_log BY trtime trkorr.

    LOOP AT lt_log ASSIGNING <fs_log>.

      CLEAR: ls_extract.

      ADD 1 TO lv_seq.

      IF <fs_log>-trkorr NOT IN s_tkorr.
        CONTINUE.
      ENDIF.

*      MOVE-CORRESPONDING <fs_log> TO gs_list.
      PERFORM feed_extract USING ls_list_sys-idx
                                 lv_seq
                                 <fs_log>
                        CHANGING ot_alv.

    ENDLOOP.

    IF ls_list_sys-idx EQ 0.
      SORT ot_alv BY trkorr_exp_src.
      DELETE ADJACENT DUPLICATES FROM ot_alv COMPARING trkorr_exp_src.
    ENDIF.
*    EXIT.
  ENDLOOP.

ENDFORM.                    " EXTRACT_TMS
FORM calc_project USING iv_val TYPE char10
                CHANGING ov_proj TYPE tr_extpid.

  STATICS: gt_ctsproj TYPE TABLE OF ctsproject.

  IF gt_ctsproj IS INITIAL.
    SELECT * FROM ctsproject INTO TABLE gt_ctsproj.
    SORT gt_ctsproj BY trkorr.
  ENDIF.

  READ TABLE gt_ctsproj INTO DATA(ls_proj)
            WITH KEY trkorr = iv_val
            BINARY SEARCH.

  IF sy-subrc EQ 0.
    ov_proj = ls_proj-externalid.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  feed_extract
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_IDX     text
*      -->IV_SEQ     text
*      -->IV_LINE    text
*----------------------------------------------------------------------*
FORM feed_extract  USING iv_idx TYPE ty_list_sys-idx
                         iv_seq TYPE ty_seq
                         iv_line TYPE tmstpalog
                CHANGING ot_alv TYPE tyt_alv.

  DATA: ls_extract LIKE LINE OF ot_alv.
  DATA: lv_struct(50) TYPE c VALUE 'LS_EXTRACT-SYST'.
  DATA: tabix_extract TYPE i.

  FIELD-SYMBOLS: <syst_from> TYPE ty_syst_suivi,
                 <syst_to>   TYPE ty_syst_suivi,
                 <e>         TYPE ty_log..

  PERFORM init_extract_line CHANGING ls_extract.

  IF iv_idx EQ 0.
    ls_extract-idx_exp_src = iv_seq.
    ls_extract-trkorr_exp_src = iv_line-trkorr.
    ls_extract-as4text = iv_line-as4text.
    PERFORM calc_project USING iv_line-project+10(10) CHANGING ls_extract-project.
    INSERT ls_extract INTO TABLE ot_alv.
  ELSE.

    CONCATENATE lv_struct iv_idx INTO lv_struct.

    CLEAR ls_extract.
    READ TABLE ot_alv INTO ls_extract
                          WITH KEY trkorr_exp_src = iv_line-trkorr
                          BINARY SEARCH.
    IF sy-subrc EQ 0.
      tabix_extract = sy-tabix.

      ASSIGN (lv_struct) TO <syst_from>.

      IF <syst_from> IS ASSIGNED.
        <syst_from>-date = iv_line-trtime+0(8).

        <syst_from>-time = <fs_log>-trtime+8(6).
        ADD 1 TO <syst_from>-cnt.

        IF <syst_from>-first EQ 99999.
          <syst_from>-first = iv_seq.
        ENDIF.
        <syst_from>-last = iv_seq.

        MODIFY ot_alv FROM ls_extract INDEX tabix_extract.

        ADD 1 TO tabix_extract.
        LOOP AT ot_alv INTO ls_extract FROM tabix_extract.
          IF ls_extract-trkorr_exp_src NE iv_line-trkorr.
            EXIT.
          ENDIF.
          ASSIGN (lv_struct) TO <syst_to>.

          IF <syst_to> IS ASSIGNED.
            MOVE-CORRESPONDING <syst_from> TO <syst_to>.
            MODIFY ot_alv FROM ls_extract INDEX sy-tabix.
          ENDIF.
          UNASSIGN <syst_to>.
        ENDLOOP.
      ENDIF.
      UNASSIGN <syst_from>.
    ENDIF.
  ENDIF.
ENDFORM.                    " feed_extract
*&---------------------------------------------------------------------*
*&      Form  init_extract_line
*&---------------------------------------------------------------------*
FORM init_extract_line  CHANGING ov_line TYPE ty_alv.

  MOVE: 99999 TO ov_line-syst1-first,
        99999 TO ov_line-syst2-first,
        99999 TO ov_line-syst3-first,
        99999 TO ov_line-syst4-first,
        99999 TO ov_line-syst5-first,
        99999 TO ov_line-syst6-first,
        99999 TO ov_line-syst7-first,
        99999 TO ov_line-syst8-first,
        99999 TO ov_line-syst9-first,
        99999 TO ov_line-syst1-last,
        99999 TO ov_line-syst2-last,
        99999 TO ov_line-syst3-last,
        99999 TO ov_line-syst4-last,
        99999 TO ov_line-syst5-last,
        99999 TO ov_line-syst6-last,
        99999 TO ov_line-syst7-last,
        99999 TO ov_line-syst8-last,
        99999 TO ov_line-syst9-last.

ENDFORM.                    " init_extract_line
*---------------------------------------------------------------------*
*       FORM EXIT_PROGRAM                                             *
*---------------------------------------------------------------------*
FORM exit_screen.
  LEAVE TO SCREEN 0.
ENDFORM.                    "exit_program
*&---------------------------------------------------------------------*
*&      Form  COLORIZE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_ALV  text
*----------------------------------------------------------------------*
FORM colorize  CHANGING ot_alv TYPE tyt_alv.

  FIELD-SYMBOLS: <alv> LIKE LINE OF ot_alv.
  DATA: lt_coll     TYPE slis_t_specialcol_alv,
        ls_coll     LIKE LINE OF lt_coll,
        ls_list_sys LIKE LINE OF gt_list_sys,

        ls_alv_old  LIKE LINE OF ot_alv.

  CLEAR ls_alv_old.

  LOOP AT ot_alv ASSIGNING <alv>.
    CLEAR: ls_coll, lt_coll.

*IDX_EXP_SRC
*TRKORR_EXP_SRC
*AS4TEXT

    ls_coll-nokeycol  = 'X'.
    ls_coll-color-col = 2.
    ls_coll-color-int = 0.
    ls_coll-color-inv = 0.
    APPEND ls_coll TO lt_coll.

    IF <alv>-comment NE 0.
      ls_coll-fieldname = 'COMMENT'.
      ls_coll-color-col = 3.
      ls_coll-color-int = 0.
      ls_coll-color-inv = 0.
      APPEND ls_coll TO lt_coll.
    ENDIF.

    LOOP AT gt_list_sys INTO ls_list_sys.
      CHECK ls_list_sys-idx NE 0.

      PERFORM colorize_syst USING ls_list_sys-idx
                                  <alv> ls_alv_old
                         CHANGING lt_coll.

    ENDLOOP.


    <alv>-t_color = lt_coll.
    ls_alv_old = <alv>.
  ENDLOOP.


ENDFORM.                    " COLORIZE
*&---------------------------------------------------------------------*
*&      Form  COLORIZE_SYST
*&---------------------------------------------------------------------*
FORM colorize_syst  USING    iv_indx    TYPE ty_indx
                             is_alv     TYPE ty_alv
                             is_alv_old TYPE ty_alv
                    CHANGING ot_coll    TYPE slis_t_specialcol_alv.

  DATA: ls_coll      LIKE LINE OF ot_coll,
        lv_field(30) TYPE c,
        lf_notransp  TYPE flag,
        lv_def_color TYPE i.

  FIELD-SYMBOLS: <val>     TYPE i,
                 <val_old> TYPE i.


  ls_coll-nokeycol  = 'X'.
  IF iv_indx MOD 2 EQ 0.
    lv_def_color = 4.
  ELSE.
    lv_def_color = 7.
  ENDIF.
  ls_coll-color-col = lv_def_color.
  ls_coll-color-int = 0.
  ls_coll-color-inv = 0.

*  ls_coll-fieldname = 'SYST*-CNT'.
  CONCATENATE 'SYST' iv_indx '-CNT' INTO ls_coll-fieldname.
  CONDENSE ls_coll-fieldname NO-GAPS.

  CONCATENATE 'IS_ALV-' ls_coll-fieldname INTO lv_field.
  CONDENSE lv_field NO-GAPS.

  UNASSIGN <val>.
  ASSIGN (lv_field) TO <val>.

  IF <val> IS ASSIGNED.
    IF <val> > 1.
      ls_coll-color-col = 3.
    ENDIF.
    IF <val> EQ 0.
      lf_notransp = abap_true.
      ls_coll-color-inv = 1.
      ls_coll-color-col = lv_def_color = 4.
    ENDIF.
  ENDIF.
  APPEND ls_coll TO ot_coll.
  IF lf_notransp EQ abap_false.
    ls_coll-color-col = lv_def_color.
  ENDIF.
*    ls_coll-fieldname = 'SYST*-DATE'.
  CONCATENATE 'SYST' iv_indx '-DATE' INTO ls_coll-fieldname.
  CONDENSE ls_coll-fieldname NO-GAPS.
  APPEND ls_coll TO ot_coll.

*  ls_coll-fieldname = 'SYST*-TIME'.
  CONCATENATE 'SYST' iv_indx '-TIME' INTO ls_coll-fieldname.
  CONDENSE ls_coll-fieldname NO-GAPS.
  APPEND ls_coll TO ot_coll.

*  ls_coll-fieldname = 'SYST*-FIRST'.
  CONCATENATE 'SYST' iv_indx '-FIRST' INTO ls_coll-fieldname.
  CONDENSE ls_coll-fieldname NO-GAPS.

  CONCATENATE 'IS_ALV-' ls_coll-fieldname INTO lv_field.
  CONDENSE lv_field NO-GAPS.

  UNASSIGN <val>.
  ASSIGN (lv_field) TO <val>.

  CONCATENATE 'IS_ALV_OLD-' ls_coll-fieldname INTO lv_field.
  CONDENSE lv_field NO-GAPS.

  UNASSIGN <val_old>.
  ASSIGN (lv_field) TO <val_old>.

  IF <val> IS ASSIGNED AND <val_old> IS ASSIGNED.
    IF <val> LT <val_old>.
      ls_coll-color-col = 6.
    ENDIF.
  ENDIF.
  APPEND ls_coll TO ot_coll.
  IF lf_notransp EQ abap_false.
    ls_coll-color-col = lv_def_color.
  ENDIF.

*  ls_coll-fieldname = 'SYST*-LAST'.
  CONCATENATE 'SYST' iv_indx '-LAST' INTO ls_coll-fieldname.
  CONDENSE ls_coll-fieldname NO-GAPS.

  CONCATENATE 'IS_ALV-' ls_coll-fieldname INTO lv_field.
  CONDENSE lv_field NO-GAPS.

  UNASSIGN <val>.
  ASSIGN (lv_field) TO <val>.

  CONCATENATE 'IS_ALV_OLD-' ls_coll-fieldname INTO lv_field.
  CONDENSE lv_field NO-GAPS.

  UNASSIGN <val_old>.
  ASSIGN (lv_field) TO <val_old>.
  IF <val> IS ASSIGNED AND <val_old> IS ASSIGNED.
    IF <val> LT <val_old>.
      ls_coll-color-col = 6.
    ENDIF.
  ENDIF.
  APPEND ls_coll TO ot_coll.

ENDFORM.                    " COLORIZE_SYST
*&---------------------------------------------------------------------*
*&      Form  START_DATE_RFC
*&---------------------------------------------------------------------*
FORM start_date_rfc  CHANGING ov_datefrom TYPE datum.

  DATA: ls_tmslog LIKE LINE OF gt_tmslog.

  SELECT * FROM zacntmslog INTO TABLE gt_tmslog
           WHERE syst EQ p_syst.

  SORT gt_tmslog BY trtime DESCENDING.
  READ TABLE gt_tmslog INTO ls_tmslog INDEX 1.

  ov_datefrom = ls_tmslog-trtime+0(8).

ENDFORM.                    " START_DATE_RFC
*&---------------------------------------------------------------------*
*&      Form  TMS_GET_HISTORY_RFC
*&---------------------------------------------------------------------*
FORM tms_get_history_rfc  USING    is_list TYPE ty_list_sys
                          CHANGING ot_log TYPE tyt_log.

  DATA: lt_log TYPE tyt_log.


  IF is_list-datefrom IS INITIAL.
    is_list-datefrom = gv_datefrom.
  ENDIF.
  IF is_list-rfcdest IS NOT INITIAL.
    CALL FUNCTION 'ZACN_TMS_RFC' DESTINATION is_list-rfcdest
      EXPORTING
        iv_system     = is_list-system
        iv_domain     = is_list-domadest
        iv_imports    = 'X'
      IMPORTING
        et_tmstpalog  = lt_log
      CHANGING
        cv_start_date = is_list-datefrom
        cv_end_date   = gv_dateto
      EXCEPTIONS
        alert         = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE 'W' NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

  PERFORM change_log_rfc USING is_list
                       CHANGING lt_log.

  ot_log = lt_log.

ENDFORM.                    " TMS_GET_HISTORY_RFC
*&---------------------------------------------------------------------*
*&      Form  CHANGE_LOG_RFC
*&---------------------------------------------------------------------*
FORM change_log_rfc  USING is_list TYPE ty_list_sys
                  CHANGING ot_log TYPE tyt_log.

  DATA: ls_log_new LIKE LINE OF ot_log.
  DATA: ls_tmslog TYPE ty_tmslog.

  LOOP AT ot_log INTO ls_log_new.

    MOVE-CORRESPONDING ls_log_new TO ls_tmslog.
    ls_tmslog-syst = is_list-system.
    APPEND ls_tmslog TO gt_tmslog.
  ENDLOOP.

  SORT gt_tmslog.
  DELETE ADJACENT DUPLICATES FROM gt_tmslog
                             COMPARING syst listname trtime trkorr
                                       trcli trstep.
  MODIFY zacntmslog FROM TABLE gt_tmslog.


  CLEAR ot_log.
  LOOP AT gt_tmslog INTO ls_tmslog.
    IF is_list-system NE ls_tmslog-syst OR
       p_datef GT ls_tmslog-trtime+0(8).
      CONTINUE.
    ENDIF.

    MOVE-CORRESPONDING ls_tmslog TO ls_log_new.
    APPEND ls_log_new TO ot_log.
  ENDLOOP.

ENDFORM.                    " CHANGE_LOG_RFC
*&---------------------------------------------------------------------*
*&      Form  EXTRACT_LOG
*&---------------------------------------------------------------------*
FORM extract_log  CHANGING ot_alv TYPE tyt_alv.

  FIELD-SYMBOLS: <alv> LIKE LINE OF ot_alv.
  DATA: tabix_log TYPE i.
  DATA: ls_log LIKE LINE OF gt_trlog.
  DATA: count_comment TYPE i.

  CHECK ot_alv IS NOT INITIAL.

  CLEAR gt_trlog.
  SELECT * FROM zacntrlog INTO TABLE gt_trlog
           FOR ALL ENTRIES IN ot_alv
           WHERE trkorr EQ ot_alv-trkorr_exp_src.

  SORT gt_trlog BY trkorr erdat DESCENDING ernam DESCENDING.


  LOOP AT ot_alv ASSIGNING <alv>.

    READ TABLE gt_trlog WITH KEY trkorr = <alv>-trkorr_exp_src
                        TRANSPORTING NO FIELDS
                        BINARY SEARCH.

    IF sy-subrc EQ 0.
      tabix_log = sy-tabix.

      CLEAR count_comment.
      LOOP AT gt_trlog INTO ls_log FROM tabix_log.
        IF ls_log-trkorr NE <alv>-trkorr_exp_src.
          EXIT.
        ENDIF.
        ADD 1 TO count_comment.
      ENDLOOP.

      <alv>-comment = count_comment.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " EXTRACT_LOG
*&---------------------------------------------------------------------*
*&      Form  EXTRACT_LOG
*&---------------------------------------------------------------------*
FORM update_log  CHANGING ot_alv TYPE tyt_alv.

  FIELD-SYMBOLS: <alv> LIKE LINE OF ot_alv.
  DATA: tabix_log TYPE i.
  DATA: ls_log LIKE LINE OF gt_trlog.
  DATA: count_comment TYPE i.

  SORT gt_trlog BY trkorr erdat DESCENDING ernam DESCENDING.

  LOOP AT ot_alv ASSIGNING <alv>.

    READ TABLE gt_trlog WITH KEY trkorr = <alv>-trkorr_exp_src
                        TRANSPORTING NO FIELDS
                        BINARY SEARCH.

    CLEAR count_comment.
    IF sy-subrc EQ 0.
      tabix_log = sy-tabix.

      LOOP AT gt_trlog INTO ls_log FROM tabix_log.
        IF ls_log-trkorr NE <alv>-trkorr_exp_src.
          EXIT.
        ENDIF.
        ADD 1 TO count_comment.
      ENDLOOP.

      <alv>-comment = count_comment.
    ELSE.
      <alv>-comment = count_comment.
    ENDIF.
  ENDLOOP.

  PERFORM colorize
              CHANGING
                 ot_alv.

ENDFORM.                    " EXTRACT_LOG

*----------------------------------------------------------------------*
*       CLASS user_outbuf DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS user_unzip DEFINITION.

  PUBLIC SECTION.
    INTERFACES if_abap_ungzip_text_handler.

    CLASS-DATA : v_xstring TYPE xstring,
                 v_len     TYPE ty_trlog-len.

ENDCLASS.                    "user_outbuf DEFINITION

*----------------------------------------------------------------------*
*       CLASS user_outbuf IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS user_unzip IMPLEMENTATION.
  METHOD if_abap_ungzip_text_handler~use_out_buf.
    IF 1 = 2.

    ENDIF.
    v_len = out_buf_len.
    v_xstring = out_buf.
  ENDMETHOD.                    "if_abap_gzip_text_handler~use_out_buf
ENDCLASS.                    "user_outbuf IMPLEMENTATION

DATA: unref       TYPE REF TO user_unzip.
DATA: unzip TYPE REF TO cl_abap_ungzip_text_stream.
*&---------------------------------------------------------------------*
*&      Form  READ_TEXT
*&---------------------------------------------------------------------*
FORM read_text  USING    iv_len TYPE int4
                         iv_lraw TYPE cva_lraw
                CHANGING ov_text TYPE string.

  CREATE OBJECT unref.
  CREATE OBJECT unzip
    EXPORTING
      output_handler = unref.

*TRY.
  CALL METHOD unzip->set_out_buf
    IMPORTING
      out_buf = ov_text
*     out_buf_len =
    .
* CATCH cx_parameter_invalid_range .
*ENDTRY.

  TRY.
      CALL METHOD unzip->decompress_text_stream
        EXPORTING
          gzip_in     = iv_lraw
          gzip_in_len = iv_len.
    CATCH cx_parameter_invalid_range .
    CATCH cx_sy_conversion_codepage .
    CATCH cx_sy_compression_error .
  ENDTRY.


ENDFORM.                    " READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  SHOW_TEXT
*&---------------------------------------------------------------------*
FORM show_text  USING iv_guid TYPE ty_trlog-guid.

  gv_textmode = gc_textmode_show.

  CALL SCREEN 0400 STARTING AT 5 5.

ENDFORM.                    " SHOW_TEXT
*&---------------------------------------------------------------------*
*&      Form  DATE_FROM_ALV
*&---------------------------------------------------------------------*
FORM date_from_alv  USING    it_alv TYPE tyt_alv
                    CHANGING iv_datefrom TYPE datum
                             iv_dateto   TYPE datum.

  DATA: ls_alv LIKE LINE OF it_alv.

  iv_datefrom = sy-datum.

  LOOP AT it_alv INTO ls_alv.
    IF ls_alv-syst1-date < iv_datefrom.
      iv_datefrom = ls_alv-syst1-date.
    ENDIF.
  ENDLOOP.

  iv_dateto = gv_dateto.
ENDFORM.                    " DATE_FROM_ALV
*&---------------------------------------------------------------------*
*&      Form  REFRESH_TABALV
*&---------------------------------------------------------------------*
FORM refresh_tabalv  USING    it_alv TYPE tyt_alv
                     CHANGING ot_alv TYPE tyt_alv.


  FIELD-SYMBOLS: <alv> LIKE LINE OF ot_alv.
  DATA: ls_alv LIKE LINE OF it_alv.
  DATA: lt_celltab TYPE lvc_t_styl.

  SORT it_alv.
  LOOP AT ot_alv ASSIGNING <alv>.
    lt_celltab = <alv>-celltab.

    CLEAR ls_alv.
    READ TABLE it_alv INTO ls_alv
                      WITH KEY idx_exp_src = <alv>-idx_exp_src
                               trkorr_exp_src  = <alv>-trkorr_exp_src
                               BINARY SEARCH.
    IF sy-subrc EQ 0.
      <alv> = ls_alv.
      <alv>-celltab = lt_celltab.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " REFRESH_TABALV
