*&---------------------------------------------------------------------*
*&  Include           LZACNFRM
*&---------------------------------------------------------------------*
FORM progress_bar USING iv_texte iv_percentage.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = iv_percentage
      text       = iv_texte.

ENDFORM.                    "progress_bar
*&---------------------------------------------------------------------*
*&      Form  GET_E071
*&---------------------------------------------------------------------*
FORM get_e071_ot  USING it_trkorr TYPE zacn_t_trkorr
            CHANGING ot_e071   TYPE tyt_e071.

  DATA: lt_e071 TYPE tyt_e071.

  CHECK it_trkorr IS NOT INITIAL.

  SELECT * FROM e071 INTO CORRESPONDING FIELDS OF TABLE lt_e071
           FOR ALL ENTRIES IN it_trkorr
           WHERE trkorr EQ it_trkorr-trkorr.

  APPEND LINES OF lt_e071 TO ot_e071.
  SORT ot_e071.
ENDFORM.                                                    " GET_E071
*&---------------------------------------------------------------------*
*&      Form  FILTER_E071
*&---------------------------------------------------------------------*
*FORM filter_e071  USING iv_pgmid TYPE pgmid
*                        it_object TYPE tyr_obj_typ
*               CHANGING ot_e071 TYPE tyt_e071.
*
*  FIELD-SYMBOLS: <f> LIKE LINE OF ot_e071.
*
*  LOOP AT ot_e071 ASSIGNING <f>.
*    IF <f>-trkorr(2) NE sy-sysid(2) OR
*      <f>-pgmid      NE iv_pgmid    OR
*       <f>-object    NOT IN it_object.
*      DELETE ot_e071 INDEX sy-tabix.
*    ENDIF.
*  ENDLOOP.
*ENDFORM.                    " FILTER_E071
*&---------------------------------------------------------------------*
*&      Form  FILTER_E071
*&---------------------------------------------------------------------*
FORM filter_e071  USING it_object TYPE tyr_obj_typ
                        it_trkorr_range TYPE zacn_tyr_trkorr
               CHANGING ot_e071 TYPE tyt_e071.

  FIELD-SYMBOLS: <f> LIKE LINE OF ot_e071.

  LOOP AT ot_e071 ASSIGNING <f>.
    IF <f>-trkorr(2) NE sy-sysid(2) OR
       <f>-object    NOT IN it_object.
      DELETE ot_e071 INDEX sy-tabix.
    ENDIF.
  ENDLOOP.
  CHECK it_trkorr_range IS NOT INITIAL.

  DELETE ot_e071 WHERE trkorr NOT IN it_trkorr_range.

ENDFORM.                    " FILTER_E071
*&---------------------------------------------------------------------*
*&      Form  GET_E071
*&---------------------------------------------------------------------*
FORM get_e071_obj  USING it_obj TYPE tyt_trkorr_obj
                CHANGING ot_e071   TYPE tyt_e071.

  DATA: lt_e071 TYPE tyt_e071.

  CHECK it_obj IS NOT INITIAL.

  SELECT * FROM e071 INTO CORRESPONDING FIELDS OF TABLE lt_e071
           FOR ALL ENTRIES IN it_obj
           WHERE pgmid    EQ it_obj-pgmid
           AND   object   EQ it_obj-object
           AND   obj_name EQ it_obj-obj_name.

  APPEND LINES OF lt_e071 TO ot_e071.
  SORT ot_e071.
ENDFORM.                                                    " GET_E071
*&---------------------------------------------------------------------*
*&      Form  FILTER_CUSTO
*&---------------------------------------------------------------------*
FORM filter_custo  USING    it_obj  TYPE tyt_trkorr_obj
                            it_range TYPE zacn_tyr_trkorr
                   CHANGING ot_e071 TYPE tyt_e071.

  TYPES: tyr_objname TYPE RANGE OF tabname.
  DATA: lr_objname TYPE tyr_objname.
  DATA: ls_objname LIKE LINE OF lr_objname.

  DATA: lt_obj  TYPE tyt_trkorr_obj.
  DATA: lt_e071 TYPE tyt_e071.

  DATA: lt_e071k      TYPE TABLE OF e071k.
  DATA: lt_e071k_next TYPE TABLE OF e071k.

  DATA: ls_e071k LIKE LINE OF lt_e071k.
  DATA: ls_e071k_next LIKE LINE OF lt_e071k_next.

  DATA: lv_max(4) TYPE n.
  DATA: lv_max_next(4) TYPE n.
  DATA: lv_max_line(10) TYPE n.
  DATA: lr_keep TYPE tyr_trkorr.
  DATA: ls_keep LIKE LINE OF lr_keep.

  ls_objname = 'IEQ'.
  ls_objname-low = ''. INSERT ls_objname INTO TABLE lr_objname.
  ls_objname-low = 'AGR_TIMEB'. INSERT ls_objname INTO TABLE lr_objname.
*  ls_objname-low = 'SPERS_OBJ'. INSERT ls_objname INTO TABLE lr_objname.
  ls_objname-low = 'UCUW001'. INSERT ls_objname INTO TABLE lr_objname.
  ls_objname-low = 'UCUW011'. INSERT ls_objname INTO TABLE lr_objname.
  ls_objname-low = 'USMD2001'. INSERT ls_objname INTO TABLE lr_objname.
  ls_objname-low = 'USMD2003'. INSERT ls_objname INTO TABLE lr_objname.
  ls_objname-low = 'USR12'. INSERT ls_objname INTO TABLE lr_objname.
  ls_objname-low = 'USR13'. INSERT ls_objname INTO TABLE lr_objname.
  ls_objname-low = 'UST12'. INSERT ls_objname INTO TABLE lr_objname.
  ls_objname-low = 'SUSPR'. INSERT ls_objname INTO TABLE lr_objname.
  ls_objname-low = 'TKESV'. INSERT ls_objname INTO TABLE lr_objname.
  ls_objname-low = 'TKESW'. INSERT ls_objname INTO TABLE lr_objname.
  ls_objname-low = 'CCCFLOW'. INSERT ls_objname INTO TABLE lr_objname.
  ls_objname-low = 'USR10'. INSERT ls_objname INTO TABLE lr_objname.
  ls_objname-low = 'USR11'. INSERT ls_objname INTO TABLE lr_objname.
  ls_objname-low = 'UST10C'. INSERT ls_objname INTO TABLE lr_objname.
  ls_objname-low = 'UST10S'. INSERT ls_objname INTO TABLE lr_objname.

  lt_obj = it_obj.
  lt_e071 = ot_e071.
  DELETE lt_obj WHERE pgmid NE 'R3TR' OR
                     ( object NE 'TDAT' AND
                       object NE 'TABU'     ).

  DELETE lt_e071 WHERE pgmid NE 'R3TR' OR
                     ( object NE 'TDAT' AND
                       object NE 'TABU'     ).

  SORT: lt_obj BY trkorr,
        lt_e071 BY trkorr.

  DELETE ADJACENT DUPLICATES FROM lt_obj  COMPARING trkorr.
  DELETE ADJACENT DUPLICATES FROM lt_e071 COMPARING trkorr.

  CHECK lt_e071 IS NOT INITIAL.
  CHECK lt_obj  IS NOT INITIAL.

  DESCRIBE TABLE lt_obj LINES lv_max.

  SELECT * FROM e071k INTO TABLE lt_e071k
           FOR ALL ENTRIES IN lt_obj
           WHERE trkorr EQ lt_obj-trkorr.

  SELECT * FROM e071k INTO TABLE lt_e071k_next
           FOR ALL ENTRIES IN lt_e071
           WHERE trkorr EQ lt_e071-trkorr.

  DELETE lt_e071k WHERE objname IN lr_objname.
  DELETE lt_e071k_next WHERE objname IN lr_objname.

  SORT: lt_e071k_next BY pgmid object objname,
        lt_e071k.

  DESCRIBE TABLE lt_e071k_next LINES lv_max_next.

  DATA: lv_keep TYPE flag.
  DATA: tabix_e071k TYPE sytabix.
  DATA: lv_text(255) TYPE c.

  ls_keep = 'IEQ'.
  LOOP AT lt_e071k INTO ls_e071k.

    READ TABLE lt_e071k_next WITH KEY pgmid = ls_e071k-pgmid
                                 object = ls_e071k-object
                                 objname = ls_e071k-objname
                                 TRANSPORTING NO FIELDS
                                 BINARY SEARCH.
    IF sy-subrc EQ 0.
      tabix_e071k = sy-tabix.
      LOOP AT lt_e071k_next INTO ls_e071k_next FROM tabix_e071k.
        IF ls_e071k-pgmid NE ls_e071k_next-pgmid OR
           ls_e071k-object NE ls_e071k_next-object OR
           ls_e071k-objname NE ls_e071k_next-objname.
          EXIT.
        ENDIF.


        IF ls_e071k-tabkey EQ ls_e071k_next-tabkey.
          ls_keep-low = ls_e071k_next-trkorr.
          COLLECT ls_keep INTO lr_keep.
        ENDIF.

        CONCATENATE 'Dép. Custo :'
                    lv_max '/' lv_max_line INTO lv_text.

*        PERFORM progress_bar USING lv_text '55'.
        SUBTRACT 1 FROM lv_max_line.
      ENDLOOP.
    ELSE.
      DELETE ot_e071 WHERE trkorr EQ ls_e071k-trkorr.
    ENDIF.

    AT END OF trkorr.
      SUBTRACT 1 FROM lv_max.
      lv_max_line = lv_max_next.
    ENDAT.

  ENDLOOP.

  DELETE ot_e071 WHERE trkorr NOT IN lr_keep.

ENDFORM.                     " FILTER_CUSTO
