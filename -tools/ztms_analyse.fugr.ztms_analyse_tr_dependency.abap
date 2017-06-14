FUNCTION ZTMS_ANALYSE_TR_DEPENDENCY.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_TRKORR) TYPE  ZACN_T_TRKORR
*"     REFERENCE(IT_TRKORR_RANGE) TYPE  ZACN_TYR_TRKORR OPTIONAL
*"  EXPORTING
*"     REFERENCE(OT_TRKORR) TYPE  ZACN_T_TRKORR
*"--------------------------------------------------------------------
  DATA: lt_e071 TYPE tyt_e071,
        lt_e071_next TYPE tyt_e071,
        ls_e071 LIKE LINE OF lt_e071.
  DATA: lr_filter_obj TYPE tyr_obj_typ,
        ls_filter_obj LIKE LINE OF lr_filter_obj,
        lt_obj TYPE tyt_trkorr_obj,
        lt_obj_next TYPE tyt_trkorr_obj,
        ls_obj LIKE LINE OF lt_obj,
        ls_out LIKE LINE OF ot_trkorr,
        lt_trkorr_next TYPE zacn_t_trkorr,
        ls_trkorr LIKE LINE OF lt_trkorr_next,
        lv_tabix TYPE sy-tabix.

* On ne garde pas tous les objets dans la liste
*DOMA / DTEL / PROG / TABL / TTYP
  CLEAR lr_filter_obj.
  ls_filter_obj-sign = 'I'.
  ls_filter_obj-option = 'EQ'.
  ls_filter_obj-low = 'DOMA'.
  INSERT ls_filter_obj INTO TABLE lr_filter_obj.
  ls_filter_obj-low = 'DTEL'.
  INSERT ls_filter_obj INTO TABLE lr_filter_obj.
  ls_filter_obj-low = 'PROG'.
  INSERT ls_filter_obj INTO TABLE lr_filter_obj.
  ls_filter_obj-low = 'TABL'.
  INSERT ls_filter_obj INTO TABLE lr_filter_obj.
  ls_filter_obj-low = 'TTYP'.
  INSERT ls_filter_obj INTO TABLE lr_filter_obj.

  ls_filter_obj-low = 'ACGR'.
  INSERT ls_filter_obj INTO TABLE lr_filter_obj.
  ls_filter_obj-low = 'TABU'.
  INSERT ls_filter_obj INTO TABLE lr_filter_obj.

  ls_filter_obj-low = 'TDAT'.
  INSERT ls_filter_obj INTO TABLE lr_filter_obj.
  ls_filter_obj-low = 'REPS'.
  INSERT ls_filter_obj INTO TABLE lr_filter_obj.

  ls_filter_obj-low = 'METH'.
  INSERT ls_filter_obj INTO TABLE lr_filter_obj.
  ls_filter_obj-low = 'FUNC'.
  INSERT ls_filter_obj INTO TABLE lr_filter_obj.
  ls_filter_obj-low = 'CLAS'.
  INSERT ls_filter_obj INTO TABLE lr_filter_obj.
  ls_filter_obj-low = 'SXCI'.
  INSERT ls_filter_obj INTO TABLE lr_filter_obj.

* Récupération objets des OTs
  PERFORM progress_bar USING text-i01 1.
  PERFORM get_e071_ot USING it_trkorr
                   CHANGING lt_e071.


  SORT lt_e071.
* Récupération des OTs à partir des objets
  PERFORM progress_bar USING text-i02 45.

  CLEAR lt_obj.
  LOOP AT lt_e071 INTO ls_e071.
    MOVE-CORRESPONDING ls_e071 TO ls_obj.
    APPEND ls_obj TO lt_obj.
    IF ls_obj-pgmid EQ 'R3TR' AND
       ls_obj-object EQ 'PROG'.
      ls_obj-pgmid = 'LIMU'.
      ls_obj-object = 'REPS'.
      APPEND ls_obj TO lt_obj.
    ENDIF.
    IF ls_obj-pgmid EQ 'LIMU' AND
       ls_obj-object EQ 'REPS'.
      ls_obj-pgmid = 'R3TR'.
      ls_obj-object = 'PROG'.
      APPEND ls_obj TO lt_obj.
    ENDIF.
  ENDLOOP.
  SORT lt_obj.
  DELETE ADJACENT DUPLICATES FROM lt_obj COMPARING ALL FIELDS.

  CLEAR lt_e071_next.
  lt_obj_next = lt_obj.

  DO.
    PERFORM get_e071_obj USING lt_obj_next
                      CHANGING lt_e071_next.

*    PERFORM filter_e071 USING 'R3TR'
*                              lr_filter_obj
*                     CHANGING lt_e071_next.
    PERFORM filter_e071 USING lr_filter_obj it_trkorr_range
                     CHANGING lt_e071_next.

    PERFORM filter_custo USING lt_obj_next it_trkorr_range
                      CHANGING lt_e071_next.

    LOOP AT lt_e071_next INTO ls_e071.
      lv_tabix = sy-tabix.
      READ TABLE lt_e071 WITH KEY trkorr = ls_e071-trkorr
                         TRANSPORTING NO FIELDS
                         BINARY SEARCH.
      IF sy-subrc EQ 0.
        DELETE lt_e071_next INDEX lv_tabix.
      ENDIF.
    ENDLOOP.
    IF lt_e071_next IS INITIAL.
      EXIT.
    ELSE.

      CLEAR lt_obj_next.
      APPEND LINES OF lt_e071_next TO lt_e071.
      SORT lt_e071.
      CLEAR lt_trkorr_next.
      LOOP AT lt_e071_next INTO ls_e071.
        MOVE-CORRESPONDING ls_e071 TO ls_trkorr.
        INSERT ls_trkorr INTO TABLE lt_trkorr_next.
      ENDLOOP.
      SORT lt_trkorr_next.
      DELETE ADJACENT DUPLICATES FROM lt_trkorr_next.

      PERFORM get_e071_ot USING lt_trkorr_next
                       CHANGING lt_e071_next.

*      PERFORM filter_e071 USING 'R3TR'
*                                lr_filter_obj
*                       CHANGING lt_e071_next.
      PERFORM filter_e071 USING lr_filter_obj it_trkorr_range
                       CHANGING lt_e071_next.

      SORT lt_e071_next.
*      DELETE ADJACENT DUPLICATES FROM lt_e071_next COMPARING trkorr.
* Récupération des OTs à partir des objets
      PERFORM progress_bar USING text-i02 45.

      CLEAR lt_obj_next.
      LOOP AT lt_e071_next INTO ls_e071.
        MOVE-CORRESPONDING ls_e071 TO ls_obj.
        APPEND ls_obj TO lt_obj_next.
        IF ls_obj-pgmid EQ 'R3TR' AND
           ls_obj-object EQ 'PROG'.
          ls_obj-pgmid = 'LIMU'.
          ls_obj-object = 'REPS'.
          APPEND ls_obj TO lt_obj_next.
        ENDIF.
        IF ls_obj-pgmid EQ 'LIMU' AND
           ls_obj-object EQ 'REPS'.
          ls_obj-pgmid = 'R3TR'.
          ls_obj-object = 'PROG'.
          APPEND ls_obj TO lt_obj_next.
        ENDIF.

      ENDLOOP.
      SORT lt_obj_next.
      DELETE ADJACENT DUPLICATES FROM lt_obj_next COMPARING ALL FIELDS.

      LOOP AT lt_obj_next INTO ls_obj.
        lv_tabix = sy-tabix.
        READ TABLE lt_obj WITH KEY pgmid = ls_obj-pgmid
                                   object = ls_obj-object
                                   obj_name = ls_obj-obj_name
                                   TRANSPORTING NO FIELDS
                                   BINARY SEARCH.
        IF sy-subrc NE 0.
          IF ls_obj-pgmid EQ 'R3TR' AND
             ls_obj-object EQ 'PROG'.
            READ TABLE lt_obj WITH KEY pgmid = 'LIMU'
                                       object = 'REPS'
                                       obj_name = ls_obj-obj_name
                                       TRANSPORTING NO FIELDS
                                       BINARY SEARCH.
          ENDIF.
          IF ls_obj-pgmid EQ 'LIMU' AND
             ls_obj-object EQ 'REPS'.
            READ TABLE lt_obj WITH KEY pgmid = 'R3TR'
                                       object = 'PROG'
                                       obj_name = ls_obj-obj_name
                                       TRANSPORTING NO FIELDS
                                       BINARY SEARCH.
          ENDIF.

        ENDIF.
        IF sy-subrc EQ 0.
          DELETE lt_obj_next INDEX lv_tabix.
        ENDIF.
      ENDLOOP.
      APPEND LINES OF lt_obj_next TO lt_obj.
      SORT lt_obj.
      CLEAR lt_e071_next.
    ENDIF.
    EXIT. " 1 seule occurence pour le moment
  ENDDO.

  SORT lt_e071.
  DELETE ADJACENT DUPLICATES FROM lt_e071 COMPARING trkorr.

  LOOP AT lt_e071 INTO ls_e071.
    MOVE-CORRESPONDING ls_e071 TO ls_out.
    APPEND ls_out TO ot_trkorr.
  ENDLOOP.
ENDFUNCTION.
