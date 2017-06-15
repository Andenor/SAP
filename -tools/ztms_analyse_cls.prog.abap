*&---------------------------------------------------------------------*
*&  Include           ZTMS_ANALYSE_CLS
*&---------------------------------------------------------------------*
CLASS lcl_tms_analyse DEFINITION.
  PUBLIC SECTION.

    TYPES:
      BEGIN OF typ_s_trkorr,
        trkorr TYPE trkorr,
      END OF typ_s_trkorr,
      typ_t_trkorr   TYPE STANDARD TABLE OF typ_s_trkorr,

      typ_r_trkorr   TYPE RANGE OF trkorr,
      typ_s_range_tr TYPE LINE OF typ_r_trkorr,

      typ_t_e071     TYPE TABLE OF e071,
      typ_r_objtyp   TYPE RANGE OF trobjtype,

      BEGIN OF typ_s_trkorr_obj,
        pgmid    TYPE pgmid,
        object   TYPE trobjtype,
        obj_name TYPE trobj_name,
        trkorr   TYPE trkorr,
      END OF typ_s_trkorr_obj,
      typ_t_trkorr_obj TYPE TABLE OF typ_s_trkorr_obj
      .


    CLASS-METHODS:

      tr_dependency
        IMPORTING
          it_trkorr   TYPE typ_t_trkorr
          it_range_tr TYPE typ_r_trkorr
        EXPORTING
          ot_trkorr   TYPE typ_t_trkorr

        .
  PROTECTED SECTION.


  PRIVATE SECTION.

    CLASS-METHODS:
      _progress_bar
        IMPORTING
          iv_texte      TYPE any
          iv_percentage TYPE any,

      _get_e071_ot
        IMPORTING
          it_trkorr TYPE typ_t_trkorr
        CHANGING
          ct_e071   TYPE typ_t_e071,

      _get_e071_obj
        IMPORTING
          it_obj  TYPE typ_t_trkorr_obj
        CHANGING
          ct_e071 TYPE typ_t_e071,

      _filter_e071
        IMPORTING
          it_object TYPE typ_r_objtyp
          it_trkorr TYPE typ_r_trkorr
        CHANGING
          ct_e071   TYPE typ_t_e071,

      _filter_custo
        IMPORTING
          it_obj   TYPE typ_t_trkorr_obj
          it_range TYPE typ_r_trkorr
        CHANGING
          ct_e071  TYPE typ_t_e071
        .

ENDCLASS.
CLASS lcl_tms_analyse IMPLEMENTATION.
  METHOD _filter_custo.
    TYPES: tyr_objname TYPE RANGE OF tabname.
    DATA: lr_objname TYPE tyr_objname.
    DATA: ls_objname LIKE LINE OF lr_objname.

    DATA: lt_obj  TYPE typ_t_trkorr_obj.
    DATA: lt_e071 TYPE typ_t_e071.

    DATA: lt_e071k      TYPE TABLE OF e071k.
    DATA: lt_e071k_next TYPE TABLE OF e071k.

    DATA: ls_e071k LIKE LINE OF lt_e071k.
    DATA: ls_e071k_next LIKE LINE OF lt_e071k_next.

    DATA: lv_max(4) TYPE n.
    DATA: lv_max_next(4) TYPE n.
    DATA: lv_max_line(10) TYPE n.
    DATA: lr_keep TYPE typ_r_trkorr.
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
    lt_e071 = ct_e071.
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
        DELETE ct_e071 WHERE trkorr EQ ls_e071k-trkorr.
      ENDIF.

      AT END OF trkorr.
        SUBTRACT 1 FROM lv_max.
        lv_max_line = lv_max_next.
      ENDAT.

    ENDLOOP.

    DELETE ct_e071 WHERE trkorr NOT IN lr_keep.

  ENDMETHOD.
  METHOD _filter_e071.
    FIELD-SYMBOLS: <f> LIKE LINE OF ct_e071.

    LOOP AT ct_e071 ASSIGNING <f>.
      IF <f>-trkorr(2) NE sy-sysid(2) OR
         <f>-object    NOT IN it_object.
        DELETE ct_e071 INDEX sy-tabix.
      ENDIF.
    ENDLOOP.
    CHECK it_trkorr IS NOT INITIAL.

    DELETE ct_e071 WHERE trkorr NOT IN it_trkorr.

  ENDMETHOD.
  METHOD _get_e071_obj.

    DATA: lt_e071 TYPE typ_t_e071.

    CHECK it_obj IS NOT INITIAL.

    SELECT * FROM e071 INTO CORRESPONDING FIELDS OF TABLE lt_e071
             FOR ALL ENTRIES IN it_obj
             WHERE pgmid    EQ it_obj-pgmid
             AND   object   EQ it_obj-object
             AND   obj_name EQ it_obj-obj_name.

    APPEND LINES OF lt_e071 TO ct_e071.
    SORT ct_e071.

  ENDMETHOD.
  METHOD _get_e071_ot.

  ENDMETHOD.
  METHOD _progress_bar.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = iv_percentage
        text       = iv_texte.

  ENDMETHOD.

  METHOD tr_dependency.

    DATA: lt_e071      TYPE typ_t_e071,
          lt_e071_next LIKE lt_e071,
          ls_e071      LIKE LINE OF lt_e071.
    DATA: lr_filter_obj  TYPE typ_r_objtyp,
          ls_filter_obj  LIKE LINE OF lr_filter_obj,
          lt_obj         TYPE typ_t_trkorr_obj,
          lt_obj_next    LIKE lt_obj,
          ls_obj         LIKE LINE OF lt_obj,
          ls_out         LIKE LINE OF ot_trkorr,
          lt_trkorr_next TYPE typ_t_trkorr,
          ls_trkorr      LIKE LINE OF lt_trkorr_next,
          lv_tabix       TYPE sy-tabix.

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
    _progress_bar( EXPORTING iv_texte = TEXT-i01 iv_percentage = 1 ).
    _get_e071_ot( EXPORTING it_trkorr = it_trkorr CHANGING ct_e071 = lt_e071 ).

    SORT lt_e071.
* Récupération des OTs à partir des objets
    _progress_bar( EXPORTING iv_texte = TEXT-i02 iv_percentage = 45 ).

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

      _get_e071_obj( EXPORTING it_obj = lt_obj_next CHANGING ct_e071 = lt_e071_next ).
      _filter_e071( EXPORTING it_object = lr_filter_obj it_trkorr = it_range_tr CHANGING  ct_e071 = lt_e071_next ).
      _filter_custo( EXPORTING it_obj = lt_obj_next  it_range = it_range_tr CHANGING ct_e071 = lt_e071_next ).

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

        _get_e071_ot( EXPORTING it_trkorr = lt_trkorr_next CHANGING ct_e071 = lt_e071_next ).
        _filter_e071( EXPORTING it_object = lr_filter_obj it_trkorr = it_range_tr CHANGING  ct_e071 = lt_e071_next ).

        SORT lt_e071_next.

* Récupération des OTs à partir des objets
        _progress_bar( EXPORTING iv_texte = TEXT-i02 iv_percentage = 45 ).

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
  ENDMETHOD.
ENDCLASS.
