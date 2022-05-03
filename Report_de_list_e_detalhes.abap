*&---------------------------------------------------------------------*
*& Report Z_TESTE_21
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_teste_21.
TABLES: bapisflkey, bapisfldra.


DATA: gt_details     TYPE TABLE OF ZTA05_FLIGH_DETAIL_ST, " Declarando tabela interna"
      wa_details     TYPE ZTA05_FLIGH_DETAIL_ST. " Declrando WorkArea "

DATA: gt_flight_list TYPE TABLE OF zbapisfldat,
      wa_flight_list TYPE zbapisfldat.

DATA: gt_flight_list_detail TYPE TABLE OF bapisfladd,
      wa_flight_list_detail TYPE bapisfladd.





SELECTION-SCREEN BEGIN OF BLOCK screen WITH FRAME.

PARAMETERS:    p_id TYPE bapisflkey-airlineid MODIF ID s2.
SELECT-OPTIONS: s_date FOR bapisfldra-low MODIF ID s1.

SELECTION-SCREEN END OF BLOCK screen.


SELECTION-SCREEN BEGIN OF BLOCK screen2 WITH FRAME.

PARAMETERS: rb1 RADIOBUTTON GROUP rad USER-COMMAND invisible DEFAULT 'X', " rb1 = radio button de lista"
            rb2 RADIOBUTTON GROUP rad. " rb2 = radio button de detalhes"
SELECTION-SCREEN END OF BLOCK screen2.

AT SELECTION-SCREEN OUTPUT.
  PERFORM visivel_invisivel.

INITIALIZATION.


START-OF-SELECTION.
  IF rb1 = 'X'.
    PERFORM seleciona_dados.
  ELSE.
    PERFORM seleciona_dados.
    LOOP  AT gt_flight_list INTO wa_flight_list.
      PERFORM seleciona_dados_details.
    ENDLOOP.
  ENDIF.

END-OF-SELECTION.
IF rb1 = abap_true.
  PERFORM: exibir_lista_alv.
  ELSE.
  PERFORM: exibir_details_alv.
    ENDIF.

FORM visivel_invisivel.
  LOOP AT SCREEN.
    IF rb1 = 'X'.
      IF screen-group1 = 's1'.
        screen-invisible  = 1.
        screen-input     = 0.
        screen-active    = 0.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

      IF screen-group1 = 'S1'.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

    ELSE.

      IF screen-group1 = 'S1'.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.


      IF screen-group1 = 'S2'.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM seleciona_dados.

*MESSAGE S000(zdev) DISPLAY LIKE 'S'.

* Busca Lista de Voo
  CALL FUNCTION 'BAPI_FLIGHT_GETLIST'
    EXPORTING
      airline     = p_id
    TABLES
      date_range  = s_date
      flight_list = gt_flight_list.
*     return      = gt_return.

*  PERFORM set_status_flight.
ENDFORM.

FORM seleciona_dados_details.

  DATA wa_additional_info TYPE bapisfladd.

  CALL FUNCTION 'BAPI_FLIGHT_GETDETAIL'
    EXPORTING
      airlineid       = wa_flight_list-airlineid
      connectionid    = wa_flight_list-connectid
      flightdate      = wa_flight_list-flightdate
    IMPORTING
*     FLIGHT_DATA     = gt_flight_list
      additional_info = wa_additional_info.
*       AVAILIBILITY     = av.


Move:wa_flight_list-airlineid             to wa_details-airlineid.
Move:wa_flight_list-connectid             to wa_details-connectionid.
Move:wa_flight_list-flightdate            to wa_details-flightate.
Move-CORRESPONDING wa_additional_info to wa_details.
Append wa_details to gt_details.

ENDFORM.

*PERFORM exibir_lista_alv.
FORM exibir_lista_alv.
  CONSTANTS c_table TYPE dd02l-tabname VALUE 'BAPISFLDAT'. "Vai exportar a tabela nessa linha no caso aqui a BAPIFLDAT"

  DATA: lt_fieldcat TYPE lvc_t_fcat, " lvc_t_fcat : Um tipo tabela de uma estrutura"
        wa_layout   TYPE lvc_s_layo, " Jogando os componentes standard  da estrutura em uma work area para podermos modificar o layout da nossa alv "
        wa_fieldcat TYPE LINE OF lvc_t_fcat. " a work area fieldcat do tipo linha da tabela (pega a estrutura) "

*    Ajusta layout
  wa_layout-zebra      = 'X'. " Definindo um layout zebra na nossa alv, com a wa que definimos logo acima "
*  wa_layout-edit       = 'X'.
*  wa_layout-no_hgridln = 'X'.
*
*  wa_layout-sel_mode   = 'C'.
*  wa_layout-box_fname  = 'BOX'.

* Obtém estrutura do report
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE' " Chamando a função "
    EXPORTING
      i_structure_name       = c_table " Estou exportando essa estrutura com os dados da c_table para a função"
    CHANGING
      ct_fieldcat            = lt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
*    PERFORM ajusta_fieldcat TABLES lt_fieldcat.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     i_callback_program       = sy-repid
      is_layout_lvc   = wa_layout
*     i_callback_pf_status_set = 'F_SET_PF_STATUS'
*     i_callback_user_command  = 'F_USER_COMMAND'
      it_fieldcat_lvc = lt_fieldcat " lt_fieldcat : Estrutura do relatório como se fosse o esqueleto e o gt_flight_list é o corpo"
    TABLES
      t_outtab        = gt_flight_list " gt_flight_list Dados do relatório"
    EXCEPTIONS
      program_error   = 1
      OTHERS          = 2.
ENDFORM.

Form exibir_details_alv.
  CONSTANTS c_table2 TYPE dd02l-tabname VALUE 'ZTA05_FLIGH_DETAIL_ST'.

  DATA: lt_fieldcat TYPE lvc_t_fcat, " lvc_t_fcat : Um tipo tabela de uma estrutura"
        wa_layout   TYPE lvc_s_layo, " Jogando os componentes standard  da estrutura em uma work area para podermos modificar o layout da nossa alv "
        wa_fieldcat TYPE LINE OF lvc_t_fcat. " a work area fieldcat do tipo linha da tabela (pega a estrutura) "
*Read table gt_details ASSIGNING FIELD-SYMBOL(<lfs_details>) index 1.

*    Ajusta layout
  wa_layout-zebra      = 'X'. " Definindo um layout zebra na nossa alv, com a wa que definimos logo acima "
*  wa_layout-edit       = 'X'.
*  wa_layout-no_hgridln = 'X'.
*
*  wa_layout-sel_mode   = 'C'.
*  wa_layout-box_fname  = 'BOX'.

* Obtém estrutura do report
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE' " Chamando a função "
    EXPORTING
      i_structure_name       = c_table2 " Estou exportando essa estrutura com os dados da c_table para a função"
    CHANGING
      ct_fieldcat            = lt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
*    PERFORM ajusta_fieldcat TABLES lt_fieldcat.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     i_callback_program       = sy-repid
      is_layout_lvc   = wa_layout
*     i_callback_pf_status_set = 'F_SET_PF_STATUS'
*     i_callback_user_command  = 'F_USER_COMMAND'
      it_fieldcat_lvc = lt_fieldcat " lt_fieldcat : Estrutura do relatório como se fosse o esqueleto e o gt_flight_list é o corpo"
    TABLES
      t_outtab        = gt_details " gt_flight_list Dados do relatório"
    EXCEPTIONS
      program_error   = 1
      OTHERS          = 2.




  ENDFORM.