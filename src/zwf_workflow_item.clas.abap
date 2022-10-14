CLASS zwf_workflow_item DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF t_attachment,
        file_name     TYPE string,
        data_xstring  TYPE xstring,
      END OF t_attachment .
    TYPES:
      t_attachment_tab TYPE STANDARD TABLE OF t_attachment WITH EMPTY KEY .

    CLASS-METHODS get_workflow_item_by_id
      IMPORTING
        !workflow_item_id         TYPE sww_wiid
      RETURNING
        VALUE(workflow_work_item) TYPE REF TO zwf_workflow_item .
    CLASS-METHODS get_workflow_item_by_object_id
      IMPORTING
        !object_por               TYPE sibflporb
      RETURNING
        VALUE(workflow_work_item) TYPE REF TO zwf_workflow_item .
    METHODS get_attachments
      RETURNING
        VALUE(attachment_tab) TYPE t_attachment_tab
      RAISING
        zcx_return3 .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA workflow_item_id TYPE sww_wiid .

    METHODS _get_attachment_file_name
      IMPORTING
        !object_header_tab TYPE swftlisti1
        !document_data     TYPE sofolenti1
      RETURNING
        VALUE(file_name)   TYPE string .
ENDCLASS.



CLASS ZWF_WORKFLOW_ITEM IMPLEMENTATION.


  METHOD get_attachments.

    "------------------------------------------
    "Get attachment links
    "------------------------------------------
    DATA return_code TYPE syst-subrc.
    DATA attachment_meta_data_list TYPE STANDARD TABLE OF swr_object.

    CALL FUNCTION 'SAP_WAPI_GET_ATTACHMENTS'
      EXPORTING
        workitem_id = workflow_item_id
      IMPORTING
        return_code = return_code
      TABLES
        attachments = attachment_meta_data_list.

    "------------------------------------------
    "Loop at attachments
    "------------------------------------------
    LOOP AT attachment_meta_data_list
      ASSIGNING FIELD-SYMBOL(<attachment_meta_data>).

      "------------------------------------------
      "Read attachment binary SOLIX
      "------------------------------------------
      DATA document_id TYPE sofolenti1-doc_id.
      document_id = <attachment_meta_data>-object_id+20(46).

      DATA(so_document) = zso_document=>get_so_document( document_id ).

      DATA(document_data) = so_document->get_data( ).

      APPEND
        VALUE #(
          file_name          = document_data-file_name
          data_xstring       = document_data-data_xstring )
        TO attachment_tab.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_workflow_item_by_id.

    workflow_work_item = NEW #( ).

    workflow_work_item->workflow_item_id = workflow_item_id.

  ENDMETHOD.


  METHOD get_workflow_item_by_object_id.

    DATA:
      return_code    TYPE syst-subrc,
      task_filter    TYPE STANDARD TABLE OF swr_task,
      worklist       TYPE STANDARD TABLE OF swr_wihdr,
      message_lines  TYPE STANDARD TABLE OF swr_messag,
      message_struct TYPE STANDARD TABLE OF swr_mstruc.

    CALL FUNCTION 'SAP_WAPI_WORKITEMS_TO_OBJECT'
      EXPORTING
        object_por               = object_por
        top_level_items          = ''
        selection_status_variant = 0000
        text                     = ''
        output_only_top_level    = 'X'
      IMPORTING
        return_code              = return_code
      TABLES
        task_filter              = task_filter
        worklist                 = worklist
        message_lines            = message_lines
        message_struct           = message_struct.


    IF worklist IS INITIAL.
      RETURN.
    ENDIF.

    "Get the newest workflow
    SORT worklist
     BY
       wi_cd DESCENDING
       wi_ct DESCENDING.

    DATA(work_item) = worklist[ 1 ].

    "Get workflow item instance
    workflow_work_item = get_workflow_item_by_id( work_item-wi_id ).

  ENDMETHOD.


  METHOD _get_attachment_file_name.

    LOOP AT object_header_tab
      ASSIGNING FIELD-SYMBOL(<object_header>).

      DATA length_13_text TYPE c LENGTH 13.
      length_13_text = <object_header>.

      IF length_13_text = '&SO_FILENAME='.

        file_name = <object_header>+13.
        RETURN.

      ENDIF.

    ENDLOOP.

    DATA file_extension TYPE string.

    CASE document_data-obj_type.
      WHEN 'JPE'.
        file_extension = 'jpg'.
      WHEN OTHERS.
        file_extension = to_lower( document_data-obj_type ).
    ENDCASE.

    file_name = |{ to_lower( document_data-obj_descr ) }.{ file_extension }|.

  ENDMETHOD.
ENDCLASS.
