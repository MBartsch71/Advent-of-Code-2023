REPORT ymbh_aoc_2023_day_3.

INTERFACE grid_types.
  TYPES: BEGIN OF grid_line,
           row   TYPE i,
           col   TYPE i,
           value TYPE char1,
         END OF grid_line.
  TYPES input_grid TYPE SORTED TABLE OF grid_line WITH UNIQUE KEY primary_key COMPONENTS row col.

  TYPES: BEGIN OF digit_map_item,
           row   TYPE i,
           col   TYPE i,
           value TYPE i,
         END OF digit_map_item.
  TYPES digit_map TYPE SORTED TABLE OF digit_map_item WITH UNIQUE KEY primary_key COMPONENTS row col.

  TYPES: BEGIN OF number_map_item,
           row       TYPE i,
           start_col TYPE i,
           end_col   TYPE i,
           value     TYPE string,
         END OF number_map_item.
  TYPES number_map TYPE SORTED TABLE OF number_map_item WITH UNIQUE KEY primary_key COMPONENTS row start_col end_col.

  TYPES: BEGIN OF number_halo_item,
           start_row TYPE i,
           start_col TYPE i,
           end_row   TYPE i,
           end_col   TYPE i,
           value     TYPE string,
         END OF number_halo_item.
  TYPES number_halo TYPE SORTED TABLE OF number_halo_item WITH UNIQUE KEY primary_key COMPONENTS start_row start_col end_row end_col.

  TYPES: BEGIN OF gear_with_number_item,
           gear   TYPE grid_line,
           number TYPE string,
         END OF gear_with_number_item.
  TYPES gears_with_numbers TYPE SORTED TABLE OF gear_with_number_item WITH NON-UNIQUE KEY primary_key COMPONENTS gear number.

  TYPES factor_tab TYPE STANDARD TABLE OF i WITH EMPTY KEY.
  TYPES: BEGIN OF gear_factor,
           factors TYPE factor_tab,
           power   TYPE i,
         END OF gear_factor.
  TYPES gears_factors TYPE STANDARD TABLE OF gear_factor WITH EMPTY KEY.

ENDINTERFACE.

CLASS aoc_grid DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS investigate IMPORTING input_grid TYPE stringtab.

    METHODS calc_sum_of_attached_values RETURNING VALUE(result)       TYPE i.

    METHODS get_sum_of_gears_powers RETURNING VALUE(result) TYPE i.

  PRIVATE SECTION.
    CONSTANTS one_digit_numbers TYPE string VALUE '0123456789'.
    CONSTANTS no_symbol_digits TYPE string VALUE '0123456789.'.
    CONSTANTS gear_symbol TYPE string VALUE '*'.

    DATA numbers_and_symbols TYPE grid_types=>number_halo.
    DATA gears_with_numbers TYPE grid_types=>gears_with_numbers.

    METHODS build_column_table IMPORTING line          TYPE string
                               RETURNING VALUE(result) TYPE stringtab.

    METHODS is_number_value IMPORTING line          TYPE grid_types=>grid_line
                            RETURNING VALUE(result) TYPE grid_types=>digit_map.

    METHODS is_symbol_value IMPORTING line          TYPE grid_types=>grid_line
                            RETURNING VALUE(result) TYPE grid_types=>input_grid.

    METHODS update_number_map_line IMPORTING digit         TYPE REF TO grid_types=>digit_map_item
                                             number_line   TYPE grid_types=>number_map_item
                                   RETURNING VALUE(result) TYPE grid_types=>number_map_item.

    METHODS col_is_not_successor IMPORTING last_col      TYPE i
                                           current_col   TYPE i
                                 RETURNING VALUE(result) TYPE abap_bool.

    METHODS initialize_number_map_line RETURNING VALUE(result) TYPE grid_types=>number_map_item.

    METHODS update_last_digit_col IMPORTING digit_col     TYPE i
                                  RETURNING VALUE(result) TYPE i.

    METHODS get_one_field_less IMPORTING number        TYPE i
                               RETURNING VALUE(result) TYPE i.

    METHODS get_one_field_more IMPORTING number        TYPE i
                                         max_number    TYPE i
                               RETURNING VALUE(result) TYPE i.

    METHODS line_value_is_a_number IMPORTING line_value    TYPE char1
                                   RETURNING VALUE(result) TYPE abap_bool.
    METHODS calculare_gears_powers RETURNING VALUE(result)      TYPE grid_types=>gears_factors.

    METHODS map_digits IMPORTING grid          TYPE grid_types=>input_grid
                       RETURNING VALUE(result) TYPE grid_types=>digit_map.

    METHODS map_numbers IMPORTING digit_map     TYPE grid_types=>digit_map
                        RETURNING VALUE(result) TYPE grid_types=>number_map.

    METHODS build_number_halos IMPORTING number_map    TYPE grid_types=>number_map
                                         grid          TYPE grid_types=>input_grid
                               RETURNING VALUE(result) TYPE grid_types=>number_halo.

    METHODS map_symbols IMPORTING grid          TYPE grid_types=>input_grid
                        RETURNING VALUE(result) TYPE grid_types=>input_grid.

    METHODS attach_symbols_to_numbers IMPORTING symbol_map    TYPE grid_types=>input_grid
                                                number_halos  TYPE grid_types=>number_halo
                                      RETURNING VALUE(result) TYPE  grid_types=>number_halo.

    METHODS gears_and_numbers IMPORTING symbol_map    TYPE grid_types=>input_grid
                                        number_halos  TYPE grid_types=>number_halo
                              RETURNING VALUE(result) TYPE grid_types=>gears_with_numbers.

    METHODS build_grid IMPORTING input_grid    TYPE stringtab
                       RETURNING VALUE(result) TYPE grid_types=>input_grid.

ENDCLASS.

CLASS aoc_grid IMPLEMENTATION.

  METHOD investigate.
    DATA(grid) = build_grid( input_grid ).
    DATA(mapped_digits) = map_digits( grid ).
    DATA(mapped_numbers) = map_numbers( mapped_digits ).
    DATA(mapped_symbols) = map_symbols( grid ).
    DATA(number_halos) = build_number_halos( grid = grid
                                             number_map = mapped_numbers ).
    numbers_and_symbols = attach_symbols_to_numbers( symbol_map   = mapped_symbols
                                                     number_halos = number_halos ).
    gears_with_numbers = gears_and_numbers( symbol_map = mapped_symbols
                                            number_halos = number_halos ).
  ENDMETHOD.

  METHOD build_grid.
    result = VALUE #( BASE result FOR line IN input_grid
                      INDEX INTO line_count
                      LET column_table = build_column_table( line )
                      IN
                      FOR column IN column_table
                      INDEX INTO column_count
                        ( col = column_count
                          row = line_count
                          value = column ) ).
  ENDMETHOD.

  METHOD build_column_table.
    DATA(col_iterator) = ycl_mbh_string=>new( line )->get_iterator( ).
    WHILE col_iterator->has_next( ).
      result = VALUE #( BASE result
                            ( CAST ycl_mbh_string( col_iterator->get_next( ) )->value( ) ) ).
    ENDWHILE.
  ENDMETHOD.

  METHOD map_digits.
    result = VALUE #( FOR line IN grid
                       ( LINES OF is_number_value( line ) ) ).
  ENDMETHOD.

  METHOD is_number_value.
    IF line_value_is_a_number( line-value ).
      result = VALUE #( ( CORRESPONDING #( line ) ) ).
    ENDIF.
  ENDMETHOD.

  METHOD line_value_is_a_number.
    result = xsdbool( line_value CA one_digit_numbers ).
  ENDMETHOD.

  METHOD map_numbers.
    DATA(last_col) = 0.
    DATA(number_map_line) = initialize_number_map_line( ).

    LOOP AT digit_map REFERENCE INTO DATA(digit).
      IF col_is_not_successor( last_col    = last_col
                               current_col = digit->col ) .
        result = VALUE #( BASE result ( number_map_line ) ).
        number_map_line = initialize_number_map_line( ).
      ENDIF.

      number_map_line = update_number_map_line( number_line = number_map_line
                                                digit       = digit ).
      last_col = update_last_digit_col( digit->col ).
    ENDLOOP.
    result = VALUE #( BASE result ( number_map_line ) ).
  ENDMETHOD.

  METHOD update_number_map_line.
    result = number_line.
    result = VALUE #( BASE result
                      row       = digit->row
                      start_col = COND #( WHEN result-start_col = -1
                                            THEN digit->col
                                            ELSE result-start_col )
                      end_col   = digit->col
                      value     = condense( |{ result-value }{ digit->value }| ) ).
  ENDMETHOD.

  METHOD col_is_not_successor.
    result = xsdbool( current_col <> last_col + 1 ).
  ENDMETHOD.

  METHOD initialize_number_map_line.
    result = VALUE #( row = -1 start_col = -1 end_col = -1 ).
  ENDMETHOD.

  METHOD update_last_digit_col.
    result = digit_col.
  ENDMETHOD.

  METHOD build_number_halos.
    DATA(maximum_extend) = grid[ lines( grid ) ].
    result = VALUE #( BASE result
                            FOR map_item IN number_map
                            ( start_row = get_one_field_less( map_item-row )
                              start_col = get_one_field_less( map_item-start_col )
                              end_row   = get_one_field_more( number     = map_item-row
                                                              max_number = maximum_extend-row )
                              end_col   = get_one_field_more( number     = map_item-end_col
                                                              max_number = maximum_extend-col )
                              value     = map_item-value ) ).
  ENDMETHOD.

  METHOD get_one_field_less.
    result = SWITCH #( number WHEN 1 THEN 1
                       ELSE number - 1 ).
  ENDMETHOD.

  METHOD get_one_field_more.
    result = COND #(  WHEN number = max_number THEN max_number
                        ELSE number + 1 ).
  ENDMETHOD.

  METHOD map_symbols.
    result = VALUE #( FOR line IN grid
                          ( LINES OF is_symbol_value( line ) ) ).
  ENDMETHOD.

  METHOD is_symbol_value.
    IF line-value NA no_symbol_digits.
      result = VALUE #( ( CORRESPONDING #( line ) ) ).
    ENDIF.
  ENDMETHOD.

  METHOD attach_symbols_to_numbers.
    LOOP AT symbol_map REFERENCE INTO DATA(symbol).
      LOOP AT number_halos REFERENCE INTO DATA(halo).
        IF symbol->col >= halo->start_col AND symbol->col <= halo->end_col AND
           symbol->row >= halo->start_row AND symbol->row <= halo->end_row.
          TRY.
              result = VALUE #( BASE result ( halo->* ) ).
            CATCH cx_sy_itab_duplicate_key.
              CONTINUE.
          ENDTRY.
          IF halo->start_col > symbol->col + 5.
            EXIT.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD calc_sum_of_attached_values.
    result = REDUCE #( INIT sum = 0
                       FOR line IN numbers_and_symbols
                       NEXT sum = sum + line-value ).
  ENDMETHOD.

  METHOD gears_and_numbers.
    LOOP AT symbol_map REFERENCE INTO DATA(symbol).
      LOOP AT number_halos REFERENCE INTO DATA(halo).
        IF symbol->col >= halo->start_col AND symbol->col <= halo->end_col AND
           symbol->row >= halo->start_row AND symbol->row <= halo->end_row AND
           symbol->value = gear_symbol.
          result = VALUE #( BASE result
                              ( gear = symbol->*
                                number = halo->value ) ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD calculare_gears_powers.
    DATA factors TYPE grid_types=>factor_tab.

    LOOP AT gears_with_numbers REFERENCE INTO DATA(line) GROUP BY line->gear.
      LOOP AT GROUP line REFERENCE INTO DATA(group_line).
        factors = VALUE #( BASE factors ( CONV #( group_line->number ) ) ).
      ENDLOOP.
      IF lines( factors ) < 2.
        CLEAR factors.
        CONTINUE.
      ENDIF.
      result = VALUE #( BASE result
                        ( factors = factors
                          power   = REDUCE #( INIT power = 1
                                              FOR factor IN factors
                                              NEXT power = power * factor ) ) ).
      CLEAR factors.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_sum_of_gears_powers.
    DATA(gears_factors) = calculare_gears_powers( ).
    result = REDUCE #( INIT sum = 0
                        FOR line IN gears_factors
                        NEXT sum = sum + line-power ).
  ENDMETHOD.

ENDCLASS.

CLASS gear_ratios DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS process_first_task IMPORTING input_data    TYPE stringtab
                               RETURNING VALUE(result) TYPE i.

    METHODS process_second_task IMPORTING input_data    TYPE stringtab
                                RETURNING VALUE(result) TYPE i.

ENDCLASS.

CLASS gear_ratios IMPLEMENTATION.

  METHOD process_first_task.
    DATA(grid) = NEW aoc_grid( ).
    grid->investigate( input_data ).
    result = grid->calc_sum_of_attached_values( ).
  ENDMETHOD.

  METHOD process_second_task.
    DATA(grid) = NEW aoc_grid( ).
    grid->investigate( input_data ).
    result = grid->get_sum_of_gears_powers( ).
  ENDMETHOD.

ENDCLASS.

CLASS input_builder DEFINITION.
  PUBLIC SECTION.
    METHODS build RETURNING VALUE(result) TYPE stringtab.
ENDCLASS.

CLASS input_builder IMPLEMENTATION.

  METHOD build.
    result = VALUE #( ( |467..114..| )
                      ( |...*......| )
                      ( |..35..633.| )
                      ( |......#...| )
                      ( |617*......| )
                      ( |.....+.58.| )
                      ( |..592.....| )
                      ( |......755.| )
                      ( |...$.*....| )
                      ( |.664.598..| ) ).
  ENDMETHOD.

ENDCLASS.

CLASS test_application DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO gear_ratios.
    DATA input_data TYPE stringtab.

    METHODS setup.

    METHODS can_create_object FOR TESTING.
    METHODS solve_first_part_of_task FOR TESTING.
    METHODS solve_second_part_of_task FOR TESTING.
ENDCLASS.

CLASS test_application IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
    input_data = NEW input_builder( )->build( ).
  ENDMETHOD.

  METHOD can_create_object.
    cl_abap_unit_assert=>assert_bound( act = cut msg = |The object should be bound!| ).
  ENDMETHOD.

  METHOD solve_first_part_of_task.
    cl_abap_unit_assert=>assert_equals( exp = 4361 act = cut->process_first_task( input_data ) ).
  ENDMETHOD.

  METHOD solve_second_part_of_task.
    cl_abap_unit_assert=>assert_equals( exp = 467835 act = cut->process_second_task( input_data ) ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  DATA(input_data) = NEW zcl_mbh_file_upload( )->file_upload_in_stringtab( ).
  DATA(application) = NEW gear_ratios( ).

  WRITE /: |The result of part 1 is: { application->process_first_task( input_data ) }|.
  WRITE /: |The result of part 2 is: { application->process_second_task( input_data ) }|.
