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

  TYPES: BEGIN OF number_and_symbol,
           number TYPE string,
           symbol TYPE string,
         END OF number_and_symbol.
  TYPES numbers_and_symbols TYPE STANDARD TABLE OF number_and_symbol WITH EMPTY KEY.

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
    METHODS build_grid IMPORTING input_grid TYPE stringtab.

    METHODS get_grid RETURNING VALUE(result) TYPE grid_types=>input_grid.

    METHODS map_digits.

    METHODS get_digit_map RETURNING VALUE(result) TYPE grid_types=>digit_map.

    METHODS map_numbers.

    METHODS get_number_map RETURNING VALUE(result) TYPE grid_types=>number_map.

    METHODS get_number_halos RETURNING VALUE(result) TYPE grid_types=>number_halo.

    METHODS build_number_halos.

    METHODS get_symbol_map RETURNING VALUE(result) TYPE grid_types=>input_grid.

    METHODS map_symbols.

    METHODS attach_symbols_to_numbers.

    METHODS get_sum_of_attached_values RETURNING VALUE(result) TYPE i.

    METHODS gears_and_numbers.

    METHODS calculare_gears_powers RETURNING VALUE(result) TYPE grid_types=>gears_factors.

    METHODS get_gears_with_numbers RETURNING VALUE(result) TYPE grid_types=>gears_with_numbers.

    METHODS get_sum_of_gears_powers RETURNING VALUE(result) TYPE i.

  PRIVATE SECTION.
    CONSTANTS one_digit_numbers TYPE string VALUE '0123456789'.
    CONSTANTS no_symbol_digits TYPE string VALUE '0123456789.'.

    DATA grid TYPE grid_types=>input_grid.
    DATA digit_map TYPE grid_types=>digit_map.
    DATA number_map TYPE grid_types=>number_map.
    DATA number_halos TYPE grid_types=>number_halo.
    DATA symbol_map TYPE grid_types=>input_grid.
    DATA numbers_and_symbols TYPE grid_types=>number_halo.
    DATA unattached_numbers TYPE grid_types=>number_halo.
    DATA gears_with_numbers TYPE grid_types=>gears_with_numbers.
    DATA gears_factors TYPE grid_types=>gears_factors.

    METHODS build_col_tab IMPORTING i_line          TYPE string
                          RETURNING VALUE(r_result) TYPE stringtab.

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

    METHODS add_number_line_to_map IMPORTING number_line   TYPE grid_types=>number_map_item.

    METHODS initialize_number_map_line RETURNING VALUE(result) TYPE grid_types=>number_map_item.

    METHODS update_last_digit_col IMPORTING digit_col     TYPE i
                                  RETURNING VALUE(result) TYPE i.

    METHODS get_one_field_less IMPORTING number        TYPE i
                               RETURNING VALUE(result) TYPE i.

    METHODS get_one_field_more IMPORTING number        TYPE i
                                         max_number    TYPE i
                               RETURNING VALUE(result) TYPE i.

ENDCLASS.

CLASS aoc_grid IMPLEMENTATION.

  METHOD build_grid.
    grid = VALUE #( BASE grid FOR line IN input_grid
                      INDEX INTO line_count
                      LET col_tab = build_col_tab( line )
                      IN
                      FOR col IN col_tab
                      INDEX INTO col_count
                        ( col = col_count row = line_count value = col ) ).
  ENDMETHOD.

  METHOD get_grid.
    result = grid.
  ENDMETHOD.

  METHOD build_col_tab.
    DATA(col_iterator) = ycl_mbh_string=>new( i_line )->get_iterator( ).
    WHILE col_iterator->has_next( ).
      r_result = VALUE #( BASE r_result ( CAST ycl_mbh_string( col_iterator->get_next( ) )->value( ) ) ).
    ENDWHILE.
  ENDMETHOD.

  METHOD map_digits.
    digit_map = VALUE #( FOR line IN grid
                              ( LINES OF is_number_value( line ) ) ).
  ENDMETHOD.

  METHOD get_digit_map.
    result = digit_map.
  ENDMETHOD.

  METHOD is_number_value.
    IF line-value CA one_digit_numbers.
      result = VALUE #( ( CORRESPONDING #( line ) ) ).
    ENDIF.
  ENDMETHOD.

  METHOD map_numbers.
    DATA(last_col) = 0.
    DATA(number_map_line) = initialize_number_map_line( ).

    LOOP AT digit_map REFERENCE INTO DATA(digit).
      IF col_is_not_successor( last_col    = last_col
                               current_col = digit->col ) .
        add_number_line_to_map( number_map_line ).
        number_map_line = initialize_number_map_line( ).
      ENDIF.

      number_map_line = update_number_map_line( number_line = number_map_line
                                                digit       = digit ).
      last_col = update_last_digit_col( digit->col ).
    ENDLOOP.
    add_number_line_to_map( number_map_line ).
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

  METHOD get_number_map.
    result = number_map.
  ENDMETHOD.

  METHOD col_is_not_successor.
    result = xsdbool( current_col <> last_col + 1 ).
  ENDMETHOD.

  METHOD add_number_line_to_map.
    number_map = VALUE #( BASE number_map ( number_line ) ).
  ENDMETHOD.

  METHOD initialize_number_map_line.
    result = VALUE #( row = -1 start_col = -1 end_col = -1 ).
  ENDMETHOD.

  METHOD update_last_digit_col.
    result = digit_col.
  ENDMETHOD.

  METHOD build_number_halos.
    DATA(maximum_extend) = grid[ lines( grid ) ].
    number_halos = VALUE #( BASE number_halos
                            FOR map_item IN number_map
                            ( start_row = get_one_field_less( map_item-row )
                              start_col = get_one_field_less( map_item-start_col )
                              end_row   = get_one_field_more( number     = map_item-row
                                                              max_number = maximum_extend-row )
                              end_col   = get_one_field_more( number     = map_item-end_col
                                                              max_number = maximum_extend-col )
                              value     = map_item-value ) ).
  ENDMETHOD.

  METHOD get_number_halos.
    result = number_halos.
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
    symbol_map = VALUE #( FOR line IN grid
                              ( LINES OF is_symbol_value( line ) ) ).
  ENDMETHOD.

  METHOD get_symbol_map.
    result = symbol_map.
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
              numbers_and_symbols = VALUE #( BASE numbers_and_symbols ( halo->* ) ).
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

  METHOD get_sum_of_attached_values.
    result = REDUCE #( INIT sum = 0
                       FOR line IN numbers_and_symbols
                       NEXT sum = sum + line-value ).
  ENDMETHOD.

  METHOD gears_and_numbers.
    LOOP AT symbol_map REFERENCE INTO DATA(symbol).
      LOOP AT number_halos REFERENCE INTO DATA(halo).
        IF symbol->col >= halo->start_col AND symbol->col <= halo->end_col AND
           symbol->row >= halo->start_row AND symbol->row <= halo->end_row AND
           symbol->value = '*'.
          gears_with_numbers = VALUE #( BASE gears_with_numbers
                              ( gear = symbol->*
                                number = halo->value ) ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_gears_with_numbers.
    result = gears_with_numbers.
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
      gears_factors = VALUE #( BASE gears_factors ( factors = factors
                                                    power = REDUCE #( INIT power = 1
                                                                      FOR factor IN factors
                                                                      NEXT power = power * factor ) ) ).
      CLEAR factors.
    ENDLOOP.

    result = gears_factors.
  ENDMETHOD.


  METHOD get_sum_of_gears_powers.
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
    grid->build_grid( input_data ).
    grid->map_digits( ).
    grid->map_numbers( ).
    grid->map_symbols( ).
    grid->build_number_halos( ).
    grid->attach_symbols_to_numbers( ).
    result = grid->get_sum_of_attached_values( ).
  ENDMETHOD.

  METHOD process_second_task.
    DATA(grid) = NEW aoc_grid( ).
    grid->build_grid( input_data ).
    grid->map_digits( ).
    grid->map_numbers( ).
    grid->map_symbols( ).
    grid->build_number_halos( ).
    grid->gears_and_numbers( ).
    grid->calculare_gears_powers( ).
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

CLASS expected_result_builder DEFINITION.
  PUBLIC SECTION.
    METHODS build RETURNING VALUE(result) TYPE grid_types=>input_grid.
    METHODS build_digit_map RETURNING VALUE(result) TYPE grid_types=>digit_map.
    METHODS build_number_map RETURNING VALUE(result) TYPE grid_types=>number_map.
    METHODS build_numbers_halo RETURNING VALUE(result) TYPE grid_types=>number_halo.
    METHODS build_symbol_map RETURNING VALUE(result) TYPE grid_types=>input_grid.
ENDCLASS.

CLASS expected_result_builder IMPLEMENTATION.

  METHOD build.
    result = VALUE #( ( row = 1 col = 1  value = |4| )
                      ( row = 1 col = 2  value = |6| )
                      ( row = 1 col = 3  value = |7| )
                      ( row = 1 col = 4  value = |.| )
                      ( row = 1 col = 5  value = |.| )
                      ( row = 1 col = 6  value = |1| )
                      ( row = 1 col = 7  value = |1| )
                      ( row = 1 col = 8  value = |4| )
                      ( row = 1 col = 9  value = |.| )
                      ( row = 1 col = 10 value = |.| )

                      ( row = 2 col = 1  value = |.| )
                      ( row = 2 col = 2  value = |.| )
                      ( row = 2 col = 3  value = |.| )
                      ( row = 2 col = 4  value = |*| )
                      ( row = 2 col = 5  value = |.| )
                      ( row = 2 col = 6  value = |.| )
                      ( row = 2 col = 7  value = |.| )
                      ( row = 2 col = 8  value = |.| )
                      ( row = 2 col = 9  value = |.| )
                      ( row = 2 col = 10 value = |.| )

                      ( row = 3 col = 1  value = |.| )
                      ( row = 3 col = 2  value = |.| )
                      ( row = 3 col = 3  value = |3| )
                      ( row = 3 col = 4  value = |5| )
                      ( row = 3 col = 5  value = |.| )
                      ( row = 3 col = 6  value = |.| )
                      ( row = 3 col = 7  value = |6| )
                      ( row = 3 col = 8  value = |3| )
                      ( row = 3 col = 9  value = |3| )
                      ( row = 3 col = 10 value = |.| )

                      ( row = 4 col = 1  value = |.| )
                      ( row = 4 col = 2  value = |.| )
                      ( row = 4 col = 3  value = |.| )
                      ( row = 4 col = 4  value = |.| )
                      ( row = 4 col = 5  value = |.| )
                      ( row = 4 col = 6  value = |.| )
                      ( row = 4 col = 7  value = |#| )
                      ( row = 4 col = 8  value = |.| )
                      ( row = 4 col = 9  value = |.| )
                      ( row = 4 col = 10 value = |.| )

                      ( row = 5 col = 1  value = |6| )
                      ( row = 5 col = 2  value = |1| )
                      ( row = 5 col = 3  value = |7| )
                      ( row = 5 col = 4  value = |*| )
                      ( row = 5 col = 5  value = |.| )
                      ( row = 5 col = 6  value = |.| )
                      ( row = 5 col = 7  value = |.| )
                      ( row = 5 col = 8  value = |.| )
                      ( row = 5 col = 9  value = |.| )
                      ( row = 5 col = 10 value = |.| )

                      ( row = 6 col = 1  value = |.| )
                      ( row = 6 col = 2  value = |.| )
                      ( row = 6 col = 3  value = |.| )
                      ( row = 6 col = 4  value = |.| )
                      ( row = 6 col = 5  value = |.| )
                      ( row = 6 col = 6  value = |+| )
                      ( row = 6 col = 7  value = |.| )
                      ( row = 6 col = 8  value = |5| )
                      ( row = 6 col = 9  value = |8| )
                      ( row = 6 col = 10 value = |.| )

                      ( row = 7 col = 1  value = |.| )
                      ( row = 7 col = 2  value = |.| )
                      ( row = 7 col = 3  value = |5| )
                      ( row = 7 col = 4  value = |9| )
                      ( row = 7 col = 5  value = |2| )
                      ( row = 7 col = 6  value = |.| )
                      ( row = 7 col = 7  value = |.| )
                      ( row = 7 col = 8  value = |.| )
                      ( row = 7 col = 9  value = |.| )
                      ( row = 7 col = 10 value = |.| )

                      ( row = 8 col = 1  value = |.| )
                      ( row = 8 col = 2  value = |.| )
                      ( row = 8 col = 3  value = |.| )
                      ( row = 8 col = 4  value = |.| )
                      ( row = 8 col = 5  value = |.| )
                      ( row = 8 col = 6  value = |.| )
                      ( row = 8 col = 7  value = |7| )
                      ( row = 8 col = 8  value = |5| )
                      ( row = 8 col = 9  value = |5| )
                      ( row = 8 col = 10 value = |.| )

                      ( row = 9 col = 1  value = |.| )
                      ( row = 9 col = 2  value = |.| )
                      ( row = 9 col = 3  value = |.| )
                      ( row = 9 col = 4  value = |$| )
                      ( row = 9 col = 5  value = |.| )
                      ( row = 9 col = 6  value = |*| )
                      ( row = 9 col = 7  value = |.| )
                      ( row = 9 col = 8  value = |.| )
                      ( row = 9 col = 9  value = |.| )
                      ( row = 9 col = 10 value = |.| )

                      ( row = 10 col = 1  value = |.| )
                      ( row = 10 col = 2  value = |6| )
                      ( row = 10 col = 3  value = |6| )
                      ( row = 10 col = 4  value = |4| )
                      ( row = 10 col = 5  value = |.| )
                      ( row = 10 col = 6  value = |5| )
                      ( row = 10 col = 7  value = |9| )
                      ( row = 10 col = 8  value = |8| )
                      ( row = 10 col = 9  value = |.| )
                      ( row = 10 col = 10 value = |.| ) ).
  ENDMETHOD.

  METHOD build_digit_map.
    result = VALUE #( ( row = 1 col = 1 value = 4 )
                      ( row = 1 col = 2 value = 6 )
                      ( row = 1 col = 3 value = 7 )
                      ( row = 1 col = 6 value = 1 )
                      ( row = 1 col = 7 value = 1 )
                      ( row = 1 col = 8 value = 4 )
                      ( row = 3 col = 3 value = 3 )
                      ( row = 3 col = 4 value = 5 )
                      ( row = 3 col = 7 value = 6 )
                      ( row = 3 col = 8 value = 3 )
                      ( row = 3 col = 9 value = 3 )
                      ( row = 5 col = 1 value = 6 )
                      ( row = 5 col = 2 value = 1 )
                      ( row = 5 col = 3 value = 7 )
                      ( row = 6 col = 8 value = 5 )
                      ( row = 6 col = 9 value = 8 )
                      ( row = 7 col = 3 value = 5 )
                      ( row = 7 col = 4 value = 9 )
                      ( row = 7 col = 5 value = 2 )
                      ( row = 8 col = 7 value = 7 )
                      ( row = 8 col = 8 value = 5 )
                      ( row = 8 col = 9 value = 5 )
                      ( row = 10 col = 2 value = 6 )
                      ( row = 10 col = 3 value = 6 )
                      ( row = 10 col = 4 value = 4 )
                      ( row = 10 col = 6 value = 5 )
                      ( row = 10 col = 7 value = 9 )
                      ( row = 10 col = 8 value = 8 ) ).
  ENDMETHOD.

  METHOD build_number_map.
    result = VALUE #( ( row = 1 start_col = 1 end_col = 3 value = |467| )
                      ( row = 1 start_col = 6 end_col = 8 value = |114| )
                      ( row = 3 start_col = 3 end_col = 4 value = |35| )
                      ( row = 3 start_col = 7 end_col = 9 value = |633| )
                      ( row = 5 start_col = 1 end_col = 3 value = |617| )
                      ( row = 6 start_col = 8 end_col = 9 value = |58| )
                      ( row = 7 start_col = 3 end_col = 5 value = |592| )
                      ( row = 8 start_col = 7 end_col = 9 value = |755| )
                      ( row = 10 start_col = 2 end_col = 4 value = |664| )
                      ( row = 10 start_col = 6 end_col = 8 value = |598| ) ).
  ENDMETHOD.

  METHOD build_numbers_halo.
    result = VALUE #( ( start_row = 1 start_col = 1 end_row = 2  end_col = 4  value = |467| )
                      ( start_row = 1 start_col = 5 end_row = 2  end_col = 9  value = |114| )
                      ( start_row = 2 start_col = 2 end_row = 4  end_col = 5  value = |35| )
                      ( start_row = 2 start_col = 6 end_row = 4  end_col = 10 value = |633| )
                      ( start_row = 4 start_col = 1 end_row = 6  end_col = 4  value = |617| )
                      ( start_row = 5 start_col = 7 end_row = 7  end_col = 10 value = |58| )
                      ( start_row = 6 start_col = 2 end_row = 8  end_col = 6  value = |592| )
                      ( start_row = 7 start_col = 6 end_row = 9  end_col = 10 value = |755| )
                      ( start_row = 9 start_col = 1 end_row = 10 end_col = 5  value = |664| )
                      ( start_row = 9 start_col = 5 end_row = 10 end_col = 9  value = |598| ) ).
  ENDMETHOD.

  METHOD build_symbol_map.
    result = VALUE #( ( row = 2 col = 4 value = |*| )
                      ( row = 4 col = 7 value = |#| )
                      ( row = 5 col = 4 value = |*| )
                      ( row = 6 col = 6 value = |+| )
                      ( row = 9 col = 4 value = |$| )
                      ( row = 9 col = 6 value = |*| ) ).
  ENDMETHOD.

ENDCLASS.

CLASS test_grid DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO aoc_grid.

    METHODS setup.

    METHODS can_create_object FOR TESTING.
    METHODS get_test_grid FOR TESTING.
    METHODS get_map_with_digits FOR TESTING.
    METHODS get_map_with_numbers FOR TESTING.
    METHODS get_numbers_halo FOR TESTING.
    METHODS get_symbol_map FOR TESTING.
    METHODS get_gears_with_numbers FOR TESTING.
    METHODS get_gear_doubles_as_factors FOR TESTING.

ENDCLASS.

CLASS test_grid IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
    cut->build_grid( NEW input_builder( )->build( ) ).
  ENDMETHOD.

  METHOD can_create_object.
    cl_abap_unit_assert=>assert_bound( act = cut msg = |The object should be bound!| ).
  ENDMETHOD.

  METHOD get_test_grid.
    DATA(expected_grid) = NEW expected_result_builder( )->build( ).
    cl_abap_unit_assert=>assert_equals( exp = expected_grid act = cut->get_grid( )  ).
  ENDMETHOD.

  METHOD get_map_with_digits.
    cut->map_digits( ).
    DATA(expected_values) = NEW expected_result_builder( )->build_digit_map( ).

    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->get_digit_map( ) ).
  ENDMETHOD.

  METHOD get_map_with_numbers.
    DATA(expected_values) = NEW expected_result_builder( )->build_number_map( ).
    cut->map_digits( ).
    cut->map_numbers( ).
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->get_number_map( ) ).
  ENDMETHOD.

  METHOD get_numbers_halo.
    DATA(expected_values) = NEW expected_result_builder( )->build_numbers_halo( ).
    cut->map_digits( ).
    cut->map_numbers( ).
    cut->build_number_halos( ).
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->get_number_halos( ) ).
  ENDMETHOD.

  METHOD get_symbol_map.
    DATA(expected_values) = NEW expected_result_builder( )->build_symbol_map( ).
    cut->map_symbols( ).
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->get_symbol_map( )  ).
  ENDMETHOD.

  METHOD get_gears_with_numbers.
    cut->map_digits( ).
    cut->map_numbers( ).
    cut->map_symbols( ).
    cut->build_number_halos( ).
    cut->gears_and_numbers( ).
    DATA(expected_values) = VALUE grid_types=>gears_with_numbers( ( gear = VALUE #( row = 2 col = 4 value = |*| )
                                                                    number = |467| )
                                                                  ( gear = VALUE #( row = 2 col = 4 value = |*| )
                                                                    number = |35| )
                                                                  ( gear = VALUE #( row = 5 col = 4 value = |*|  )
                                                                    number = |617| )
                                                                  ( gear = VALUE #( row = 9 col = 6 value = |*| )
                                                                    number = |755| )
                                                                  ( gear = VALUE #( row = 9 col = 6 value = |*| )
                                                                    number = |598| ) ).

    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->get_gears_with_numbers( ) ).
  ENDMETHOD.

  METHOD get_gear_doubles_as_factors.
    cut->map_digits( ).
    cut->map_numbers( ).
    cut->map_symbols( ).
    cut->build_number_halos( ).
    cut->gears_and_numbers( ).
    DATA(expected_values) = VALUE grid_types=>gears_factors( ( factors = VALUE #( ( 35 )
                                                                                  ( 467 ) )
                                                               power = 16345 )

                                                             ( factors = VALUE #( ( 598 )
                                                                                  ( 755 ) )
                                                               power = 451490 ) ).
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->calculare_gears_powers( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS test_application DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO gear_ratios.

    METHODS setup.
    METHODS can_create_object FOR TESTING.
    METHODS solve_first_part_of_task FOR TESTING.
    METHODS solve_second_part_of_task FOR TESTING.
ENDCLASS.

CLASS test_application IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD can_create_object.
    cl_abap_unit_assert=>assert_bound( act = cut msg = |The object should be bound!| ).
  ENDMETHOD.

  METHOD solve_first_part_of_task.
    DATA(input_data) = NEW input_builder( )->build( ).
    cl_abap_unit_assert=>assert_equals( exp = 4361 act = cut->process_first_task( input_data ) ).
  ENDMETHOD.

  METHOD solve_second_part_of_task.
    DATA(input_data) = NEW input_builder( )->build( ).
    cl_abap_unit_assert=>assert_equals( exp = 467835 act = cut->process_second_task( input_data ) ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  DATA(input_data) = NEW zcl_mbh_file_upload( )->file_upload_in_stringtab( ).
  DATA(application) = NEW gear_ratios( ).

  WRITE /: |The result of part 1 is: { application->process_first_task( input_data ) }|.
  WRITE /: |The result of part 2 is: { application->process_second_task( input_data ) }|.
