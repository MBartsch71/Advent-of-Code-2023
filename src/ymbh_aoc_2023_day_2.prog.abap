REPORT ymbh_aoc_2023_day_2.

INTERFACE cube_conundrum.
  TYPES: BEGIN OF draw_detail,
           color  TYPE string,
           amount TYPE i,
         END OF draw_detail.
  TYPES draw_details TYPE STANDARD TABLE OF draw_detail WITH EMPTY KEY.

  TYPES: BEGIN OF draw,
           draw_nr            TYPE i,
           draw_details       TYPE draw_details,
           draw_limit_overrun TYPE abap_bool,
         END OF draw.
  TYPES draws TYPE STANDARD TABLE OF draw WITH EMPTY KEY.

  TYPES: BEGIN OF game_round,
           game                 TYPE string,
           draws                TYPE draws,
           over_limit           TYPE abap_bool,
           minimum_color_amount TYPE draw_details,
           power_of_cubes       TYPE i,
         END OF game_round.
  TYPES game_rounds TYPE STANDARD TABLE OF game_round WITH EMPTY KEY.

  TYPES: BEGIN OF limit,
           color TYPE string,
           limit TYPE i,
         END OF limit.
  TYPES limits TYPE STANDARD TABLE OF limit WITH EMPTY KEY.
ENDINTERFACE.

CLASS game_detector DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS run IMPORTING input_values TYPE stringtab.

    METHODS get_game_records RETURNING VALUE(result) TYPE cube_conundrum=>game_rounds.

    METHODS get_sum_of_valid_ids RETURNING VALUE(result) TYPE i.

    METHODS get_sum_of_power_of_cubes RETURNING VALUE(result) TYPE i.

    METHODS set_limits IMPORTING limits TYPE cube_conundrum=>limits.

    METHODS build_minimum_color_stats.

  PRIVATE SECTION.
    DATA game_nr TYPE char5.
    DATA game_draws TYPE cube_conundrum=>draws.
    DATA game_record TYPE cube_conundrum=>game_round.
    DATA game_records TYPE cube_conundrum=>game_rounds.
    DATA game_limits TYPE cube_conundrum=>limits.

    METHODS extract_game_number IMPORTING input_line TYPE string.

    METHODS extract_draw_record IMPORTING input_line TYPE string.

    METHODS build_game_record.

    METHODS initialize_color_stats RETURNING VALUE(result) TYPE cube_conundrum=>draw_details.

    METHODS investigate IMPORTING input_line    TYPE string
                        RETURNING VALUE(result) TYPE cube_conundrum=>game_round.

    METHODS determine_draw_details IMPORTING colors        TYPE string_table
                                   RETURNING VALUE(result) TYPE cube_conundrum=>draw_details.

    METHODS split_string IMPORTING string        TYPE string
                                   delimiter     TYPE char2
                         RETURNING VALUE(result) TYPE stringtab.

    METHODS check_draw_limit_exceeded IMPORTING draw_details_tab TYPE cube_conundrum=>draw_details
                                      RETURNING VALUE(result)    TYPE abap_bool.

    METHODS extract_draws_from_input IMPORTING input_line    TYPE string
                                     RETURNING VALUE(result) TYPE stringtab.

    METHODS build_draws IMPORTING i_temp_draws TYPE stringtab.

    METHODS check_game_execeeds_limit RETURNING VALUE(r_game_over_limit) TYPE abap_bool.

    METHODS calculate_power_of_cubes IMPORTING color_stats   TYPE cube_conundrum=>draw_details
                                     RETURNING VALUE(result) TYPE i.

    METHODS populate_color_statistics IMPORTING color_stats   TYPE cube_conundrum=>draw_details
                                                draw          TYPE REF TO cube_conundrum=>draw
                                      RETURNING VALUE(result) TYPE cube_conundrum=>draw_details.
ENDCLASS.

CLASS game_detector IMPLEMENTATION.

  METHOD run.
    LOOP AT input_values REFERENCE INTO DATA(line).
      investigate( line->* ).
      game_records = VALUE cube_conundrum=>game_rounds( BASE game_records ( game_record ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD investigate.
    extract_game_number( input_line ).
    extract_draw_record( input_line ).
    build_game_record( ).
  ENDMETHOD.

  METHOD extract_game_number.
    SPLIT input_line AT ':' INTO  DATA(game) DATA(draw_record).
    DATA(split_game_expression) = ycl_mbh_string=>new( game )->split_string( 5 ).
    game_nr = split_game_expression[ 2 ].
  ENDMETHOD.

  METHOD extract_draw_record.
    DATA(temp_draws) = extract_draws_from_input( input_line ).
    build_draws( temp_draws ).
  ENDMETHOD.

  METHOD build_draws.
    game_draws = VALUE #( FOR temp_draw IN i_temp_draws
                            INDEX INTO counter
                            LET colors              = split_string( string = temp_draw delimiter = ', ' )
                                draw_details_tab    = determine_draw_details( colors )
                                game_limit_overflow = check_draw_limit_exceeded( draw_details_tab )
                                IN
                               ( draw_nr            = counter
                                 draw_details       = draw_details_tab
                                 draw_limit_overrun = game_limit_overflow ) ).
  ENDMETHOD.

  METHOD determine_draw_details.
    result = VALUE #( FOR line IN colors
                        LET splitted_string = split_string( string    = line
                                                            delimiter = space )
                        IN
                        ( amount = splitted_string[ 1 ]
                          color  = splitted_string[ 2 ] ) ).
  ENDMETHOD.

  METHOD check_draw_limit_exceeded.
    LOOP AT draw_details_tab REFERENCE INTO DATA(draw_detail).
      TRY.
          DATA(limit_entry) = game_limits[ color = draw_detail->color ].
          IF limit_entry-limit < draw_detail->amount.
            result = abap_true.
            EXIT.
          ENDIF.
        CATCH cx_sy_itab_line_not_found.
          CONTINUE.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD split_string.
    IF delimiter = space.
      SPLIT string AT space INTO TABLE result.
    ELSE.
      SPLIT string AT delimiter INTO TABLE result.
    ENDIF.
  ENDMETHOD.

  METHOD build_game_record.
    game_record = VALUE #( game  = game_nr
                           draws = game_draws
                           over_limit = check_game_execeeds_limit( ) ).
  ENDMETHOD.

  METHOD check_game_execeeds_limit.
    IF line_exists( game_draws[ draw_limit_overrun = abap_true ] ).
      r_game_over_limit = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD get_game_records.
    result = game_records.
  ENDMETHOD.

  METHOD set_limits.
    game_limits = limits.
  ENDMETHOD.

  METHOD get_sum_of_valid_ids.
    result = REDUCE #( INIT sum = 0
                       FOR line IN game_records
                       NEXT sum = sum + COND #( WHEN line-over_limit IS INITIAL
                                                THEN line-game
                                                ELSE 0 ) ).
  ENDMETHOD.

  METHOD build_minimum_color_stats.
    LOOP AT game_records REFERENCE INTO DATA(game_record).
      DATA(color_stats) = initialize_color_stats( ).
      LOOP AT game_record->draws REFERENCE INTO DATA(draw).
        color_stats = populate_color_statistics( color_stats = color_stats
                                                 draw        = draw ).
      ENDLOOP.
      game_record->minimum_color_amount = color_stats.
      game_record->power_of_cubes = calculate_power_of_cubes( color_stats ).
    ENDLOOP.
  ENDMETHOD.

  METHOD initialize_color_stats.
    result  = VALUE #( ( color = |blue|  amount = 0 )
                       ( color = |red|   amount = 0 )
                       ( color = |green| amount = 0 ) ).
  ENDMETHOD.

  METHOD get_sum_of_power_of_cubes.
    result = REDUCE #( INIT sum = 0
                       FOR line IN game_records
                       NEXT sum = sum + line-power_of_cubes ).
  ENDMETHOD.

  METHOD extract_draws_from_input.
    SPLIT input_line AT ': ' INTO DATA(game) DATA(draw_record).
    SPLIT draw_record AT '; ' INTO TABLE result.
  ENDMETHOD.

  METHOD calculate_power_of_cubes.
    result = REDUCE #( INIT power = 1
                       FOR line IN color_stats
                       NEXT power = power * line-amount ).
  ENDMETHOD.

  METHOD populate_color_statistics.
    result = color_stats.
    LOOP AT draw->draw_details REFERENCE INTO DATA(detail).
      ASSIGN result[ color = detail->color ] TO FIELD-SYMBOL(<color_stat>).
      IF sy-subrc = 0.
        <color_stat>-amount = COND #( WHEN <color_stat>-amount < detail->amount
                                        THEN detail->amount
                                        ELSE <color_stat>-amount ).
      ENDIF.
      UNASSIGN <color_stat>.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS expected_result_builder DEFINITION.
  PUBLIC SECTION.
    METHODS build_expected_results RETURNING VALUE(result) TYPE cube_conundrum=>game_rounds.

  PRIVATE SECTION.
    METHODS build_expected_game_round_1 RETURNING VALUE(result) TYPE cube_conundrum=>game_round.
    METHODS build_expected_game_round_2 RETURNING VALUE(result) TYPE cube_conundrum=>game_round.
    METHODS build_expected_game_round_3 RETURNING VALUE(result) TYPE cube_conundrum=>game_round.
    METHODS build_expected_game_round_4 RETURNING VALUE(result) TYPE cube_conundrum=>game_round.
    METHODS build_expected_game_round_5 RETURNING VALUE(result) TYPE cube_conundrum=>game_round.

ENDCLASS.

CLASS expected_result_builder IMPLEMENTATION.

  METHOD build_expected_results.
    result = VALUE #( ( build_expected_game_round_1( ) )
                        ( build_expected_game_round_2( ) )
                        ( build_expected_game_round_3( ) )
                        ( build_expected_game_round_4( ) )
                        ( build_expected_game_round_5( ) ) ).
  ENDMETHOD.

  METHOD build_expected_game_round_1.
    result = VALUE #( game                 = |1|
                      draws                = VALUE #( ( draw_nr            = 1
                                                        draw_details       = VALUE #( ( color = |blue| amount = 3 )
                                                                                      ( color = |red|  amount = 4 ) )
                                                        draw_limit_overrun = || )

                                                      ( draw_nr = 2
                                                        draw_details       = VALUE #( ( color = |red|  amount = 1 )
                                                                                      ( color = |green| amount = 2 )
                                                                                      ( color = |blue|  amount = 6 ) )
                                                        draw_limit_overrun = || )

                                                      ( draw_nr = 3
                                                        draw_details       = VALUE #( ( color = |green| amount = 2 ) ) ) )
                      over_limit           = ||
                      minimum_color_amount = VALUE #( ( color = |blue|  amount = 6 )
                                                      ( color = |red|   amount = 4 )
                                                      ( color = |green| amount = 2 ) )
                      power_of_cubes       = 48 ).

  ENDMETHOD.

  METHOD build_expected_game_round_2.
    result = VALUE #( game                 = |2|
                      draws                = VALUE #( ( draw_nr            = 1
                                                        draw_details       = VALUE #( ( color = |blue|  amount = 1 )
                                                                                      ( color = |green| amount = 2 ) )
                                                        draw_limit_overrun = || )

                                                      ( draw_nr            = 2
                                                        draw_details       = VALUE #( ( color = |green| amount = 3 )
                                                                                      ( color = |blue|  amount = 4 )
                                                                                      ( color = |red|   amount = 1 ) )
                                                        draw_limit_overrun = || )

                                                      ( draw_nr            = 3
                                                        draw_details       = VALUE #( ( color = |green| amount = 1 )
                                                                                      ( color = |blue|  amount = 1 ) )
                                                        draw_limit_overrun = || ) )
                      over_limit           = ||
                      minimum_color_amount = VALUE #( ( color = |blue|  amount = 4 )
                                                      ( color = |red|   amount = 1 )
                                                      ( color = |green| amount = 3 ) )
                      power_of_cubes       = 12 ).
  ENDMETHOD.

  METHOD build_expected_game_round_3.
    result = VALUE #( game                 = |3|
                      draws                = VALUE #( ( draw_nr            = 1
                                                        draw_details       = VALUE #( ( color = |green| amount = 8 )
                                                                                      ( color = |blue|  amount = 6 )
                                                                                      ( color = |red|   amount = 20 ) )
                                                        draw_limit_overrun = |X| )

                                                      ( draw_nr            = 2
                                                        draw_details       = VALUE #( ( color = |blue|  amount = 5 )
                                                                                      ( color = |red|   amount = 4 )
                                                                                      ( color = |green| amount = 13 ) )
                                                        draw_limit_overrun = || )

                                                      ( draw_nr            = 3
                                                        draw_details       = VALUE #( ( color = |green| amount = 5 )
                                                                                      ( color = |red|   amount = 1 ) )
                                                        draw_limit_overrun = || ) )
                      over_limit           = |X|
                      minimum_color_amount = VALUE #( ( color = |blue|  amount = 6 )
                                                      ( color = |red|   amount = 20 )
                                                      ( color = |green| amount = 13 ) )
                      power_of_cubes       = 1560 ).
  ENDMETHOD.

  METHOD build_expected_game_round_4.
    result = VALUE #( game                 = |4|
                      draws                = VALUE #( ( draw_nr            = 1
                                                        draw_details       = VALUE #( ( color = |green| amount = 1 )
                                                                                      ( color = |red|   amount = 3 )
                                                                                      ( color = |blue|  amount = 6 ) )
                                                        draw_limit_overrun = || )

                                                      ( draw_nr            = 2
                                                        draw_details       = VALUE #( ( color = |green| amount = 3 )
                                                                                      ( color = |red|   amount = 6 ) )
                                                        draw_limit_overrun = || )

                                                      ( draw_nr            = 3
                                                        draw_details       = VALUE #( ( color = |green| amount = 3 )
                                                                                      ( color = |blue|  amount = 15 )
                                                                                      ( color = |red|   amount = 14 ) )
                                                        draw_limit_overrun = |X| ) )
                      over_limit           = |X|
                      minimum_color_amount = VALUE #( ( color = |blue|  amount = 15 )
                                                      ( color = |red|   amount = 14 )
                                                      ( color = |green| amount = 3 ) )
                      power_of_cubes       = 630 ).
  ENDMETHOD.

  METHOD build_expected_game_round_5.
    result = VALUE #( game                 = |5|
                      draws                = VALUE #( ( draw_nr            = 1
                                                        draw_details       = VALUE #( ( color = |red|   amount = 6 )
                                                                                      ( color = |blue|  amount = 1 )
                                                                                      ( color = |green| amount = 3 ) )
                                                        draw_limit_overrun = || )

                                                      ( draw_nr            = 2
                                                        draw_details       = VALUE #( ( color = |blue|  amount = 2 )
                                                                                      ( color = |red|   amount = 1 )
                                                                                      ( color = |green| amount = 2 ) )
                                                        draw_limit_overrun = || ) )
                      over_limit           = ||
                      minimum_color_amount = VALUE #( ( color = |blue|  amount = 2 )
                                                      ( color = |red|   amount = 6 )
                                                      ( color = |green| amount = 3 ) )
                      power_of_cubes       = 36 ).
  ENDMETHOD.

ENDCLASS.

CLASS input_values DEFINITION.
  PUBLIC SECTION.
    METHODS build RETURNING VALUE(result) TYPE stringtab.
ENDCLASS.

CLASS input_values IMPLEMENTATION.

  METHOD build.
    result  = VALUE stringtab( ( |Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green| )
                               ( |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue| )
                               ( |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red| )
                               ( |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red| )
                               ( |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green| ) ).
  ENDMETHOD.

ENDCLASS.

CLASS draw_limits DEFINITION.
  PUBLIC SECTION.
    METHODS build RETURNING VALUE(result) TYPE cube_conundrum=>limits.
ENDCLASS.

CLASS draw_limits IMPLEMENTATION.

  METHOD build.
    result = VALUE #( ( color = |red|   limit = 12 )
                          ( color = |blue|  limit = 14 )
                          ( color = |green| limit = 13 ) ).
  ENDMETHOD.

ENDCLASS.

CLASS test_game_detector DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO game_detector.

    METHODS setup.
    METHODS build_draw_limits RETURNING VALUE(result) TYPE cube_conundrum=>limits.

    METHODS get_games_inside_limit FOR TESTING.
    METHODS get_sum_of_valid_ids FOR TESTING.
    METHODS get_sum_of_power_of_cubes FOR TESTING.

ENDCLASS.

CLASS test_game_detector IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
    cut->set_limits( NEW draw_limits( )->build( ) ).
    cut->run( NEW input_values( )->build( ) ).
    cut->build_minimum_color_stats( ).
  ENDMETHOD.

  METHOD build_draw_limits.
  ENDMETHOD.

  METHOD get_games_inside_limit.
    DATA(expected_values) = NEW expected_result_builder( )->build_expected_results( ).
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->get_game_records( ) ).
  ENDMETHOD.

  METHOD get_sum_of_valid_ids.
    cut->build_minimum_color_stats( ).
    cl_abap_unit_assert=>assert_equals( exp = 8 act = cut->get_sum_of_valid_ids( ) ).
  ENDMETHOD.

  METHOD get_sum_of_power_of_cubes.
    cut->build_minimum_color_stats( ).
    cl_abap_unit_assert=>assert_equals( exp = 2286 act = cut->get_sum_of_power_of_cubes( )  ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  DATA(input_data) = NEW zcl_mbh_file_upload( )->file_upload_in_stringtab( ).
  DATA(game) = NEW game_detector( ).

  game->set_limits( NEW draw_limits( )->build( ) ).
  game->run( input_data ).
  game->build_minimum_color_stats( ).

  WRITE /: |The result of part 1 is: { game->get_sum_of_valid_ids( ) }|.
  WRITE /: |The result of part 2 is: { game->get_sum_of_power_of_cubes( ) }|.
