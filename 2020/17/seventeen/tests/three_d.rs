use seventeen::three_d as three;
use std::collections::HashSet;

#[test]
fn test_parse_slice_active_set() {
    let input = "
.#.
..#
###
";
    let expected_active = HashSet::from_iter([
            (0,0,0), (1,0,0), (2,0,0),
                              (2,1,0),
                     (1,2,0),
            ]);

    let result = three::parse_slice(input).active;

    assert_eq!(expected_active, result, "\nEXPECTED: {:#?}\nACTUAL:{:#?}", expected_active, result)
}

#[test]
fn test_next_state() {
    let mut state = three::parse_slice("
.#.
..#
###
");

    three::next_state(&mut state);

    assert_eq!(state.active.iter().count(), 11);
}


#[test]
fn test_cycles() {
    let mut state = three::parse_slice("
.#.
..#
###
");

    three::cycle(6, &mut state);

    assert_eq!(state.active.iter().count(), 112);
}

#[test]
fn test_build_neighbors() {
    let coord = (0, 0, 0);

    let mut result: Vec<three::Coord> = Vec::from_iter(three::build_neighbors(&coord));
    result.sort();

    let expected = [(-1, -1, -1), (-1, -1, 0), (-1, -1, 1), (-1, 0, -1), (-1, 0, 0), (-1, 0, 1), (-1, 1, -1), (-1, 1, 0), (-1, 1, 1), (0, -1, -1), (0, -1, 0), (0, -1, 1), (0, 0, -1), (0, 0, 1), (0, 1, -1), (0, 1, 0), (0, 1, 1), (1, -1, -1), (1, -1, 0), (1, -1, 1), (1, 0, -1), (1, 0, 0), (1, 0, 1), (1, 1, -1), (1, 1, 0), (1, 1, 1)];
    assert!(result.iter().eq(expected.iter()),
            "\nEXPECTED: {:?}\nactual: {:?}\n",
            expected, result);
}
