use seventeen::four_d as four;
use std::collections::HashSet;

#[test]
fn test_parse_slice_active_set() {
    let input = "
.#.
..#
###
";
    let expected_active = HashSet::from_iter([
        (0,0,0,0),
        (1,0,0,0),
        (2,0,0,0),
        (2,1,0,0),
        (1,2,0,0),
    ]);

    let result = four::parse_slice(input).active;

    assert_eq!(expected_active, result, "\nEXPECTED: {:#?}\nACTUAL:{:#?}", expected_active, result)
}


#[test]
fn test_build_neighbors() {
    let coord = (0, 0, 0, 0);

    let mut result: Vec<four::Coord> = Vec::from_iter(four::build_neighbors(&coord));
    result.sort();

    let expected = [(-1, -1, -1, -1), (-1, -1, -1, 0), (-1, -1, -1, 1), (-1, -1, 0, -1), (-1, -1, 0, 0), (-1, -1, 0, 1), (-1, -1, 1, -1), (-1, -1, 1, 0), (-1, -1, 1, 1), (-1, 0, -1, -1), (-1, 0, -1, 0), (-1, 0, -1, 1), (-1, 0, 0, -1), (-1, 0, 0, 0), (-1, 0, 0, 1), (-1, 0, 1, -1), (-1, 0, 1, 0), (-1, 0, 1, 1), (-1, 1, -1, -1), (-1, 1, -1, 0), (-1, 1, -1, 1), (-1, 1, 0, -1), (-1, 1, 0, 0), (-1, 1, 0, 1), (-1, 1, 1, -1), (-1, 1, 1, 0), (-1, 1, 1, 1), (0, -1, -1, -1), (0, -1, -1, 0), (0, -1, -1, 1), (0, -1, 0, -1), (0, -1, 0, 0), (0, -1, 0, 1), (0, -1, 1, -1), (0, -1, 1, 0), (0, -1, 1, 1), (0, 0, -1, -1), (0, 0, -1, 0), (0, 0, -1, 1), (0, 0, 0, -1), (0, 0, 0, 1), (0, 0, 1, -1), (0, 0, 1, 0), (0, 0, 1, 1), (0, 1, -1, -1), (0, 1, -1, 0), (0, 1, -1, 1), (0, 1, 0, -1), (0, 1, 0, 0), (0, 1, 0, 1), (0, 1, 1, -1), (0, 1, 1, 0), (0, 1, 1, 1), (1, -1, -1, -1), (1, -1, -1, 0), (1, -1, -1, 1), (1, -1, 0, -1), (1, -1, 0, 0), (1, -1, 0, 1), (1, -1, 1, -1), (1, -1, 1, 0), (1, -1, 1, 1), (1, 0, -1, -1), (1, 0, -1, 0), (1, 0, -1, 1), (1, 0, 0, -1), (1, 0, 0, 0), (1, 0, 0, 1), (1, 0, 1, -1), (1, 0, 1, 0), (1, 0, 1, 1), (1, 1, -1, -1), (1, 1, -1, 0), (1, 1, -1, 1), (1, 1, 0, -1), (1, 1, 0, 0), (1, 1, 0, 1), (1, 1, 1, -1), (1, 1, 1, 0), (1, 1, 1, 1)];
    assert!(result.iter().eq(expected.iter()),
            "\nEXPECTED: {:?}\nactual: {:?}\n",
            expected, result);
}

#[test]
fn test_neighbor_count() {
    let coord = (0, 0, 0, 0);

    let result = four::build_neighbors(&coord).len();

    assert_eq!(result, 80,
            "\nEXPECTED: {:?}\nACTUAL: {:?}\n",
            80, result);
}

#[test]
fn test_next_state() {
    let mut state = four::parse_slice("
.#.
..#
###
");

    four::next_state(&mut state);

    assert_eq!(state.active.iter().count(), 29);
}

#[test]
fn test_cycles() {
    let mut state = four::parse_slice("
.#.
..#
###
");

    four::cycle(6, &mut state);

    assert_eq!(state.active.iter().count(), 848,
    "\nEXPECTED: {}\nACTUAL: {}", 848, state.active.iter().count());
}
