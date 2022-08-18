use twenty;
use twenty::tiles;
use std::collections::HashMap;

#[test]
fn test_rot_90() {
    let mut tile = tiles::Input { id: 1,  top: 0, right: 1, bottom: 2, left: 3}.build();

    tiles::rot_90(&mut tile);

    assert_eq!(tile.sides, [3,0,1,2]);
}

#[test]
fn test_rot_180() {
    let mut tile = tiles::Input { id: 1,  top: 0, right: 1, bottom: 2, left: 3}.build();

    tiles::rot_180(&mut tile);

    assert_eq!(tile.sides, [2,3,0,1]);
}

#[test]
fn test_rot_270() {
    let mut tile = tiles::Input { id: 1,  top: 0, right: 1, bottom: 2, left: 3}.build();

    tiles::rot_270(&mut tile);

    assert_eq!(tile.sides, [1,2,3,0]);
}

#[test]
fn test_build_top_segment() {
    let root_tile = tiles::Input { id: 1,  top: 10, right: 1010, bottom: 2020, left: 1 }.build();
    let root_row = vec![
        root_tile.clone(),
        tiles::Input { id: 2, top: 777, right: 1, bottom: 888, left: 2 }.build(),
        tiles::Input { id: 3,  top: 222, right: 2, bottom: 333, left: 111 }.build(),
        tiles::Input { id: 4, top: 11,  right: 20, bottom: 10, left: 3032 }.build(),
        tiles::Input { id: 5,  top: 119, right: 988, bottom: 759, left: 20 }.build(),
        tiles::Input { id: 6,  top: 132, right: 30, bottom: 11, left: 659 }.build(),
        tiles::Input { id: 7,  top: 4827, right: 1847, bottom: 9938, left: 30 }.build(),

    ];
    let state = twenty::State {
        tile_lookup: &twenty::build_tile_lookup(&root_row),
        side_lookup: &twenty::build_side_lookup(&root_row),
    };

    let result = twenty::build_top_segment(&state, &root_row);

    let expected = [
        [
            tiles::Tile { id: 6, sides: [132, 30, 11, 659] },
            tiles::Tile { id: 7, sides: [4827, 1847, 9938, 30] }
        ], [
            tiles::Tile { id: 4, sides: [11, 20, 10, 3032] },
            tiles::Tile { id: 5, sides: [119, 988, 759, 20] }
        ]
    ];
    assert_eq!(result, expected);
}

#[test]
fn test_build_row() {
    let root_tile = tiles::Input { id: 2,  top: 20, right: 1, bottom: 8989, left: 2}.build();
    let tiles = vec![
        root_tile.clone(),
        tiles::Input { id: 777,  top: 20, right: 555, bottom: 777, left: 777}.build(),
        tiles::Input { id: 1,    top: 111, right: 222, bottom: 2,   left: 333}.build(),
        tiles::Input { id: 3,    top: 1,   right: 999, bottom: 1010, left: 2020}.build(),
    ];
    let state = twenty::State {
        tile_lookup: &twenty::build_tile_lookup(&tiles),
        side_lookup: &twenty::build_side_lookup(&tiles),
    };

    let result: Vec<u16> = twenty::build_row(&state, &root_tile)
        .iter()
        .map(|t| t.id.clone() )
        .collect();

    let expected = vec![1,2,3];
    assert_eq!(result, expected);
}

#[test]
fn test_build_side_lookup() {
    let input = vec![
        tiles::Input { id: 1,  top: 1, bottom: 2, left: 3, right: 4 }.build(),
        tiles::Input { id: 2, top: 4, bottom: 5, left: 6, right: 7 }.build(),
        tiles::Input { id: 3,  top: 7, bottom: 8, left: 9, right: 10 }.build(),
    ];

    let result = twenty::build_side_lookup(&input);

    let mut expected = HashMap::new();
    expected.insert(4, vec![1, 2]);
    expected.insert(7, vec![2, 3]);
    assert_eq!(result, expected);
}

#[test]
#[ignore]
fn test_parse_all_tiles() {
    let input = "
Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..
";

    let result = twenty::parse_all_tiles(input);

    let expected = [
        tiles::Input { id: 2311, top: 300, bottom: 924, left: 318, right: 616 }.build(),
        tiles::Input { id: 1951, top: 397, bottom: 177, left: 587, right: 318 }.build(),
    ];
    assert_eq!(result, expected)
}

#[test]
#[ignore]
fn test_parse_all_sides() {
    let input = "
Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...
";

    let result = twenty::parse_all_sides(input);

    let expected = vec![
        (2311, 300),
        (2311, 231),
        (2311, 498),
        (2311, 616),
        (1951, 397),
        (1951, 564),
        (1951, 841),
        (1951, 318)
    ];
    assert_eq!(result, expected)
}

#[test]
#[ignore]
fn test_parse_tile() {
    let input = "
Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###
";

    let result = twenty::parse_tile(input);

    let expected = tiles::Input { id: 2311, top: 300, bottom: 924, left: 318, right: 616 }.build();
    assert_eq!(result, expected)
}

#[test]
#[ignore]
fn test_parse_sides() {
    let input = "
Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###
";

    let result = twenty::parse_sides(input);

    let expected = vec![
        (2311, 300),
        (2311, 231),
        (2311, 498),
        (2311, 616)
    ];
    assert_eq!(result, expected)
}

#[test]
#[ignore]
fn test_parse_side() {
    let side = vec!['#','.','#','#'];

    let result = twenty::parse_side(side);

    assert_eq![result, 13];
}


#[test]
fn test_parse_side_flipee() {
    let side1 = vec!['#', '.', '.', '#', '#', '.', '#', '.', '.', '.'];
    let side2 = side1.clone().into_iter().rev().collect();

    let result1 = twenty::parse_side(side1);
    let result2 = twenty::parse_side(side2);

    assert_eq![result1, result2];
}


#[test]
fn test_assemble_input() {
    let input = "
Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
v####..#...
.....##...

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...
";

    let result: Vec<Vec<u16>> = twenty::assemble_input(input)
        .into_iter()
        .map(|row| { row.into_iter().map(|t| t.id ).collect() })
        .collect();

    let expected = vec![
        [2971, 1489, 1171,],
        [2729, 1427, 2473,],
        [1951, 2311, 3079,],
    ];
    assert_eq!(result, expected);
}

#[test]
fn test_cantor_pair_commutativity() {
    let x = 255;
    let y = 524;

    let result1 = twenty::cantor_pair(x,y);
    let result2 = twenty::cantor_pair(y,x);

    assert_eq!(result1, result2);
}

#[test]
fn test_order_dependencies() {
    use rand::thread_rng;
    use rand::seq::SliceRandom;

    for rep in 1..=50 {

        let mut input = vec![
            tiles::Tile { id: 1427, sides: [743250, 237690, 183810, 173754] },
            tiles::Tile { id: 2311, sides: [183810, 279974, 800646, 462990] },
            tiles::Tile { id: 1951, sides: [832861, 462990, 340296, 1439297] },
            tiles::Tile { id: 1171, sides: [1204329, 49590, 8664, 605610] },
            tiles::Tile { id: 1489, sides: [416140, 49590, 743250, 1141078] },
            tiles::Tile { id: 2473, sides: [769777, 62516, 237690, 1204329] },
            tiles::Tile { id: 2971, sides: [296072, 1141078, 325210, 163482] },
            tiles::Tile { id: 2729, sides: [325210, 173754, 832861, 927487] },
            tiles::Tile { id: 3079, sides: [1025457, 65406, 62516, 279974] }
        ];
        input.shuffle(&mut thread_rng());


        let result_square: Vec<Vec<u16>> = twenty::assemble_tiles(input.clone())
            .into_iter()
            .map(|row| { row.into_iter().map(|t| t.id ).collect() })
            .collect();

        assert_eq!(result_square.len(), 3, "input: {:?}", input);
        for row in result_square.clone() {
            assert_eq!(row.len(), 3, "input: {:?}", input);
        }


        let result: u64 =
            result_square[0][0] as u64 *
            result_square[0][2] as u64 *
            result_square[2][0] as u64 *
            result_square[2][2] as u64;


        let expected = 2971 * 1171 * 1951 * 3079;

        assert_eq!(
            result, expected,
            "\nINPUT:\n{:?}\nRESULT:\n{:?}", input, result_square,
        );
    }
}
