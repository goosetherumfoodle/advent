use twenty;
use twenty::tiles;
use std::collections::{HashSet, HashMap};

#[test]
fn test_rot_90() {
    let mut tile = tiles::Input { id: "id".to_string(),  top: 0, right: 1, bottom: 2, left: 3}.build();

    tiles::rot_90(&mut tile);

    assert_eq!(tile.sides, [3,0,1,2]);
}

#[test]
fn test_rot_180() {
    let mut tile = tiles::Input { id: "id".to_string(),  top: 0, right: 1, bottom: 2, left: 3}.build();

    tiles::rot_180(&mut tile);

    assert_eq!(tile.sides, [2,3,0,1]);
}

#[test]
fn test_rot_270() {
    let mut tile = tiles::Input { id: "id".to_string(),  top: 0, right: 1, bottom: 2, left: 3}.build();

    tiles::rot_270(&mut tile);

    assert_eq!(tile.sides, [1,2,3,0]);
}

#[test]
fn test_build_top_segment() {
    let root_tile = tiles::Input { id: "first".to_string(),  top: 10, right: 1010, bottom: 2020, left: 1 }.build();
    let root_row = vec![
        root_tile.clone(),
        tiles::Input { id: "second".to_string(), top: 777, right: 1, bottom: 888, left: 2 }.build(),
        tiles::Input { id: "third".to_string(),  top: 222, right: 2, bottom: 333, left: 111 }.build(),
        tiles::Input { id: "fourth".to_string(), top: 11,  right: 20, bottom: 10, left: 3032 }.build(),
        tiles::Input { id: "fifth".to_string(),  top: 119, right: 988, bottom: 759, left: 20 }.build(),

        tiles::Input { id: "sixth".to_string(),  top: 132, right: 30, bottom: 11, left: 659 }.build(),
        tiles::Input { id: "seventh".to_string(),top: 4827, right: 1847, bottom: 9938, left: 30 }.build(),

    ];
    let state = twenty::State {
        tile_lookup: &twenty::build_tile_lookup(&root_row),
        side_lookup: &twenty::build_side_lookup(&root_row),
    };

    let result = twenty::build_top_segment(&state, &root_row);

    let expected = [
        [
            tiles::Tile { id: "sixth".to_string(), sides: [132, 30, 11, 659] },
            tiles::Tile { id: "seventh".to_string(), sides: [4827, 1847, 9938, 30] }
        ], [
            tiles::Tile { id: "fourth".to_string(), sides: [11, 20, 10, 3032] },
            tiles::Tile { id: "fifth".to_string(), sides: [119, 988, 759, 20] }
        ]
    ];
    assert_eq!(result, expected);
}

#[test]
fn test_build_row() {
    let root_tile = tiles::Input { id: "second".to_string(),  top: 20, right: 1, bottom: 8989, left: 2}.build();
    let tiles = vec![
        root_tile.clone(),
        tiles::Input { id: "skip me".to_string(),  top: 20, right: 555, bottom: 777, left: 777}.build(),
        tiles::Input { id: "first".to_string(),    top: 111, right: 222, bottom: 2,   left: 333}.build(),
        tiles::Input { id: "third".to_string(),    top: 1,   right: 999, bottom: 1010, left: 2020}.build(),
    ];
    let state = twenty::State {
        tile_lookup: &twenty::build_tile_lookup(&tiles),
        side_lookup: &twenty::build_side_lookup(&tiles),
    };

    let result: Vec<String> = twenty::build_row(&state, &root_tile)
        .iter()
        .map(|t| t.id.clone() )
        .collect();

    let expected = vec!["first", "second", "third"];
    assert_eq!(result, expected);
}

#[test]
fn test_build_side_lookup() {
    let input = vec![
        tiles::Input { id: "first".to_string(),  top: 1, bottom: 2, left: 3, right: 4 }.build(),
        tiles::Input { id: "second".to_string(), top: 4, bottom: 5, left: 6, right: 7 }.build(),
        tiles::Input { id: "third".to_string(),  top: 7, bottom: 8, left: 9, right: 10 }.build(),
    ];

    let result = twenty::build_side_lookup(&input);

    let mut expected = HashMap::new();
    expected.insert(4, vec!["first".to_string(), "second".to_string()]);
    expected.insert(7, vec!["second".to_string(), "third".to_string()]);
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
        tiles::Input { id: "Tile 2311:".to_string(), top: 300, bottom: 924, left: 318, right: 616 }.build(),
        tiles::Input { id: "Tile 1951:".to_string(), top: 397, bottom: 177, left: 587, right: 318 }.build(),
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
        ("Tile 2311:".to_string(), 300),
        ("Tile 2311:".to_string(), 231),
        ("Tile 2311:".to_string(), 498),
        ("Tile 2311:".to_string(), 616),
        ("Tile 1951:".to_string(), 397),
        ("Tile 1951:".to_string(), 564),
        ("Tile 1951:".to_string(), 841),
        ("Tile 1951:".to_string(), 318)
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

    let expected = tiles::Input { id: "Tile 2311:".to_string(), top: 300, bottom: 924, left: 318, right: 616 }.build();
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
        ("Tile 2311:".to_string(), 300),
        ("Tile 2311:".to_string(), 231),
        ("Tile 2311:".to_string(), 498),
        ("Tile 2311:".to_string(), 616)
    ];
    assert_eq!(result, expected)
}

#[test]
#[ignore]
fn test_parse_sides_2() {
    let input = "
Tile 3079:
#.#.#####.
.--------#
.--------.
#--------.
#--------.
.--------.
#--------#
.--------.
.--------.
..#.###...
";

    let result = twenty::parse_sides_2(input);

    let expected = vec![vec!['a']];
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

    let result = twenty::assemble_input(input);

    let expected = vec![
        [
            tiles::Tile { id: "Tile 2971:".to_string(), sides: [900, 2277, 859, 623] },
            tiles::Tile { id: "Tile 1489:".to_string(), sides: [935, 323, 1347, 2277] },
            tiles::Tile { id: "Tile 1171:".to_string(), sides: [148, 1188, 1926, 323] }
        ], [
            tiles::Tile { id: "Tile 2729:".to_string(), sides: [859, 593, 1722, 1578] },
            tiles::Tile { id: "Tile 1427:".to_string(), sides: [1347, 969, 862, 593] },
            tiles::Tile { id: "Tile 2473:".to_string(), sides: [1926, 1925, 485, 969] }],
        [
            tiles::Tile { id: "Tile 1951:".to_string(), sides: [1722, 1333, 971, 2420] },
            tiles::Tile { id: "Tile 2311:".to_string(), sides: [862, 805, 1441, 1333] },
            tiles::Tile { id: "Tile 3079:".to_string(), sides: [2057, 410, 485, 805] }
        ]
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
// left: `[[Tile { id: "Tile 2971:", sides: [900, 2277, 859, 623] }, Tile { id: "Tile 1489:", sides: [935, 323, 1347, 2277] }, Tile { id: "Tile 1171:", sides: [148, 1188, 1926, 323] }],
//         [Tile { id: "Tile 2729:", sides: [859, 593, 1722, 1578] }, Tile { id: "Tile 1427:", sides: [1347, 969, 862, 593] }, Tile { id: "Tile 2473:", sides: [1926, 1925, 485, 969] }],
//         [Tile { id: "Tile 1951:", sides: [1722, 1333, 971, 2420] }, Tile { id: "Tile 2311:", sides: [862, 805, 1441, 1333] }, Tile { id: "Tile 3079:", sides: [2057, 410, 485, 805] }]]`

// top: [[Tile { id: "Tile 2971:", sides: [900, 2277, 859, 623] }, Tile { id: "Tile 1489:", sides: [935, 323, 1347, 2277] }, Tile { id: "Tile 1171:", sides: [148, 1188, 1926, 323] }]]
// root: [Tile { id: "Tile 2729:", sides: [859, 593, 1722, 1578] }, Tile { id: "Tile 1427:", sides: [1347, 969, 862, 593] }, Tile { id: "Tile 2473:", sides: [1926, 1925, 485, 969] }]
// bott: [[Tile { id: "Tile 1951:", sides: [1722, 1333, 971, 2420] }, Tile { id: "Tile 2311:", sides: [862, 805, 1441, 1333] }, Tile { id: "Tile 3079:", sides: [2057, 410, 485, 805] }]]

// top: [[Tile { id: "Tile 2971:", sides: [900, 2277, 859, 623] },
//        Tile { id: "Tile 1489:", sides: [935, 323, 1347, 2277] },
//        Tile { id: "Tile 1171:", sides: [148, 1188, 1926, 323] }],
//       [Tile { id: "Tile 2729:", sides: [859, 593, 1722, 1578] },
//        Tile { id: "Tile 1427:", sides: [1347, 969, 862, 593] },
//        Tile { id: "Tile 2473:", sides: [1926, 1925, 485, 969] }]]
//     root: [Tile { id: "Tile 1951:", sides: [1722, 1333, 971, 2420] },
//            Tile { id: "Tile 2311:", sides: [862, 805, 1441, 1333] },
//            Tile { id: "Tile 3079:", sides: [2057, 410, 485, 805] }]

// OLD

// [
//     [Tile { id: "Tile 2729:", sides: [680, 9, 397, 962] }, Tile { id: "Tile 1427:", sides: [183, 348, 300, 9] }, Tile { id: "Tile 2473:", sides: [399, 481, 184, 348] }],
//     [Tile { id: "Tile 2971:", sides: [532, 689, 680, 78] }, Tile { id: "Tile 1489:", sides: [43, 288, 183, 689] }],
//     [Tile { id: "Tile 1951:", sides: [397, 318, 177, 587] }, Tile { id: "Tile 2311:", sides: [300, 616, 924, 318] }]]


// top:
// [[Tile { id: "Tile 2729:", sides: [680, 9, 397, 962] }, Tile { id: "Tile 1427:", sides: [183, 348, 300, 9] }, Tile { id: "Tile 2473:", sides: [399, 481, 184, 348] }]
//  [Tile { id: "Tile 2971:", sides: [532, 689, 680, 78] }, Tile { id: "Tile 1489:", sides: [43, 288, 183, 689] }]]
// [[Tile { id: "Tile 1951:", sides: [397, 318, 177, 587] }, Tile { id: "Tile 2311:", sides: [300, 616, 924, 318] }]]


// top: [[Tile { id: "Tile 2971:", sides: [532, 689, 680, 78] }, Tile { id: "Tile 1489:", sides: [43, 288, 183, 689] }],
//       [Tile { id: "Tile 2729:", sides: [680, 9, 397, 962] }, Tile { id: "Tile 1427:", sides: [183, 348, 300, 9] }, Tile { id: "Tile 2473:", sides: [399, 481, 184, 348] }]]
// root: [Tile { id: "Tile 1951:", sides: [397, 318, 177, 587] }, Tile { id: "Tile 2311:", sides: [300, 616, 924, 318] }]


// NEW

// [[Tile { id: "Tile 2971:", sides: [532, 689, 680, 78] },
//   Tile { id: "Tile 1489:", sides: [43, 288, 183, 689] }],
//  [Tile { id: "Tile 2729:", sides: [680, 9, 397, 962] },
//   Tile { id: "Tile 1427:", sides: [183, 348, 300, 9] },
//   Tile { id: "Tile 2473:", sides: [399, 481, 184, 348] }],
//  [Tile { id: "Tile 1951:", sides: [397, 318, 177, 587] },
//   Tile { id: "Tile 2311:", sides: [300, 616, 924, 318] }]]
