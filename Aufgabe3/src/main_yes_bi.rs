#![feature(non_ascii_idents)]
#![feature(custom_derive)]
#![feature(test)]
#![feature(plugin)]

#![plugin(clippy)]

extern crate colored;
use colored::*;


use std::collections::{HashSet, HashMap};
use std::env;
use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::time::Instant;
use std::path::Path;


#[derive(PartialEq)]
enum Stabrichtung {
	Horizontal,
	Vertikal
}

#[derive(Clone, Copy, Debug, Hash, PartialEq)]
pub struct Punkt {
	x: usize,
	y: usize
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Puzzle {
	raster: Vec<Vec<i64>> 
}

impl std::fmt::Display for Puzzle {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		for row in &self.raster {
			write!(f, "\n")?;
			for cell in row {
				match *cell {
					WALL => write!(f, "{}", format!("{:02} ", cell).black())?,
					NIX => write!(f, "{}", format!("{:02} ", cell).black())?,
					AUSGANG => write!(f, "{}", format!("{:02} ", cell).green())?,
					_ if cell % 5 == 0 => write!(f, "{}", format!("{:02} ", cell).red())?,
					_ if cell % 5 == 1 => write!(f, "{}", format!("{:02} ", cell).yellow())?,
					_ if cell % 5 == 2 => write!(f, "{}", format!("{:02} ", cell).blue())?,
					_ if cell % 5 == 3 => write!(f, "{}", format!("{:02} ", cell).purple())?,
					_ if cell % 5 == 4 => write!(f, "{}", format!("{:02} ", cell).cyan())?,
					_ => write!(f, "{:02} ", cell)?
				};
			}
		}
		write!(f, "\n")
	}
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum RotationDir {
	// clockwise
	CW,
	// counterclockwise
	CCW
}

const WALL: i64 = -3;
const NIX: i64 = -2;
const AUSGANG: i64 = -1;


pub fn lade_puzzle(dateiname: &str) -> Result<Puzzle, String> {
	let path = Path::new(dateiname);
	let display = path.display();
	
	// Open the path in read-only mode, returns `io::Result<File>`
	let mut file = match File::open(&path) {
		// The `description` method of `io::Error` returns a string that
		// describes the error
		Err(why) => { return Err(format!("couldn't open {}: {}", display,
		                                 why.description())); },
		Ok(file) => file,
	};
	
	// Read the file contents into a string, returns `io::Result<usize>`
	let mut s = String::new();
	if let Err(why) = file.read_to_string(&mut s) {
		return Err(format!("couldn't read {}: {}", display,
		                                           why.description()));
	}
	let mut p = Puzzle { raster: Vec::new() };
	let lines: Vec<&str> = s.split('\n').collect();
	let seitenlänge = lines[0].parse().expect("erste Zeile keine Zahl");
	
	for (index, line) in lines.iter().skip(1).enumerate() {
		// zeile nach der letzten zeile erreicht
		if index == seitenlänge {
			break;
		}
		
		let mut line_vec = Vec::new();
		for (x, c) in line.chars().enumerate() {
			// ausgang
			if c == ' ' && ((x == 0 || x == line.len()-1) || (index == 0 || index == seitenlänge-1)) {
				line_vec.push(AUSGANG);
				continue;
			}
			match c.to_digit(10) {
				Some(digit) => line_vec.push(digit as i64),
				// -1 == " "
				None => {
					match c {
						'#' => line_vec.push(WALL),
						' ' => line_vec.push(NIX),
						_ => panic!("unbekannter buchstabe {:?}", c)
					}
				}
			}
		}
		p.raster.push(line_vec);
		//println!("{:?}: {:?}", index, line);
	}
	Ok(p)
}

pub fn rotate(puzzle: Puzzle, dir: &RotationDir) -> Puzzle {
	// seitenlänge
	let s = puzzle.raster.len();
	let mut raster = Vec::new();
	for r in &puzzle.raster {
		let mut row_vec = Vec::new();
		for c in r {
			row_vec.push(*c);
		}
		raster.push(row_vec);
	}
	let mut roted_raster = raster.clone();
	macro_rules! set {
		($x:expr, $y:expr, $new:expr) => (
			roted_raster[$y][$x] = $new;
		)
	}
	
	let mut p = puzzle;
	
	let mut y = 0;
	for row in raster {
		for (x, cell) in row.iter().enumerate() {
			match *dir {
				RotationDir::CW => set!(s-y-1, x, *cell),
				RotationDir::CCW => set!(y, s-x-1, *cell)
			}
		}
		y += 1;
	}
	p.raster = roted_raster;
	p
}

fn get_stäbchen(raster: &[Vec<i64>]) -> HashMap<u64, Vec<Punkt>> {
	let mut stäbchen: HashMap<u64, Vec<Punkt>> = HashMap::new();
	
	for (y, row) in raster.iter().enumerate() {
		for (x, cell) in row.iter().enumerate() {
			if !cell.is_negative() {
				let cell = *cell as u64;
				let value = stäbchen.entry(cell).or_insert_with(Vec::new);
				value.push(Punkt { x: x, y: y });
			}
		}
	}
	
	stäbchen
}

fn get_stäbchen_richtung(stab: &[Punkt]) -> Stabrichtung {
	if stab.len() == 1 {
		// egal
		return Stabrichtung::Vertikal;
	}
	if stab[0].x == stab[1].x {
		Stabrichtung::Vertikal
	} else {
		Stabrichtung::Horizontal
	}
}

pub fn do_the_gravity(mut puzzle: Puzzle) -> Puzzle {
	// seitenlänge
	let s = puzzle.raster.len();
	macro_rules! get {
		($x:expr, $y:expr) => (
			puzzle.raster[$y][$x];
		)
	}
	macro_rules! set {
		($x:expr, $y:expr, $new:expr) => (
			puzzle.raster[$y][$x] = $new;
		)
	}
	
	let mut stäbchen1 = get_stäbchen(&puzzle.raster);
	let mut stäbchen2 = HashMap::new();
	while stäbchen1 != stäbchen2 {
		//println!("sloop");
		stäbchen1 = get_stäbchen(&puzzle.raster);
		//println!("{:?}", stäbchen1);
		for (index, stab) in &stäbchen1 {
			let mut move_possible = true;
			let richtung = get_stäbchen_richtung(stab);
			let mut highest_y = 0;
			let mut lowest_punkt = &stab[0];
			for p in stab {
				if p.y == s-2 {
					move_possible = false;
				}
				if p.y+1 <= s-1 && ((get!(p.x, p.y+1) != NIX && get!(p.x, p.y+1) != AUSGANG) && richtung == Stabrichtung::Horizontal) {
					move_possible = false;
				}
				if richtung == Stabrichtung::Vertikal && p.y > highest_y {
					highest_y = p.y;
					lowest_punkt = p;
				}
			}
			if richtung == Stabrichtung::Vertikal && lowest_punkt.y+1 <= s-1 {
				if get!(lowest_punkt.x, lowest_punkt.y+1) >= 0 {
					//println!("{:?} blocked-low because of {:?}, {:?}: {:?}", index,
					//                                                         lowest_punkt.x,
					//                                                         lowest_punkt.y+1,
					//                                                         get!(lowest_punkt.x, lowest_punkt.y+1));
					move_possible = false;
				} else if highest_y == s-2 && get!(lowest_punkt.x, lowest_punkt.y+1) == AUSGANG {
					//println!("Puzzle ist möglich!");
					move_possible = true;
				}
			}
			
			
			if move_possible { // checked
				let new_stab = stab;
				for p in stab {
					set!(p.x, p.y, NIX);
				}
				let index = *index as i64;
				for p in new_stab {
					set!(p.x, p.y+1, index);
				}
			}
		}
		stäbchen2 = stäbchen1;
		stäbchen1 = get_stäbchen(&puzzle.raster);
		//println!("eloop");
	}
	//println!("EOG");
	puzzle
}

fn puzzle_gelöst(puzzle: &Puzzle) -> bool {
	let mut ausgang_gefunden = false;
	for row in &puzzle.raster {
		for cell in row {
			if *cell == AUSGANG {
				ausgang_gefunden = true;
			}
		}
	}
	!ausgang_gefunden
}

fn apply_choices(puzzle: Puzzle, choices: &[RotationDir], cache: &mut HashMap<Vec<RotationDir>, Puzzle>) -> Puzzle {
	if cache.contains_key(choices) {
		//println!("{:?} cached", choices);
		cache.get(choices).unwrap().clone()
	} else if choices.len() == 1 {
		//println!("len one");
		do_the_gravity(rotate(puzzle, &choices[0]))
	} else {
		let mut choices = Vec::from(choices);
		let last_action = choices.pop().unwrap();
		let before_last = apply_choices(puzzle, &choices, cache);
		//println!("before: {}", before_last);
		let applied = do_the_gravity(rotate(before_last, &last_action));
		//println!("applied: {}", applied);
		choices.push(last_action);
		cache.insert(choices, applied.clone());
		applied
	}
}



pub fn build_children(base: &[Vec<RotationDir>]) -> Vec<Vec<RotationDir>> {
	let mut children = Vec::new();
	for path in base {
		let mut c1 = path.clone();
		let mut c2 = path.clone();
		c1.push(RotationDir::CW);
		c2.push(RotationDir::CCW);
		children.push(c1);
		children.push(c2);
	}
	children
}

pub fn breadth_first_search(puzzle: Puzzle) -> Option<Vec<RotationDir>> {
	let mut cache = HashMap::new();
	let mut known_outcomes = HashSet::new();
	let mut base_paths = vec![vec![]];
	let mut depth = 0;
	let start = Instant::now();
	loop {
		depth += 1;
		
		let children = build_children(&base_paths);
		if children.is_empty() {
			return None;
		}
		base_paths = Vec::new();
		for child_path in &children {
			let tmp_puzzle = apply_choices(puzzle.clone(), child_path, &mut cache);
			if !known_outcomes.contains(&tmp_puzzle) {
				base_paths.push(child_path.clone());
			} else {
				cache.remove(child_path); // save that RAM!
			}
			if puzzle_gelöst(&tmp_puzzle) {
				return Some(child_path.clone());
			} else {
				known_outcomes.insert(tmp_puzzle);
			}
		}
		println!("{:03} depth, {:06} children, {:07} known, {:07} cached, {:04} sec", depth, children.len(), known_outcomes.len(), cache.len(), start.elapsed().as_secs());
	}
}



fn main() {
	if let Some(dateiname) = env::args().nth(1) {
		//let pause = std::time::Duration::from_millis((16.6*4.0) as u64);
		
		let puzzle = lade_puzzle(&dateiname);
		if let Err(e) = puzzle {
			println!("Error: {:?}", e);
			return;
		}
		let puzzle = puzzle.unwrap();
		println!("Puzzle: {}", puzzle);
		println!("{:?}", puzzle);
		//let puzzle = Puzzle { seitenlänge: 10, raster: vec![vec![-3, -3, -3, -3, -3, -3, -3, -3, -3, -3], vec![-3, -2, -2, -2, -2, -2, -2, -2, -2, -3], vec![-3, -2, -2, -2, -2, -2, -2, -2, -2, -3], vec![-3, -2, -2, -2, -2, -2, -2, -2, -2, -3], vec![-3, -2, -2, -2, -2, 7, 7, 7, 5, -3], vec![-3, -2, -2, -2, 1, 1, -2, -2, 5, -3], vec![-3, -2, -2, -2, 2, -2, 8, 8, 8, -3], vec![-3, 9, 9, 0, 2, 3, 3, -2, -2, -3], vec![-3, 4, 4, 0, 6, 6, 6, 6, 6, -3], vec![-3, -3, -3, -3, -3, -1, -3, -3, -3, -3]] };
		/*
		//let puzzle = do_the_gravity(rotate(puzzle, &RotationDir::CW));
		let puzzle_a = apply_choices(puzzle.clone(), &vec![RotationDir::CW, RotationDir::CW, RotationDir::CCW], &mut HashMap::new());
		println!("Puzzlea: {}", puzzle_a);
		let puzzle_b = do_the_gravity(rotate(puzzle.clone(), &RotationDir::CCW));
		let puzzle_b = do_the_gravity(rotate(puzzle_b, &RotationDir::CCW));
		let puzzle_b = do_the_gravity(rotate(puzzle_b, &RotationDir::CCW));
		let puzzle_b = do_the_gravity(rotate(puzzle_b, &RotationDir::CCW));
		let puzzle_b = do_the_gravity(rotate(puzzle_b, &RotationDir::CW));
		let puzzle_b = do_the_gravity(rotate(puzzle_b, &RotationDir::CCW));
		println!("Puzzleb: {}", puzzle_b);
		*/
		let solution = breadth_first_search(puzzle);
		if let Some(solution) = solution {
			println!("lösung: {:?}", solution);
		} else {
			println!("keine lösung gefunden!");
		}
		/*
		let steps = [RotationDir::CCW, RotationDir::CW, RotationDir::CW, RotationDir::CW, RotationDir::CW, RotationDir::CCW, RotationDir::CCW, RotationDir::CCW, RotationDir::CW, RotationDir::CW, RotationDir::CW, RotationDir::CCW, RotationDir::CCW, RotationDir::CCW, RotationDir::CCW, RotationDir::CCW, RotationDir::CCW, RotationDir::CCW, RotationDir::CW, RotationDir::CW, RotationDir::CCW, RotationDir::CCW, RotationDir::CW, RotationDir::CW, RotationDir::CW, RotationDir::CW, RotationDir::CW, RotationDir::CW, RotationDir::CW, RotationDir::CCW, RotationDir::CW, RotationDir::CW, RotationDir::CW, RotationDir::CW, RotationDir::CW, RotationDir::CW, RotationDir::CW, RotationDir::CW, RotationDir::CW, RotationDir::CW, RotationDir::CW, RotationDir::CCW, RotationDir::CCW, RotationDir::CCW, RotationDir::CW, RotationDir::CW, RotationDir::CW, RotationDir::CW, RotationDir::CW, RotationDir::CCW, RotationDir::CCW, RotationDir::CCW, RotationDir::CCW, RotationDir::CCW, RotationDir::CW, RotationDir::CW, RotationDir::CW, RotationDir::CW, RotationDir::CCW, RotationDir::CCW, RotationDir::CW, RotationDir::CW, RotationDir::CW, RotationDir::CW, RotationDir::CW, RotationDir::CW, RotationDir::CW, RotationDir::CCW, RotationDir::CW, RotationDir::CCW, RotationDir::CCW, RotationDir::CCW, RotationDir::CCW, RotationDir::CCW, RotationDir::CCW, RotationDir::CCW, RotationDir::CCW, RotationDir::CCW, RotationDir::CCW, RotationDir::CCW, RotationDir::CW, RotationDir::CCW, RotationDir::CCW, RotationDir::CW, RotationDir::CW, RotationDir::CW, RotationDir::CW, RotationDir::CW, RotationDir::CCW, RotationDir::CW];
		for s in steps.iter() {
			println!("action: {:?}", s);
			puzzle = do_the_gravity(rotate(puzzle, s));
			println!("{}", puzzle);
			std::thread::sleep(pause);
		}
		*/
	} else {
		println!("Bitte so aufrufen: ./aufgabe3 <dateiname>");
	}
}


#[cfg(test)]
mod tests {
	use super::*;
	
	#[test]
	fn children() {
		let children = build_children(&vec![vec![RotationDir::CW]]);
		let expected = vec![vec![RotationDir::CW, RotationDir::CW], vec![RotationDir::CW, RotationDir::CCW]];
		if children == expected {
			()
		} else {
			panic!()
		}
	}
}

#[cfg(test)]
mod benchs {
	extern crate test;
	
	use super::*;
	use self::test::Bencher;
	
	#[bench]
	fn test1(b: &mut Bencher) {
		b.iter(|| {
			let puzzle = lade_puzzle("rotation1_03.txt");
			if puzzle.is_err() {
				panic!();
			}
			let puzzle = puzzle.unwrap();
			//println!("Puzzle: {}", puzzle);
			breadth_first_search(puzzle);
		});
	}
	#[bench]
	fn test2(b: &mut Bencher) {
		b.iter(|| {
			let puzzle = lade_puzzle("rotation2_03.txt");
			if puzzle.is_err() {
				panic!();
			}
			let puzzle = puzzle.unwrap();
			//println!("Puzzle: {}", puzzle);
			breadth_first_search(puzzle);
		});
	}
	#[bench]
	fn rotate_cw(b: &mut Bencher) {
		let puzzle = lade_puzzle("rotation3_03.txt").unwrap();
		b.iter(|| {
			rotate(puzzle.clone(), &test::black_box(RotationDir::CW))
		})
	}
	#[bench]
	fn rotate_ccw(b: &mut Bencher) {
		let puzzle = lade_puzzle("rotation3_03.txt").unwrap();
		b.iter(|| {
			rotate(puzzle.clone(), &test::black_box(RotationDir::CCW))
		})
	}
	#[bench]
	fn rotate_cw_grav(b: &mut Bencher) {
		let puzzle = lade_puzzle("rotation3_03.txt").unwrap();
		b.iter(|| {
			do_the_gravity(rotate(puzzle.clone(), &test::black_box(RotationDir::CW)))
		})
	}
	#[bench]
	fn rotate_ccw_grav(b: &mut Bencher) {
		let puzzle = lade_puzzle("rotation3_03.txt").unwrap();
		b.iter(|| {
			do_the_gravity(rotate(puzzle.clone(), &test::black_box(RotationDir::CCW)))
		})
	}
}


/*
fn apply_choices(mut puzzle: Puzzle, choices: &Vec<RotationDir>) -> Puzzle {
	for rotation in choices {
		puzzle = do_the_gravity(rotate(puzzle, rotation));
	}
	puzzle
}
*/

/*
fn cmp_vectors(small: &Vec<RotationDir>, large: &Vec<RotationDir>) -> bool {
	if small.len() > large.len() {
		false
	} else {
		let mut same = true;
		for (index, value) in small.iter().enumerate() {
			if value != &large[index] {
				same = false;
			}
		}
		same
	}
}
*/

/*
fn breadth_first_search(puzzle: Puzzle) -> Option<Vec<RotationDir>> {
	let mut choices = vec![RotationDir::CW];
	let mut known_outcomes = HashSet::new();
	let mut banned_paths = HashSet::new();
	let mut cont_counter = 0;
	let mut ban_counter = 0;
	loop {
		for path in &banned_paths {
			if cmp_vectors(&path, &choices) {
				//println!("banned, skip");
				ban_counter += 1;
				choices = next_combination(choices);
				continue;
			}
		}
		if cont_counter % 100 == 0 && cont_counter > 0 {
			println!("{:06} conts, {:06} ban-conts.", cont_counter, ban_counter);
		}
		//println!("trying {:?}", choices);
		let new = apply_choices(puzzle.clone(), &choices);
		if known_outcomes.contains(&new) {
			cont_counter += 1;
			banned_paths.insert(choices.clone());
			choices = next_combination(choices);
			continue;
		}
		if puzzle_gelöst(&new) {
			return Some(choices);
		}
		known_outcomes.insert(new);
		choices = next_combination(choices);
	}
}
*/
/* 
fn breadth_first_search(puzzle: Puzzle, mut known_outcomes: &mut HashSet<Puzzle>, mut choices: Vec<RotationDir>) -> Option<Vec<RotationDir>> {
	//println!("base: {:?}, known: {:?}", choices, known_outcomes.len());
	choices.push(RotationDir::CW);
	let puzzle_cw = apply_choices(puzzle.clone(), &choices);
	if known_outcomes.contains(&puzzle_cw) {
		
	} else {
		known_outcomes.insert(puzzle_cw.clone());
		if puzzle_gelöst(&puzzle_cw) {
			return Some(choices);
		} else {
			if let Some(solution) = breadth_first_search(puzzle.clone(), &mut known_outcomes, choices.clone()) {
				return Some(solution);
			}
		}
	}
	
	choices.pop().unwrap();
	choices.push(RotationDir::CCW);
	let puzzle_cw = apply_choices(puzzle.clone(), &choices);
	if known_outcomes.contains(&puzzle_cw) {
		return None;
	} else {
		known_outcomes.insert(puzzle_cw.clone());
		if puzzle_gelöst(&puzzle_cw) {
			return Some(choices);
		} else {
			if let Some(solution) = breadth_first_search(puzzle.clone(), &mut known_outcomes, choices.clone()) {
				return Some(solution);
			} else {
				return None
			}
		}
	}
	
	None
}
*/

/*
pub fn next_combination(choices: Vec<RotationDir>) -> Vec<RotationDir> {
	let mut choices = choices;
	if choices.iter().filter(|&x| x == &RotationDir::CCW).count() == choices.len() {
		choices = choices.iter().map(|_| RotationDir::CW).collect();
		choices.push(RotationDir::CW);
		choices
	} else {
		let mut rchoices = choices.clone();
		rchoices.reverse();
		let mut cw_index = 0;
		for (index, c) in rchoices.iter().enumerate() {
			if c == &RotationDir::CW {
				cw_index = rchoices.len()-index-1;
				break;
			}
		}
		choices[cw_index] = RotationDir::CCW;
		for mut item in choices.iter_mut().skip(cw_index+1) {
			*item = RotationDir::CW;
		}
		choices
	}
}
*/

/*
	#[test]
	fn a() {
		let next = next_combination(vec![RotationDir::CW]);
		let expected = vec![RotationDir::CCW];
		if next == expected {
			()
		} else {
			panic!()
		}
	}
	
	#[test]
	fn b() {
		let next = next_combination(vec![RotationDir::CCW]);
		let expected = vec![RotationDir::CW, RotationDir::CW];
		if next == expected {
			()
		} else {
			panic!()
		}
	}
	
	#[test]
	fn aa() {
		let next = next_combination(vec![RotationDir::CW, RotationDir::CW]);
		let expected = vec![RotationDir::CW, RotationDir::CCW];
		if next == expected {
			()
		} else {
			panic!()
		}
	}
	
	#[test]
	fn ab() {
		let next = next_combination(vec![RotationDir::CW, RotationDir::CCW]);
		let expected = vec![RotationDir::CCW, RotationDir::CW];
		if next == expected {
			()
		} else {
			panic!()
		}
	}
	
	#[test]
	fn ba() {
		let next = next_combination(vec![RotationDir::CCW, RotationDir::CW]);
		let expected = vec![RotationDir::CCW, RotationDir::CCW];
		if next == expected {
			()
		} else {
			panic!()
		}
	}
	
	#[test]
	fn bb() {
		let next = next_combination(vec![RotationDir::CCW, RotationDir::CCW]);
		let expected = vec![RotationDir::CW, RotationDir::CW, RotationDir::CW];
		if next == expected {
			()
		} else {
			panic!()
		}
	}
	
	#[test]
	fn aba() {
		let next = next_combination(vec![RotationDir::CW, RotationDir::CCW, RotationDir::CW]);
		let expected = vec![RotationDir::CW, RotationDir::CCW, RotationDir::CCW];
		if next == expected {
			()
		} else {
			panic!()
		}
	}
	
	#[test]
	fn abb() {
		let next = next_combination(vec![RotationDir::CW, RotationDir::CCW, RotationDir::CCW]);
		let expected = vec![RotationDir::CCW, RotationDir::CW, RotationDir::CW];
		if next == expected {
			()
		} else {
			panic!()
		}
	}
	
	#[test]
	fn abba() {
		let next = next_combination(vec![RotationDir::CW, RotationDir::CCW, RotationDir::CCW, RotationDir::CW]);
		let expected = vec![RotationDir::CW, RotationDir::CCW, RotationDir::CCW, RotationDir::CCW];
		if next == expected {
			()
		} else {
			panic!()
		}
	}*/
