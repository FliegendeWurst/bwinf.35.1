#![feature(non_ascii_idents)]
#![feature(custom_derive)]
#![feature(test)]
#![feature(plugin)]

#![plugin(clippy)]

#[cfg(target_os = "linux")] 
extern crate colored;
#[cfg(target_os = "linux")] 
use colored::*;

use std::collections::{HashSet, HashMap};
use std::env;
use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::time::Instant;
use std::path::Path;

#[derive(Clone, Copy, PartialEq)]
pub struct Punkt {
	x: usize,
	y: usize
}

#[derive(PartialEq)]
enum Stabrichtung {
	Horizontal,
	Vertikal
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Puzzle {
	raster: Vec<Vec<i8>> 
}

#[cfg(target_os = "linux")]
impl std::fmt::Display for Puzzle {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		for row in &self.raster {
			write!(f, "\n")?;
			for cell in row {
				match *cell {
					WALL => write!(f, "{}", format!("{:02} ", cell).black().bold())?,
					NIX => write!(f, "{}", format!("{:02} ", cell).black().bold())?,
					AUSGANG => write!(f, "{}", format!("{:02} ", cell).green().bold())?,
					_ if cell % 5 == 0 => write!(f, "{}", format!("{:02} ", cell).red().bold())?,
					_ if cell % 5 == 1 => write!(f, "{}", format!("{:02} ", cell).yellow().bold())?,
					_ if cell % 5 == 2 => write!(f, "{}", format!("{:02} ", cell).blue().bold())?,
					_ if cell % 5 == 3 => write!(f, "{}", format!("{:02} ", cell).purple().bold())?,
					_ if cell % 5 == 4 => write!(f, "{}", format!("{:02} ", cell).cyan().bold())?,
					_ => write!(f, "{:02} ", cell)?
				};
			}
		}
		write!(f, "\n")
	}
}
#[cfg(not(target_os = "linux"))]
impl std::fmt::Display for Puzzle {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		for row in &self.raster {
			write!(f, "\n")?;
			for cell in row {
				write!(f, "{:02} ", cell)?;
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

const WALL: i8 = -3;
const NIX: i8 = -2;
const AUSGANG: i8 = -1;

#[derive(Default)]
struct MyMap<'a> {
	realmap: HashMap<Vec<RotationDir>, Puzzle>,
	valuesset: HashSet<&'a Puzzle>
}

use std::collections::hash_map::Keys;
trait SemiBidirMap<'a> {
	fn insert(&'a mut self, key: Vec<RotationDir>, value: Puzzle);
	fn get(&'a self, key: &[RotationDir]) -> Option<&'a Puzzle>;
	fn remove_key(&'a mut self, key: &Vec<RotationDir>) -> Option<Puzzle>;
	fn contains_key(&'a self, key: &[RotationDir]) -> bool;
	fn contains_value(&'a self, value: &Puzzle) -> bool;
	fn keys(&'a self) -> Keys<Vec<RotationDir>, Puzzle>;
	fn len_keys(&'a self) -> usize;
}

impl<'a> SemiBidirMap<'a> for MyMap<'a> {
	fn get(&'a self, key: &[RotationDir]) -> Option<&'a Puzzle> {
		self.realmap.get(key)
	}
	fn insert(&'a mut self, key: Vec<RotationDir>, value: Puzzle) {
		self.realmap.insert(key.clone(), value);
		self.valuesset.insert(self.realmap.get(&key).unwrap());
	}
	fn remove_key(&'a mut self, key: &Vec<RotationDir>) -> Option<Puzzle> {
		self.realmap.remove(key)
	}
	fn contains_key(&'a self, key: &[RotationDir]) -> bool {
		self.realmap.contains_key(key)
	}
	fn contains_value(&'a self, value: &Puzzle) -> bool {
		self.valuesset.contains(value)
	}
	fn keys(&'a self) -> Keys<Vec<RotationDir>, Puzzle> {
		self.realmap.keys()
	}
	fn len_keys(&'a self) -> usize {
		self.realmap.len()
	}
}

impl<'a> MyMap<'a> {
	fn new() -> MyMap<'a> {
		MyMap { realmap: HashMap::new(), valuesset: HashSet::new() }
	}
}

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
			match c.to_digit(36) {
				Some(digit) => line_vec.push(digit as i8),
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
	let mut roted_raster = puzzle.raster.clone();
	macro_rules! set {
		($x:expr, $y:expr, $new:expr) => (
			roted_raster[$y][$x] = $new;
		)
	}
	
	for (y, row) in puzzle.raster.iter().enumerate() {
		for (x, cell) in row.iter().enumerate() {
			match *dir {
				RotationDir::CW => set!(s-y-1, x, *cell),
				RotationDir::CCW => set!(y, s-x-1, *cell)
			}
		}
	}
	Puzzle { raster: roted_raster }
}

fn get_stäbchen(raster: &[Vec<i8>]) -> HashMap<u64, Vec<Punkt>> {
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
	if stab.len() > 1 && stab[0].x == stab[1].x {
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
			let richtung = get_stäbchen_richtung(stab);
			let stab = stab.iter();
			let mut move_possible = stab.clone()
				.fold(true, |acc, &x|
					// falls der Stab schon direkt über dem Rahmen ist, 
					//  sollte er nicht bewegt werden
					if x.y >= s-2
					// falls der Stab (horizontal) liegt und unter ihm kein Platz ist,
					//  sollte er nicht bewegt werden
					|| (richtung == Stabrichtung::Horizontal && ((get!(x.x, x.y+1) != NIX && get!(x.x, x.y+1) != AUSGANG))) { false } else { acc });
			
			// falls der Stab (vertikal) steht,
			if richtung == Stabrichtung::Vertikal {
				// und der unterste Teil des Stabes
				let lowest_punkt = stab.clone().max_by_key(|x| x.y).unwrap();
				// über/auf dem unteren Begrenzung liegt
				if lowest_punkt.y+1 <= s-1 {
					// und unter ihm bereits ein Stab liegt,
					if get!(lowest_punkt.x, lowest_punkt.y+1) >= 0 {
						// sollte er nicht bewegt werden
						move_possible = false;
					// sonderfall: unter dem Teil des Stabes liegt der Ausgang
					} else if get!(lowest_punkt.x, lowest_punkt.y+1) == AUSGANG {
						//println!("Puzzle ist möglich!");
						move_possible = true;
					}
				}
			}
			
			if move_possible {
				let stab: Vec<&Punkt> = stab.collect();
				for p in &stab {
					set!(p.x, p.y, NIX);
				}
				let index = *index as i8;
				for p in &stab {
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
	puzzle.raster
	.last().unwrap().iter()
	.fold(
		false, |acc, &x| 
		// falls dieses Element zu einem Stab gehört,
		//  wird acc auf true gesetzt
		if x >= 0 { true } 
		else { acc })
}

fn apply_choices<'a>(puzzle: Puzzle, choices: &'a [RotationDir], cache: &'a mut MyMap<'a>) -> Puzzle {
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
	//let mut cache = HashMap::new();
	let mut my_cache = MyMap::new();
	let mut known_outcomes = HashSet::new();
	let mut base_paths = vec![vec![]];
	let mut depth = 0;
	let start = Instant::now();
	loop {
		{
			let paths: Vec<Vec<RotationDir>> = my_cache.keys().cloned().collect();
			for path in paths {
				if path.len() < depth {
					my_cache.remove_key(&path);
				}
			}
		}
		depth += 1;
		
		let children = build_children(&base_paths);
		if children.is_empty() {
			return None;
		}
		base_paths = Vec::new();
		for child_path in &children {
			let tmp_puzzle = apply_choices(puzzle.clone(), child_path, &mut my_cache);
			// !known_outcomes.contains(&tmp_puzzle)
			if !my_cache.contains_value(&tmp_puzzle) {
				base_paths.push(child_path.clone());
			} else {
				//cache.remove(path); would be removed next layer anyways
			}
			if puzzle_gelöst(&tmp_puzzle) {
				return Some(child_path.clone());
			} else {
				known_outcomes.insert(tmp_puzzle);
			}
		}
		println!("{:03} depth, {:06} children, {:07} known, {:07} cached, {:04} sec", depth, children.len(), known_outcomes.len(), my_cache.len_keys(), start.elapsed().as_secs());
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
		println!("{}", puzzle);
		let solution = breadth_first_search(puzzle.clone());
		if let Some(solution) = solution {
			println!("lösung: {:?}", solution);
			println!("{}", apply_choices(puzzle, &solution, &mut MyMap::new()));
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
#[allow(useless_vec)]
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
	
	#[test]
	fn t1() {
		let puzzle = lade_puzzle("rotation1_03.txt");
		if puzzle.is_err() {
			panic!();
		}
		let puzzle = puzzle.unwrap();
		//println!("Puzzle: {}", puzzle);
		assert!(breadth_first_search(puzzle).unwrap().len() == 6);
	}
	
	#[test]
	fn t2() {
		let puzzle = lade_puzzle("rotation2_03.txt");
		if puzzle.is_err() {
			panic!();
		}
		let puzzle = puzzle.unwrap();
		//println!("Puzzle: {}", puzzle);
		assert!(breadth_first_search(puzzle).unwrap().len() == 22);
	}
	
	#[test]
	fn t3() {
		let puzzle = lade_puzzle("rotation3_03.txt");
		if puzzle.is_err() {
			panic!();
		}
		let puzzle = puzzle.unwrap();
		//println!("Puzzle: {}", puzzle);
		assert!(breadth_first_search(puzzle).unwrap().len() == 90);
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
