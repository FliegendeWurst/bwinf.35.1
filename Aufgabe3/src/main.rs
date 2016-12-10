#![feature(non_ascii_idents)] // für umlaute in variablen_namen
#![feature(test)] // für benchmarks

/* Generelle Informationen
 * - die Makros set und get setten und getten
 *    jeweils einen Punkt im Raster
 * - farbige Ausgabe des Puzzles nur mit Linux und evtl. Mac
 * - Groß- und Kleinschreibung (in kommentaren) eher weniger
 * - index eines Stabes == die Zahl in der Datei (0-9 und A-Z bzw. 0-36)
 */

const RAHMEN: i8 = -3;
const LEER: i8 = -2;
const AUSGANG: i8 = -1;

const MINIMALE_AUSGÄNGE: usize = 0; // problemlos veränderbar (Aufgabenstellung -> 1)
const MAXIMALE_AUSGÄNGE: usize = 1000; // problemlos veränderbar (Aufgabenstellung -> 1)

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


#[derive(Clone, Eq, Hash, PartialEq)]
pub struct Puzzle {
	raster: Vec<Vec<i8>> 
}

// meistens nur als Vec<Punkt> == stab verwendet
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Punkt {
	x: usize,
	y: usize
}

// in der Gravitationsfunktion benutzt
#[derive(PartialEq)]
enum StabRichtung {
	Horizontal,
	Vertikal
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum DrehRichtung {
	// UhrzeigerSinn (rechtsrum)
	US,
	// Gegen den UhrzeigerSinn (linksrum)
	GUS
}

// zum farbigen anzeigen des puzzles
#[cfg(target_os = "linux")]
impl std::fmt::Display for Puzzle {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		for zeile in &self.raster {
			for zelle in zeile {
				match *zelle {
					RAHMEN => write!(f, "{}", format!("{:02} ", zelle).black().bold())?,
					LEER => write!(f, "{}", format!("{:02} ", zelle).black().bold())?,
					AUSGANG => write!(f, "{}", format!("{:02} ", zelle).green().bold())?,
					// hiermit werden die Farben abwechselnd verteilt,
					// die gleiche Farbe wird aber evtl. an mehrere Stäbe verteilt
					_ if zelle % 5 == 0 => write!(f, "{}", format!("{:02} ", zelle).red().bold())?,
					_ if zelle % 5 == 1 => write!(f, "{}", format!("{:02} ", zelle).yellow().bold())?,
					_ if zelle % 5 == 2 => write!(f, "{}", format!("{:02} ", zelle).blue().bold())?,
					_ if zelle % 5 == 3 => write!(f, "{}", format!("{:02} ", zelle).purple().bold())?,
					_ if zelle % 5 == 4 => write!(f, "{}", format!("{:02} ", zelle).cyan().bold())?,
					_ => write!(f, "{:02} ", zelle)?
				};
			}
			write!(f, "\n")?;
		}
		Ok(())
	}
}

// und nochmal für windows usw. (ohne farben)
#[cfg(not(target_os = "linux"))]
impl std::fmt::Display for Puzzle {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		for zeile in &self.raster {
			for zelle in zeile {
				write!(f, "{:02} ", zelle)?;
			}
			write!(f, "\n")?
		}
		Ok(())
	}
}

// weil ich es kann, wird bei mir [US, GUS] als [↻, ↺] angezeigt
impl std::fmt::Display for DrehRichtung {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		if self == &DrehRichtung::US {
			write!(f, "↻")
		} else {
			write!(f, "↺")
		}
	}
}

// lädt ein Puzzle aus einer Datei und kann Fehler zurückgeben
pub fn lade_puzzle(dateiname: &str) -> Result<Puzzle, String> {
	let pfad = Path::new(dateiname);
	
	let mut datei = match File::open(&pfad) {
		Err(fehler) => { return Err(format!("Konnte Datei {} nicht öffnen: {}", dateiname,
		                                    fehler.description())); },
		Ok(datei) => datei,
	};
	
	let mut s = String::new();
	if let Err(fehler) = datei.read_to_string(&mut s) {
		return Err(format!("Konnte Datei {} nicht lesen: {}",
		                   dateiname, fehler.description()));
	}
	let zeilen: Vec<&str> = s.split('\n').collect();
	let mut puzzle = Puzzle { raster: Vec::with_capacity(zeilen.len()) };
	let seitenlänge = zeilen[0].parse();
	if seitenlänge.is_err() {
		return Err("erste Zeile der Datei enthält mehr oder weniger als eine Zahl".to_string());
	}
	let seitenlänge = seitenlänge.unwrap();
	if seitenlänge < 3 {
		return Err(format!("Puzzle-Raster ist zu klein, Seitenlänge == {:?}", seitenlänge).to_string());
	}
	
	// in den Beispielen ist der Index der Stäbe jeweils zwischen 0-9
	let mut gefundene_stab_indexe = Vec::new();
	let mut gefundene_ausgänge = 0;
	
	// skip(1) weil die erste Zeile nur die Seitenlänge ist
	for (y, zeile) in zeilen.iter().skip(1).enumerate() {
		// zeile nach der letzten raster-zeile erreicht
		// (nach dem Raster könnten noch Kommentare folgen)
		if y == seitenlänge {
			break;
		}
		
		let mut raster_zeile = Vec::with_capacity(zeile.len());
		for (x, buchstabe) in zeile.chars().enumerate() {
			// der Ausgang kann entweder links, rechts, oben oder unten sein
			if buchstabe == ' ' && ((x == 0 || x == zeile.len()-1) || (y == 0 || y == seitenlänge-1)) {
				gefundene_ausgänge += 1;
				if gefundene_ausgänge > MAXIMALE_AUSGÄNGE {
					return Err("Zu viele Ausgänge in Datei".to_string())
				}
				raster_zeile.push(AUSGANG);
				continue;
			}
			// zahlen von 0-9 und buchstaben von A-Z oder a-z werden anerkannt (A == 10, Z == 36)
			match buchstabe.to_digit(36) {
				Some(zahl) => {
					if !gefundene_stab_indexe.contains(&zahl) {
						gefundene_stab_indexe.push(zahl);
					}
					raster_zeile.push(zahl as i8)
				},
				// buchstabe ist keine zahl..
				None => {
					match buchstabe {
						'#' => raster_zeile.push(RAHMEN),
						' ' => raster_zeile.push(LEER),
						// mit '_' wird jeder buchstabe "gematcht"
						_ => return Err(format!("Unbekannter Buchstabe: {:?}", buchstabe))
					}
				}
			}
		}
		if seitenlänge != raster_zeile.len() {
			return Err(format!("Seitenlänge stimmt nicht mit Länge der Raster-Zeile {:?} überein!", y+1))
		}
		puzzle.raster.push(raster_zeile);
	}
	
	if seitenlänge != puzzle.raster.len() {
		return Err("Seitenlänge stimmt nicht mit Raster-Höhe überein".to_string())
	}
	
	if gefundene_stab_indexe.is_empty() {
		return Err("Leider enthält das Raster keine Stäbe :(".to_string())
	}
	
	gefundene_stab_indexe.sort(); // aufsteigend, also z.b. [0, 1, ..]
	let erster = gefundene_stab_indexe[0];
	if erster != 0 {
		return Err("Die Stäbe müssen von 0 aufsteigend nummeriert sein.".to_string())
	}
	// hier wird fold missbraucht, um sicherzustellen, dass die Stäbe
	//  von 0 ansteigend durchgängig nummeriert sind
	gefundene_stab_indexe.iter().skip(1).fold(erster, |acc, &x| if (x as isize) - 1 == acc as isize { x } else { panic!("Die Stäbe sind nicht aufsteigend nummeriert :(") });
	
	// Stäbe, bei denen nicht klar ist, ob sie vertikal oder
	// horizontal vorliegen, werden hier erkannt
	if !verifiziere_stäbchen(&finde_stäbchen(&puzzle.raster)) {
		return Err("Stäbchen ergeben keinen Sinn".to_string())
	}
	
	// falls der Rahmen zu wenig Ausgänge enthält oder
	// unvollständig ist, wird ein Fehler ausgegeben
	if !verifiziere_rahmen(&puzzle.raster) {
		return Err("Rahmen des Puzzles ist nicht korrekt aufgebaut".to_string())
	}
	
	Ok(puzzle)
}

fn verifiziere_rahmen(raster: &[Vec<i8>]) -> bool {
	// Rahmen, deren Länge zwei oder kleiner beträgt,
	// könnene keine Stäbe oder Ausgänge enthalten
	if raster.len() <= 2 {
		false
	} else {
		let mut gefundene_ausgänge = 0;
		for (y, zeile) in raster.iter().enumerate() {
			// ist die erste und letzte erste Zahl ein ausgang oder der rahmen
			if !(zeile[0] != RAHMEN || zeile.last() != Some(&RAHMEN))
			&& !(zeile[0] != AUSGANG || zeile.last() != Some(&AUSGANG)) {
				return false;
			}
			if y == 0 || y == raster.len()-1 {
				// an den Ecken des Rahmens darf nur der Rahmen sein,
				// und nicht z.b. der Ausgang
				if zeile[0] != RAHMEN || zeile.last() != Some(&RAHMEN) {
					return false;
				}
				for feld in zeile {
					match *feld {
						AUSGANG => gefundene_ausgänge += 1,
						// in der obersten und untersten Zeile
						// sollten keine Stäbe sein
						_ if *feld >= 0 => return false,
						_ => {}
					}
				}
			}
		}
		// logisch
		if !(gefundene_ausgänge >= MINIMALE_AUSGÄNGE) || !(gefundene_ausgänge <= MAXIMALE_AUSGÄNGE) {
			false
		} else {
			true
		}
	}
}

// überprüft, dass alle stäbchen eindeutig vertikal
// oder horizontal sind
fn verifiziere_stäbchen(stäbchen: &[Vec<Punkt>]) -> bool {
	// keine Stäbchen -> unnötiges Puzzle?!
	if stäbchen.len() == 0 {
		false
	} else {
		for stab in stäbchen {
			if !verifiziere_stab(stab) {
				return false;
			}
		}
		true
	}
}
// überprüft, ob ein Stab eindeutig vertikal
// oder horizontal ist
fn verifiziere_stab(stab: &[Punkt]) -> bool {
	// Stab mit Länge 0 -> eig. unmöglich
	if stab.len() == 0 {
		false
	} else {
		let eig_richtung = get_stäbchen_richtung(stab);
		if eig_richtung == StabRichtung::Horizontal {
			// bei horizontalen Stäben sollte die y-Koordinate immer gleich sein
			let eig_y = stab[0].y;
			for p in stab {
				if p.y != eig_y {
					return false;
				}
			}
		} else {
			// bei vertikalen Stäben sollte die x-Koordinate immer gleich sein
			let eig_x = stab[0].x;
			for p in stab {
				if p.x != eig_x {
					return false;
				}
			}
		}
		true
	}
}

pub fn drehe(mut puzzle: Puzzle, dir: &DrehRichtung) -> Puzzle {
	// seitenlänge
	let s = puzzle.raster.len();
	
	// makros wie immer
	macro_rules! get {
		($x:expr, $y:expr) => (
			puzzle.raster[$y][$x]
		)
	}
	macro_rules! set {
		($x:expr, $y:expr, $new:expr) => (
			puzzle.raster[$y][$x] = $new;
		)
	}
	let maxx;
	let maxy;
	if s % 2 == 0 { // gerade seitenlänge
		// mit 'x' markierte Punkte werden unten in der Schleife
		// unten bearbeitet
		/* - - - - - -
		 * - x x     -
		 * - x x     -
		 * -         -
		 * -         -
		 * - - -   - - */
		maxx = s/2-1;
		maxy = s/2-1;
	} else { // ungerade seitenlänge
		// Punkt 'c' bleibt gleich
		/* - - - - - - -
		 * - x x x     -
		 * - x x x     -
		 * -     c     -
		 * -           -
		 * -           -
		 * - - -   - - - */
		maxx = ((s as f32/2.0)-0.5) as usize;
		maxy = ((s as f32/2.0)-1.5) as usize;
	}
	// "Bereiche" oben-links bis unten-links
	let mut ol;
	let mut or;  // enthält den Wert des Punktes,
	let mut orx; // die x-Koordinate des Punktes und
	let mut ory; // die y-Koordinate des Punktes
	// usw.
	let mut ur; let mut urx; let mut ury;
	let mut ul; let mut ulx; let mut uly;
	// oberer linker Bereich wird abgearbeitet
	for oly in 0..maxy+1 {
		for olx in 0..maxx+1 {
			ol = get!(olx, oly);
			orx = s-oly-1; ory = olx; or = get!(orx, ory);
			urx = s-ory-1; ury = orx; ur = get!(urx, ury);
			ulx = s-ury-1; uly = urx; ul = get!(ulx, uly);
			if dir == &DrehRichtung::US {
				set!(orx, ory, ol);
				set!(urx, ury, or);
				set!(ulx, uly, ur);
				set!(olx, oly, ul);
				// Ergebnis: (nur das Innere des Rahmens)
				// Punkt B erhält den Wert von A (ol)
				// C von B (or)
				// D von C (ur)
				// A von D (ul)
				/* - A - -
				 * - - - B
				 * D - - -
				 * - - C - */
			} else {
				// hier genau umgekehrt wie oben
				set!(ulx, uly, ol);
				set!(urx, ury, ul);
				set!(orx, ory, ur);
				set!(olx, oly, or);
			}
		}
	}
	
	puzzle
}

// findet alle Stäbe in einem Raster bzw. Puzzle
pub fn finde_stäbchen(raster: &[Vec<i8>]) -> Vec<Vec<Punkt>> {
	// hier werden die stäbchen nach ihrem index sortiert gespeichert
	let mut stäbchen = Vec::with_capacity(STÄBCHEN_ANZAHL_SCHÄTZUNG);
	
	// jeden Punkt besuchen
	for (y, zeile) in raster.iter().enumerate() {
		for (x, zelle) in zeile.iter().enumerate() {
			// falls negativ: kein teil eines stabes
			if !zelle.is_negative() {
				// in richtigen "zahltyp" umrechnen
				let zelle = *zelle as usize;
				// falls der Vektor noch keinen Vektor für diesen
				// Stab enthält, wird er bis dahin aufgefüllt
				if stäbchen.is_empty() || stäbchen.len() - 1 < zelle {
					for _ in stäbchen.len()..zelle+1 {
						stäbchen.push(Vec::new());
					}
				}
				// danach wird der Punkt im Vektor gespeichert
				stäbchen.get_mut(zelle).expect("Code 0-un").push(Punkt { x: x, y: y });
			}
		}
	}
	
	stäbchen
}

fn get_stäbchen_richtung(stab: &[Punkt]) -> StabRichtung {
	// falls der Stab kein Würfel ist und die x-Koordinaten der
	// ersten beiden elemente gleich sind,
	if stab.len() > 1 && stab[0].x == stab[1].x {
		// ist er vertikal
		StabRichtung::Vertikal
	} else {
		StabRichtung::Horizontal
	}
}

// diese Variable dient nur der Optimierung
//  (diese Kapazität wird der Vektor, der in finde_stäbchen erstellt wird,
//  zunächst haben (diese Kapatizät nachträglich zu vergrößern benötigt
//  einige Zeit))
const STÄBCHEN_ANZAHL_SCHÄTZUNG: usize = 10;

pub fn wende_gravitation_an(mut puzzle: Puzzle) -> Puzzle {
	// seitenlänge
	let s = puzzle.raster.len();
	// makros wie immer
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
	
	// erst alle Stäbchen finden
	let stäbchen = finde_stäbchen(&puzzle.raster);
	
	// mithilfe dieses Vektor werden die Stäbe sortiert, s.u.
	let mut unterste_teile_der_stäbe = Vec::with_capacity(STÄBCHEN_ANZAHL_SCHÄTZUNG);
	for stab in &stäbchen {
		// findet den Punkt mit der größten y-Koordinate,
		// also den untersten
		let unterster_punkt = stab.iter().max_by_key(|&x| x.y);
		match unterster_punkt {
			// dies passiert nur, falls der Stab keine Elemente hat (unmöglich)
			None => panic!("Code 1-un"),
			Some(punkt) => unterste_teile_der_stäbe.push(punkt)
		}
	}
	// unterste Punkte der stäbe nach ihrer y-Koordinate sortieren,
	// sodass die stäbe von unten nach oben
	// auf einmal bewegt werden können
	let mut sortiert = unterste_teile_der_stäbe.clone();
	sortiert.sort_by_key(|&x| x.y);
	sortiert.reverse();
	
	// diese Schleife beginnt also mit dem untersten Stab
	for punkt_aus_stab in sortiert {
		// stab_index == zahl des stabes (in den Beispielen nur 0-9)
		let mut stab_index = None;
		// manuelle Suche des punktes in unterste_punkt,
		// um den eigentlichen Index des Stabes zu erfahren
		for (index, punkt) in unterste_teile_der_stäbe.iter().enumerate() {
			if punkt == &punkt_aus_stab {
				stab_index = Some(index);
				break;
			}
		}
		let stab_index = stab_index.expect("Code 2-un");
		let stab = stäbchen.get(stab_index).expect("Code 3-un");
		let richtung = get_stäbchen_richtung(stab);
		// die Schritte, die der Stab nach unten fallen soll
		let mut schritte = 0;
		
		if richtung == StabRichtung::Vertikal {
			let unterster_punkt = punkt_aus_stab;
			// hier wird berechnet, wie viele der Punkte bis zum Rahmen (einschließlich dem Rahmen)
			// hintereinander leer sind
			for y in unterster_punkt.y+1..s {
				if get!(unterster_punkt.x, y) == LEER || get!(unterster_punkt.x, y) == AUSGANG {
					schritte += 1;
				} else { // ein anderer Stab oder der Rahmen wurde "getroffen"
					break;
				}
			}
		} else {
			// die y-Koordinate der horizontalen Stäbe ist für jeden teil gleich
			let y_level = stab[0].y;
			
			// hier wird berechnet, wie viele der Punkte bis zum Rahmen (einschließlich dem Rahmen)
			// hintereinander leer sind
			for y in y_level+1..s {
				// diese variable gibt an, ob der Stab noch weiter bewegt
				// werden sollte
				let mut sollte_bewegen = true;
				for p in stab {
					// falls unter irgendeinem der Punkte des Stabes kein freier Platz ist,
					// sollte der Stab nicht weiter bewegt werden 
					if !(get!(p.x, y) == LEER || get!(p.x, y) == AUSGANG) {
						sollte_bewegen = false;
						break;
					}
				}
				if sollte_bewegen {
					schritte += 1;
				} else { // ein anderer Stab oder der Rahmen wurde "getroffen"
					break;
				}
			}
			
		}
		
		// falls schritte == 0 ist, passiert nichts
		for offset in 0..schritte {
			// erste vorherige Position mit LEER ersetzen
			for p in stab {
				set!(p.x, p.y+offset, LEER);
			}
			// dann neue Position mit stab_index füllen
			for p in stab {
				set!(p.x, p.y+offset+1, stab_index as i8);
			}
		}
	}
	puzzle
}

// gibt an, ob das Puzzle gelöst ist
fn puzzle_gelöst(puzzle: &Puzzle) -> bool {
	puzzle.raster
	// unterer Teil des Rahmens
	.last().expect("Code 4-un").iter()
	.fold(
		false, |acc, &x| 
		// falls dieses Element zu einem Stab gehört,
		//  wird acc auf true gesetzt
		if x >= 0 { true }
		else { acc })
}

// wendet einen Entscheidungsweg an (mithilfe eines caches)
fn wende_entscheidungen_an(puzzle: Puzzle, ew: &[DrehRichtung], cache: &mut HashMap<Vec<DrehRichtung>, Puzzle>) -> Puzzle {
	// ew == Entscheidungsweg
	if cache.contains_key(ew) {
		cache.get(ew).expect("Code 5-un").clone()
	} else if ew.len() == 1 { // z.b. [US] oder [GUS] (cache lohnt nicht)s
		wende_gravitation_an(drehe(puzzle, &ew[0]))
	} else {
		// Bsp. ew == [US, GUS, US]
		let mut ew = Vec::from(ew);
		// Bsp. US
		let letzte_drehung = ew.pop().unwrap();
		// Bsp. Ergebnis des EWs [US, GUS]
		let puzzle_vor_letzter_drehung = wende_entscheidungen_an(puzzle, &ew, cache);
		// Bsp. Ergebnis der Drehung US auf vorige variable
		let endzustand = wende_gravitation_an(drehe(puzzle_vor_letzter_drehung, &letzte_drehung));
		ew.push(letzte_drehung);
		cache.insert(ew, endzustand.clone());
		endzustand
	}
}

pub fn erstelle_nächste_schicht(base: &[Vec<DrehRichtung>]) -> Vec<Vec<DrehRichtung>> {
	let mut nächste_schicht = Vec::new();
	// Bsp. base == [[US]]
	// -> nächste_schicht == [[US, US], [US, GUS]]
	for path in base {
		
		let mut c1 = path.clone();
		let mut c2 = path.clone();
		c1.push(DrehRichtung::US);
		c2.push(DrehRichtung::GUS);
		nächste_schicht.push(c1);
		nächste_schicht.push(c2);
	}
	nächste_schicht
}

// findet einen EW, der das Puzzle löst oder stellt
// fest, dass es keine Lösung gibt
pub fn optimierte_breiten_suche(puzzle: Puzzle) -> Option<Vec<DrehRichtung>> {
	// EW == Entscheidungsweg, z.b. [US, GUS, US]
	// cache wie in der Dokumentation beschriben
	let mut cache = HashMap::new();
	// wie in der Dokumentation
	let mut bekannte_zustände = HashSet::new();
	// ein leerer EW == vec![]
	let mut base = vec![vec![]];
	let mut tiefe = 0; // für Fortschritts-Anzeige
	let start_zeit = Instant::now(); // ^
	loop {
		let paths: Vec<Vec<DrehRichtung>> = cache.keys().cloned().collect();
		for path in paths {
			// falls der Entscheidungsweg kleiner als die Tiefe ist,
			// wird er eh nie mehr aus dem Cache geholt,
			// weil der Cache immer zuerst versucht, die Entscheidungswege
			// der letzten Ebene zu benutzen
			// (betrifft hier also die ebene vor der letzten)
			if path.len() < tiefe {
				cache.remove(&path); // spart RAM
			}
		}
		tiefe += 1;
		
		// aus den EWs der letzten Ebene werden
		// die neuen EWs gebaut
		let nächste_schicht = erstelle_nächste_schicht(&base);
		
		// falls diese leer ist, gibt es keine lösung
		if nächste_schicht.is_empty() {
			return None;
		}
		// eine neue Basis für die nächste runde wird erstellt
		base = Vec::new();
		for ew in &nächste_schicht {
			// EW auf Puzzle anwenden
			let tmp_puzzle = wende_entscheidungen_an(puzzle.clone(), ew, &mut cache);
			// falls noch nicht bekannt
			if !bekannte_zustände.contains(&tmp_puzzle) {
				// an base für nächste runde anfügen
				base.push(ew.clone());
			}
			if puzzle_gelöst(&tmp_puzzle) {
				// puzzle gelöst: EW zurückgeben
				return Some(ew.clone());
			} else {
				// sonst: zustand merken
				bekannte_zustände.insert(tmp_puzzle);
			}
		}
		// "Fortschrittsanzeige"
		println!("{:03} tiefe, {:06} nächste_schicht, {:07} bekannt, {:07} gecached, {:04} sekunden", tiefe, nächste_schicht.len(), bekannte_zustände.len(), cache.len(), start_zeit.elapsed().as_secs());
	}
}

fn main() {
	if let Some(dateiname) = env::args().nth(1) {
		let puzzle = lade_puzzle(&dateiname);
		if let Err(e) = puzzle {
			println!("Fehler beim Einlesen: {:?}", e);
			return;
		}
		
		// Start-Zustand anzeigen
		let puzzle = wende_gravitation_an(puzzle.unwrap());
		println!("{}", puzzle);
		
		if puzzle_gelöst(&puzzle) {
			println!("Puzzle ist bereits gelöst?!");
			return
		}
		
		// hoffentlich eine Lösung finden
		let solution = optimierte_breiten_suche(puzzle.clone());
		if let Some(solution) = solution {
			print!("Lösung: [");
			for (index, drehung) in solution.iter().enumerate() {
				if index < solution.len()-1 {
					print!("{}, ", drehung);
				} else {
					println!("{}]", drehung);
				}
			}
			// gelöstes Puzzle anzeigen
			println!("{}", wende_entscheidungen_an(puzzle, &solution, &mut HashMap::new()));
		} else {
			println!("keine lösung gefunden!");
		}
	} else {
		println!("Bitte so aufrufen: ./target/debug/Aufgabe3 <dateiname>");
	}
}


// ein paar tests
#[cfg(test)]
mod tests {
	use super::*;
	
	#[test]
	fn nächste_schicht() {
		let nächste_schicht = erstelle_nächste_schicht(&vec![vec![DrehRichtung::US]]);
		let erwartung = vec![vec![DrehRichtung::US, DrehRichtung::US], vec![DrehRichtung::US, DrehRichtung::GUS]];
		if nächste_schicht != erwartung {
			panic!()
		}
	}
	
	#[test]
	fn t1() {
		let puzzle = lade_puzzle("rotation1_03.txt").expect("konnte datei nicht laden!");
		assert!(optimierte_breiten_suche(puzzle).expect("puzzle nicht gelöst").len() == 6);
	}
	#[test]
	fn t2() {
		let puzzle = lade_puzzle("rotation2_03.txt").expect("konnte datei nicht laden!");
		assert!(optimierte_breiten_suche(puzzle).expect("puzzle nicht gelöst").len() == 22);
	}
	#[test]
	fn t3() {
		let puzzle = lade_puzzle("rotation3_03.txt").expect("konnte datei nicht laden!");
		assert!(optimierte_breiten_suche(puzzle).expect("puzzle nicht gelöst").len() == 90);
	}
	
	#[test]
	fn stäbchen() {
		let puzzle = lade_puzzle("rotation1_03.txt").expect("konnte datei nicht laden!");
		let expected = vec![vec![Punkt { x: 6, y: 1 }, Punkt { x: 6, y: 2 }],
		                    vec![Punkt { x: 1, y: 3 }, Punkt { x: 2, y: 3 }],
		                    vec![Punkt { x: 3, y: 3 }, Punkt { x: 4, y: 3 }, Punkt { x: 5, y: 3 }, Punkt { x: 6, y: 3 }],
		                    vec![Punkt { x: 1, y: 4 }, Punkt { x: 2, y: 4 }],
		                    vec![Punkt { x: 6, y: 4 }, Punkt { x: 6, y: 5 }, Punkt { x: 6, y: 6 }],
		                    vec![Punkt { x: 1, y: 5 }, Punkt { x: 2, y: 5 }],
		                    vec![Punkt { x: 1, y: 6 }, Punkt { x: 2, y: 6 }, Punkt { x: 3, y: 6 }]];
		assert_eq!(finde_stäbchen(&puzzle.raster), expected);
	}
}

// paar benchmarks
#[cfg(test)]
mod benchs {
	extern crate test;
	
	use super::*;
	use self::test::Bencher;
	
	/* deaktiviert wegen der Fortschrittsanzeige
	#[bench]
	fn test1(b: &mut Bencher) {
		b.iter(|| {
			let puzzle = lade_puzzle("rotation1_03.txt").expect("konnte datei nicht laden!");
			optimierte_breiten_suche(puzzle)
		});
	}
	#[bench]
	fn test2(b: &mut Bencher) {
		b.iter(|| {
			let puzzle = lade_puzzle("rotation2_03.txt").expect("konnte datei nicht laden!");
			optimierte_breiten_suche(puzzle)
		});
	}
	*/
	
	#[bench]
	fn drehe_us(b: &mut Bencher) {
		let puzzle = lade_puzzle("rotation3_03.txt").expect("konnte datei nicht laden!");
		b.iter(|| {
			drehe(puzzle.clone(), &test::black_box(DrehRichtung::US))
		})
	}
	#[bench]
	fn drehe_gus(b: &mut Bencher) {
		let puzzle = lade_puzzle("rotation3_03.txt").expect("konnte datei nicht laden!");
		b.iter(|| {
			drehe(puzzle.clone(), &test::black_box(DrehRichtung::GUS))
		})
	}
	#[bench]
	fn drehe_us_mit_grav(b: &mut Bencher) {
		let puzzle = lade_puzzle("rotation3_03.txt").expect("konnte datei nicht laden!");
		b.iter(|| {
			wende_gravitation_an(drehe(puzzle.clone(), &test::black_box(DrehRichtung::US)))
		})
	}
	#[bench]
	fn drehe_gus_mit_grav(b: &mut Bencher) {
		let puzzle = lade_puzzle("rotation3_03.txt").expect("konnte datei nicht laden!");
		b.iter(|| {
			wende_gravitation_an(drehe(puzzle.clone(), &test::black_box(DrehRichtung::GUS)))
		})
	}
	#[bench]
	fn get_stäbe(b: &mut Bencher) {
		let puzzle = lade_puzzle("rotation3_03.txt").expect("konnte datei nicht laden!");
		b.iter(|| {
			finde_stäbchen(&puzzle.raster)
		})
	}
}
