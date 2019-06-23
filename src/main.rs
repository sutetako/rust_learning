fn main() {}

#[test]
fn overflow() {
    let big_val = std::i32::MAX;
    // let x = big_val + 1; // panic
    let _x = big_val.wrapping_add(1); // ok
}

#[test]
fn float_test() {
    assert_eq!(5f32.sqrt() * 5f32.sqrt(), 5.);
    assert_eq!((-1.01f64).floor(), -2.0);
    assert!((-1. / std::f32::INFINITY).is_sign_negative());
}

#[test]
fn char_test() {
    assert_eq!('*'.is_alphabetic(), false);
    assert_eq!('β'.is_alphabetic(), true);
    assert_eq!('8'.to_digit(10), Some(8));
    assert_eq!('\u{CA0}'.len_utf8(), 3);
    assert_eq!(std::char::from_digit(2, 10), Some('2'));
}

#[test]
fn tuple_test() {
    let text = "I see the eigenvalue in thine eye";
    let (head, tail) = text.split_at(21);
    assert_eq!(head, "I see the eigenvalue ");
    assert_eq!(tail, "in thine eye");
}

#[test]
fn array_test() {
    let lazy_caterer: [u32; 6] = [1, 2, 4, 7, 11, 16];
    let taxonomy = ["Animalia", "arthropoda", "Insecta"];

    assert_eq!(lazy_caterer[3], 7);
    assert_eq!(taxonomy.len(), 3);

    let mut sieve = [true; 10000];
    for i in 2..100 {
        if sieve[i] {
            let mut j = i * i;
            while j < 10000 {
                sieve[j] = false;
                j += i;
            }
        }
    }

    assert!(sieve[211]);
    assert!(!sieve[9876]);

    let mut chaos = [3, 5, 4, 1, 2];
    chaos.sort();
    assert_eq!(chaos, [1, 2, 3, 4, 5]);
}

#[test]
fn vector_test() {
    {
        fn build_vector() -> Vec<i16> {
            let mut v: Vec<i16> = Vec::<i16>::new();
            v.push(10i16);
            v.push(20i16);
            v
        }

        fn build_vector_2() -> Vec<i16> {
            let mut v = Vec::new();
            v.push(10);
            v.push(20);
            v
        }
        let v1 = build_vector();
        let v2 = build_vector_2();
        assert_eq!(v1, v2);
    }
    let mut v1 = vec![2, 3, 5, 7];
    assert_eq!(v1.iter().fold(1, |a, b| a * b), 210);

    v1.push(11);
    v1.push(13);
    assert_eq!(v1.iter().fold(1, |a, b| a * b), 30030);

    let mut v2 = Vec::new();
    v2.push("step");
    v2.push("on");
    v2.push("no");
    v2.push("pets");
    assert_eq!(v2, vec!["step", "on", "no", "pets"]);

    let v3: Vec<i32> = (0..5).collect();
    assert_eq!(v3, [0, 1, 2, 3, 4]);

    let mut v4 = vec!["a man", "a plan", "a canal", "panama"];
    v4.reverse();
    assert_eq!(v4, vec!["panama", "a canal", "a plan", "a man"]);

    let mut v5 = Vec::with_capacity(2);
    assert_eq!(v5.len(), 0);
    assert_eq!(v5.capacity(), 2);

    v5.push(1);
    v5.push(2);
    assert_eq!(v5.len(), 2);
    assert_eq!(v5.capacity(), 2);

    v5.push(3);
    assert_eq!(v5.len(), 3);
    assert_eq!(v5.capacity(), 4);

    let mut v6 = vec![10, 20, 30, 40, 50];

    v6.insert(3, 35);
    assert_eq!(v6, [10, 20, 30, 35, 40, 50]);

    v6.remove(1);
    assert_eq!(v6, [10, 30, 35, 40, 50]);

    let mut v7 = vec!["carmen", "miranda"];
    assert_eq!(v7.pop(), Some("miranda"));
    assert_eq!(v7.pop(), Some("carmen"));
    assert_eq!(v7.pop(), None);

    // let languages: Vec<String> = std::env::args().skip(1).collect();
    let languages = vec!["Lisp", "Scheme", "C", "C++", "Fortran"];
    let mut v8 = Vec::new();
    for l in languages {
        if l.len() % 2 == 0 {
            v8.push("functional");
        } else {
            v8.push("imperative");
        }
    }
    assert_eq!(
        v8,
        [
            "functional",
            "functional",
            "imperative",
            "imperative",
            "imperative"
        ]
    );

    // slice

    let v9: Vec<f64> = vec![0.0, 0.707, 1.0, 0.707];
    let a9: [f64; 4] = [0.0, 0.707, 1.0, 0.707];

    let sv: &[f64] = &v9;
    let sa: &[f64] = &a9;

    assert_eq!(sv[0..2], [0.0, 0.707]);
    assert_eq!(sa[2..], [1.0, 0.707]);
    assert_eq!(&sv[1..3], [0.707, 1.0]);
}

#[test]
fn string_test() {
    // literal
    let speech = "\"Ouch!\" said the well.\n";
    println!("{}", speech);
    println!(
        "In the room the women come and go,
         Singing of Mount Abora"
    );
    println!(
        "It was a bright, cold day in Aplil, and \
         there were four of us \
         more or less."
    );

    let default_win_install_path = r"C:\Program Files\Gorillas";
    println!("{}", default_win_install_path);
    // let pattern = Regex::new(r"\d(\.\d+)*");

    println!(
        r###"
        This raw string started with 'r###"'.
        Therefore it does not end until we reach a quote mark ('"')
        followed immediately by three pound signs ('###'):
    "###
    );

    // byte strings
    let method = b"GET";
    assert_eq!(method, &[b'G', b'E', b'T']);

    let noodles = "noodles".to_string();
    let oodles = &noodles[1..];
    let poodles = "\u{CA0}_\u{CA0}";

    assert_eq!(oodles.len(), 6);
    assert_eq!(poodles.len(), 7);
    assert_eq!(poodles.chars().count(), 3);

    // let mut s = "hello";
    // s[0] = 'c'; error: tye thpe 'str' cannot be mutably indexed
    // s.push('\n'); error: no method named `push` found for type `&str`

    assert_eq!(
        format!("{}° {:02}’ {:02}” N", 24, 5, 23),
        "24° 05’ 23” N".to_string()
    );
    let bits = vec!["veni", "vidi", "vici"];
    assert_eq!(bits.concat(), "venividivici");
    assert_eq!(bits.join(","), "veni,vidi,vici");

    assert!("ONE".to_lowercase() == "one");

    assert!("peanut".contains("nut"));
    assert_eq!("\u{CA0}_\u{CA0}".replace("\u{CA0}", "■"), "■_■");
    assert_eq!("     clean\n".trim(), "clean");

    for word in "veni, vidi, vici".split(", ") {
        assert!(word.starts_with("v"));
    }
}

#[test]
fn ownership_test() {
    let mut v = Vec::new();
    for i in 101..106 {
        v.push(i.to_string());
    }

    let fifth = v.pop().unwrap();
    assert_eq!(fifth, "105");

    let second = v.swap_remove(1);
    assert_eq!(second, "102");

    let third = std::mem::replace(&mut v[2], "substitute".to_string());
    assert_eq!(third, "103");

    assert_eq!(v, vec!["101", "104", "substitute"]);

    struct Person {
        name: Option<String>,
        birth: Option<i32>,
    };

    let mut composers = Vec::new();
    composers.push(Person {
        name: Some("Palestrina".to_string()),
        birth: Some(1525),
    });

    // let first_name = composers[0].name // error

    let first_name = std::mem::replace(&mut composers[0].name, None);
    assert_eq!(first_name, Some("Palestrina".to_string()));
    assert_eq!(composers[0].name, None);
    let birth = composers[0].birth.take();
    assert_eq!(birth, Some(1525));
    assert_eq!(composers[0].birth, None);
}

#[test]
fn copy_test() {
    {
        /*
        struct Label {
            number: u32,
        }

        fn print(l: Label) {
            println!("STAMP: {}", l.number);
        }
        let l = Label { number: 3 };
        print(l);
        println!("My label number is: {}", l.number); // error
        */

        #[derive(Copy, Clone)]
        struct Label {
            number: u32,
        }

        fn print(l: Label) {
            println!("STAMP: {}", l.number);
        }
        let l = Label { number: 3 };
        print(l);
        println!("My label number is: {}", l.number);

        /*
        #[derive(Copy, Clone)]
        struct StringLabel {
            name: String,
        }
        */
    }
}

#[test]
fn rc_test() {
    use std::rc::Rc;

    let s: Rc<String> = Rc::new("shirataki".to_string());
    let t: Rc<String> = s.clone();
    let u: Rc<String> = s.clone();

    assert!(s.contains("shira"));
    assert_eq!(t.find("taki"), Some(5));
    println!("{} are quite chewy, almost bouncy, but lack flavor", u);

    // s.push_str(" noodles"); // error
}

#[test]
fn reference_test() {
    use std::collections::HashMap;
    type Table = HashMap<String, Vec<String>>;
    let mut table = Table::new();
    table.insert(
        "Gesualdo".to_string(),
        vec![
            "many madrigals".to_string(),
            "Tenebrae Responsoria".to_string(),
        ],
    );
    table.insert(
        "Caravaggio".to_string(),
        vec![
            "The Musicians".to_string(),
            "The Calling of St. Matthew".to_string(),
        ],
    );
    table.insert(
        "Cellini".to_string(),
        vec![
            "Perseus with the head of Medusa".to_string(),
            "a salt cellar".to_string(),
        ],
    );

    fn show(table: Table) {
        for (artist, works) in table {
            println!("works by {}", artist);
            for work in works {
                println!("  {}", work);
            }
        }
    }
    fn show_with_ref(table: &Table) {
        for (artist, works) in table {
            println!("works by {}", artist);
            for work in works {
                println!("  {}", work);
            }
        }
    }

    fn sort_works(table: &mut Table) {
        for (_artist, works) in table {
            works.sort();
        }
    }
    show_with_ref(&table);
    assert_eq!(table["Gesualdo"][0], "many madrigals"); // OK
    sort_works(&mut table);
    assert_eq!(table["Gesualdo"][1], "many madrigals"); // OK
    show(table);
    // assert_eq!(table["Cellini"][0], "a salt cellar"); // error, use of moved value

    // implicitily borrows
    struct Anime {
        name: &'static str,
        bechdel_pass: bool,
    };
    let aria = Anime {
        name: "Aria: The Animation",
        bechdel_pass: true,
    };
    let anime_ref = &aria;
    assert_eq!(anime_ref.name, "Aria: The Animation");
    assert_eq!((*anime_ref).name, "Aria: The Animation");
    assert_eq!((*anime_ref).bechdel_pass, true);

    let mut v = vec![1973, 1968];
    v.sort();
    (&mut v).sort();

    let mut x = 10;
    let mut y = 20;
    let mut r = &x;
    let b = true;

    if b {
        r = &y;
    }

    assert!(*r == 20);

    struct Point {
        x: i32,
        y: i32,
    }
    let point = Point { x: 1000, y: 729 };
    let r: &Point = &point;
    let rr: &&Point = &r;
    let rrr: &&&Point = &rr;
    assert_eq!(rrr.x, 1000);
    assert_eq!(rrr.y, 729);

    x = 10;
    y = 10;

    let rx = &x;
    let ry = &y;

    let rrx = &rx;
    let rry = &ry;
    // assert!(rrx <= rry);
    assert!(rrx == rry);
    assert!(!std::ptr::eq(rrx, rry));
    fn factorial(n: usize) -> usize {
        (1..n + 1).fold(1, |a, b| a * b)
    }
    let f = &factorial(6);
    assert_eq!(f + &1009, 1729);

    {
        let r;
        {
            let x = 1;
            r = &x;
            assert_eq!(*r, 1); // OK
        }
        // assert_eq!(*r, 1); // error;
    }

    static mut STASH: &i32 = &128;
    // fn test_func(p: &i32) { // error
    // fn test_func<'a>(p: &'a &i32) { // error too, this is the same as the above definition
    fn test_func(p: &'static i32) {
        // OK
        unsafe {
            STASH = p;
        }
    }

    static WORTH_POINTING_AT: i32 = 1000;
    test_func(&WORTH_POINTING_AT);
    unsafe {
        assert_eq!(STASH, &1000);
    }
    fn smallest(v: &[i32]) -> &i32 {
        let mut s = &v[0];
        for r in &v[1..] {
            if *r < *s {
                s = r;
            }
        }
        s
    }
    {
        let parabola = [9, 4, 1, 0, 1, 4, 9];
        let s = smallest(&parabola);
        assert_eq!(*s, 0);
    }

    /*
    struct S {
        r: &i32,
    }

    let s;
    {
        let x = 10;
        s = S { r: &x };
    }
    */
    // assert_eq!(*s, 10); // error

    /*
    struct S<'a, 'b> {
        x: &'a i32,
        y: &'b i32,
    }

    // fn sum_r_xy<'a, 'b, 'c>(r: &'a &i32, s: S<'b, 'c>) -> i32 {
    fn sum_r_xy(r: &i32, s: S) -> i32 {
        r + s.x + s.y
    }
    // fn first_third<'a>(point: &'a &[i32; 3]) -> (&'a i32, &'a i32) {
    fn first_third(point: &[i32; 3]) -> (&i32, &i32) {
        (&point[0], &point[2])
    }

    struct StringTable {
        elements: Vec<String>,
    }

    impl StringTable {
        // fn find_by_prefix<'a, 'b>(&'a self, prefix: &'b str) -> Option<&'a String> {
        fn find_by_prefix(&self, prefix: &str) -> Option<&String> {
            for i in 0..self.elements.len() {
                if self.elements[i].starts_with(prefix) {
                    return Some(&self.elements[i]);
                }
            }
            None
        }
    }
    */

    {
        /*
        let v = vec![4, 8, 19, 27, 34, 10];
        let r = &v;
        let aside = v;
        r[0]; // error
        */
        let v = vec![4, 8, 19, 27, 34, 10];
        {
            let r = &v;
            r[0];
        }
        let aside = v;
        assert_eq!(aside[0], 4);
    }
    {
        fn extend(vec: &mut Vec<f64>, slice: &[f64]) {
            for elt in slice {
                vec.push(*elt);
            }
        }
        let mut wave = Vec::new();
        let head = vec![0.0, 1.0];
        let tail = [0.0, -1.0];

        extend(&mut wave, &head);
        extend(&mut wave, &tail);

        assert_eq!(wave, vec![0.0, 1.0, 0.0, -1.0]);

        // extend(&mut wave, &wave); // error
    }
    {
        let mut x = 10;
        {
            let r1 = &x;
            let r2 = &x;
            assert_eq!(r1, r2);
            // x += 10; // error, it is borrowed
        }
        x += 10;
        assert_eq!(x, 20);

        // let m = &mut x; // error, it is also borrowed as immutable

        let mut y = 20;
        let m1 = &mut y;
        // let m2 = &mut y; // error, cannot borrow as mutable more than once
        // let z = y; // error, cannot use 'y' because it was mutably borrowed
        assert_eq!(&20, m1);
    }
}
