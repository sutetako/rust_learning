fn main() {
    let v1 = build_vector();
    let v2 = build_vector_2();
    assert_eq!(v1, v2);
}

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
    // s[0] = 'c'; error: tye thpe 'str' cannnot be mutably indexed
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
    }

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
