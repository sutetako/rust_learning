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

        {
            let mut w = (107, 109);
            w.0 = 108;
            let r = &w;
            let r0 = &r.0;
            // let m1 = &mut r.1; // error: can't reborrow shared as mutable

            assert_eq!(r0, &108);
            assert_eq!(w, (108, 109));
        }
    }
}

#[test]
fn test_expression() {
    // 6.1
    // expression
    // 5 * (fahr-32) / 9;

    /* statement
     for (; begin != end; ++begin) {
        if (*begin == target)
            break;
     }
    */
    /*
     pixels[r * bounds.0 + c] =
        match escapes(Complex { re: point.0, im: point.1 }, 255) {
            None => 0,
            Some(count) => 255 - count as u8
        };
    */
    /*
     let status =
        if cpu.temperature <= MAX_TEMP {
            HttpStatus::Ok
        } else {
            HttpStatus::ServerError
        };
    */
    /*
     println!("Inside the vat, you see {}.",
        match vat.contents {
            Some(brain) => brain.desc(),
            None => "nothing of interest"
        });
    */
    // 6.2
    /*
     let display_name = match post.author() {
        Some(author) => author.name(),
        None => {
            let network_info = post.get_network_metadata()?;
            let ip = network_info.client_address();
            ip.to_string()
        }
     };
    */
    /*
     let msg = {
        // let-declaration: semicolon is always required
        let dandelion_control = puffball.open();

        // expression + semicolon: method is called, return value dropped
        dandelion_control.release_all_seeds(launch_codes);

        // expression with no semicolon: method is called,
        // return value stored in `msg`
        dandelion_control.get_status()
     }
    */

    // 6.3
    /*
     loop {
         work();
         play();
         ;  // <-- empty statement
     }
    */

    /*
     * let name: type = expr;
     */

    /*
    let name;
    if user.has_nickname() {
       name = user.nickname();
    } else {
       name = generate_unique_name();
       user.register(&name);
    }
    */

    /*
    use std::io;
    use std::cmp::Ordering;
    fn show_files() -> io::Result<()> {
       let mut v = vec![];
       ...
       fn cmp_by_timestamp_then_name(a: &FileInfo, b: &FileInfo) -> Ordering {
           a.timestamp.cmp(&b.timestamp)
               .reverse()
               .then(a.path.cmp(&b.path))
       }
       v.sort_by(cmp_by_timestamp_then_name);
    }
    */

    // 6.4
    /*
    if condition1 {
       block1
    } else if condition2 {
       block2
    } else {
       block_n
    }
    */

    /*
     match value {
        pattern => expr,
        ...
     }
    */
    let code = 2;
    match code {
        0 => println!("OK"),
        1 => println!("Wires Tangled"),
        2 => println!("User Asleep"),
        _ => println!("Unrecognized Error {}", code),
    }

    /*
    match params.get("name") {
        Some(name) => println!("Hello, {}!", name),
        None => println!("Greetings, stranger.")
    }
    */

    /*
     let score = match card.rank {
        Jack => 10,
        Queen = > 10,
        Ace = 11
     }; // error: nonexhaustive patterns
    */

    /*
     let suggested_pet =
        if with_wings { Pet::Buzzard } else { Pet::Hyena }; //ok

     let favorite_number =
        if user.is_hobbit() { "eleventy-one" } else { 9 }; //error

     let best_sports_team =
        if is_hockey_season() { "Predators" }; // error
    */

    /*
    let suggested_per =
        match favotites.elements {
            Fire => Pet::RedPanda,
            Air => Pet::Buffalo,
            Water => Pet::Orca,
            _ => None // error: incompatible types
        }
    */

    // 6.4.1
    /*
     if let pattern = expr {
         block1
     } else {
         block2
     }

     match expr {
        pattern => { block1 }
        _ => { block2 }
    */

    /*
    if let Some(cookie) = request.session_cookie {
       return restore_session(cookie);
    }

    if let Err(err) = present_cheesy_anti_robot_task() {
       log_robot_attempt(err);
       politely_accuse_user_of_being_a_robot();
    } else {
       session.mark_as_human();
    }
    */

    // 6.5 loop
    /*
    while condition {
        block
    }

    while let pattern = expr {
        block
    }

    loop {
        block
    }

    for pattern in collection {
        block
    }
    */

    for i in 0..20 {
        println!("{}", i);
    }

    /*
    let strings: Vec<String> = error_messages();
    for s in strings { // each String is moved into s here
        println!("{}", s);
    } // ...and dropped here
    println("{} error(s)", strings.len()); // error: use of moved value
    */

    /*
    for rs in &strings {
        println!("String {:?} is at address {:p}.", *rs, rs); // ok
    }
    */

    /*
    for rs in &mut strings { // tye type of rs is &mut String
        rs.push('\n'); // add a newline to each string
    }
    */

    /*

    for line in input_lines {
        let trimmed = trim_comments_and_whitespac(line);
        if trimmed.is_empty() {
            continue;
        }
        ...
    }
    */
    /*
    'seach:
    for room in apartment {
        for stop in room.hiding_spots() {
            if spot.contains(keys) {
                println!("Your keys are {} in the {}.", spot, room);
                break 'search;
            }
        }
    }
    */

    // 6.6 return
    fn f() {
        // return type omitted: default to ()
        return; // return value comitted: default to ()
    }
    assert_eq!(f(), ());

    /*
    let output = File::create(filename)?;

    let output = match File::create(filename) {
        Ok(f) => f,
        Err(err) => return Err(err)
    };

    */

    // 6.7

    /*
    fn wait_for_process(process: &mut Process) -> i32 {
        while true {
            if process.wait() {
                return process.exit_code();
            }
        }
    } // error: not all control paths return a value
    */

    /*
    fn serve_forever(socket: ServerSocket, handler: ServerHandler) -> ! {
        socket.listen();
        loop {
            let s = socket.accept();
            handler.handle(s);
        }
    }
    */

    // 6.8

    /*
    let x = gcd(1302, 462); // function call
    let room = player.location(); // method call
    let mut numbers = Vec::new(); // static method call

    Iron::new(router).http("localhost:3000").unwrap();

    return Vec<i32>::with_capacity(1000); // error: something about chanined comparisons
    let ramp = (0 .. n).collect<Vec<i32>>(); // same error
    return Vec::<i32>::with_capacity(1000); // ok, using ::<
    let ramp = (0 .. n).collect::<Vec<i32>>(); // ok, using ::<
    return Vec::with_capacity(10); // ok, if the fn return type is Vec<i32>
    let ramp: Vec<i32> = (0 .. n).collect(); // ok, variable's type is given
    */

    // 6.9

    /*
    game.black_pawns // struct field
    coords.1         // tuple element
    pieces[i]        // array element, they are lvalue

    fn quicksort<T: Ord>(slice: &mut [T]) {
        if slice.len() <= 1 {
            return; // Nothing to sort.
        }

        // Partition the slice into two parts, front and back.
        let pivot_index = partition(slice);

        // Recursively sort the front half of `slice`.
        quicksort(&mut slice[.. pivot_index]);
        // And the back half.
        quicksort(&mut slice[pivot_index + 1 ..]);
    }
    */

    // 6.10

    /*
    let padovan: Vec<u64> = compute_padovan_sequence(n);
    for elem in &padovan {
        draw_triangle(turtle, *elem);
    }
    */

    // 6.11
    /*

       println!("{}", -100);     // -100
       println!("{}", -100u32);  // error: can't apply unary '-' to type 'u32'
       println!("{}", +100);     // error: expected expression, found '+'

       let x = 1234.567 % 10.0;  // approximetely 4.567

       let hi: u8 = 0xe0;
       let lo = !hi; // 0x1f
    */

    // 6.12
    /*
    total += item.price;
    // rust does not have increment operator and decrement operator.
    */

    // 6.13
    /*
    let x = 17;             // x is type i32
    let index = x as usize; // convert to usize
    */

    // 6.14
    /*
    let is_even = |x| x % 2 == 0;

    let is_evan = |x: u64| -> bool x % 2 == 0; // error
    */
    let is_even = |x: u64| -> bool { x % 2 == 0 }; // ok
    assert_eq!(is_even(14), true);
}

#[test]
fn error_test() {
    // 7.1.1
    // fn pirate_share(total: u64, crew_size: usize) -> u64 {
    //     let half = total / 2;
    //     half / crew_size as u64
    // }
    // pirate_share(100, 0);

    // 7.2 Result
    // fn get_weather(location: LatLng) -> Result<WeatherReport, io::Error>

    // 7.2.1
    // match get_weather(hometown) {
    //     Ok(report) => {
    //         display_weather(hometown, &report);
    //     }
    //     Err(err) => {
    //         println!("error querying the weather: {}", err);
    //         schedule_weather_retry();
    //     }
    // }
    // A fairly safe prediction for Southern California.
    // const THE_USEAL: WeatherReport = WeatherReport::Sunny(72);
    //
    // result.is_ok()
    // result.is_err()
    //
    // result.ok()
    // result.err()
    //
    // Get a real weather report, if possible.
    // If not, fail back on the usual.
    // let report = get_weather(los_angels).unwrap_or(THE_USEAL);
    // display_weather(los_angels, &report);
    //
    // let report =
    //    get_weather(hometown)
    //    .unwrap_or_else(|_err| vague_prediction(hometown));
    //
    // result.unwrap()
    // result.expect(message)
    // result.as_ref()
    // result.as_mut()
    //
    // 7.2.2
    // fn remove_file(path: &Path) -> Result<()>
    // pub type Result<T> = result::Result<T, Error>;
    //
    // 7.2.3
    // println!("error querying the weather: {}", err);
    // println!("error: {}", err);
    // println!("error: {:?}", err);
    // err.description()
    // err.cause()
    // use std::error::Error;
    // use std::io::{stderr, Write};

    // /// Dump an error message to `stderr`.
    // ///
    // /// If another error happens while building the error message or
    // /// writing to `stderr`, it is ignored.

    // fn print_error(mut err: &Error) {
    //     let _ = writeln!(stderr(), "error: {}", err);
    //     while let Some(cause) = err.cause() {
    //         let _ = writeln!(stderr(), "caused by: {}", cause);
    //         err = cause;
    //     }
    // }
    //
    // 7.2.4
    //
    // let weather = get_weather(hometown)?;
    //
    // let weather = match get_weather(hometown) {
    //    Ok(success_value) => success_value,
    //    Err(err) = > return Err(err)
    // };
    //
    // use std::fs;
    // use std::io;
    // use std::path::Path;
    //
    // fn move_all(src: &Path, dst: &Path) -> io::Result<()> {
    //    for entry_result in src.read_dir()? { // opening dir could fail
    //        let entry = entry_result?;        // reading dir could fail
    //        let dst_file = dst.join(entry.file_name());
    //        fs::rename(entry.path(), dst_file)?; // renaming could fail
    //    }
    //    Ok(())
    // }
    //
    // 7.2.5
    //
    // use std::io::{self, BufRead};
    //
    // /// Read integers from a text file.
    // /// The file sould have one number on each line.
    // fn read_numbers(file: &mut BufRead) -> Result<Vec<i64>, io::Error> {
    //    let mut numbers = vec![];
    //    for line_result in file.lines() {
    //        let line = line_result?;      // reading lines can fail
    //        numbers.push(line.parse()?);  // parsing integers can fail
    //    }
    //    Ok(numbers)
    // }
    //
    // type GenError = Box<std::error:Error>;
    // type GenResult<T> = Result<T, GenError>;
    //
    // let io_error = io::Error::new{           // make our own io::Error
    //     io::ErrorKind::Other, "timed out"};
    // return Err(GenError::from(io_error));    // manually convert to GenError
    //
    // 7.2.7
    // let _ = writeln!(stderr(), "error: {}", err);
    //
    // 7.2.8
    //
    // fn main() {
    //    if let Err(err) = calculate_tides() {
    //        print_error(&err);
    //        std::process::exit(1);
    //    }
    // }
}

#[test]
fn struct_test() {
    // 9.1
    /// A rectangle of eight-bit grayscale pixels
    struct GrayscaleMap {
        pixels: Vec<u8>,
        size: (usize, usize),
    }

    let width = 1024;
    let height = 576;
    // let image = GrayscaleMap {
    //     pixels: vec![0; width * height],
    //     size: (width, height),
    // };

    fn new_map(size: (usize, usize), pixels: Vec<u8>) -> GrayscaleMap {
        assert_eq!(pixels.len(), size.0 * size.1);
        GrayscaleMap { pixels, size }
    }
    let image = new_map((width, height), vec![0; width * height]);

    assert_eq!(image.size, (1024, 576));
    assert_eq!(image.pixels.len(), 1024 * 576);

    // pub struct GrayscaleMap {
    //     pub pixels: Vec<u8>,
    //     pub size: (usize, usize)
    // }

    // pub struct GrayscaleMap {
    //     pixels: Vec<u8>,
    //     size: (usize, usize)
    // }
    //

    struct Broom {
        name: String,
        height: u32,
        health: u32,
        position: (f32, f32, f32),
        intent: BroomIntent,
    }

    /// Two possitble alternatives for what a ~Broom` could be working on.
    #[derive(Copy, Clone)]
    enum BroomIntent {
        FetchWater,
        DumpWater,
    }

    // Receive the input Broom by value, taking ownership.
    fn chop(b: Broom) -> (Broom, Broom) {
        // Initialize `broom1` mostly from `b`, changing only `height`, Since
        // `String` is not `Copy`, `broom1` takes ownership of `b`'s name.
        let mut broom1 = Broom {
            height: b.height / 2,
            ..b
        };

        // Initialize `broom2` mostly from `broom1`. Since `String` is not
        // `Copy`, we must clone `name` explicitly.
        let mut broom2 = Broom {
            name: broom1.name.clone(),
            ..broom1
        };

        broom1.name.push_str(" I");
        broom2.name.push_str(" II");

        (broom1, broom2)
    }

    let hokey = Broom {
        name: "Hokey".to_string(),
        height: 60,
        health: 100,
        position: (100.0, 200.0, 0.0),
        intent: BroomIntent::FetchWater,
    };

    let (hokey1, hokey2) = chop(hokey);
    assert_eq!(hokey1.name, "Hokey I");
    assert_eq!(hokey1.health, 100);

    assert_eq!(hokey2.name, "Hokey II");
    assert_eq!(hokey2.health, 100);

    // 9.2
    struct Bounds(usize, usize);

    let image_bounds = Bounds(1024, 768);
    assert_eq!(image_bounds.0 * image_bounds.1, 786432);

    // pub struct Bounds(pub usize, pub usize);

    // 9.3
    // struct Onesuch;
    // let o = Onesuch;

    // 9.4
    // 9.5

    /// A first-in, first-out queue of characters.
    pub struct Queue {
        older: Vec<char>,   // older elements, eldest last.
        younger: Vec<char>, // younger elements, youngest last.
    }

    impl Queue {
        /// Push a character onto the back of a queue.
        pub fn push(&mut self, c: char) {
            self.younger.push(c);
        }

        /// Pop a character off the front of a queue. Return `Some(c)` if there
        /// was a character to pop, or `None` if the queue was empty.
        pub fn pop(&mut self) -> Option<char> {
            if self.older.is_empty() {
                if self.younger.is_empty() {
                    return None;
                }

                // Bring the elements in younger over to older, and put them in
                // the promised order.
                use std::mem::swap;
                swap(&mut self.older, &mut self.younger);
                self.older.reverse();
            }

            // Now older is guaranteed to have something,. Vec's pop method
            // already returns an Option, so we're set.
            self.older.pop()
        }

        pub fn is_empty(&self) -> bool {
            self.older.is_empty() && self.younger.is_empty()
        }

        pub fn split(self) -> (Vec<char>, Vec<char>) {
            (self.older, self.younger)
        }

        pub fn new() -> Queue {
            Queue {
                older: Vec::new(),
                younger: Vec::new(),
            }
        }
    }

    let mut q = Queue::new();
    // let mut q = Queue {
    //     older: Vec::new(),
    //     younger: Vec::new(),
    // };

    q.push('0');
    q.push('1');
    assert_eq!(q.pop(), Some('0'));

    q.push('∞');
    assert_eq!(q.pop(), Some('1'));
    assert_eq!(q.pop(), Some('∞'));
    assert_eq!(q.pop(), None);

    assert!(q.is_empty());
    q.push('⦿');
    assert!(!q.is_empty());
    q.pop();

    q.push('P');
    q.push('D');
    assert_eq!(q.pop(), Some('P'));
    q.push('X');

    let (older, younger) = q.split();
    // q is now uninitialized.
    assert_eq!(older, vec!['D']);
    assert_eq!(younger, vec!['X']);

    // 9.6

    pub struct QueueT<T> {
        older: Vec<T>,
        younger: Vec<T>,
    }

    impl<T> QueueT<T> {
        pub fn new() -> Self {
            QueueT {
                older: Vec::new(),
                younger: Vec::new(),
            }
        }

        pub fn push(&mut self, t: T) {
            self.younger.push(t);
        }

        pub fn is_empty(&self) -> bool {
            self.older.is_empty() && self.younger.is_empty()
        }
    }

    // let mut qt = QueueT::<char>::new();

    let mut qt = QueueT::new();
    let mut rt = QueueT::new();

    qt.push("CAD"); // apparently a Queue<&'static str>
    rt.push(0.74); // apparently a Queue<f64>

    qt.push("BTC"); // Bitcoins per USD, 2017-5
    rt.push(2737.7); // Rust fails to detect ittational exuberance

    // 9.7

    struct Extrema<'elt> {
        greatest: &'elt i32,
        least: &'elt i32,
    }

    fn find_extrema<'s>(slice: &'s [i32]) -> Extrema<'s> {
        let mut greatest = &slice[0];
        let mut least = &slice[0];

        for i in 1..slice.len() {
            if slice[i] < *least {
                least = &slice[i];
            }
            if slice[i] > *greatest {
                greatest = &slice[i];
            }
        }
        Extrema { greatest, least }
    }

    let a = [0, -3, 0, 15, 48];
    let e = find_extrema(&a);
    assert_eq!(*e.least, -3);
    assert_eq!(*e.greatest, 48);

    // 9.8
    // #[derive(Copy, Clone, Debug, PartialEq)]
    // struct Point {
    //     x: f64,
    //     y: f64,
    // }

    // 9.9

    // pub struct SpiderRobot {
    //     species: String,
    //     web_enabled: bool,
    //     log_device: [fd::FileDesc; 8],
    //     ...
    // }

    // use std::rc::Rc;
    // pub struct SpiderSenses {
    //     robot: Rc<SpiderRobot>, /// <-- pointer to settings and I/O
    //     eyes: [Camera; 32],
    //     motion: Accelerometer,
    //     ...
    // }

    use std::cell::Cell;
    use std::cell::RefCell;
    use std::fs::File;

    pub struct SpiderRobot {
        hardware_error_count: Cell<u32>,
        log_file: RefCell<File>,
    }

    impl SpiderRobot {
        /// Increase the error count by 1.
        pub fn add_hardware_error(&self) {
            let n = self.hardware_error_count.get();
            self.hardware_error_count.set(n + 1);
        }

        /// True if any hardware errors have been reported.
        pub fn has_hardware_errors(&self) -> bool {
            self.hardware_error_count.get() > 0
        }

        /// Write a line to the log file.
        pub fn log(&self, message: &str) {
            let mut file = self.log_file.borrow_mut();
            // writeln!(file, "{}", message).unwrap();
        }
    }

    let ref_cell: RefCell<String> = RefCell::new("hello".to_string());

    let r = ref_cell.borrow(); // ok, return a Ref<String>
    let count = r.len(); // ok, returns "hello".len()
    assert_eq!(count, 5);

    // let mut w = ref_cell.borrow_mut(); // panic: already borrowed
    // w.push_str(" world");
}

#[test]
fn enum_test() {
    // enum Ordering {
    //     Less,
    //     Equal,
    //     Greater / 2.0
    // }

    use std::cmp::Ordering;

    fn compare(n: i32, m: i32) -> Ordering {
        if n < m {
            Ordering::Less
        } else if n > m {
            Ordering::Greater
        } else {
            Ordering::Equal
        }
    }
    // use std::cmp::Ordering::*;

    // fn compare(n: i32, m: i32) -> Ordering {
    //     if n < m {
    //         Less
    //     } else if n > m {
    //         Greater
    //     } else {
    //         Equal
    //     }
    // }

    // enum Pet {
    //     Orca,
    //     Giraffe,
    // }

    // use self::Pet::*;

    #[derive(Debug, PartialEq)]
    enum HttpStatus {
        Ok = 200,
        NotModified = 304,
        NotFound = 404,
    }

    use std::mem::size_of;
    assert_eq!(size_of::<Ordering>(), 1);
    assert_eq!(size_of::<HttpStatus>(), 2); // 404 doesn't fit in a u8

    assert_eq!(HttpStatus::Ok as i32, 200);

    fn http_status_from_u32(n: u32) -> Option<HttpStatus> {
        match n {
            200 => Some(HttpStatus::Ok),
            304 => Some(HttpStatus::NotModified),
            404 => Some(HttpStatus::NotFound),
            _ => None,
        }
    }

    let status = http_status_from_u32(404).unwrap();
    // assert_eq!(status as i32, 404);
    assert_eq!(status, HttpStatus::NotFound);

    #[derive(Copy, Clone, Debug, PartialEq)]
    enum TimeUnit {
        Seconds,
        Minutes,
        Hours,
        Days,
        Months,
        Years,
    }

    impl TimeUnit {
        /// Return the plural noun for this time unit.
        fn plural(self) -> &'static str {
            match self {
                TimeUnit::Seconds => "seconds",
                TimeUnit::Minutes => "minutes",
                TimeUnit::Hours => "hours",
                TimeUnit::Days => "days",
                TimeUnit::Months => "months",
                TimeUnit::Years => "years",
            }
        }
        /// Return the singular noun for this time unit.
        fn singular(self) -> &'static str {
            self.plural().trim_right_matches('s')
        }
    }

    /// A timestamp that has been deliberately rounded off, so our program
    /// says "6 monthes ago" instead of "February 9, 2016, at 9:49 AM".
    #[derive(Copy, Clone, Debug, PartialEq)]
    enum RoughTime {
        InThePast(TimeUnit, u32),
        JustNow,
        InTheFuture(TimeUnit, u32),
    }

    let four_score_and_seven_years_ago = RoughTime::InThePast(TimeUnit::Years, 4 * 20 + 7);

    let three_hours_from_now = RoughTime::InTheFuture(TimeUnit::Hours, 3);

    struct Point3d(u32, u32, u32);
    enum Shape {
        Sphere { center: Point3d, radius: f32 },
        Cubold { corner1: Point3d, corner2: Point3d },
    }

    let unit_sphere = Shape::Sphere {
        center: Point3d(0, 0, 0),
        radius: 1.0,
    };

    // enum RelationshipStatus {
    //     Single,
    //     InARelationship,
    //     ItsComplicated(Option<String>),
    //     ItsExtremelyComplicated {
    //         car: DifferentialEquation,
    //         cdr: EarlyModernistPoem
    //     }
    // }
    //

    use std::collections::HashMap;
    enum Json {
        Null,
        Boolean(bool),
        Number(f64),
        String(String),
        Array(Vec<Json>),
        Object(Box<HashMap<String, Json>>),
    }

    // An ordered collection of `T`s
    enum BinaryTree<T> {
        Empty,
        NonEmpty(Box<TreeNode<T>>),
    }

    // A part of a BinaryTree.
    struct TreeNode<T> {
        element: T,
        left: BinaryTree<T>,
        right: BinaryTree<T>,
    }

    let jupiter_tree = BinaryTree::NonEmpty(Box::new(TreeNode {
        element: "Jupiter",
        left: BinaryTree::Empty,
        right: BinaryTree::Empty,
    }));
    let mercury_tree = BinaryTree::NonEmpty(Box::new(TreeNode {
        element: "Mercury",
        left: BinaryTree::Empty,
        right: BinaryTree::Empty,
    }));
    let uranus_tree = BinaryTree::NonEmpty(Box::new(TreeNode {
        element: "Uranus",
        left: BinaryTree::Empty,
        right: BinaryTree::Empty,
    }));
    let mars_tree = BinaryTree::NonEmpty(Box::new(TreeNode {
        element: "Mars",
        left: jupiter_tree,
        right: mercury_tree,
    }));
    let tree = BinaryTree::NonEmpty(Box::new(TreeNode {
        element: "Saturn",
        left: mars_tree,
        right: uranus_tree,
    }));

    // let mut tree = BinaryTree::Empty;
    // for planet in planets {
    //    tree.add(planet);
    // }

    // 10.2

    fn rough_time_to_english(rt: RoughTime) -> String {
        match rt {
            RoughTime::InThePast(units, count) => format!("{}, {} ago", count, units.plural()),
            RoughTime::JustNow => format!("just now"),
            RoughTime::InTheFuture(units, 1) => format!("a {} from now", units.plural()),
            RoughTime::InTheFuture(units, count) => {
                format!("{}, {} from now", count, units.plural())
            }
        }
    }

    rough_time_to_english(four_score_and_seven_years_ago);

    // 10.2.1

    // match meadow.count_rabbits() {
    //     0 => {} // nothing to say
    //     1 => println!("A rabbit is nosing around inthe clover."),
    //     n => println!("There are {} rabbits hopping about in the meadow", n)
    // }
    //
    // let calendar =
    //     match settings.get_string("calendar") {
    //        "gregorian" => Calendar::Gregorian,
    //        "chinese" => Calendar::Chinese,
    //        "ethiopian" => Calendar::Ethiopian,
    //        other => return parse_error("calendar", other)
    //     };
    // let caption =
    //     match photo.tagged_pet() {
    //        Pet::Tyrannosaur => "RRRRAAAAAHHHHH",
    //        Pet::Samoyed => "*dog thoughts*",
    //        _ => "I'm cute, love me" // generic caption, works for any pet
    //     }
    // // there are many Shapes, but we only support "selecting"
    // // either some text, or everything in a rectangular area.
    // // You can't select an ellipse or trapezoid.
    // match document.selection() {
    //    Shape::TextSpan(start, end) => paint_text_selection(start, end),
    //    Shape::Rectangle(rect) => paint_rect_selection(rect),
    //    _ => panic!("unexpected selection type")
    // }
    //
    // fn check_move(current_hex: Hex, click: Point) -> game::Result<Hex> {
    //    match point_to_hex(click) {
    //        None =>
    //            Err("That's not a game space."),
    //        Some(current_hex) => // try to match if user clicked the current_hex
    //                             // (if doesn't work)
    //            Err("You are already there! You must click somewhere else."),
    //        Some(other_hex) =>
    //            Ok(other_hex)
    //    }
    // }
    //
    // fn check_move(current_hex: Hex, click: Point) -> game::Result<Hex> {
    //    match point_to_hex(click) {
    //        None =>
    //            Err("That's not a game space."),
    //        Some(hex) =>
    //            if hex == current_hex {
    //                Err("You are already there! You must click somewhere else."),
    //            } else {
    //                Ok(hex)
    //            }
    //        Some(other_hex) =>
    //            Ok(other_hex)
    //    }
    // }
    //
    // fn describe_point(x: i32, y: i32) -> &'static str {
    //     use std::cmp::Ordering::*;
    //     match (x.cmp(&0), y.cmp(&0) {
    //         (Equal, Equal) -> "at the origin",
    //         (_, Equal) => "on the x axis",
    //         (Equal, _) => "on the y axis",
    //         (Greater, Greater) => "in the first quadrant",
    //         (Less, Grater) => "in the second quadrant",
    //         _ => "somewhere else"
    //     }
    // }
    //
    // match balloon.location {
    //     Point { x: 0, y: height } =>
    //        println!("straight up {} meters", height),
    //     Point { x: x, y: y } =>
    //        println!("at ({}m, {}m)", x, y);
    // }
    //
    // match get_acount(id) {
    //    Some(Account { name, language, .. {) =>
    //        language.show_custom_greeting(name)
    // }
    //
    // 10.2.3
    //
    // match account {
    //    Account { name, language, .. } => {
    //        ui.greet(&name, &language);
    //        ui.show_settigs(&account); // error: use of moved value `account`
    //    }
    // }
    // match account {
    //   Account { ref name, ref language, .. } => {
    //        ui.greet(name, language);
    //        ui.show_settings(&account); // ok
    //   }
    // }
    //
    // match line_result {
    //     Err(ref err) => log_error(err), // `err` is &Error (shared ref)
    //     Ok(ref mut line) -> {           // `line` is &mut String (mut ref)
    //         trim_comments(line);        // modify the String in place
    //         handle(line);
    //     }
    // }
    //
    // match sphere.center() {
    //     &Point3d { x, y, z } => ...
    // }
    //
    // match friend.borrow_car() {
    //     Some(&Car { engine, .. }) => // error: can't move out of borrow
    //     ...
    //     None -> {}
    // }
    //
    // Some(&Car {ref engine, .. }) => // ok, engine is a reference
    //
    // match chars.peek() {
    //     Some(&c) -> println!("coming up: {:?}", c),
    //     None =-> println!("end of chars")
    // }
    //
    // 10.2.4
    //
    // let at_end =
    //     match chars.peek() {
    //         Some(&'\r') | Some(&'\n') | None => true,
    //         _ => false
    //     };
    // match next_char {
    //     '0' ... '9' =>
    //         self.read_number(),
    //     'a' ... 'z' | 'A' ... 'Z' =>
    //         self.read_word(),
    //     ' ' | '\t' | '\n' =>
    //         self.skip_whitespace(),
    //     _ =>
    //         self.handle_punctuation()
    // }
    //
    // 10.2.5
    //
    // match robot.last_known_location() {
    //     Some(point) if self.distance_to(point) < 10 =>
    //         short_distance_strategy(point),
    //     Some(point) ->
    //         long_distance_strategy(point),
    //     None ->
    //         searching_strategy()
    // }
    //
    // 10.2.6
    //
    // match self.get_selection() {
    //     Shape::Rect(top_left, bottom_right) ->
    //         optimized_paint(&Shape::Rect(top_left, bottom_right)),
    //     other_shape =>
    //         paint_outline(other_shape.get_outline()),
    // }
    //
    // rect @ Shape::Rect(..) -> optimized_paint(&rect)
    //
    // match chars.next() {
    //     Some(digit @ '0' ... '9') => read_number(disit, chars),
    // }
    //
    // 10.2.7
    //
    // // ...unpack a struct into three new local variables
    // let Track { album, track_number, title, ..} = song;
    //
    // // ...unpack a function argument that's a tuple
    // fn distance_to((x,y): (f64, f64)) -> f64 { ... }
    //
    // // ...iterate over keys and values of a HashMap
    // for (id, document) in &cache_map {
    //    println!("Document #{}: {}", id, document.title);
    // }
    //
    // // ...automatically dereference an argument to a closure
    // // (handy because sometimes other code passes you a reference
    // // when you'd rather have a copy)
    // let sum = numbers.fold(0, |a, &num| a + num);
    //
    // // ...handle just one enum variant specially
    // if let RoughTime::InTheFuture(_, _) = user.date_of_birth() {
    //     user.set_time_traveler(true);
    // }
    //
    // // ...run some code only if a table lookup succeeds
    // if let Some(document) = cache_map.get(&id) {
    //     return send_cached_response(document);
    // }
    //
    // // ...repeatedly try something until it succeeds
    // while let Err(err) = present_cheesy_anti_robot_task() {
    //     log_robot_attempt(err);
    //     // let the user try again (it might still be a human)
    // }
    //
    // // ...manually loop over an iterator
    // while let Some(_) = lines.peek() {
    //     read_paragraph(&mut lines);
    // }
    //
    // 10.2.8

    impl<T: Ord> BinaryTree<T> {
        fn add(&mut self, value: T) {
            match *self {
                BinaryTree::Empty => {
                    *self = BinaryTree::NonEmpty(Box::new(TreeNode {
                        element: value,
                        left: BinaryTree::Empty,
                        right: BinaryTree::Empty,
                    }))
                }
                BinaryTree::NonEmpty(ref mut node) => {
                    if value <= node.element {
                        node.left.add(value);
                    } else {
                        node.right.add(value);
                    }
                }
            }
        }
    }

    let mut add_tree = BinaryTree::Empty;
    add_tree.add("Mercury");
    add_tree.add("Venus");
}

#[test]
fn trait_test() {
    {
        use std::io::Write;

        fn say_hello(out: &mut Write) -> std::io::Result<()> {
            out.write_all(b"hello world\n")?;
            out.flush()
        }

        // use std::fs::File;

        // let mut local_file = File::create("hello.txt");
        // say_hello(&mut local_file).expect("error"); // could not work, now

        let mut bytes = vec![];
        say_hello(&mut bytes).expect("error"); // works
        assert_eq!(bytes, b"hello world\n");

        // 11.1

        let mut buf: Vec<u8> = vec![];
        buf.write_all(b"hello").expect("error");
    }
    // 11.1.1

    {
        use std::io::Write;

        let mut buf: Vec<u8> = vec![];
        // let writer: Write = buf; // error: `Write` does not have a constant size
        let writer: &mut Write = &mut buf; // ok
        writer.write_all(b"hello").expect("error");
        assert_eq!(buf, b"hello");
    }

    // 11.1.3
    {
        use std::io::Write;
        fn say_hello<W: Write>(out: &mut W) -> std::io::Result<()> {
            out.write_all(b"hello world\n")?;
            out.flush()
        }
        let mut buf: Vec<u8> = vec![];
        buf.write_all(b"hello").expect("error");
        buf::<Vec>.write_all(b"hello").expect("error");

        // let v1 = (0 .. 1000).collect(); // error: can't infer type
        let v2 = (0..1000).collect::<Vec<i32>>(); // ok

        // /// Run a query on large, partitioned data set.
        // /// See <http://research.google.com/archive/mapreduce.html>.
        // fn run_query<M: Mapper + Serialize, R: Reducer + Serialize>(data: &dataSet, map: M, reduce: R) -> Results {
        // }
        //
        // fun run_query<M, R>(data: &Dataset, map: M, reduce: R) -> Results
        //     where M: Mapper + Serialize,
        //           R: Reducer + Serialize
        // {}

        // fn nearest<'t, 'c, P>(target: &'t P, candidates: &'c [P]) -> &'c P
        //     where P: MeasureDistance
        // {}
        //
        // impl PancakeStack {
        //    fn Push<:T Topping>(&mut self, goop: T) - PancakeResult<()> {
        //    }
        // }
        // type PancakeResult<T> = Result<T, PancakeError>;
    }
    {
        // struct Broom {
        //     name: String,
        //     height: u32,
        //     health: u32,
        //     position: (f32, f32, f32),
        //     intent: BroomIntent,
        // }
        // impl Broom {
        //     fn boomstick_range(&self) -> Range<i32> {
        //         self.y - self.height - 1 .. self.y
        //     }
        // }
        // trait Visible {
        //     fn draw(&self, canvas: &mut Canvas);
        //     fn hit_test(&self, x: i32, y: i32) -> bool;
        // }
        // impl Visible for Broom {
        //     fn draw(&self, canvas: &mut Canvas) {
        //         //for y in self.y - self.height - 1 .. self.y {
        //         for y in self.broomstick_range() {
        //             canvas.write_at(self.x, y, '|');
        //         }
        //         canvas.write_at(self.x, y, 'M');
        //     }
        // }

        // fn hit_test(&self, x: i32, y:i32) -> bool {
        //     self.x == x
        //         && self.y - self.height - 1 <= y
        //         && y <- self.y
        // }

    }
    {
        // 11.2.1

        /// A writer that ignores whatever data you write to it.
        pub struct Sink;

        use std::io::{Result, Write};

        impl Write for Sink {
            fn write(&mut self, buf: &[u8]) -> Result<usize> {
                Ok(buf.len())
            }
            fn flush(&mut self) -> Result<()> {
                Ok(())
            }
        }
    }
    {
        // 11.2.2

        trait IsEmoji {
            fn is_emoji(&self) -> bool;
        }

        impl IsEmoji for char {
            fn is_emoji(&self) -> bool {
                return false;
            }
        }
        assert_eq!('$'.is_emoji(), false);

        use std::io::{self, Write};

        struct HtmlDocument;

        trait WriteHtml {
            fn write_html(&mut self, html: &HtmlDocument) -> std::io::Result<()>;
        }

        impl<W: Write> WriteHtml for W {
            fn write_html(&mut self, html: &HtmlDocument) -> io::Result<()> {
                Ok(())
            }
        }

        extern crate serde;
        use serde::Serialize;
        use serde_json;
        use std::collections::HashMap;
        use std::fs::File;

        pub fn save_configuration(config: &HashMap<String, String>) -> std::io::Result<()> {
            let writer = File::create("test.json").expect("error");
            let mut serializer = serde_json::Serializer::new(writer);

            config.serialize(&mut serializer).expect("error");
            Ok(())
        }

        {
            // 11.2.3
        }
    }
}
