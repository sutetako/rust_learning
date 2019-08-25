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
    fn pirate_share(total: u64, crew_size: usize) -> u64 {
        let half = total / 2;
        half / crew_size as u64
    }
    pirate_share(100, 0);

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
