use gin_macros::message;

// syntax = "proto3";

// package snazzy.items;

// // A snazzy new shirt!
// message Shirt {
//   enum Size {
//     SMALL = 0;
//     MEDIUM = 1;
//     LARGE = 2;
//   }

//   string color = 1;
//   Size size = 2;
// }

#[message]
struct Shirt {
    color: String,
    size: Size,
}

#[message]
enum Size {
    SMALL,
    MEDIUM,
    LARGE,
}

fn main() {
    let shirt = Shirt {
        color: "Teal".to_string(),
        size: Size::LARGE,
    };
    let shirt = Shirt::parse(shirt.serialize()).unwrap();

    println!("{:#?}", shirt);
}
