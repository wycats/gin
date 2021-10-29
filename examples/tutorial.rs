use gin_macros::message;

// syntax = "proto3";
// package tutorial;

// message Person {
//   string name = 1;
//   int32 id = 2;  // Unique ID number for this person.
//   string email = 3;

//   enum PhoneType {
//     MOBILE = 0;
//     HOME = 1;
//     WORK = 2;
//   }

//   message PhoneNumber {
//     string number = 1;
//     PhoneType type = 2;
//   }

//   repeated PhoneNumber phones = 4;
// }

// // Our address book file is just one of these.
// message AddressBook {
//   repeated Person people = 1;
// }

#[message]
struct Person {
    // TODO: Hint with positive bias to match tutorial
    id: i32,
    name: String,
    email: String,
    phones: Vec<PhoneNumber>,
}

#[message]
enum PhoneType {
    MOBILE,
    HOME,
    WORK,
}

#[message]
struct PhoneNumber {
    number: String,
    r#type: PhoneType,
}

#[message]
struct AddressBook {
    people: Vec<Person>,
}

fn main() {
    let person = Person {
        id: 1,
        name: format!("Yehuda Katz"),
        email: format!("wycats@gmail.com"),
        phones: vec![PhoneNumber {
            number: format!("718.877.1325"),
            r#type: PhoneType::MOBILE,
        }],
    };

    let person = Person::parse(person.serialize()).unwrap();

    println!("{:#?}", person);
}
