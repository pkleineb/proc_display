# proc_display

A crate for automatically deriving the standard libraries `std::fmt::Display` trait on multiple different data types.

## Installation
Add this to your `Cargo.toml`

```toml
[dependencies]
proc_display = "0.1.0"
```

or add using `cargo add proc_display`.

## Usage
```rust
use proc_display::Display;

#[derive(Display)]
// data type
```

## Examples
### On structs:
Use `#[display("message")]` on the structs declaration.
The message may contain any named or unnamed field as a format argument. All format arguments may have format specifiers based on the rust fmt specifier [specification](https://doc.rust-lang.org/std/fmt/index.html).

#### Struct with named fields
```rust
use proc_display::Display;

#[derive(Display)]
#[display("I am a Named struct: {field} {number}.")]
struct Named {
    field: String,
    number: i32,
}
```

#### Struct with unnamed fields
```rust
use proc_display::Display;

#[derive(Display)]
#[display("I am an unnamed struct: {0} {1}.")]
struct Point(i32, i32);
```


### On enums:
Use `#[display("message")]` on the enum declaration as a default fallback for the message.
Use `#[display("message")]` on the enum variant declarations to set the message for that enum variant. You may use any format argument and format specifier the same as on any other struct.

#### Enum with different variations
```rust
use proc_display::Display;

#[derive(Display)]
#[display("Default String here!")]
enum Status {
    #[display("Loading...")]
    Loading,

    #[display("Status successfull with message: {message}")]
    Success {
        message: String,
    },

    #[display("Error: {0}")]
    Error(String),

    DefaultFallback,
}
```

### Unions (who uses this?)
```rust
#[derive(Display)]
#[display("Fields are not supported since they are unsafe")]
union Union {
    field: i32,
}
```
