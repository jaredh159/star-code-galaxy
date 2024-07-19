mod decode;

fn main() {
  let args: Vec<String> = std::env::args().collect();
  if args.len() != 2 {
    eprintln!("Usage: sim8086 <filename>");
    std::process::exit(1);
  }

  let data = std::fs::read(&args[1]).expect("Error reading file");
  // for byte in &data {
  //   println!("{:08b}", byte);
  // }

  let instructions = decode::disassemble(&data);
  println!("; src: `{}`\n{instructions}", &args[1]);
}
