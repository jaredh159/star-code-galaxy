pub fn disassemble(instructions: &[u8]) -> String {
  let mut lines = vec!["bits 16".to_string()];
  let mut index = 0;
  while index < instructions.len() - 1 {
    let (line, bytes_processed) = decode(&instructions[index..]);
    index += bytes_processed;
    lines.push(line);
  }
  lines.push("".to_string());
  lines.join("\n")
}

fn decode(bytes: &[u8]) -> (String, usize) {
  assert!(bytes.len() >= 2);
  let b1 = bytes[0];
  let b2 = bytes[1];
  let opcode = b1 >> 2;
  let _d_bit = (b1 >> 1) & 0b_0000_0001;
  let w_bit = b1 & 0b_0000_0001;
  let _d_bit_set = _d_bit == 1;
  let w_bit_set = w_bit == 1;
  let r#_mod = b2 >> 6;
  let reg = (b2 >> 3) & 0b0000_0111;
  let rm = b2 & 0b0000_0111;
  // println!("opcode: {:06b}", opcode);
  // println!("mod {:02b}", r#mod);
  // println!("reg {:03b}", reg);
  // println!("rm {:03b}", rm);
  match opcode {
    0b_100010 => {
      let dst = register(rm, w_bit_set);
      let src = register(reg, w_bit_set);
      (format!("mov {dst}, {src}",), 2)
    }
    _ => todo!("unhandled opcode {:06b}", opcode),
  }
}

fn register(reg: u8, w_bit_set: bool) -> &'static str {
  match (reg, w_bit_set) {
    (0b_000, false) => "al",
    (0b_001, false) => "cl",
    (0b_010, false) => "dl",
    (0b_011, false) => "bl",
    (0b_100, false) => "ah",
    (0b_101, false) => "ch",
    (0b_110, false) => "dh",
    (0b_111, false) => "bh",
    (0b_000, true) => "ax",
    (0b_001, true) => "cx",
    (0b_010, true) => "dx",
    (0b_011, true) => "bx",
    (0b_100, true) => "sp",
    (0b_101, true) => "bp",
    (0b_110, true) => "si",
    (0b_111, true) => "di",
    _ => unreachable!(),
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use indoc::indoc as asm;
  use pretty_assertions::assert_eq;

  #[test]
  fn test_listing_37() {
    assert_eq!(
      disassemble(&[0b100010_0_1, 0b11_011_001]),
      asm! {"
        bits 16
        mov cx, bx
      "}
    );
  }

  #[test]
  fn test_listing_38() {
    assert_eq!(
      disassemble(&[
        0b_10001001,
        0b_11011001,
        0b_10001000,
        0b_11100101,
        0b_10001001,
        0b_11011010,
        0b_10001001,
        0b_11011110,
        0b_10001001,
        0b_11111011,
        0b_10001000,
        0b_11001000,
        0b_10001000,
        0b_11101101,
        0b_10001001,
        0b_11000011,
        0b_10001001,
        0b_11110011,
        0b_10001001,
        0b_11111100,
        0b_10001001,
        0b_11000101,
      ]),
      asm! {"
        bits 16
        mov cx, bx
        mov ch, ah
        mov dx, bx
        mov si, bx
        mov bx, di
        mov al, cl
        mov ch, ch
        mov bx, ax
        mov bx, si
        mov sp, di
        mov bp, ax
      "}
    );
  }
}
