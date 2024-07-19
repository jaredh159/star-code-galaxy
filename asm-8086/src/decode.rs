pub fn disassemble(bytes: &[u8]) -> String {
  let mut lines = vec!["bits 16".to_string()];
  let mut index = 0;
  while index < bytes.len() - 1 {
    let (line, bytes_processed) = disassemble_one(&bytes[index..]);
    index += bytes_processed;
    lines.push(line);
  }
  lines.push("".to_string());
  lines.join("\n")
}

fn disassemble_one(bytes: &[u8]) -> (String, usize) {
  assert!(!bytes.is_empty());
  let byte = bytes[0];
  if byte >> 2 == 0b_100010 {
    disassemble_mov_reg_mem_to_reg(bytes)
  } else if byte >> 4 == 0b_1011 {
    disassemble_mov_immediate_to_reg(bytes)
  } else {
    todo!("unhandled opcode {:08b}", byte)
  }
}

fn disassemble_mov_immediate_to_reg(bytes: &[u8]) -> (String, usize) {
  assert!(!bytes.is_empty());
  let wide = bytes[0] >> 3 & 0b_0000_0001 == 1;
  let reg = bytes[0] & 0b_0000_0111;
  let dst = register(reg, wide);
  if wide {
    (format!("mov {dst}, {}", to_i16(&bytes[1..])), 3)
  } else {
    (format!("mov {dst}, {}", bytes[1] as i8), 2)
  }
}

fn disassemble_mov_reg_mem_to_reg(bytes: &[u8]) -> (String, usize) {
  assert!(!bytes.is_empty());
  let b1 = bytes[0];
  let b2 = bytes[1];
  let d_bit_set = (b1 >> 1) & 0b_0000_0001 == 1;
  let w_bit_set = b1 & 0b_0000_0001 == 1;
  let r#mod = b2 >> 6;
  let rm = b2 & 0b_0000_0111;
  let reg = (b2 >> 3) & 0b0000_0111;
  // println!("\nbyte: {:08b}", b1);
  // print!("mod: {:02b}, ", r#mod);
  // print!("r/m: {:03b}, ", rm);
  // print!("dbit: {d_bit_set}, ");
  // println!("reg: {:03b}", reg);
  match r#mod {
    // memory mode (no displacement)
    0b_00 if rm != 0b_110 => {
      let dst = register(reg, w_bit_set);
      let src = match rm {
        0b_000 => "[bx + si]",
        0b_001 => "[bx + di]",
        0b_010 => "[bp + si]",
        0b_011 => "[bp + di]",
        0b_100 => "[si]",
        0b_101 => "[di]",
        0b_110 => "[???]", // todo...
        0b_111 => "[bx]",
        _ => unreachable!(),
      };
      (format!("mov {}", operands(dst, src, d_bit_set)), 2)
    }
    // memory mode (8-bit displacement)
    0b_01 => {
      let dst = register(reg, w_bit_set);
      let add_disp = add_disp_i8(bytes[2]);
      let src = match rm {
        0b_000 => format!("[bx + si{}]", add_disp),
        0b_001 => format!("[bx + di{}]", add_disp),
        0b_010 => format!("[bp + si{}]", add_disp),
        0b_011 => format!("[bp + di{}]", add_disp),
        0b_100 => format!("[si{}]", add_disp),
        0b_101 => format!("[di{}]", add_disp),
        0b_110 => format!("[bp{}]", add_disp),
        0b_111 => format!("[bx{}]", add_disp),
        _ => unreachable!(),
      };
      (format!("mov {}", operands(dst, &src, d_bit_set)), 3)
    }
    // memory mode (16-bit displacement)
    0b_10 => {
      let dst = register(reg, w_bit_set);
      let add_disp = add_disp_i16(&bytes[2..]);
      let src = match rm {
        0b_000 => format!("[bx + si{}]", add_disp),
        0b_001 => format!("[bx + di{}]", add_disp),
        0b_010 => format!("[bp + si{}]", add_disp),
        0b_011 => format!("[bp + di{}]", add_disp),
        0b_100 => format!("[si{}]", add_disp),
        0b_101 => format!("[di{}]", add_disp),
        0b_110 => format!("[bp{}]", add_disp),
        0b_111 => format!("[bx{}]", add_disp),
        _ => unreachable!(),
      };
      (format!("mov {}", operands(dst, &src, d_bit_set)), 4)
    }
    // register mode (no displacement)
    0b_11 => {
      let dst = register(rm, w_bit_set);
      let src = register(reg, w_bit_set);
      (format!("mov {dst}, {src}"), 2)
    }
    _ => todo!("unhandled mov mode"),
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

fn operands<S: AsRef<str>>(src: S, dest: S, d_bit_set: bool) -> String {
  if d_bit_set {
    format!("{}, {}", src.as_ref(), dest.as_ref())
  } else {
    format!("{}, {}", dest.as_ref(), src.as_ref())
  }
}

fn to_i16(bytes: &[u8]) -> i16 {
  let high = (bytes[1] as i16) << 8;
  let low = bytes[0] as i16;
  high | low
}

fn add_disp_i8(byte: u8) -> String {
  if byte == 0 {
    "".to_string()
  } else {
    format!(" + {}", byte as i8)
  }
}

fn add_disp_i16(bytes: &[u8]) -> String {
  let wide = to_i16(bytes);
  if wide == 0 {
    "".to_string()
  } else {
    format!(" + {}", wide)
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

  #[test]
  fn test_listing_39() {
    assert_eq!(
      disassemble(&[
        // Register-to-register
        0b_10001001,
        0b_11011110,
        0b_10001000,
        0b_11000110,
        // 8-bit immediate-to-register
        0b_10110001,
        0b_00001100,
        0b_10110101,
        0b_11110100,
        // 16-bit immediate-to-register
        0b_10111001,
        0b_00001100,
        0b_00000000,
        0b_10111001,
        0b_11110100,
        0b_11111111,
        0b_10111010,
        0b_01101100,
        0b_00001111,
        0b_10111010,
        0b_10010100,
        0b_11110000,
        // Source address calculation
        0b_10001010,
        0b_00000000,
        0b_10001011,
        0b_00011011,
        0b_10001011,
        0b_01_010_110,
        0b_00000000,
        // source address calc + 8 bit
        0b_10001010,
        0b_01100000,
        0b_00000100,
        // sorce address calc + 16
        0b_10001010,
        0b_10000000,
        0b_10000111,
        0b_00010011,
        // dest address calc
        0b_10001001,
        0b_00001001,
        0b_10001000,
        0b_00001010,
        0b_10001000,
        0b_01101110,
        0b_00000000,
      ]),
      asm! {"
        bits 16
        mov si, bx
        mov dh, al
        mov cl, 12
        mov ch, -12
        mov cx, 12
        mov cx, -12
        mov dx, 3948
        mov dx, -3948
        mov al, [bx + si]
        mov bx, [bp + di]
        mov dx, [bp]
        mov ah, [bx + si + 4]
        mov al, [bx + si + 4999]
        mov [bx + di], cx
        mov [bp + si], cl
        mov [bp], ch
      "}
    );
  }
}
