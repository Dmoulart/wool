type TypeBits = number;
const TERMINAL_TYPE: TypeBits = 1 << 11;

const ANY_TYPE: TypeBits = 1 << 0;

const NUMBER_TYPE: TypeBits = (1 << 1) | ANY_TYPE;

const INT_TYPE: TypeBits = (1 << 2) | NUMBER_TYPE;
const I32_TYPE: TypeBits = (1 << 3) | INT_TYPE | TERMINAL_TYPE;
const I64_TYPE: TypeBits = (1 << 4) | INT_TYPE | TERMINAL_TYPE;

const FLOAT_TYPE: TypeBits = (1 << 5) | NUMBER_TYPE;
const F32_TYPE: TypeBits = (1 << 6) | FLOAT_TYPE | TERMINAL_TYPE;
const F64_TYPE: TypeBits = (1 << 7) | FLOAT_TYPE | TERMINAL_TYPE;

const BOOL_TYPE: TypeBits = (1 << 8) | ANY_TYPE | TERMINAL_TYPE;
const STRING_TYPE: TypeBits = (1 << 9) | ANY_TYPE | TERMINAL_TYPE;

const VOID_TYPE: TypeBits = (1 << 10) | TERMINAL_TYPE;

// const intersects = (a: TypeBit, b: TypeBit) => (a & b) !== 0;

const subtype = (supertype: TypeBits, subtype: TypeBits) =>
  (supertype & subtype) === supertype;

const terminal = (type: TypeBits) => (type & TERMINAL_TYPE) === TERMINAL_TYPE;

console.log(terminal(INT_TYPE));
