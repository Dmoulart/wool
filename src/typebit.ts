type TypeBit = number;
const ANY_TYPE: TypeBit = 1 << 0;

const NUMBER_TYPE: TypeBit = (1 << 1) | ANY_TYPE;

const INT_TYPE: TypeBit = (1 << 2) | NUMBER_TYPE;
const I32_TYPE: TypeBit = (1 << 3) | INT_TYPE;
const I64_TYPE: TypeBit = (1 << 4) | INT_TYPE;

const FLOAT_TYPE: TypeBit = (1 << 5) | NUMBER_TYPE;
const F32_TYPE: TypeBit = (1 << 6) | FLOAT_TYPE;
const F64_TYPE: TypeBit = (1 << 7) | FLOAT_TYPE;

const BOOL_TYPE: TypeBit = (1 << 8) | ANY_TYPE;
const STRING_TYPE: TypeBit = (1 << 9) | ANY_TYPE;
const VOID_TYPE: TypeBit = 1 << 10;

// const intersects = (a: TypeBit, b: TypeBit) => (a & b) !== 0;

const subtype = (supertype: TypeBit, subtype: TypeBit) =>
  (supertype & subtype) === supertype;

console.log(subtype(STRING_TYPE, F32_TYPE));
