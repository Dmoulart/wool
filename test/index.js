const dir = import.meta.dir;

// Fetch the WebAssembly file
const src = Bun.file(`${dir}/test.wasm`);
const bytes = await src.arrayBuffer();

const importObject = {
  console: {
    log(arg) {
      console.log(arg);
    },
  },
};

// Create a WebAssembly instance
const {instance, module} = await WebAssembly.instantiate(bytes, importObject);

console.log("Running exported WASM function from Bun");

