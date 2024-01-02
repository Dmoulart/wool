const dir = import.meta.dir;

// Fetch the WebAssembly file
const src = Bun.file(`${dir}/out.wasm`);
const bytes = await src.arrayBuffer();

// Create a WebAssembly instance
const {instance, module} = await WebAssembly.instantiate(bytes);

// Call the exported function
const result = instance.exports.add(10, 20);

// Display the result
console.log("Result:", result);
