pub const c = @cImport({
    @cInclude("binaryen-c.h");
});

pub fn compile() void {
    // var module = c.BinaryenModuleCreate();
    // var ret = c.BinaryenModulePrint(module);
    // print("{any}", .{ret});
    var module = c.BinaryenModuleCreate();

    // Create a function type for  i32 (i32, i32)
    var ii = [2]c.BinaryenType{ c.BinaryenTypeInt32(), c.BinaryenTypeInt32() };
    var params = c.BinaryenTypeCreate(@ptrCast(&ii), 2);
    var results = c.BinaryenTypeInt32();

    // Get the 0 and 1 arguments, and add them
    var x = c.BinaryenLocalGet(module, 0, c.BinaryenTypeInt32());
    var y = c.BinaryenLocalGet(module, 1, c.BinaryenTypeInt32());
    var add = c.BinaryenBinary(module, c.BinaryenAddInt32(), x, y);

    // Create the add function
    // Note: no additional local variables
    // Note: no basic blocks here, we are an AST. The function body is just an
    // expression node.
    var adder =
        c.BinaryenAddFunction(module, "adder", params, results, null, 0, add);
    _ = adder;

    // Print it out
    c.BinaryenModulePrint(module);

    // Clean up the module, which owns all the objects we created above
    c.BinaryenModuleDispose(module);
}
