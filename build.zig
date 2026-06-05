const std = @import("std");

pub fn build(b: *std.Build) void {
    const ocaml = b.addSystemCommand(&.{
        "ocamlfind",
        "ocamlopt",
        "-package",
        "raylib",
        "-linkpkg",
        "-o",
    });
    const output_path = ocaml.addOutputFileArg("breakaml");
    ocaml.addFileArg(b.path("src/main.ml"));

    const install = b.addInstallBinFile(output_path, "breakaml");
    b.getInstallStep().dependOn(&install.step);

    const run_cmd = b.addSystemCommand(&.{
        b.getInstallPath(.bin, "breakaml"),
    });
    run_cmd.step.dependOn(&install.step);

    const run_step = b.step("run", "Run Program");
    run_step.dependOn(&run_cmd.step);
}
