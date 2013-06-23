solution "ponyProgram"
  configurations {
    "Debug",
    "Release"
  }

  includedirs "inc"

  buildoptions {
    "-std=gnu99",
    "-mcx16",
    "-march=native",
    "-pthread",
    "-Wno-unused-label",
    "-Wno-unused-variable",
  }

  linkoptions {
    "-pthread",
  }

  flags {
    "ExtraWarnings",
    "FatalWarnings",
  }

  configuration "macosx"
    buildoptions "-mno-avx"

  configuration "Debug"
    targetdir "bin/debug"
    flags "Symbols"

  configuration "Release"
    targetdir "bin/release"
    defines "NDEBUG"

  buildoptions {
    "-O3",
    "-flto=jobserver",
    "-ffunction-sections",
    "-fdata-sections",
    "-freorder-blocks-and-partition",
  }

  linkoptions {
    "-flto=jobserver",
    "-fuse-ld=gold",
  }

  configuration {
    "Release",
    "not macosx",
  }
  linkoptions {
    "-fuse-linker-plugin",
  }

  project "libPony"
    kind "StaticLib"
    language "C"
    files "src/*.c"

  project "pony"
    kind "ConsoleApp"
    language "C"
    files {
      "pony_class_ids.c",
      "pony_class.c"
    }
    links "libPony"
