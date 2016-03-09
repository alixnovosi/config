import os

HERE = os.path.dirname(os.path.__abspath(__file__))
SOURCE_EXTENSIONS = ['.cpp', '.cxx', '.cc', '.c', '.m', '.mm']

# These are the compilation flags that will be used in case there's no compilation database set (by
# default, one is not set).
# CHANGE THIS LIST OF FLAGS. YES, THIS IS THE DROID YOU HAVE BEEN LOOKING FOR.
COMMON_FLAGS = ['-Wall', '-Wextra', '-Werror', '-Wpedantic', '-Weverything']

C_STD = "-std=c99"
CPP_STD = "-std=c++14"

# Some of these will only work on OS X, but extra paths that don't exist are not harmful.
PATHS = ['/System/Library/Frameworks/Python.framework/Headers',
         '-isystem',
         '../llvm/include',
         '-isystem',
         '../llvm/tools/clang/include',
         '-I',
         '.',
         '-I',
         './ClangCompleter',
         '-isystem',
         '/usr/include',
         '-isystem',
         '/usr/local/include',
         '-isystem',
         '/Applications/Xcode.app/Contents/Developer/Toolchains/' +
         'XcodeDefault.xctoolchain/usr/bin/../include/c++/v1',
         '-isystem',
         '/Applications/Xcode.app/Contents/Developer/Toolchains/' +
         'XcodeDefault.xctoolchain/usr/include']

C_FLAGS = COMMON_FLAGS.extend([C_STD, '-x', 'c'].extend(PATHS))

# These are the compilation flags that will be used in case there's no
# compilation database set (by default, one is not set).
# CHANGE THIS LIST OF FLAGS. YES, THIS IS THE DROID YOU HAVE BEEN LOOKING FOR.
CPP_FLAGS = COMMON_FLAGS.extend(['-Wno-missing-field-initializers' '-Wstrict-overflow'
                                 '-fno-strict-aliasing',
                                 CPP_STD, '-x', 'c++'].extend(PATHS))


def is_header_file(filename):
    extension = os.path.splitext(filename)[1]
    return extension in ['.h', '.hxx', '.hpp', '.hh']
