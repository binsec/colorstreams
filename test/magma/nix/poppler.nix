{stdenv, fetchgit, fetchFromGitHub, symlinkJoin, lib, autoconf, autogen, automake, libtool, pkg-config, cmake, extra-cmake-modules, pinstrio_stubs, fuzzer_main, zlib, libjpeg, openjpeg, libpng, cairo, libtiff, lcms, boost, glibc}:
    let 
        magma_poppler = fetchgit
            {
                url = "https://github.com/HexHive/magma";
                rev = "75d1ae7b180443a778b8830c79176ca5f93642ac";
                sparseCheckout = ["targets/poppler"];
                sha256 = "sha256-YRvzrqgp7TOVQikzcXUGvhDXlq5w+ga2bzvLzCE2m0A=";
            };
        mk_fuzzers =
            asan:
                let 
                    asan_suff = if asan then "_asan" else "";
                    asan_opt = if asan then " -fsanitize=address" else "";
                    freetype2 =
                        stdenv.mkDerivation
                            {
                                name = "freetype2_magma";
                                src = fetchgit
                                    {
                                        url = "https://gitlab.freedesktop.org/freetype/freetype";
                                        rev = "50d0033f7ee600c5f5831b28877353769d1f7d48";
                                        sha256 = "sha256-rW/Jwp3ku49fL2no+UZCcqibIHmgWjLcQcXaArLmi5Y=";
                                    };
                                version = "50d0033";
                                nativeBuildInputs =
                                    [
                                        libtool
                                        pkg-config
                                        autoconf
                                        automake
                                        autogen
                                    ];
                                buildInputs =
                                    [
                                    ];
                                configurePhase =
                                    ''
                                        export CFLAGS="$CFLAGS -ggdb${asan_opt}"
                                        export CXXFLAGS="$CXXFLAGS -ggdb${asan_opt}"
                                        export LDFLAGS="$LDFLAGS${asan_opt}"
                                        ./autogen.sh
                                        ./configure --disable-shared --prefix="$out" PKG_CONFIG_PATH="$out/lib/pkgconfig"
                                    '';
                                buildPhase =
                                    ''
                                        make -j$(nproc) clean
                                        make -j$(nproc)
                                    '';
                                installPhase =
                                    ''
                                        make install
                                    '';
                            };
                in
                stdenv.mkDerivation
                    {
                        name = "poppler_magma";
                        src = fetchgit
                            {
                                url = "https://gitlab.freedesktop.org/poppler/poppler";
                                rev = "1d23101ccebe14261c6afc024ea14f29d209e760";
                                sha256 = "sha256-AK5jgLFYeNgwS+Y77eAYhk2HlujiOGYvwAOSKUIVOE0=";
                            };
                        version = "1d23101";
                        nativeBuildInputs =
                            [
                                cmake
                                extra-cmake-modules
                            ];
                        buildInputs =
                            [
                                zlib
                                libjpeg
                                openjpeg
                                libpng
                                cairo
                                libtiff
                                lcms
                                boost
                             #   glibc
                            ];
                        patches = [(builtins.toPath "${magma_poppler}/targets/poppler/patches/bugs/*.patch")];
                        configurePhase =
                            ''
                                export CFLAGS="$CFLAGS -ggdb${asan_opt}"
                                export CXXFLAGS="$CXXFLAGS -ggdb${asan_opt}"
                                export LDFLAGS="$LDFLAGS${asan_opt}"
                                cmake . -DCMAKE_BUILD_TYPE=RelWithDebInfo \
                                  -DCMAKE_BUILD_TYPE=debug \
                                  -DBUILD_SHARED_LIBS=OFF \
                                  -DFONT_CONFIGURATION=generic \
                                  -DBUILD_GTK_TESTS=OFF \
                                  -DBUILD_QT5_TESTS=OFF \
                                  -DBUILD_CPP_TESTS=OFF \
                                  -DENABLE_LIBPNG=ON \
                                  -DENABLE_LIBTIFF=ON \
                                  -DENABLE_LIBJPEG=ON \
                                  -DENABLE_SPLASH=ON \
                                  -DENABLE_UTILS=ON \
                                  -DWITH_Cairo=ON \
                                  -DENABLE_CMS=none \
                                  -DENABLE_LIBCURL=OFF \
                                  -DENABLE_GLIB=OFF \
                                  -DENABLE_GOBJECT_INTROSPECTION=OFF \
                                  -DENABLE_QT5=OFF \
                                  -DENABLE_LIBCURL=OFF \
                                  -DWITH_NSS3=OFF \
                                  -DFREETYPE_INCLUDE_DIRS="${freetype2}/include/freetype2" \
                                  -DFREETYPE_LIBRARY="${freetype2}/lib/libfreetype.a" \
                                  -DICONV_LIBRARIES="${stdenv.cc.cc.lib}"
                            '';
                        buildPhase =
                            ''
                                make -j$(nproc) poppler poppler-cpp pdfimages pdftoppm
                                $CXX $CXXFLAGS -ggdb -std=c++11 -c -Icpp \
                                    ${magma_poppler}/targets/poppler/src/pdf_fuzzer.cc -o pdf_fuzzer.o
                                $CC $CFLAGS -ggdb -c -I${pinstrio_stubs}/include -DPINSTRIO ${fuzzer_main}/fuzzer_main.c -o fuzzer_main.o
                                $CXX $CXXFLAGS -ggdb -std=c++11 pdf_fuzzer.o fuzzer_main.o -o poppler_pdf_fuzzer${asan_suff}.run \
                                    cpp/libpoppler-cpp.a libpoppler.a \
                                    ${freetype2}/lib/libfreetype.a $LDFLAGS $LIBS -ljpeg -lz \
                                    -lopenjp2 -lpng -ltiff -llcms2 -lm -lpthread -pthread -L${pinstrio_stubs}/lib -lpinstrio
                            '';
                        installPhase =
                            ''
                                mv utils/pdfimages poppler_pdfimages${asan_suff}.run
                                mv utils/pdftoppm poppler_pdftoppm${asan_suff}.run
                                install -Dt $out/bin *.run
                            '';
                        dontStrip = true;
                    };
    in 
    symlinkJoin
        {
            name = "magma_poppler_fuzzers";
            paths = (map (d: d.out) [(mk_fuzzers true) (mk_fuzzers false)]);
        }
