{stdenv, fetchgit, fetchFromGitHub, symlinkJoin, lib, autoconf, automake, autogen, libtool, pkg-config, bison, re2c, icu60, pinstrio_stubs, fuzzer_main}:
    let
        magma_php = fetchgit
            {
                url = "https://github.com/HexHive/magma";
                rev = "75d1ae7b180443a778b8830c79176ca5f93642ac";
                sparseCheckout = ["targets/php"];
                sha256 = "sha256-FvwS+/dEcZMIcLxiP8+bSwO5hjVnG8yUZEOVkTb74TA=";
            };
        oniguruma = fetchFromGitHub
            {
                owner = "kkos";
                repo = "oniguruma";
                rev = "227ec0bd690207812793c09ad70024707c405376";
                sha256 = "sha256-iYbfxFI3QlKwKEmDOz5CGcgm1MV3+0QeZnZSl4cPnO0=";
            };
        mk_fuzzers =
            asan: fixes:
                let
                    asan_suff = if asan then "_asan" else "";
                    asan_opt = if asan then "-fsanitize=address" else "";
                    enable_fixes = if fixes then "-DMAGMA_ENABLE_FIXES" else "";
                    fixes_suff = if fixes then "_fixed" else "";
                in
                stdenv.mkDerivation
                    {
                        name = "php_magma";
                        src = fetchFromGitHub
                            {
                                owner = "php";
                                repo = "php-src";
                                rev = "bc39abe8c3c492e29bc5d60ca58442040bbf063b";
                                sha256 = "sha256-aTP77DQ9nkTIbmaNXyVaLi7iAAmSfTdGIS89yKpXzhk=";
                            };
                        version = "bc39abe";
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
                                pinstrio_stubs
                                bison
                                re2c
                                icu60
                            ];
                        patches = [(builtins.toPath "${magma_php}/targets/php/patches/setup/setup.patch") (builtins.toPath "${magma_php}/targets/php/patches/bugs/*.patch")];
                        configurePhase =
                            ''
                                export ONIG_CFLAGS="-I$PWD/oniguruma/src"
                                export ONIG_LIBS="-L$PWD/oniguruma/src/.libs -l:libonig.a"
                                export EXTRA_CFLAGS="$CFLAGS -fno-sanitize=object-size -ggdb ${asan_opt} ${enable_fixes}"
                                export EXTRA_CXXFLAGS="$CXXFLAGS -fno-sanitize=object-size -ggdb ${asan_opt} ${enable_fixes}"
                                unset CFLAGS
                                unset CXXFLAGS
                                    
                                ./buildconf
                                    LIB_FUZZING_ENGINE="-Wall" ./configure \
                                        --disable-all \
                                        --enable-option-checking=fatal \
                                        --enable-exif \
                                        --enable-phar \
                                        --enable-intl \
                                        --enable-mbstring \
                                        --without-pcre-jit \
                                        --disable-phpdbg \
                                        --disable-cgi \
                                        --with-pic

                                make -j$(nproc) clean
                                cp -a ${oniguruma} ./oniguruma
                                chmod -R +w ./oniguruma
                            '';
                        buildPhase =
                            ''
                                cd oniguruma
                                autoreconf -vfi
                                ./configure -disable-shared
                                make -j$(nproc)
                                cd ..
                                
                                make -j$(nproc)
                                export INCLUDES="-I. -IZend -Imain -Isapi/cli -Iinclude -ITSRM -Ioniguruma/src"
                                export OBJS="ext/date/php_date.o ext/date/lib/astro.o ext/date/lib/dow.o ext/date/lib/parse_date.o ext/date/lib/parse_tz.o ext/date/lib/parse_posix.o ext/date/lib/timelib.o ext/date/lib/tm2unixtime.o ext/date/lib/unixtime2tm.o ext/date/lib/parse_iso_intervals.o ext/date/lib/interval.o ext/pcre/pcre2lib/pcre2_auto_possess.o ext/pcre/pcre2lib/pcre2_chartables.o ext/pcre/pcre2lib/pcre2_compile.o ext/pcre/pcre2lib/pcre2_config.o ext/pcre/pcre2lib/pcre2_context.o ext/pcre/pcre2lib/pcre2_dfa_match.o ext/pcre/pcre2lib/pcre2_error.o ext/pcre/pcre2lib/pcre2_jit_compile.o ext/pcre/pcre2lib/pcre2_maketables.o ext/pcre/pcre2lib/pcre2_match.o ext/pcre/pcre2lib/pcre2_match_data.o ext/pcre/pcre2lib/pcre2_newline.o ext/pcre/pcre2lib/pcre2_ord2utf.o ext/pcre/pcre2lib/pcre2_pattern_info.o ext/pcre/pcre2lib/pcre2_serialize.o ext/pcre/pcre2lib/pcre2_string_utils.o ext/pcre/pcre2lib/pcre2_study.o ext/pcre/pcre2lib/pcre2_substitute.o ext/pcre/pcre2lib/pcre2_substring.o ext/pcre/pcre2lib/pcre2_tables.o ext/pcre/pcre2lib/pcre2_ucd.o ext/pcre/pcre2lib/pcre2_valid_utf.o ext/pcre/pcre2lib/pcre2_xclass.o ext/pcre/pcre2lib/pcre2_find_bracket.o ext/pcre/pcre2lib/pcre2_convert.o ext/pcre/pcre2lib/pcre2_extuni.o ext/pcre/pcre2lib/pcre2_script_run.o ext/pcre/php_pcre.o ext/exif/exif.o ext/hash/hash.o ext/hash/hash_md.o ext/hash/hash_sha.o ext/hash/hash_ripemd.o ext/hash/hash_haval.o ext/hash/hash_tiger.o ext/hash/hash_gost.o ext/hash/hash_snefru.o ext/hash/hash_whirlpool.o ext/hash/hash_adler32.o ext/hash/hash_crc32.o ext/hash/hash_fnv.o ext/hash/hash_joaat.o ext/hash/sha3/generic64lc/KeccakP-1600-opt64.o ext/hash/sha3/generic64lc/KeccakHash.o ext/hash/sha3/generic64lc/KeccakSponge.o ext/hash/hash_sha3.o ext/hash/murmur/PMurHash.o ext/hash/murmur/PMurHash128.o ext/hash/hash_murmur.o ext/hash/hash_xxhash.o ext/intl/php_intl.o ext/intl/intl_error.o ext/intl/intl_convert.o ext/intl/collator/collator.o ext/intl/collator/collator_class.o ext/intl/collator/collator_sort.o ext/intl/collator/collator_convert.o ext/intl/collator/collator_locale.o ext/intl/collator/collator_compare.o ext/intl/collator/collator_attr.o ext/intl/collator/collator_create.o ext/intl/collator/collator_is_numeric.o ext/intl/collator/collator_error.o ext/intl/common/common_error.o ext/intl/converter/converter.o ext/intl/formatter/formatter.o ext/intl/formatter/formatter_main.o ext/intl/formatter/formatter_class.o ext/intl/formatter/formatter_attr.o ext/intl/formatter/formatter_data.o ext/intl/formatter/formatter_format.o ext/intl/formatter/formatter_parse.o ext/intl/normalizer/normalizer.o ext/intl/normalizer/normalizer_class.o ext/intl/normalizer/normalizer_normalize.o ext/intl/locale/locale.o ext/intl/locale/locale_class.o ext/intl/locale/locale_methods.o ext/intl/dateformat/dateformat.o ext/intl/dateformat/dateformat_class.o ext/intl/dateformat/dateformat_attr.o ext/intl/dateformat/dateformat_data.o ext/intl/dateformat/dateformat_format.o ext/intl/dateformat/dateformat_parse.o ext/intl/msgformat/msgformat.o ext/intl/msgformat/msgformat_attr.o ext/intl/msgformat/msgformat_class.o ext/intl/msgformat/msgformat_data.o ext/intl/msgformat/msgformat_format.o ext/intl/msgformat/msgformat_parse.o ext/intl/grapheme/grapheme_string.o ext/intl/grapheme/grapheme_util.o ext/intl/resourcebundle/resourcebundle.o ext/intl/resourcebundle/resourcebundle_class.o ext/intl/resourcebundle/resourcebundle_iterator.o ext/intl/transliterator/transliterator.o ext/intl/transliterator/transliterator_class.o ext/intl/transliterator/transliterator_methods.o ext/intl/uchar/uchar.o ext/intl/idn/idn.o ext/intl/spoofchecker/spoofchecker_class.o ext/intl/spoofchecker/spoofchecker.o ext/intl/spoofchecker/spoofchecker_create.o ext/intl/spoofchecker/spoofchecker_main.o ext/intl/intl_convertcpp.o ext/intl/common/common_enum.o ext/intl/common/common_date.o ext/intl/dateformat/dateformat_format_object.o ext/intl/dateformat/dateformat_create.o ext/intl/dateformat/dateformat_attrcpp.o ext/intl/dateformat/dateformat_helpers.o ext/intl/dateformat/datepatterngenerator_class.o ext/intl/dateformat/datepatterngenerator_methods.o ext/intl/msgformat/msgformat_helpers.o ext/intl/timezone/timezone_class.o ext/intl/timezone/timezone_methods.o ext/intl/calendar/calendar_class.o ext/intl/calendar/calendar_methods.o ext/intl/calendar/gregoriancalendar_methods.o ext/intl/breakiterator/breakiterator_class.o ext/intl/breakiterator/breakiterator_iterators.o ext/intl/breakiterator/breakiterator_methods.o ext/intl/breakiterator/rulebasedbreakiterator_methods.o ext/intl/breakiterator/codepointiterator_internal.o ext/intl/breakiterator/codepointiterator_methods.o ext/json/json.o ext/json/json_encoder.o ext/json/json_parser.o ext/json/json_scanner.o ext/mbstring/mbstring.o ext/mbstring/php_unicode.o ext/mbstring/mb_gpc.o ext/mbstring/php_mbregex.o ext/mbstring/libmbfl/filters/html_entities.o ext/mbstring/libmbfl/filters/mbfilter_7bit.o ext/mbstring/libmbfl/filters/mbfilter_base64.o ext/mbstring/libmbfl/filters/mbfilter_big5.o ext/mbstring/libmbfl/filters/mbfilter_cp5022x.o ext/mbstring/libmbfl/filters/mbfilter_cp51932.o ext/mbstring/libmbfl/filters/mbfilter_cp932.o ext/mbstring/libmbfl/filters/mbfilter_cp936.o ext/mbstring/libmbfl/filters/mbfilter_gb18030.o ext/mbstring/libmbfl/filters/mbfilter_euc_cn.o ext/mbstring/libmbfl/filters/mbfilter_euc_jp.o ext/mbstring/libmbfl/filters/mbfilter_euc_jp_2004.o ext/mbstring/libmbfl/filters/mbfilter_euc_jp_win.o ext/mbstring/libmbfl/filters/mbfilter_euc_kr.o ext/mbstring/libmbfl/filters/mbfilter_euc_tw.o ext/mbstring/libmbfl/filters/mbfilter_htmlent.o ext/mbstring/libmbfl/filters/mbfilter_hz.o ext/mbstring/libmbfl/filters/mbfilter_iso2022_jp_ms.o ext/mbstring/libmbfl/filters/mbfilter_iso2022jp_2004.o ext/mbstring/libmbfl/filters/mbfilter_iso2022jp_mobile.o ext/mbstring/libmbfl/filters/mbfilter_iso2022_kr.o ext/mbstring/libmbfl/filters/mbfilter_jis.o ext/mbstring/libmbfl/filters/mbfilter_qprint.o ext/mbstring/libmbfl/filters/mbfilter_singlebyte.o ext/mbstring/libmbfl/filters/mbfilter_sjis.o ext/mbstring/libmbfl/filters/mbfilter_sjis_mobile.o ext/mbstring/libmbfl/filters/mbfilter_sjis_mac.o ext/mbstring/libmbfl/filters/mbfilter_sjis_2004.o ext/mbstring/libmbfl/filters/mbfilter_tl_jisx0201_jisx0208.o ext/mbstring/libmbfl/filters/mbfilter_ucs2.o ext/mbstring/libmbfl/filters/mbfilter_ucs4.o ext/mbstring/libmbfl/filters/mbfilter_uhc.o ext/mbstring/libmbfl/filters/mbfilter_utf16.o ext/mbstring/libmbfl/filters/mbfilter_utf32.o ext/mbstring/libmbfl/filters/mbfilter_utf7.o ext/mbstring/libmbfl/filters/mbfilter_utf7imap.o ext/mbstring/libmbfl/filters/mbfilter_utf8.o ext/mbstring/libmbfl/filters/mbfilter_utf8_mobile.o ext/mbstring/libmbfl/filters/mbfilter_uuencode.o ext/mbstring/libmbfl/mbfl/mbfilter.o ext/mbstring/libmbfl/mbfl/mbfilter_8bit.o ext/mbstring/libmbfl/mbfl/mbfilter_pass.o ext/mbstring/libmbfl/mbfl/mbfilter_wchar.o ext/mbstring/libmbfl/mbfl/mbfl_convert.o ext/mbstring/libmbfl/mbfl/mbfl_encoding.o ext/mbstring/libmbfl/mbfl/mbfl_filter_output.o ext/mbstring/libmbfl/mbfl/mbfl_language.o ext/mbstring/libmbfl/mbfl/mbfl_memory_device.o ext/mbstring/libmbfl/mbfl/mbfl_string.o ext/mbstring/libmbfl/nls/nls_de.o ext/mbstring/libmbfl/nls/nls_en.o ext/mbstring/libmbfl/nls/nls_ja.o ext/mbstring/libmbfl/nls/nls_kr.o ext/mbstring/libmbfl/nls/nls_neutral.o ext/mbstring/libmbfl/nls/nls_ru.o ext/mbstring/libmbfl/nls/nls_uni.o ext/mbstring/libmbfl/nls/nls_zh.o ext/mbstring/libmbfl/nls/nls_hy.o ext/mbstring/libmbfl/nls/nls_tr.o ext/mbstring/libmbfl/nls/nls_ua.o ext/phar/util.o ext/phar/tar.o ext/phar/zip.o ext/phar/stream.o ext/phar/func_interceptors.o ext/phar/dirstream.o ext/phar/phar.o ext/phar/phar_object.o ext/phar/phar_path_check.o ext/reflection/php_reflection.o ext/spl/php_spl.o ext/spl/spl_functions.o ext/spl/spl_iterators.o ext/spl/spl_array.o ext/spl/spl_directory.o ext/spl/spl_exceptions.o ext/spl/spl_observer.o ext/spl/spl_dllist.o ext/spl/spl_heap.o ext/spl/spl_fixedarray.o ext/standard/crypt_freesec.o ext/standard/crypt_blowfish.o ext/standard/crypt_sha512.o ext/standard/crypt_sha256.o ext/standard/php_crypt_r.o ext/standard/array.o ext/standard/base64.o ext/standard/basic_functions.o ext/standard/browscap.o ext/standard/crc32.o ext/standard/crypt.o ext/standard/datetime.o ext/standard/dir.o ext/standard/dl.o ext/standard/dns.o ext/standard/exec.o ext/standard/file.o ext/standard/filestat.o ext/standard/flock_compat.o ext/standard/formatted_print.o ext/standard/fsock.o ext/standard/head.o ext/standard/html.o ext/standard/image.o ext/standard/info.o ext/standard/iptc.o ext/standard/lcg.o ext/standard/link.o ext/standard/mail.o ext/standard/math.o ext/standard/md5.o ext/standard/metaphone.o ext/standard/microtime.o ext/standard/pack.o ext/standard/pageinfo.o ext/standard/quot_print.o ext/standard/rand.o ext/standard/mt_rand.o ext/standard/soundex.o ext/standard/string.o ext/standard/scanf.o ext/standard/syslog.o ext/standard/type.o ext/standard/uniqid.o ext/standard/url.o ext/standard/var.o ext/standard/versioning.o ext/standard/assert.o ext/standard/strnatcmp.o ext/standard/levenshtein.o ext/standard/incomplete_class.o ext/standard/url_scanner_ex.o ext/standard/ftp_fopen_wrapper.o ext/standard/http_fopen_wrapper.o ext/standard/php_fopen_wrapper.o ext/standard/credits.o ext/standard/css.o ext/standard/var_unserializer.o ext/standard/ftok.o ext/standard/sha1.o ext/standard/user_filters.o ext/standard/uuencode.o ext/standard/filters.o ext/standard/proc_open.o ext/standard/streamsfuncs.o ext/standard/http.o ext/standard/password.o ext/standard/random.o ext/standard/net.o ext/standard/hrtime.o ext/standard/crc32_x86.o Zend/asm/make_x86_64_sysv_elf_gas.o Zend/asm/jump_x86_64_sysv_elf_gas.o TSRM/TSRM.o main/main.o main/snprintf.o main/spprintf.o main/fopen_wrappers.o main/alloca.o main/php_scandir.o main/php_ini.o main/SAPI.o main/rfc1867.o main/php_content_types.o main/strlcpy.o main/strlcat.o main/explicit_bzero.o main/reentrancy.o main/php_variables.o main/php_ticks.o main/network.o main/php_open_temporary_file.o main/output.o main/getopt.o main/php_syslog.o main/streams/streams.o main/streams/cast.o main/streams/memory.o main/streams/filter.o main/streams/plain_wrapper.o main/streams/userspace.o main/streams/transports.o main/streams/xp_socket.o main/streams/mmap.o main/streams/glob_wrapper.o Zend/zend_language_parser.o Zend/zend_language_scanner.o Zend/zend_ini_parser.o Zend/zend_ini_scanner.o Zend/zend_alloc.o Zend/zend_compile.o Zend/zend_constants.o Zend/zend_dtrace.o Zend/zend_execute_API.o Zend/zend_highlight.o Zend/zend_llist.o Zend/zend_vm_opcodes.o Zend/zend_opcode.o Zend/zend_operators.o Zend/zend_ptr_stack.o Zend/zend_stack.o Zend/zend_variables.o Zend/zend.o Zend/zend_API.o Zend/zend_extensions.o Zend/zend_hash.o Zend/zend_list.o Zend/zend_builtin_functions.o Zend/zend_attributes.o Zend/zend_execute.o Zend/zend_ini.o Zend/zend_sort.o Zend/zend_multibyte.o Zend/zend_ts_hash.o Zend/zend_stream.o Zend/zend_iterators.o Zend/zend_interfaces.o Zend/zend_exceptions.o Zend/zend_strtod.o Zend/zend_gc.o Zend/zend_closures.o Zend/zend_weakrefs.o Zend/zend_float.o Zend/zend_string.o Zend/zend_signal.o Zend/zend_generators.o Zend/zend_virtual_cwd.o Zend/zend_ast.o Zend/zend_objects.o Zend/zend_object_handlers.o Zend/zend_objects_API.o Zend/zend_default_classes.o Zend/zend_inheritance.o Zend/zend_smart_str.o Zend/zend_cpuinfo.o Zend/zend_gdb.o Zend/zend_observer.o Zend/zend_system_id.o Zend/zend_enum.o Zend/zend_fibers.o Zend/Optimizer/zend_optimizer.o Zend/Optimizer/pass1.o Zend/Optimizer/pass3.o Zend/Optimizer/optimize_func_calls.o Zend/Optimizer/block_pass.o Zend/Optimizer/optimize_temp_vars_5.o Zend/Optimizer/nop_removal.o Zend/Optimizer/compact_literals.o Zend/Optimizer/zend_cfg.o Zend/Optimizer/zend_dfg.o Zend/Optimizer/dfa_pass.o Zend/Optimizer/zend_ssa.o Zend/Optimizer/zend_inference.o Zend/Optimizer/zend_func_info.o Zend/Optimizer/zend_call_graph.o Zend/Optimizer/sccp.o Zend/Optimizer/scdf.o Zend/Optimizer/dce.o Zend/Optimizer/escape_analysis.o Zend/Optimizer/compact_vars.o Zend/Optimizer/zend_dump.o main/internal_functions_cli.o"
                                $CC $CFLAGS_CLEAN $EXTRA_CFLAGS $INCLUDES -c sapi/fuzzer/fuzzer-sapi.c -o fuzzer-sapi.o
                                $CC $CFLAGS_CLEAN $EXTRA_CFLAGS -I${pinstrio_stubs}/include -DPINSTRIO -DFUZZ_INIT -c ${fuzzer_main}/fuzzer_main.c -o fuzzer_main.o
                                
                                for fuzzer in fuzzer-json fuzzer-exif fuzzer-mbstring fuzzer-unserialize fuzzer-parser; do
                                    $CC $CFLAGS_CLEAN $EXTRA_CFLAGS $INCLUDES -c sapi/fuzzer/$fuzzer.c -o $fuzzer.o;
                                    $CXX $CXXFLAGS_CLEAN $EXTRA_CXXFLAGS $OBJS fuzzer-sapi.o $fuzzer.o fuzzer_main.o -o php_''${fuzzer}${asan_suff}${fixes_suff}.run $LDFLAGS -lstdc++ -lrt -lm -licuio -licuuc -licui18n -licudata $ONIG_LIBS -L${pinstrio_stubs}/lib -lpinstrio;
                                done
                            '';
                        installPhase =
                            ''
                                install -Dt $out/bin *.run
                            '';
                        dontStrip = true;
                    };
    in
    symlinkJoin
        {
            name = "magma_php_fuzzers";
            paths = (map (d: d.out) [(mk_fuzzers false false) (mk_fuzzers true false) (mk_fuzzers true true) (mk_fuzzers false true)]);
        }
