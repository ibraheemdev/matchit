use matchit::{InsertError, MatchError, Node};

macro_rules! match_tests {
    ($($name:ident {
        routes = $routes:expr,
        $( $path:literal :: $route:literal =>
            $( $(@$none:tt)? None )?
            $( $(@$some:tt)? { $( $key:literal => $val:literal ),* $(,)? } )?
        ),* $(,)?
    }),* $(,)?) => { $(
        #[test]
        fn $name() {
            let mut tree = Node::new();

            for route in $routes {
                tree.insert(route, route.to_owned()).unwrap();
            }

            $(match tree.at($path) {
                Err(_) => {
                    $($( @$some )?
                        panic!("Expected value for route '{}'", $path)
                    )?
                }
                Ok(result) => {
                    $($( @$some )?
                        if result.value != $route {
                            panic!(
                                "Wrong value for route '{}'. Expected '{}', found '{}')",
                                $path, result.value, $route
                            );
                        }

                        let expected_params = vec![$(($key, $val)),*];
                        let got_params = result.params.iter().collect::<Vec<_>>();

                        assert_eq!(
                            got_params, expected_params,
                            "Wrong params for route '{}'",
                            $path
                        );

                        tree.at_mut($path).unwrap().value.push_str("CHECKED");
                        assert!(tree.at($path).unwrap().value.contains("CHECKED"));

                        let val = tree.at_mut($path).unwrap().value;
                        *val = val.replace("CHECKED", "");
                    )?

                    $($( @$none )?
                        panic!(
                            "Unexpected value for route '{}', got: {:?}",
                            $path,
                            result.params.iter().collect::<Vec<_>>()
                        );
                    )?
                }
            })*

            if let Err((got, expected)) = tree.check_priorities() {
                panic!(
                    "priority mismatch for node: got '{}', expected '{}'",
                    got, expected
                )
            }
        }
   )* };
}

macro_rules! insert_tests {
    ($($name:ident {
        $($route:literal => $res:expr),* $(,)?
    }),* $(,)?) => { $(
        #[test]
        fn $name() {
            let mut tree = Node::new();

            $(
                let res = tree.insert($route, $route.to_owned());
                assert_eq!(res, $res, "unexpected result for path '{}'", $route);
            )*
        }
   )* };
}

macro_rules! tsr_tests {
    ($($name:ident {
        routes = $routes:expr,
        $($path:literal => $tsr:expr),* $(,)?
    }),* $(,)?) => { $(
        #[test]
        fn $name() {
            let mut tree = Node::new();

            for route in $routes {
                tree.insert(route, route.to_owned()).unwrap();
            }

            $(
                match tree.at($path) {
                    Err(m @ MatchError { .. }) => assert_eq!(
                        m.tsr(),
                        $tsr,
                        "wrong tsr value for '{}', expected {}, found {}", $path, $tsr, m.tsr()
                    ),
                    res => panic!("unexpected result for '{}': {:?}", $path, res)
                }
            )*
        }
   )* };
}

match_tests! {
    basic {
        routes = [
            "/hi",
            "/contact",
            "/co",
            "/c",
            "/a",
            "/ab",
            "/doc/",
            "/doc/rust_faq.html",
            "/doc/rust1.26.html",
            "/ʯ",
            "/β",
        ],
        "/a"       :: "/a"       => {},
        ""         :: "/"        => None,
        "/hi"      :: "/hi"      => {},
        "/contact" :: "/contact" => {},
        "/co"      :: "/co"      => {},
        ""         :: "/con"     => None,
        ""         :: "/cona"    => None,
        ""         :: "/no"      => None,
        "/ab"      :: "/ab"      => {},
        "/ʯ"       :: "/ʯ"       => {},
        "/β"       :: "/β"       => {}
    },
    wildcard {
        routes = [
            "/",
            "/cmd/:tool/",
            "/cmd/:tool/:sub",
            "/cmd/whoami",
            "/cmd/whoami/root",
            "/cmd/whoami/root/",
            "/src/*filepath",
            "/search/",
            "/search/:query",
            "/search/actix-web",
            "/search/google",
            "/user_:name",
            "/user_:name/about",
            "/files/:dir/*filepath",
            "/doc/",
            "/doc/rust_faq.html",
            "/doc/rust1.26.html",
            "/info/:user/public",
            "/info/:user/project/:project",
            "/info/:user/project/rustlang",
            "/aa/*xx",
            "/ab/*xx",
            "/:cc",
            "/c1/:dd/e",
            "/c1/:dd/e1",
            "/:cc/cc",
            "/:cc/:dd/ee",
            "/:cc/:dd/:ee/ff",
            "/:cc/:dd/:ee/:ff/gg",
            "/:cc/:dd/:ee/:ff/:gg/hh",
            "/get/test/abc/",
            "/get/:param/abc/",
            "/something/:paramname/thirdthing",
            "/something/secondthing/test",
            "/get/abc",
            "/get/:param",
            "/get/abc/123abc",
            "/get/abc/:param",
            "/get/abc/123abc/xxx8",
            "/get/abc/123abc/:param",
            "/get/abc/123abc/xxx8/1234",
            "/get/abc/123abc/xxx8/:param",
            "/get/abc/123abc/xxx8/1234/ffas",
            "/get/abc/123abc/xxx8/1234/:param",
            "/get/abc/123abc/xxx8/1234/kkdd/12c",
            "/get/abc/123abc/xxx8/1234/kkdd/:param",
            "/get/abc/:param/test",
            "/get/abc/123abd/:param",
            "/get/abc/123abddd/:param",
            "/get/abc/123/:param",
            "/get/abc/123abg/:param",
            "/get/abc/123abf/:param",
            "/get/abc/123abfff/:param",
        ],
        "/"                                     :: "/"                                     => {},
        "/cmd/test"                             :: "/cmd/:tool/"                           => None,
        "/cmd/test/"                            :: "/cmd/:tool/"                           => { "tool" => "test" },
        "/cmd/test/3"                           :: "/cmd/:tool/:sub"                       => { "tool" => "test", "sub" => "3" },
        "/cmd/who"                              :: "/cmd/:tool/"                           => None,
        "/cmd/who/"                             :: "/cmd/:tool/"                           => { "tool" => "who" },
        "/cmd/whoami"                           :: "/cmd/whoami"                           => {},
        "/cmd/whoami/"                          :: "/cmd/whoami"                           => None,
        "/cmd/whoami/r"                         :: "/cmd/:tool/:sub"                       => { "tool" => "whoami", "sub" => "r" },
        "/cmd/whoami/r/"                        :: "/cmd/:tool/:sub"                       => None,
        "/cmd/whoami/root"                      :: "/cmd/whoami/root"                      => {},
        "/cmd/whoami/root/"                     :: "/cmd/whoami/root/"                     => {},
        "/src/"                                 :: "/src/*filepath"                        => { "filepath" => "/" },
        "/src/some/file.png"                    :: "/src/*filepath"                        => { "filepath" => "/some/file.png" },
        "/search/"                              :: "/search/"                              => {},
        "/search/actix"                         :: "/search/:query"                        => { "query" => "actix" },
        "/search/actix-web"                     :: "/search/actix-web"                     => {},
        "/search/someth!ng+in+ünìcodé"          :: "/search/:query"                        => { "query" => "someth!ng+in+ünìcodé" },
        "/search/someth!ng+in+ünìcodé/"         :: ""                                      => None,
        "/user_rustacean"                       :: "/user_:name"                           => { "name" => "rustacean" },
        "/user_rustacean/about"                 :: "/user_:name/about"                     => { "name" => "rustacean" },
        "/files/js/inc/framework.js"            :: "/files/:dir/*filepath"                 => { "dir" => "js", "filepath" => "/inc/framework.js" },
        "/info/gordon/public"                   :: "/info/:user/public"                    => { "user" => "gordon" },
        "/info/gordon/project/rust"             :: "/info/:user/project/:project"          => { "user" => "gordon", "project" => "rust" } ,
        "/info/gordon/project/rustlang"         :: "/info/:user/project/rustlang"          => { "user" => "gordon" },
        "/aa/aa"                                :: "/aa/*xx"                               => { "xx" => "/aa" },
        "/ab/ab"                                :: "/ab/*xx"                               => { "xx" => "/ab" },
        "/a"                                    :: "/:cc"                                  => { "cc" => "a" },
        "/all"                                  :: "/:cc"                                  => { "cc" => "all" },
        "/d"                                    :: "/:cc"                                  => { "cc" => "d" },
        "/ad"                                   :: "/:cc"                                  => { "cc" => "ad" },
        "/dd"                                   :: "/:cc"                                  => { "cc" => "dd" },
        "/dddaa"                                :: "/:cc"                                  => { "cc" => "dddaa" },
        "/aa"                                   :: "/:cc"                                  => { "cc" => "aa" },
        "/aaa"                                  :: "/:cc"                                  => { "cc" => "aaa" },
        "/aaa/cc"                               :: "/:cc/cc"                               => { "cc" => "aaa" },
        "/ab"                                   :: "/:cc"                                  => { "cc" => "ab" },
        "/abb"                                  :: "/:cc"                                  => { "cc" => "abb" },
        "/abb/cc"                               :: "/:cc/cc"                               => { "cc" => "abb" },
        "/allxxxx"                              :: "/:cc"                                  => { "cc" => "allxxxx" },
        "/alldd"                                :: "/:cc"                                  => { "cc" => "alldd" },
        "/all/cc"                               :: "/:cc/cc"                               => { "cc" => "all" },
        "/a/cc"                                 :: "/:cc/cc"                               => { "cc" => "a" },
        "/c1/d/e"                               :: "/c1/:dd/e"                             => { "dd" => "d" },
        "/c1/d/e1"                              :: "/c1/:dd/e1"                            => { "dd" => "d" },
        "/c1/d/ee"                              :: "/:cc/:dd/ee"                           => { "cc" => "c1", "dd" => "d" },
        "/cc/cc"                                :: "/:cc/cc"                               => { "cc" => "cc" },
        "/ccc/cc"                               :: "/:cc/cc"                               => { "cc" => "ccc" },
        "/deedwjfs/cc"                          :: "/:cc/cc"                               => { "cc" => "deedwjfs" },
        "/acllcc/cc"                            :: "/:cc/cc"                               => { "cc" => "acllcc" },
        "/get/test/abc/"                        :: "/get/test/abc/"                        => {},
        "/get/te/abc/"                          :: "/get/:param/abc/"                      => { "param" => "te" },
        "/get/testaa/abc/"                      :: "/get/:param/abc/"                      => { "param" => "testaa" },
        "/get/xx/abc/"                          :: "/get/:param/abc/"                      => { "param" => "xx" },
        "/get/tt/abc/"                          :: "/get/:param/abc/"                      => { "param" => "tt" },
        "/get/a/abc/"                           :: "/get/:param/abc/"                      => { "param" => "a" },
        "/get/t/abc/"                           :: "/get/:param/abc/"                      => { "param" => "t" },
        "/get/aa/abc/"                          :: "/get/:param/abc/"                      => { "param" => "aa" },
        "/get/abas/abc/"                        :: "/get/:param/abc/"                      => { "param" => "abas" },
        "/something/secondthing/test"           :: "/something/secondthing/test"           => {},
        "/something/abcdad/thirdthing"          :: "/something/:paramname/thirdthing"      => { "paramname" => "abcdad" },
        "/something/secondthingaaaa/thirdthing" :: "/something/:paramname/thirdthing"      => { "paramname" => "secondthingaaaa" },
        "/something/se/thirdthing"              :: "/something/:paramname/thirdthing"      => { "paramname" => "se" },
        "/something/s/thirdthing"               :: "/something/:paramname/thirdthing"      => { "paramname" => "s" },
        "/c/d/ee"                               :: "/:cc/:dd/ee"                           => { "cc" => "c", "dd" => "d" },
        "/c/d/e/ff"                             :: "/:cc/:dd/:ee/ff"                       => { "cc" => "c", "dd" => "d", "ee" => "e" },
        "/c/d/e/f/gg"                           :: "/:cc/:dd/:ee/:ff/gg"                   => { "cc" => "c", "dd" => "d", "ee" => "e", "ff" => "f" },
        "/c/d/e/f/g/hh"                         :: "/:cc/:dd/:ee/:ff/:gg/hh"               => { "cc" => "c", "dd" => "d", "ee" => "e", "ff" => "f", "gg" => "g" },
        "/cc/dd/ee/ff/gg/hh"                    :: "/:cc/:dd/:ee/:ff/:gg/hh"               => { "cc" => "cc", "dd" => "dd", "ee" => "ee", "ff" => "ff", "gg" => "gg" },
        "/get/abc"                              :: "/get/abc"                              => {},
        "/get/a"                                :: "/get/:param"                           => { "param" => "a" },
        "/get/abz"                              :: "/get/:param"                           => { "param" => "abz" },
        "/get/12a"                              :: "/get/:param"                           => { "param" => "12a" },
        "/get/abcd"                             :: "/get/:param"                           => { "param" => "abcd" },
        "/get/abc/123abc"                       :: "/get/abc/123abc"                       => {},
        "/get/abc/12"                           :: "/get/abc/:param"                       => { "param" => "12" },
        "/get/abc/123ab"                        :: "/get/abc/:param"                       => { "param" => "123ab" },
        "/get/abc/xyz"                          :: "/get/abc/:param"                       => { "param" => "xyz" },
        "/get/abc/123abcddxx"                   :: "/get/abc/:param"                       => { "param" => "123abcddxx" },
        "/get/abc/123abc/xxx8"                  :: "/get/abc/123abc/xxx8"                  => {},
        "/get/abc/123abc/x"                     :: "/get/abc/123abc/:param"                => { "param" => "x" },
        "/get/abc/123abc/xxx"                   :: "/get/abc/123abc/:param"                => { "param" => "xxx" },
        "/get/abc/123abc/abc"                   :: "/get/abc/123abc/:param"                => { "param" => "abc" },
        "/get/abc/123abc/xxx8xxas"              :: "/get/abc/123abc/:param"                => { "param" => "xxx8xxas" },
        "/get/abc/123abc/xxx8/1234"             :: "/get/abc/123abc/xxx8/1234"             => {},
        "/get/abc/123abc/xxx8/1"                :: "/get/abc/123abc/xxx8/:param"           => { "param" => "1" },
        "/get/abc/123abc/xxx8/123"              :: "/get/abc/123abc/xxx8/:param"           => { "param" => "123" },
        "/get/abc/123abc/xxx8/78k"              :: "/get/abc/123abc/xxx8/:param"           => { "param" => "78k" },
        "/get/abc/123abc/xxx8/1234xxxd"         :: "/get/abc/123abc/xxx8/:param"           => { "param" => "1234xxxd" },
        "/get/abc/123abc/xxx8/1234/ffas"        :: "/get/abc/123abc/xxx8/1234/ffas"        => {},
        "/get/abc/123abc/xxx8/1234/f"           :: "/get/abc/123abc/xxx8/1234/:param"      => { "param" => "f" },
        "/get/abc/123abc/xxx8/1234/ffa"         :: "/get/abc/123abc/xxx8/1234/:param"      => { "param" => "ffa" },
        "/get/abc/123abc/xxx8/1234/kka"         :: "/get/abc/123abc/xxx8/1234/:param"      => { "param" => "kka" },
        "/get/abc/123abc/xxx8/1234/ffas321"     :: "/get/abc/123abc/xxx8/1234/:param"      => { "param" => "ffas321" },
        "/get/abc/123abc/xxx8/1234/kkdd/12c"    :: "/get/abc/123abc/xxx8/1234/kkdd/12c"    => {},
        "/get/abc/123abc/xxx8/1234/kkdd/1"      :: "/get/abc/123abc/xxx8/1234/kkdd/:param" => { "param" => "1" },
        "/get/abc/123abc/xxx8/1234/kkdd/12"     :: "/get/abc/123abc/xxx8/1234/kkdd/:param" => { "param" => "12" },
        "/get/abc/123abc/xxx8/1234/kkdd/12b"    :: "/get/abc/123abc/xxx8/1234/kkdd/:param" => { "param" => "12b" },
        "/get/abc/123abc/xxx8/1234/kkdd/34"     :: "/get/abc/123abc/xxx8/1234/kkdd/:param" => { "param" => "34" },
        "/get/abc/123abc/xxx8/1234/kkdd/12c2e3" :: "/get/abc/123abc/xxx8/1234/kkdd/:param" => { "param" => "12c2e3" },
        "/get/abc/12/test"                      :: "/get/abc/:param/test"                  => { "param" => "12" },
        "/get/abc/123abdd/test"                 :: "/get/abc/:param/test"                  => { "param" => "123abdd" },
        "/get/abc/123abdddf/test"               :: "/get/abc/:param/test"                  => { "param" => "123abdddf" },
        "/get/abc/123ab/test"                   :: "/get/abc/:param/test"                  => { "param" => "123ab" },
        "/get/abc/123abgg/test"                 :: "/get/abc/:param/test"                  => { "param" => "123abgg" },
        "/get/abc/123abff/test"                 :: "/get/abc/:param/test"                  => { "param" => "123abff" },
        "/get/abc/123abffff/test"               :: "/get/abc/:param/test"                  => { "param" => "123abffff" },
        "/get/abc/123abd/test"                  :: "/get/abc/123abd/:param"                => { "param" => "test" },
        "/get/abc/123abddd/test"                :: "/get/abc/123abddd/:param"              => { "param" => "test" },
        "/get/abc/123/test22"                   :: "/get/abc/123/:param"                   => { "param" => "test22" },
        "/get/abc/123abg/test"                  :: "/get/abc/123abg/:param"                => { "param" => "test" },
        "/get/abc/123abf/testss"                :: "/get/abc/123abf/:param"                => { "param" => "testss" },
        "/get/abc/123abfff/te"                  :: "/get/abc/123abfff/:param"              => { "param" => "te" },
    }
}

insert_tests! {
    wildcard_conflict {
        "/cmd/:tool/:sub"     => Ok(()),
        "/cmd/vet"            => Ok(()),
        "/foo/bar"            => Ok(()),
        "/foo/:name"          => Ok(()),
        "/foo/:names"         => Err(InsertError::Conflict { with: "/foo/:name".into() }),
        "/cmd/*path"          => Err(InsertError::Conflict { with: "/cmd/:tool/:sub".into() }),
        "/cmd/:badvar"        => Err(InsertError::Conflict { with: "/cmd/:tool/:sub".into() }),
        "/cmd/:tool/names"    => Ok(()),
        "/cmd/:tool/:bad/foo" => Err(InsertError::Conflict { with: "/cmd/:tool/:sub".into() }),
        "/src/*filepath"      => Ok(()),
        "/src/:file"          => Err(InsertError::Conflict { with: "/src/*filepath".into() }),
        "/src/static.json"    => Err(InsertError::Conflict { with: "/src/*filepath".into() }),
        "/src/$filepathx"     => Err(InsertError::Conflict { with: "/src/*filepath".into() }),
        "/src/"               => Err(InsertError::Conflict { with: "/src/*filepath".into() }),
        "/src/foo/bar"        => Err(InsertError::Conflict { with: "/src/*filepath".into() }),
        "/src1/"              => Ok(()),
        "/src1/*filepath"     => Err(InsertError::Conflict { with: "/src1/".into() }),
        "/src2*filepath"      => Err(InsertError::InvalidCatchAll),
        "/src2/*filepath"     => Ok(()),
        "/search/:query"      => Ok(()),
        "/search/valid"       => Ok(()),
        "/user_:name"         => Ok(()),
        "/user_x"             => Ok(()),
        "/user_:bar"          => Err(InsertError::Conflict { with: "/user_:name".into() }),
        "/id:id"              => Ok(()),
        "/id/:id"             => Ok(()),
    },
    invalid_catchall {
        "/non-leading-*catchall" => Err(InsertError::InvalidCatchAll),
        "/src/*filepath/x"       => Err(InsertError::InvalidCatchAll),
        "/src2/"                 => Ok(()),
        "/src2/*filepath/x"      => Err(InsertError::InvalidCatchAll),
    },
    catchall_root_conflict {
        "/"          => Ok(()),
        "/*filepath" => Err(InsertError::Conflict { with: "/".into() }),
    },
    child_conflict {
        "/cmd/vet"        => Ok(()),
        "/cmd/:tool"      => Ok(()),
        "/cmd/:tool/:sub" => Ok(()),
        "/cmd/:tool/misc" => Ok(()),
        "/cmd/:tool/:bad" => Err(InsertError::Conflict { with: "/cmd/:tool/:sub".into() }),
        "/src/AUTHORS"    => Ok(()),
        "/src/*filepath"  => Err(InsertError::Conflict { with: "/src/AUTHORS".into() }),
        "/user_x"         => Ok(()),
        "/user_:name"     => Ok(()),
        "/id/:id"         => Ok(()),
        "/id:id"          => Ok(()),
        "/:id"            => Ok(()),
        "/*filepath"      => Err(InsertError::Conflict { with: "/:id".into() }),
    },
    duplicates {
        "/"              => Ok(()),
        "/"              => Err(InsertError::Conflict { with: "/".into() }),
        "/doc/"          => Ok(()),
        "/doc/"          => Err(InsertError::Conflict { with: "/doc/".into() }),
        "/src/*filepath" => Ok(()),
        "/src/*filepath" => Err(InsertError::Conflict { with: "/src/*filepath".into() }),
        "/search/:query" => Ok(()),
        "/search/:query" => Err(InsertError::Conflict { with: "/search/:query".into() }),
        "/user_:name"    => Ok(()),
        "/user_:name"    => Err(InsertError::Conflict { with: "/user_:name".into() }),
    },
    unnamed_param {
        "/user:"  => Err(InsertError::UnnamedParam),
        "/user:/" => Err(InsertError::UnnamedParam),
        "/cmd/:/" => Err(InsertError::UnnamedParam),
        "/src/*"  => Err(InsertError::UnnamedParam),
    },
    double_params {
        "/:foo:bar"  => Err(InsertError::TooManyParams),
        "/:foo:bar/" => Err(InsertError::TooManyParams),
        "/:foo*bar/" => Err(InsertError::TooManyParams),
    },
    malformed_route {
        "*x" => Err(InsertError::MalformedRoute)
    },
    more_conflicts {
        "/con:tact"           => Ok(()),
        "/who/are/*you"       => Ok(()),
        "/who/foo/hello"      => Ok(()),
        "/whose/:users/:name" => Ok(()),
        "/who/are/foo"        => Err(InsertError::Conflict { with: "/who/are/*you".into() }),
        "/who/are/foo/bar"    => Err(InsertError::Conflict { with: "/who/are/*you".into() }),
        "/con:nection"        => Err(InsertError::Conflict { with: "/con:tact".into() }),
        "/whose/:users/:user" => Err(InsertError::Conflict { with: "/whose/:users/:name".into() }),
    }
}

tsr_tests! {
    tsr {
        routes = [
            "/hi",
            "/b/",
            "/search/:query",
            "/cmd/:tool/",
            "/src/*filepath",
            "/x",
            "/x/y",
            "/y/",
            "/y/z",
            "/0/:id",
            "/0/:id/1",
            "/1/:id/",
            "/1/:id/2",
            "/aa",
            "/a/",
            "/admin",
            "/admin/static",
            "/admin/:category",
            "/admin/:category/:page",
            "/doc",
            "/doc/rust_faq.html",
            "/doc/rust1.26.html",
            "/no/a",
            "/no/b",
            "/api/:page/:name",
            "/api/hello/:name/bar/",
            "/api/bar/:name",
            "/api/baz/foo",
            "/api/baz/foo/bar",
            "/foo/:p",
        ],
        "/hi/"               => true,
        "/b"                 => true,
        "/search/rustacean/" => true,
        "/cmd/vet"           => true,
        "/src"               => true,
        "/x/"                => true,
        "/y"                 => true,
        "/0/rust/"           => true,
        "/1/rust"            => true,
        "/a"                 => true,
        "/admin/"            => true,
        "/doc/"              => true,
        "/admin/static/"     => true,
        "/admin/cfg/"        => true,
        "/admin/cfg/users/"  => true,
        // TODO: fix
        // "/api/hello/x/bar"   => true,
        "/api/baz/foo/"      => true,
        "/api/baz/bax/"      => true,
        "/api/bar/huh/"      => true,
        "/api/baz/foo/bar/"  => true,
        "/api/world/abc/"    => true,
        "/foo/pp/"           => true,
        "/"                  => false,
        "/no"                => false,
        "/no/"               => false,
        "/_"                 => false,
        "/_/"                => false,
        "/api"               => false,
        "/api/"              => false,
        "/api/hello/x/foo"   => false,
        "/api/baz/foo/bad"   => false,
        "/foo/p/p"           => false,
    },
    root_tsr_wildcard {
        routes = ["/:foo"],
        "/" => false,
    },
    root_tsr_static {
        routes = ["/foo"],
        "/" => false,
    },
    root_tsr {
        routes = [
            "/foo",
            "/bar",
            "/:baz"
        ],
        "/" => false,
    }
}

#[test]
fn test_tree_find_case_insensitive_path() {
    let mut tree = Node::new();

    let routes = vec![
            "/hi",
            "/b/",
            "/ABC/",
            "/search/:query",
            "/cmd/:tool/",
            "/src/*filepath",
            "/x",
            "/x/y",
            "/y/",
            "/y/z",
            "/0/:id",
            "/0/:id/1",
            "/1/:id/",
            "/1/:id/2",
            "/aa",
            "/a/",
            "/doc",
            "/doc/rust_faq.html",
            "/doc/rust1.26.html",
            "/doc/go/away",
            "/no/a",
            "/no/b",
            "/Π",
            "/u/apfêl/",
            "/u/äpfêl/",
            "/u/öpfêl",
            "/v/Äpfêl/",
            "/v/Öpfêl",
            "/w/♬",
            "/w/♭/",
            "/w/𠜎",
            "/w/𠜏/",
            "/loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong",
    ];

    for route in &routes {
        tree.insert(*route, route.to_owned()).unwrap();
    }

    for route in &routes {
        let out = tree.path_ignore_case(route, true);
        match out {
            None => panic!("Route '{}' not found!", route),
            Some(out) => {
                if out != *route {
                    panic!("Wrong result for route '{}': {}", route, out);
                }
            }
        };
    }

    for route in &routes {
        let out = tree.path_ignore_case(route, false);
        match out {
            None => panic!("Route '{}' not found!", route),
            Some(out) => {
                if out != *route {
                    panic!("Wrong result for route '{}': {}", route, out);
                }
            }
        };
    }

    let tests = vec![
            ("/HI", "/hi", false),
            ("/HI/", "/hi", true),
            ("/B", "/b/", true),
            ("/B/", "/b/", false),
            ("/abc", "/ABC/", true),
            ("/abc/", "/ABC/", false),
            ("/aBc", "/ABC/", true),
            ("/aBc/", "/ABC/", false),
            ("/abC", "/ABC/", true),
            ("/abC/", "/ABC/", false),
            ("/SEARCH/QUERY", "/search/QUERY", false),
            ("/SEARCH/QUERY/", "/search/QUERY", true),
            ("/CMD/TOOL/", "/cmd/TOOL/", false),
            ("/CMD/TOOL", "/cmd/TOOL/", true),
            ("/SRC/FILE/PATH", "/src/FILE/PATH", false),
            ("/x/Y", "/x/y", false),
            ("/x/Y/", "/x/y", true),
            ("/X/y", "/x/y", false),
            ("/X/y/", "/x/y", true),
            ("/X/Y", "/x/y", false),
            ("/X/Y/", "/x/y", true),
            ("/Y/", "/y/", false),
            ("/Y", "/y/", true),
            ("/Y/z", "/y/z", false),
            ("/Y/z/", "/y/z", true),
            ("/Y/Z", "/y/z", false),
            ("/Y/Z/", "/y/z", true),
            ("/y/Z", "/y/z", false),
            ("/y/Z/", "/y/z", true),
            ("/Aa", "/aa", false),
            ("/Aa/", "/aa", true),
            ("/AA", "/aa", false),
            ("/AA/", "/aa", true),
            ("/aA", "/aa", false),
            ("/aA/", "/aa", true),
            ("/A/", "/a/", false),
            ("/A", "/a/", true),
            ("/DOC", "/doc", false),
            ("/DOC/", "/doc", true),
            ("/NO", "", true),
            ("/DOC/RUST", "", true),
            // [TODO] unicode case sensitivity
            // ("/π", "/Π", false),
            // ("/π/", "/Π", true),
            // ("/u/ÄPFÊL/", "/u/äpfêl/", false),
            // ("/u/ÄPFÊL", "/u/äpfêl/", true),
            // ("/u/ÖPFÊL/", "/u/öpfêl", true),
            // ("/u/ÖPFÊL", "/u/öpfêl", false),
            // ("/v/äpfêL/", "/v/Äpfêl/", false),
            // ("/v/äpfêL", "/v/Äpfêl/", true),
            // ("/v/öpfêL/", "/v/Öpfêl", true),
            // ("/v/öpfêL", "/v/Öpfêl", false),
            ("/w/♬/", "/w/♬", true),
            ("/w/♭", "/w/♭/", true),
            ("/w/𠜎/", "/w/𠜎", true),
            ("/w/𠜏", "/w/𠜏/", true),
            (
                "/lOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOng/",
                "/loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong",
                true
            ),
    ];

    struct Test {
        inn: &'static str,
        out: &'static str,
        slash: bool,
    }

    let tests: Vec<Test> = tests
        .into_iter()
        .map(|test| Test {
            inn: test.0,
            out: test.1,
            slash: test.2,
        })
        .collect();

    for test in &tests {
        let res = tree.path_ignore_case(test.inn, true).unwrap_or_default();
        if res != test.out {
            panic!("Wrong result for route '{}': {}", res, test.out);
        }
    }

    for test in &tests {
        let res = tree.path_ignore_case(test.inn, false);
        match res {
            None => (),
            Some(res) => {
                if test.slash {
                    panic!(
                        "Found without fix_trailing_slash: {}; got {}",
                        test.inn, res
                    );
                }
                if res != test.out {
                    panic!("Wrong result for route '{}': {}", res, test.out);
                }
            }
        };
    }
}
