{ fetchurl, fetchgit, linkFarm, runCommandNoCC, gnutar }: rec {
  offline_cache = linkFarm "offline" packages;
  packages = [
    {
      name = "accepts___accepts_1.3.7.tgz";
      path = fetchurl {
        name = "accepts___accepts_1.3.7.tgz";
        url  = "https://registry.yarnpkg.com/accepts/-/accepts-1.3.7.tgz";
        sha1 = "531bc726517a3b2b41f850021c6cc15eaab507cd";
      };
    }
    {
      name = "array_flatten___array_flatten_1.1.1.tgz";
      path = fetchurl {
        name = "array_flatten___array_flatten_1.1.1.tgz";
        url  = "https://registry.yarnpkg.com/array-flatten/-/array-flatten-1.1.1.tgz";
        sha1 = "9a5f699051b1e7073328f2a008968b64ea2955d2";
      };
    }
    {
      name = "async_limiter___async_limiter_1.0.1.tgz";
      path = fetchurl {
        name = "async_limiter___async_limiter_1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/async-limiter/-/async-limiter-1.0.1.tgz";
        sha1 = "dd379e94f0db8310b08291f9d64c3209766617fd";
      };
    }
    {
      name = "basic_auth___basic_auth_2.0.1.tgz";
      path = fetchurl {
        name = "basic_auth___basic_auth_2.0.1.tgz";
        url  = "https://registry.yarnpkg.com/basic-auth/-/basic-auth-2.0.1.tgz";
        sha1 = "b998279bf47ce38344b4f3cf916d4679bbf51e3a";
      };
    }
    {
      name = "body_parser___body_parser_1.18.3.tgz";
      path = fetchurl {
        name = "body_parser___body_parser_1.18.3.tgz";
        url  = "https://registry.yarnpkg.com/body-parser/-/body-parser-1.18.3.tgz";
        sha1 = "5b292198ffdd553b3a0f20ded0592b956955c8b4";
      };
    }
    {
      name = "bytes___bytes_3.0.0.tgz";
      path = fetchurl {
        name = "bytes___bytes_3.0.0.tgz";
        url  = "https://registry.yarnpkg.com/bytes/-/bytes-3.0.0.tgz";
        sha1 = "d32815404d689699f85a4ea4fa8755dd13a96048";
      };
    }
    {
      name = "content_disposition___content_disposition_0.5.2.tgz";
      path = fetchurl {
        name = "content_disposition___content_disposition_0.5.2.tgz";
        url  = "https://registry.yarnpkg.com/content-disposition/-/content-disposition-0.5.2.tgz";
        sha1 = "0cf68bb9ddf5f2be7961c3a85178cb85dba78cb4";
      };
    }
    {
      name = "content_type___content_type_1.0.4.tgz";
      path = fetchurl {
        name = "content_type___content_type_1.0.4.tgz";
        url  = "https://registry.yarnpkg.com/content-type/-/content-type-1.0.4.tgz";
        sha1 = "e138cc75e040c727b1966fe5e5f8c9aee256fe3b";
      };
    }
    {
      name = "cookie_parser___cookie_parser_1.4.5.tgz";
      path = fetchurl {
        name = "cookie_parser___cookie_parser_1.4.5.tgz";
        url  = "https://registry.yarnpkg.com/cookie-parser/-/cookie-parser-1.4.5.tgz";
        sha1 = "3e572d4b7c0c80f9c61daf604e4336831b5d1d49";
      };
    }
    {
      name = "cookie_signature___cookie_signature_1.0.6.tgz";
      path = fetchurl {
        name = "cookie_signature___cookie_signature_1.0.6.tgz";
        url  = "https://registry.yarnpkg.com/cookie-signature/-/cookie-signature-1.0.6.tgz";
        sha1 = "e303a882b342cc3ee8ca513a79999734dab3ae2c";
      };
    }
    {
      name = "cookie___cookie_0.3.1.tgz";
      path = fetchurl {
        name = "cookie___cookie_0.3.1.tgz";
        url  = "https://registry.yarnpkg.com/cookie/-/cookie-0.3.1.tgz";
        sha1 = "e7e0a1f9ef43b4c8ba925c5c5a96e806d16873bb";
      };
    }
    {
      name = "cookie___cookie_0.4.0.tgz";
      path = fetchurl {
        name = "cookie___cookie_0.4.0.tgz";
        url  = "https://registry.yarnpkg.com/cookie/-/cookie-0.4.0.tgz";
        sha1 = "beb437e7022b3b6d49019d088665303ebe9c14ba";
      };
    }
    {
      name = "debug___debug_2.6.9.tgz";
      path = fetchurl {
        name = "debug___debug_2.6.9.tgz";
        url  = "https://registry.yarnpkg.com/debug/-/debug-2.6.9.tgz";
        sha1 = "5d128515df134ff327e90a4c93f4e077a536341f";
      };
    }
    {
      name = "depd___depd_1.1.2.tgz";
      path = fetchurl {
        name = "depd___depd_1.1.2.tgz";
        url  = "https://registry.yarnpkg.com/depd/-/depd-1.1.2.tgz";
        sha1 = "9bcd52e14c097763e749b274c4346ed2e560b5a9";
      };
    }
    {
      name = "destroy___destroy_1.0.4.tgz";
      path = fetchurl {
        name = "destroy___destroy_1.0.4.tgz";
        url  = "https://registry.yarnpkg.com/destroy/-/destroy-1.0.4.tgz";
        sha1 = "978857442c44749e4206613e37946205826abd80";
      };
    }
    {
      name = "ee_first___ee_first_1.1.1.tgz";
      path = fetchurl {
        name = "ee_first___ee_first_1.1.1.tgz";
        url  = "https://registry.yarnpkg.com/ee-first/-/ee-first-1.1.1.tgz";
        sha1 = "590c61156b0ae2f4f0255732a158b266bc56b21d";
      };
    }
    {
      name = "encodeurl___encodeurl_1.0.2.tgz";
      path = fetchurl {
        name = "encodeurl___encodeurl_1.0.2.tgz";
        url  = "https://registry.yarnpkg.com/encodeurl/-/encodeurl-1.0.2.tgz";
        sha1 = "ad3ff4c86ec2d029322f5a02c3a9a606c95b3f59";
      };
    }
    {
      name = "escape_html___escape_html_1.0.3.tgz";
      path = fetchurl {
        name = "escape_html___escape_html_1.0.3.tgz";
        url  = "https://registry.yarnpkg.com/escape-html/-/escape-html-1.0.3.tgz";
        sha1 = "0258eae4d3d0c0974de1c169188ef0051d1d1988";
      };
    }
    {
      name = "etag___etag_1.8.1.tgz";
      path = fetchurl {
        name = "etag___etag_1.8.1.tgz";
        url  = "https://registry.yarnpkg.com/etag/-/etag-1.8.1.tgz";
        sha1 = "41ae2eeb65efa62268aebfea83ac7d79299b0887";
      };
    }
    {
      name = "express___express_4.15.5.tgz";
      path = fetchurl {
        name = "express___express_4.15.5.tgz";
        url  = "https://registry.yarnpkg.com/express/-/express-4.15.5.tgz";
        sha1 = "670235ca9598890a5ae8170b83db722b842ed927";
      };
    }
    {
      name = "finalhandler___finalhandler_1.0.6.tgz";
      path = fetchurl {
        name = "finalhandler___finalhandler_1.0.6.tgz";
        url  = "https://registry.yarnpkg.com/finalhandler/-/finalhandler-1.0.6.tgz";
        sha1 = "007aea33d1a4d3e42017f624848ad58d212f814f";
      };
    }
    {
      name = "forwarded___forwarded_0.1.2.tgz";
      path = fetchurl {
        name = "forwarded___forwarded_0.1.2.tgz";
        url  = "https://registry.yarnpkg.com/forwarded/-/forwarded-0.1.2.tgz";
        sha1 = "98c23dab1175657b8c0573e8ceccd91b0ff18c84";
      };
    }
    {
      name = "fresh___fresh_0.5.2.tgz";
      path = fetchurl {
        name = "fresh___fresh_0.5.2.tgz";
        url  = "https://registry.yarnpkg.com/fresh/-/fresh-0.5.2.tgz";
        sha1 = "3d8cadd90d976569fa835ab1f8e4b23a105605a7";
      };
    }
    {
      name = "http_errors___http_errors_1.6.3.tgz";
      path = fetchurl {
        name = "http_errors___http_errors_1.6.3.tgz";
        url  = "https://registry.yarnpkg.com/http-errors/-/http-errors-1.6.3.tgz";
        sha1 = "8b55680bb4be283a0b5bf4ea2e38580be1d9320d";
      };
    }
    {
      name = "iconv_lite___iconv_lite_0.4.23.tgz";
      path = fetchurl {
        name = "iconv_lite___iconv_lite_0.4.23.tgz";
        url  = "https://registry.yarnpkg.com/iconv-lite/-/iconv-lite-0.4.23.tgz";
        sha1 = "297871f63be507adcfbfca715d0cd0eed84e9a63";
      };
    }
    {
      name = "inherits___inherits_2.0.3.tgz";
      path = fetchurl {
        name = "inherits___inherits_2.0.3.tgz";
        url  = "https://registry.yarnpkg.com/inherits/-/inherits-2.0.3.tgz";
        sha1 = "633c2c83e3da42a502f52466022480f4208261de";
      };
    }
    {
      name = "ipaddr.js___ipaddr.js_1.4.0.tgz";
      path = fetchurl {
        name = "ipaddr.js___ipaddr.js_1.4.0.tgz";
        url  = "https://registry.yarnpkg.com/ipaddr.js/-/ipaddr.js-1.4.0.tgz";
        sha1 = "296aca878a821816e5b85d0a285a99bcff4582f0";
      };
    }
    {
      name = "media_typer___media_typer_0.3.0.tgz";
      path = fetchurl {
        name = "media_typer___media_typer_0.3.0.tgz";
        url  = "https://registry.yarnpkg.com/media-typer/-/media-typer-0.3.0.tgz";
        sha1 = "8710d7af0aa626f8fffa1ce00168545263255748";
      };
    }
    {
      name = "merge_descriptors___merge_descriptors_1.0.1.tgz";
      path = fetchurl {
        name = "merge_descriptors___merge_descriptors_1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/merge-descriptors/-/merge-descriptors-1.0.1.tgz";
        sha1 = "b00aaa556dd8b44568150ec9d1b953f3f90cbb61";
      };
    }
    {
      name = "methods___methods_1.1.2.tgz";
      path = fetchurl {
        name = "methods___methods_1.1.2.tgz";
        url  = "https://registry.yarnpkg.com/methods/-/methods-1.1.2.tgz";
        sha1 = "5529a4d67654134edcc5266656835b0f851afcee";
      };
    }
    {
      name = "mime_db___mime_db_1.45.0.tgz";
      path = fetchurl {
        name = "mime_db___mime_db_1.45.0.tgz";
        url  = "https://registry.yarnpkg.com/mime-db/-/mime-db-1.45.0.tgz";
        sha1 = "cceeda21ccd7c3a745eba2decd55d4b73e7879ea";
      };
    }
    {
      name = "mime_types___mime_types_2.1.28.tgz";
      path = fetchurl {
        name = "mime_types___mime_types_2.1.28.tgz";
        url  = "https://registry.yarnpkg.com/mime-types/-/mime-types-2.1.28.tgz";
        sha1 = "1160c4757eab2c5363888e005273ecf79d2a0ecd";
      };
    }
    {
      name = "mime___mime_1.3.4.tgz";
      path = fetchurl {
        name = "mime___mime_1.3.4.tgz";
        url  = "https://registry.yarnpkg.com/mime/-/mime-1.3.4.tgz";
        sha1 = "115f9e3b6b3daf2959983cb38f149a2d40eb5d53";
      };
    }
    {
      name = "morgan___morgan_1.9.1.tgz";
      path = fetchurl {
        name = "morgan___morgan_1.9.1.tgz";
        url  = "https://registry.yarnpkg.com/morgan/-/morgan-1.9.1.tgz";
        sha1 = "0a8d16734a1d9afbc824b99df87e738e58e2da59";
      };
    }
    {
      name = "mpd___mpd_1.3.0.tgz";
      path = fetchurl {
        name = "mpd___mpd_1.3.0.tgz";
        url  = "https://registry.yarnpkg.com/mpd/-/mpd-1.3.0.tgz";
        sha1 = "a9a0e028f6808e5594f76fa9f0c574ad86f0c0dd";
      };
    }
    {
      name = "ms___ms_2.0.0.tgz";
      path = fetchurl {
        name = "ms___ms_2.0.0.tgz";
        url  = "https://registry.yarnpkg.com/ms/-/ms-2.0.0.tgz";
        sha1 = "5608aeadfc00be6c2901df5f9861788de0d597c8";
      };
    }
    {
      name = "negotiator___negotiator_0.6.2.tgz";
      path = fetchurl {
        name = "negotiator___negotiator_0.6.2.tgz";
        url  = "https://registry.yarnpkg.com/negotiator/-/negotiator-0.6.2.tgz";
        sha1 = "feacf7ccf525a77ae9634436a64883ffeca346fb";
      };
    }
    {
      name = "on_finished___on_finished_2.3.0.tgz";
      path = fetchurl {
        name = "on_finished___on_finished_2.3.0.tgz";
        url  = "https://registry.yarnpkg.com/on-finished/-/on-finished-2.3.0.tgz";
        sha1 = "20f1336481b083cd75337992a16971aa2d906947";
      };
    }
    {
      name = "on_headers___on_headers_1.0.2.tgz";
      path = fetchurl {
        name = "on_headers___on_headers_1.0.2.tgz";
        url  = "https://registry.yarnpkg.com/on-headers/-/on-headers-1.0.2.tgz";
        sha1 = "772b0ae6aaa525c399e489adfad90c403eb3c28f";
      };
    }
    {
      name = "parseurl___parseurl_1.3.3.tgz";
      path = fetchurl {
        name = "parseurl___parseurl_1.3.3.tgz";
        url  = "https://registry.yarnpkg.com/parseurl/-/parseurl-1.3.3.tgz";
        sha1 = "9da19e7bee8d12dff0513ed5b76957793bc2e8d4";
      };
    }
    {
      name = "path_to_regexp___path_to_regexp_0.1.7.tgz";
      path = fetchurl {
        name = "path_to_regexp___path_to_regexp_0.1.7.tgz";
        url  = "https://registry.yarnpkg.com/path-to-regexp/-/path-to-regexp-0.1.7.tgz";
        sha1 = "df604178005f522f15eb4490e7247a1bfaa67f8c";
      };
    }
    {
      name = "proxy_addr___proxy_addr_1.1.5.tgz";
      path = fetchurl {
        name = "proxy_addr___proxy_addr_1.1.5.tgz";
        url  = "https://registry.yarnpkg.com/proxy-addr/-/proxy-addr-1.1.5.tgz";
        sha1 = "71c0ee3b102de3f202f3b64f608d173fcba1a918";
      };
    }
    {
      name = "qs___qs_6.5.0.tgz";
      path = fetchurl {
        name = "qs___qs_6.5.0.tgz";
        url  = "https://registry.yarnpkg.com/qs/-/qs-6.5.0.tgz";
        sha1 = "8d04954d364def3efc55b5a0793e1e2c8b1e6e49";
      };
    }
    {
      name = "qs___qs_6.5.2.tgz";
      path = fetchurl {
        name = "qs___qs_6.5.2.tgz";
        url  = "https://registry.yarnpkg.com/qs/-/qs-6.5.2.tgz";
        sha1 = "cb3ae806e8740444584ef154ce8ee98d403f3e36";
      };
    }
    {
      name = "range_parser___range_parser_1.2.1.tgz";
      path = fetchurl {
        name = "range_parser___range_parser_1.2.1.tgz";
        url  = "https://registry.yarnpkg.com/range-parser/-/range-parser-1.2.1.tgz";
        sha1 = "3cf37023d199e1c24d1a55b84800c2f3e6468031";
      };
    }
    {
      name = "raw_body___raw_body_2.3.3.tgz";
      path = fetchurl {
        name = "raw_body___raw_body_2.3.3.tgz";
        url  = "https://registry.yarnpkg.com/raw-body/-/raw-body-2.3.3.tgz";
        sha1 = "1b324ece6b5706e153855bc1148c65bb7f6ea0c3";
      };
    }
    {
      name = "safe_buffer___safe_buffer_5.1.1.tgz";
      path = fetchurl {
        name = "safe_buffer___safe_buffer_5.1.1.tgz";
        url  = "https://registry.yarnpkg.com/safe-buffer/-/safe-buffer-5.1.1.tgz";
        sha1 = "893312af69b2123def71f57889001671eeb2c853";
      };
    }
    {
      name = "safe_buffer___safe_buffer_5.1.2.tgz";
      path = fetchurl {
        name = "safe_buffer___safe_buffer_5.1.2.tgz";
        url  = "https://registry.yarnpkg.com/safe-buffer/-/safe-buffer-5.1.2.tgz";
        sha1 = "991ec69d296e0313747d59bdfd2b745c35f8828d";
      };
    }
    {
      name = "safer_buffer___safer_buffer_2.1.2.tgz";
      path = fetchurl {
        name = "safer_buffer___safer_buffer_2.1.2.tgz";
        url  = "https://registry.yarnpkg.com/safer-buffer/-/safer-buffer-2.1.2.tgz";
        sha1 = "44fa161b0187b9549dd84bb91802f9bd8385cd6a";
      };
    }
    {
      name = "send___send_0.15.6.tgz";
      path = fetchurl {
        name = "send___send_0.15.6.tgz";
        url  = "https://registry.yarnpkg.com/send/-/send-0.15.6.tgz";
        sha1 = "20f23a9c925b762ab82705fe2f9db252ace47e34";
      };
    }
    {
      name = "serve_favicon___serve_favicon_2.4.5.tgz";
      path = fetchurl {
        name = "serve_favicon___serve_favicon_2.4.5.tgz";
        url  = "https://registry.yarnpkg.com/serve-favicon/-/serve-favicon-2.4.5.tgz";
        sha1 = "49d9a46863153a9240691c893d2b0e7d85d6d436";
      };
    }
    {
      name = "serve_static___serve_static_1.12.6.tgz";
      path = fetchurl {
        name = "serve_static___serve_static_1.12.6.tgz";
        url  = "https://registry.yarnpkg.com/serve-static/-/serve-static-1.12.6.tgz";
        sha1 = "b973773f63449934da54e5beba5e31d9f4211577";
      };
    }
    {
      name = "setprototypeof___setprototypeof_1.0.3.tgz";
      path = fetchurl {
        name = "setprototypeof___setprototypeof_1.0.3.tgz";
        url  = "https://registry.yarnpkg.com/setprototypeof/-/setprototypeof-1.0.3.tgz";
        sha1 = "66567e37043eeb4f04d91bd658c0cbefb55b8e04";
      };
    }
    {
      name = "setprototypeof___setprototypeof_1.1.0.tgz";
      path = fetchurl {
        name = "setprototypeof___setprototypeof_1.1.0.tgz";
        url  = "https://registry.yarnpkg.com/setprototypeof/-/setprototypeof-1.1.0.tgz";
        sha1 = "d0bd85536887b6fe7c0d818cb962d9d91c54e656";
      };
    }
    {
      name = "statuses___statuses_1.5.0.tgz";
      path = fetchurl {
        name = "statuses___statuses_1.5.0.tgz";
        url  = "https://registry.yarnpkg.com/statuses/-/statuses-1.5.0.tgz";
        sha1 = "161c7dac177659fd9811f43771fa99381478628c";
      };
    }
    {
      name = "statuses___statuses_1.3.1.tgz";
      path = fetchurl {
        name = "statuses___statuses_1.3.1.tgz";
        url  = "https://registry.yarnpkg.com/statuses/-/statuses-1.3.1.tgz";
        sha1 = "faf51b9eb74aaef3b3acf4ad5f61abf24cb7b93e";
      };
    }
    {
      name = "type_is___type_is_1.6.18.tgz";
      path = fetchurl {
        name = "type_is___type_is_1.6.18.tgz";
        url  = "https://registry.yarnpkg.com/type-is/-/type-is-1.6.18.tgz";
        sha1 = "4e552cd05df09467dcbc4ef739de89f2cf37c131";
      };
    }
    {
      name = "unpipe___unpipe_1.0.0.tgz";
      path = fetchurl {
        name = "unpipe___unpipe_1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/unpipe/-/unpipe-1.0.0.tgz";
        sha1 = "b2bf4ee8514aae6165b4817829d21b2ef49904ec";
      };
    }
    {
      name = "utils_merge___utils_merge_1.0.0.tgz";
      path = fetchurl {
        name = "utils_merge___utils_merge_1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/utils-merge/-/utils-merge-1.0.0.tgz";
        sha1 = "0294fb922bb9375153541c4f7096231f287c8af8";
      };
    }
    {
      name = "vary___vary_1.1.2.tgz";
      path = fetchurl {
        name = "vary___vary_1.1.2.tgz";
        url  = "https://registry.yarnpkg.com/vary/-/vary-1.1.2.tgz";
        sha1 = "2299f02c6ded30d4a5961b0b9f74524a18f634fc";
      };
    }
    {
      name = "ws___ws_4.1.0.tgz";
      path = fetchurl {
        name = "ws___ws_4.1.0.tgz";
        url  = "https://registry.yarnpkg.com/ws/-/ws-4.1.0.tgz";
        sha1 = "a979b5d7d4da68bf54efe0408967c324869a7289";
      };
    }
  ];
}
