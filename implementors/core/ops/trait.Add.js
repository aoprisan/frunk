(function() {var implementors = {};
implementors["frunk"] = ["impl&lt;T, E, T2&gt; <a class="trait" href="https://doc.rust-lang.org/nightly/core/ops/trait.Add.html" title="trait core::ops::Add">Add</a>&lt;<a class="enum" href="https://doc.rust-lang.org/nightly/core/result/enum.Result.html" title="enum core::result::Result">Result</a>&lt;T2, E&gt;&gt; for <a class="enum" href="frunk/validated/enum.Validated.html" title="enum frunk::validated::Validated">Validated</a>&lt;T, E&gt; <span class="where fmt-newline">where T: <a class="trait" href="frunk_core/hlist/trait.HList.html" title="trait frunk_core::hlist::HList">HList</a> + <a class="trait" href="https://doc.rust-lang.org/nightly/core/ops/trait.Add.html" title="trait core::ops::Add">Add</a>&lt;<a class="struct" href="frunk_core/hlist/struct.HCons.html" title="struct frunk_core::hlist::HCons">HCons</a>&lt;T2, <a class="struct" href="frunk_core/hlist/struct.HNil.html" title="struct frunk_core::hlist::HNil">HNil</a>&gt;&gt;,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;T::<a class="trait" href="https://doc.rust-lang.org/nightly/core/ops/trait.Add.html" title="trait core::ops::Add">Output</a>: <a class="trait" href="frunk_core/hlist/trait.HList.html" title="trait frunk_core::hlist::HList">HList</a></span>","impl&lt;T, E, T2&gt; <a class="trait" href="https://doc.rust-lang.org/nightly/core/ops/trait.Add.html" title="trait core::ops::Add">Add</a>&lt;<a class="enum" href="frunk/validated/enum.Validated.html" title="enum frunk::validated::Validated">Validated</a>&lt;T2, E&gt;&gt; for <a class="enum" href="frunk/validated/enum.Validated.html" title="enum frunk::validated::Validated">Validated</a>&lt;T, E&gt; <span class="where fmt-newline">where T: <a class="trait" href="frunk_core/hlist/trait.HList.html" title="trait frunk_core::hlist::HList">HList</a> + <a class="trait" href="https://doc.rust-lang.org/nightly/core/ops/trait.Add.html" title="trait core::ops::Add">Add</a>&lt;T2&gt;,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;T2: <a class="trait" href="frunk_core/hlist/trait.HList.html" title="trait frunk_core::hlist::HList">HList</a>,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;T::<a class="trait" href="https://doc.rust-lang.org/nightly/core/ops/trait.Add.html" title="trait core::ops::Add">Output</a>: <a class="trait" href="frunk_core/hlist/trait.HList.html" title="trait frunk_core::hlist::HList">HList</a></span>",];
implementors["frunk_core"] = ["impl&lt;RHS&gt; <a class="trait" href="https://doc.rust-lang.org/nightly/core/ops/trait.Add.html" title="trait core::ops::Add">Add</a>&lt;RHS&gt; for <a class="struct" href="frunk_core/hlist/struct.HNil.html" title="struct frunk_core::hlist::HNil">HNil</a> <span class="where fmt-newline">where RHS: <a class="trait" href="frunk_core/hlist/trait.HList.html" title="trait frunk_core::hlist::HList">HList</a></span>","impl&lt;H, T, RHS&gt; <a class="trait" href="https://doc.rust-lang.org/nightly/core/ops/trait.Add.html" title="trait core::ops::Add">Add</a>&lt;RHS&gt; for <a class="struct" href="frunk_core/hlist/struct.HCons.html" title="struct frunk_core::hlist::HCons">HCons</a>&lt;H, T&gt; <span class="where fmt-newline">where T: <a class="trait" href="https://doc.rust-lang.org/nightly/core/ops/trait.Add.html" title="trait core::ops::Add">Add</a>&lt;RHS&gt;, RHS: <a class="trait" href="frunk_core/hlist/trait.HList.html" title="trait frunk_core::hlist::HList">HList</a></span>",];
implementors["syn"] = ["impl <a class="trait" href="https://doc.rust-lang.org/nightly/core/ops/trait.Add.html" title="trait core::ops::Add">Add</a>&lt;<a class="struct" href="https://doc.rust-lang.org/nightly/std/time/duration/struct.Duration.html" title="struct std::time::duration::Duration">Duration</a>&gt; for <a class="struct" href="https://doc.rust-lang.org/nightly/std/time/duration/struct.Duration.html" title="struct std::time::duration::Duration">Duration</a>","impl <a class="trait" href="https://doc.rust-lang.org/nightly/core/ops/trait.Add.html" title="trait core::ops::Add">Add</a>&lt;<a class="struct" href="https://doc.rust-lang.org/nightly/std/time/duration/struct.Duration.html" title="struct std::time::duration::Duration">Duration</a>&gt; for <a class="struct" href="https://doc.rust-lang.org/nightly/std/time/struct.Instant.html" title="struct std::time::Instant">Instant</a>","impl <a class="trait" href="https://doc.rust-lang.org/nightly/core/ops/trait.Add.html" title="trait core::ops::Add">Add</a>&lt;<a class="struct" href="https://doc.rust-lang.org/nightly/std/time/duration/struct.Duration.html" title="struct std::time::duration::Duration">Duration</a>&gt; for <a class="struct" href="https://doc.rust-lang.org/nightly/std/time/struct.SystemTime.html" title="struct std::time::SystemTime">SystemTime</a>","impl&lt;'a&gt; <a class="trait" href="https://doc.rust-lang.org/nightly/core/ops/trait.Add.html" title="trait core::ops::Add">Add</a>&lt;&amp;'a <a class="primitive" href="https://doc.rust-lang.org/nightly/std/primitive.str.html">str</a>&gt; for <a class="enum" href="https://doc.rust-lang.org/nightly/collections/borrow/enum.Cow.html" title="enum collections::borrow::Cow">Cow</a>&lt;'a, <a class="primitive" href="https://doc.rust-lang.org/nightly/std/primitive.str.html">str</a>&gt;","impl&lt;'a&gt; <a class="trait" href="https://doc.rust-lang.org/nightly/core/ops/trait.Add.html" title="trait core::ops::Add">Add</a>&lt;<a class="enum" href="https://doc.rust-lang.org/nightly/collections/borrow/enum.Cow.html" title="enum collections::borrow::Cow">Cow</a>&lt;'a, <a class="primitive" href="https://doc.rust-lang.org/nightly/std/primitive.str.html">str</a>&gt;&gt; for <a class="enum" href="https://doc.rust-lang.org/nightly/collections/borrow/enum.Cow.html" title="enum collections::borrow::Cow">Cow</a>&lt;'a, <a class="primitive" href="https://doc.rust-lang.org/nightly/std/primitive.str.html">str</a>&gt;","impl&lt;'a&gt; <a class="trait" href="https://doc.rust-lang.org/nightly/core/ops/trait.Add.html" title="trait core::ops::Add">Add</a>&lt;&amp;'a <a class="primitive" href="https://doc.rust-lang.org/nightly/std/primitive.str.html">str</a>&gt; for <a class="struct" href="https://doc.rust-lang.org/nightly/collections/string/struct.String.html" title="struct collections::string::String">String</a>",];

            if (window.register_implementors) {
                window.register_implementors(implementors);
            } else {
                window.pending_implementors = implementors;
            }
        
})()
