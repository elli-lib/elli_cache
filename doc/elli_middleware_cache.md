

# Module elli_middleware_cache #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Generic caching middleware.

Copyright (c) 2016, elli-lib team

__Authors:__ Eric Bailey ([`eric@ericb.me`](mailto:eric@ericb.me)) (_web site:_ [`https://github.com/yurrriq`](https://github.com/yurrriq)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#postprocess-3">postprocess/3</a></td><td></td></tr><tr><td valign="top"><a href="#preprocess-2">preprocess/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="postprocess-3"></a>

### postprocess/3 ###

<pre><code>
postprocess(Req, Res1, Config) -&gt; Res2
</code></pre>

<ul class="definitions"><li><code>Req = <a href="http://raw.github.com/elli-lib/elli/develop/doc/elli.md#type-req">elli:req()</a></code></li><li><code>Res1 = <a href="http://raw.github.com/elli-lib/elli/develop/doc/elli_handler.md#type-result">elli_handler:result()</a></code></li><li><code>Config = <a href="http://raw.github.com/elli-lib/elli/develop/doc/elli_handler.md#type-callback_args">elli_handler:callback_args()</a></code></li><li><code>Res2 = <a href="http://raw.github.com/elli-lib/elli/develop/doc/elli_handler.md#type-result">elli_handler:result()</a></code></li></ul>

<a name="preprocess-2"></a>

### preprocess/2 ###

<pre><code>
preprocess(Req1, Config) -&gt; Req2
</code></pre>

<ul class="definitions"><li><code>Req1 = <a href="http://raw.github.com/elli-lib/elli/develop/doc/elli.md#type-req">elli:req()</a></code></li><li><code>Config = <a href="http://raw.github.com/elli-lib/elli/develop/doc/elli_handler.md#type-callback_args">elli_handler:callback_args()</a></code></li><li><code>Req2 = <a href="http://raw.github.com/elli-lib/elli/develop/doc/elli.md#type-req">elli:req()</a></code></li></ul>

