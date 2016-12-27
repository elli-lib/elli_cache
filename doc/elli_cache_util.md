

# Module elli_cache_util #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Useful helper functions for elli_cache.

Copyright (c) 2016, elli-lib team

__Authors:__ Eric Bailey ([`eric@ericb.me`](mailto:eric@ericb.me)) (_web site:_ [`https://github.com/yurrriq`](https://github.com/yurrriq)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#comma_split-1">comma_split/1</a></td><td></td></tr><tr><td valign="top"><a href="#convert_date-1">convert_date/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_values-2">get_values/2</a></td><td></td></tr><tr><td valign="top"><a href="#ifdef_delete-3">ifdef_delete/3</a></td><td>If <code>List1</code> contains at least one entry associated with <code>Key1</code>,
delete all entries associated with <code>Key2</code>.</td></tr><tr><td valign="top"><a href="#store-3">store/3</a></td><td></td></tr><tr><td valign="top"><a href="#update_element-3">update_element/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="comma_split-1"></a>

### comma_split/1 ###

<pre><code>
comma_split(Subject::binary()) -&gt; [binary()]
</code></pre>
<br />

<a name="convert_date-1"></a>

### convert_date/1 ###

<pre><code>
convert_date(Bin::binary()) -&gt; non_neg_integer() | bad_date
</code></pre>
<br />

<a name="get_values-2"></a>

### get_values/2 ###

<pre><code>
get_values(Key::binary(), Headers::<a href="https://github.com/elli-lib/elli/blob/develop/doc/elli.md#type-headers">elli:headers()</a>) -&gt; [binary()]
</code></pre>
<br />

<a name="ifdef_delete-3"></a>

### ifdef_delete/3 ###

<pre><code>
ifdef_delete(Key1, Key2, List1) -&gt; List2
</code></pre>

<ul class="definitions"><li><code>Key1 = term()</code></li><li><code>Key2 = term()</code></li><li><code>List1 = [term()]</code></li><li><code>List2 = [term()]</code></li></ul>

If `List1` contains at least one entry associated with `Key1`,
delete all entries associated with `Key2`. Otherwise, return `List1`.

<a name="store-3"></a>

### store/3 ###

<pre><code>
store(Key::binary(), Value::binary(), List::<a href="https://github.com/elli-lib/elli/blob/develop/doc/elli.md#type-headers">elli:headers()</a>) -&gt; <a href="https://github.com/elli-lib/elli/blob/develop/doc/elli.md#type-headers">elli:headers()</a>
</code></pre>
<br />

<a name="update_element-3"></a>

### update_element/3 ###

<pre><code>
update_element(Index, Tuple1, Fun) -&gt; Tuple2
</code></pre>

<ul class="definitions"><li><code>Index = pos_integer()</code></li><li><code>Tuple1 = tuple()</code></li><li><code>Fun = fun((term()) -&gt; term())</code></li><li><code>Tuple2 = tuple()</code></li></ul>

