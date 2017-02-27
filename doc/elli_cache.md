

# Module elli_cache #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

elli_cache behaviour.

Copyright (c) 2016, elli-lib team

__This module defines the `elli_cache` behaviour.__<br /> Required callback functions: `get_modified/2`, `get_size/2`.

__Authors:__ Eric Bailey ([`eric@ericb.me`](mailto:eric@ericb.me)) (_web site:_ [`https://github.com/yurrriq`](https://github.com/yurrriq)).

<a name="types"></a>

## Data Types ##




### <a name="type-config">config()</a> ###


<pre><code>
config() = [{mod, Mod::module()}]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_modified-2">get_modified/2</a></td><td>Maybe get the last modified date for a request.</td></tr><tr><td valign="top"><a href="#get_size-2">get_size/2</a></td><td>Maybe get the size of the response to a request.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_modified-2"></a>

### get_modified/2 ###

<pre><code>
get_modified(Req::<a href="https://github.com/elli-lib/elli/blob/develop/doc/elli.md#type-req">elli:req()</a>, Args::<a href="#type-config">config()</a>) -&gt; <a href="maybe_m.md#type-maybe">maybe_m:maybe</a>(<a href="calendar.md#type-datetime">calendar:datetime()</a>)
</code></pre>
<br />

Maybe get the last modified date for a request.
If `{mod, Mod}` is present in `Args` and `Mod:get_modified/2` is exported,
return `Mod:get_modified(Req, Args)`. Otherwise, return `nothing`.

<a name="get_size-2"></a>

### get_size/2 ###

<pre><code>
get_size(Req::<a href="https://github.com/elli-lib/elli/blob/develop/doc/elli.md#type-req">elli:req()</a>, Args::<a href="#type-config">config()</a>) -&gt; <a href="maybe_m.md#type-maybe">maybe_m:maybe</a>(non_neg_integer())
</code></pre>
<br />

Maybe get the size of the response to a request.
If `{mod, Mod}` is present in `Args` and `Mod:get_size/2` is exported,
return `Mod:get_size(Req, Args)`. Otherwise, return `nothing`.

