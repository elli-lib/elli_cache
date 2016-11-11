

# Module elli_cache #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

elli_cache behaviour.

__This module defines the `elli_cache` behaviour.__<br /> Required callback functions: `modified/2`.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#modified-2">modified/2</a></td><td>Iff given a <code>Mod`, return `Mod:modified(Req, Args)</code> iff exported.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="modified-2"></a>

### modified/2 ###

<pre><code>
modified(Req, Args) -&gt; false | Mtime
</code></pre>

<ul class="definitions"><li><code>Req = <a href="elli.md#type-req">elli:req()</a></code></li><li><code>Args = [{mod, Mod::module()}]</code></li><li><code>Mtime = <a href="calendar.md#type-datetime">calendar:datetime()</a></code></li></ul>

Iff given a `Mod`, return `Mod:modified(Req, Args)` iff exported.

