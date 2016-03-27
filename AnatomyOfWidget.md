# Definitions #

**Widget**: reusable software component capable of one or more of the following:
  * displaying and updating of visual interface elements by means of the underlying graphics toolkit;
  * receiving messages from other widgets;
  * sending messages to other widgets;
  * maintaining the local state remembered between message transmissions.

**Context Record**: a data structure containing information about widget's parent widget (in terms of the underlying graphics toolkit), receiver of widget's messages (event link), and layout information (such as sizer flags). Context is implicitly passed to all widgets' creation phase code.

**Parent**: a widget nesting one or more subordinates. A parent controls layout of and routing of messages to and from its subordinates.

**Subordinate**: a widget that has a parent widget is its parent's subordinate. Subordinates may create visual interface elements that are child elements to their parent elements in terms of the underlying graphics toolkit. A widget without a parent widget is a toplevel widget.

# Types of Composition #

Composition of widgets defines their visual placement inside the application window and routing of messages between widgets, that is, static and dynamic behaviors of a GUI application.

## Nesting ##

Nesting is relation between parent and subordinate widgets (a parent nests its subordinates). Subordinate widgets are in most cases visible inside the display area of their parent. Subordinates however do not always create their own displays. Some may just affect their parent's visual style or other aspects of parents behavior (e. g. connection to events), as they usually have access to the underlying graphics toolkit representation of its parent. Some would just pass messages through, conditionally or uncondiionally, to/from other subordinates of the same parent (and possibly to/from subordinates of  widgets connected with their parent).

Typical behavior of a parent widget is:
  * During the creation phase, also run the creation phase code of its subordinates (usually via calling [wxi:comp/2](http://wxi.googlecode.com/hg/doc/wxi.html#comp-2) or [wxi:rcomp/2](http://wxi.googlecode.com/hg/doc/wxi.html#rcomp-2), passing the reference to itself as a parent to the subordinates via the context record;
  * During the message passing phase, pass all messages it receives to its subordinates, receive all messages from subordinates, and pass them further.

## Parallel Composition ##

A message sent to a group of widgets composed in parallel, is passed to each widget in the group, and messages these widgets send are multiplexed.

## Sequential Composition ##

A message sent to a sequence of widgets is received by the first widget in the sequence, and is passed along from widget to widget, resulting in what the last widget in the sequence sends.

# Context #

The context record that every widget has access to during its creation phase, is defined as follows:

```
-record(context, {parent, evtlink, szflags}).
```

where `parent` refers to the parent widget (to be used when creating a visual element, or setting/changing parent's visual style), `evtlink` refers to the receiver of this widget's messages, and `szflags` define placement of this widget relatively to its parent in terms of parent's sizer of similar layout mechanism.

# Program Structure of Widget #

A typical widget's lifecycle consists of the creation phase and the message handling phase. A typical widget's program structure is shown below:

```
widget(Args) -> fun (#context {}) -> 
%% code of the creation phase,
    fun (R, _) ->
%% code of the message handling phase
    end
end.
```

As seen in the example above, a widget is a function of one or more arguments which in turn returns another function taking the Context as its only argument. The latter function may either return one more function (if the widget handles messages), or a PID, or just a meaningless atom (e. g. `ok`). In the latter case the widget does not handle incoming messages.

The Context record contains information about this widget's parent, and also defines the destination of widget's messages (event link) if the widget sends any messages.

The creation phase code runs only once per widget's lifecycle, before any message may be handled by the widget. The creation phase code is expected to create a visual interface element as a child element of the given parent widget. This is usually achieved by function calls to the underlying graphics toolkit. In some cases the creation phase code will invoke some operations on the widget's parent. In some cases the creation code will do nothing (widgets that do not display any visual information, intended for messages processing and/or filtering).

The creation phase code may return a function that handles widget's incoming messages. Such function should take two arguments, as described in [wxEvtHandler:connect/3](http://erlang.org/doc/man/wxEvtHandler.html#connect-3) for the `callback` option. Only the first argument, `R` is expected to carry useful information, that is, the message itself (which can be any valid Erlang expression). The second argument should always be ignored.

This message handling function may either pass an incoming (possibly altered by application of some function) message further, as prescribed by the `evtlink` member of the Context record, or block the incoming message from passing.

The creation phase code may alternatively create a process and return its PID. See the "Process Based Widgets" paragraph further in the text.

Below are some widget examples:

## The `map` Widget ##

This widget applies some function to each message received, and passes the result further:

```
map(F) -> fun (#context {evtlink = E}) ->
    fun (R, _) ->
        Z = F(R),
        passEvent(Z, E)
    end
end.
```

The [wxi:passEvent/2](http://wxi.googlecode.com/hg/doc/wxi.html#passEvent-2) function is called in order to pass the message to the prescribed recipient. It is recommended to make this a tail call.

## The `never` Widget ##

This widget blocks any message it receives from propagating further:

```
never() -> fun(#context {}) ->
    fun (_, _) -> ok end
end.
```

The two examples above illustrate widgets whose creation phase is empty. Next comes a widget that generates messages as result of user actions:

## The `button` Widget ##

```
button(T, I) -> fun (#context {parent = X, szflags = F, evtlink = E}) ->
    B = wxButton:new(X, I, [{label, T}]),
    addSelf(X, B, F),
    linkEvent(B, E, [command_button_clicked]),
    ok
end.
```

This widget has creation phase code that creates a button as a child of the parent widget as defined in the Context record, then calls [wxi:addSelf/3](http://wxi.googlecode.com/hg/doc/wxi.html#addSelf-3) to display itself properly within the parent's sizer. The [wxi:linkEvent/3](http://wxi.googlecode.com/hg/doc/wxi.html#linkEvent-3) function is called to connect the event source to the destination prescribed by the Context record. This widget will not handle any input messages, so its creation phase code just returns `ok` rather than a function or a PID.

## The `textLabel` Widget ##

```
textLabel(Fmt, T) -> fun (#context {parent = X, szflags = F}) ->
    Tx = wxStaticText:new(X, -1, T),
    addSelf(X, Tx, F),
    fun(R, _) ->
        Z = io_lib:format(Fmt, [R]),
        wxStaticText:setLabel(Tx, Z),
        wxWindow:fit(X),
        ok
    end
end.
```

This widget implements a static text label whose contents may be changed by an incoming message. The creation phase code creates a static text label and displays it within the parent. The message handling function formats each incoming message using the format specified as the first widget argument (`Fmt`), and updates the static text with the result of formatting.

## The `panel` Widget ##

```
panel(Dir, Sub) -> fun(C = #context{parent = X, szflags = F}) ->
    P = wxPanel:new(X),
    Sz = wxBoxSizer:new(abs(Dir)),
    wxWindow:setSizer(P, Sz),
    Ud = if
      Dir > 0 -> comp(Sub, C#context{parent = P});
      true -> rcomp(Sub, C#context{parent = P})
    end,
    addSelf(X, P, F),
    Ud
end.
```

This is an example of nesting widget. The `Dir` argument determines whether the panel's subordinates are laid out horizontally or vertically. The `Sub` argument refers to the composition of subordinate widgets of the panel. During the creation phase, a panel is created within the parent widget as provided by the Context record. Next, [wxi:comp/2](http://wxi.googlecode.com/hg/doc/wxi.html#comp-2) or [wxi:rcomp/2](http://wxi.googlecode.com/hg/doc/wxi.html#rcomp-2), depending on the sign of the `Dir` argument (see the "Reverse the Placement Order" section) is called on the subordinate widgets with the Context record holding the reference to this panel in the `parent` field. Whatever is returned by the subordinates, is used as the message handling function: the panel simply forwards all its incoming messages to its subordinates, and all events from subordinate widgets will go wherever is specified by the Context record: the panel does not interfere with this.

## Process-Based Widgets ##

In some cases it is necessary to remember some internal value of a widget between message transmissions (that is, local mutable state), or to implement a timing function without blocking other widgets. This may be achieved by process-based widgets. Unlike typical widgets based on event handling function, code of widget creation phase spawns a new process and returns its PID. Functions like [wxi:linkEvent/3](http://wxi.googlecode.com/hg/doc/wxi.html#linkEvent-3) and [wxi:passEvent/2](http://wxi.googlecode.com/hg/doc/wxi.html#passEvent-2) recognize such widgets properly (a widget connected to a process-based widget will see a PID as `evtlink` in the Context record rather than a function reference). Development of process-based widgets has to bo done carefully to
  * ensure that when the whole application shuts down, widget's process also terminates;
  * wxWidgets environment is passed to the widget's process upon its creation, e. g.:

```
mapState(F, S) -> fun (#context {evtlink = E}) ->
    W = wx:get_env(),
    % maploop is the widget process body
    spawn_link(fun() -> wx:set_env(W), maploop(F, S, E) end) 
end.
```

# Program Notation for Composition #

Nesting composition is achieved via passing the subordinate contents to their parent widget as an argument, and calling `wxi:comp/2` or `wxi:rcomp/2` from within the parent. Program notation for parallel composition is a list. Program notation for sequential composition is a tuple. This choice is dictated by the fact that parallel compositions are often formed as result of some transformation of another list or sequence, just like a set of calculator number buttons can be obtained from a sequence of numbers. Notation for empty subordinate content is an empty list.

# Example: Two Buttons and a Counter #

Let us create a simple program that illustrates the principles described above. This will be a "canonical" example consisting of two buttons labeled with "+" and "-" signs, and a number display (counter) which can be incremented and decremented by clicking on these buttons. This example is based on this [wxWidgets tutorial](http://zetcode.com/tutorials/wxwidgetstutorial/firstprograms/), see the "Widgets communicate" section.

## Traditional Use of wxErlang ##

First, let's program the desired functionality using wxErlang in "traditional" way. The code can be seen [here](http://code.google.com/p/wxi/source/browse/examples/comwid.erl). When running, it produces a window like shown below:

![http://wxi.googlecode.com/hg/examples/comwid.png](http://wxi.googlecode.com/hg/examples/comwid.png)

## Building Composable GUI ##

Let's now start implementing the same functionality using widgets composition.

### Empty Toplevel Window ###

The example [program](http://wxi.googlecode.com/hg/examples/ex1.erl) below creates an empty toplevel window:

```
%% The wxi library example 1: empty toplevel frame.

-module(ex1).

-include_lib("wxi.hrl").

-export([start/0]).

start() ->
    wxi:topFrame("WXI Example 1", 300, 100, ?wxHORIZONTAL ,[]).
```

![http://wxi.googlecode.com/hg/examples/ex1.png](http://wxi.googlecode.com/hg/examples/ex1.png)

### A Button and a Text Label ###

Let's add a button and a text label display to the frame. Layout may be ugly, but we'll take care of it later. See the [program](http://wxi.googlecode.com/hg/examples/ex2.erl) below:

```
%% The wxi library example 2: button and text label.

-module(ex2).

-include_lib("wxi.hrl").

-define(ID_PLUS, 100).

-export([start/0]).

start() ->
    wxi:topFrame("WXI Example 2", 300, 100, ?wxHORIZONTAL, 
        {wxi:button("+", ?ID_PLUS), wxi:textLabel("~p", "0")}).

```

![http://wxi.googlecode.com/hg/examples/ex2.png](http://wxi.googlecode.com/hg/examples/ex2.png)

The screenshot shows the window after the button was clicked (the window was resized afterwards, so the whole text label can be seen).

Note that the button and the text label are members of a tuple which means they are sequentially composed. The text label is given the `~p` format for now, that is, it displays incoming messages as Erlang terms. Of interest is the `100` number which is the identifier of the button labeled with "+". As we add more buttons, they must be given distinct numeric identifiers, so events coming from each can be distinguished.

### Adding Buttons, Mapping Messages ###

Let's add the second button and extract useful information from messages the button send. See the [program](http://wxi.googlecode.com/hg/examples/ex3.erl) below:

```
%% The wxi library example 3: two buttons and message mapper.

-module(ex3).

-include_lib("wxi.hrl").

-define(ID_PLUS, 100).
-define(ID_MINUS, 200).

-export([start/0]).

start() ->
    wxi:topFrame("WXI Example 3", 300, 100, ?wxHORIZONTAL, 
        {[wxi:button("+", ?ID_PLUS), wxi:button("-", ?ID_MINUS)], 
         wxi:map(fun (#wx {id = I}) -> I end),
         wxi:textLabel("~p", "0")}).

```

![http://wxi.googlecode.com/hg/examples/ex3.png](http://wxi.googlecode.com/hg/examples/ex3.png)

The additional button (labeled "-") is parallel-composed with the button labeled "+". This means that clicking on either button results in sending a message to the same event link. Now it happens to be the `map` widget discussed above which extracts the `id` field from the `#wx` record (event message). The screenshot shows that after the "-" button was pressed, the text label displays `200` that is the numeric identifier of the "-" button.

### Adding a Stateful Counter ###

The [program](http://wxi.googlecode.com/hg/examples/ex4.erl) below completes the task logic-wise by adding a stateful counter that remembers its value between message transmissions:

```
%% The wxi library example 4: stateful counter.

-module(ex4).

-include_lib("wxi.hrl").

-define(ID_PLUS, 100).
-define(ID_MINUS, 200).

-export([start/0]).

incd(?ID_PLUS, State) -> State + 1;
incd(?ID_MINUS, State) -> State - 1;
incd(_, State) -> State.

start() ->
    wxi:topFrame("WXI Example 4", 300, 100, ?wxHORIZONTAL, 
        {[wxi:button("+", ?ID_PLUS), wxi:button("-", ?ID_MINUS)], 
         wxi:map(fun (#wx {id = I}) -> I end),
         wxi:mapState(fun (A, B) -> incd(A, B) end, 0),
         wxi:textLabel("~B", "0")}).
```

![http://wxi.googlecode.com/hg/examples/ex4.png](http://wxi.googlecode.com/hg/examples/ex4.png)

The screenshot shows that after several clicks on the "-" button, counter's value was decremented, and became negative. This is achieved by inserting the process-based [wxi:mapState/2](http://wxi.googlecode.com/hg/doc/wxi.html#mapState-2) widget using the `incd` function to modify its internal state upon every message arrival, and 0 as initial state (any Erlang data structure may serve as state, not just a number). The `mapState` widget sends the modified state each time it receives a message. The number display's text label now has its format changed to "~B" to display numeric values only.

### Fixing the Layout ###

The [program](http://wxi.googlecode.com/hg/examples/ex5.erl) below functions identically to the previous one, but the layout has been changed:

```
%% The wxi library example 5: fixing layout.

-module(ex5).

-include_lib("wxi.hrl").

-define(ID_PLUS, 100).
-define(ID_MINUS, 200).

-export([start/0]).

incd(?ID_PLUS, State) -> State + 1;
incd(?ID_MINUS, State) -> State - 1;
incd(_, State) -> State.

start() ->
    wxi:topFrame("WXI Example 5", 300, 100, ?wxHORIZONTAL, 
        wxi:modSizerFlags ([{border, 5}, {flag, ?wxALL bor ?wxALIGN_CENTER_VERTICAL}], 
            {wxi:panel(?wxVERTICAL, [wxi:button("+", ?ID_PLUS), wxi:button("-", ?ID_MINUS)]), 
             wxi:map(fun (#wx {id = I}) -> I end),
             wxi:mapState(fun (A, B) -> incd(A, B) end, 0),
             wxi:textLabel("~B", "0")})).
```

![http://wxi.googlecode.com/hg/examples/ex5.png](http://wxi.googlecode.com/hg/examples/ex5.png)

The [wxi:modSizerFlags/2](http://wxi.googlecode.com/hg/doc/wxi.html#modSizerFlags-2) function modifies the Context record so that its subordinates receive modified sizer flags while the parent remains the same. So, each widget as it is inserted into its parent gets a 5 pixel border and will be centered vertically. Another addition: the buttons are now subordinates of an additional [panel](http://wxi.googlecode.com/hg/doc/wxi.html#panel-2) with vertical sizer, so they are now one above another.

### More Fun: Filtering Messages ###

The [program](http://wxi.googlecode.com/hg/examples/ex6.erl) below has an additional formatted text display (combined in parallel with one that already exists within a panel with vertical sizer). Since they are parallel-composed, they both receive all messages that their parent (panel) receives. So clicking on either button causes both displays to update. This example illustrates two filtering widgets: [wxi:always/0](http://wxi.googlecode.com/hg/doc/wxi.html#always-0) and [wxi:maybe/1](http://wxi.googlecode.com/hg/doc/wxi.html#maybe-1).

```
%% The wxi library example 6: filtering messages.

-module(ex6).

-include_lib("wxi.hrl").

-define(ID_PLUS, 100).
-define(ID_MINUS, 200).

-export([start/0]).

incd(?ID_PLUS, State) -> State + 1;
incd(?ID_MINUS, State) -> State - 1;
incd(_, State) -> State.

p(E) -> if
    E >= 0 -> {just, E};
    true -> nothing
end.

start() ->
    wxi:topFrame("WXI Example 6", 300, 100, ?wxHORIZONTAL, 
        wxi:modSizerFlags ([{border, 5}, {flag, ?wxALL bor ?wxALIGN_CENTER_VERTICAL}], 
            {wxi:panel(?wxVERTICAL, [wxi:button("+", ?ID_PLUS), wxi:button("-", ?ID_MINUS)]), 
             wxi:map(fun (#wx {id = I}) -> I end),
             wxi:mapState(fun (A, B) -> incd(A, B) end, 0),
             wxi:panel(?wxVERTICAL, [{wxi:always(), wxi:textLabel("~B", "0")}, 
                                     {wxi:maybe(fun(E) -> p(E) end), wxi:textLabel("~B", "0")}])})).
```


![http://wxi.googlecode.com/hg/examples/ex6.png](http://wxi.googlecode.com/hg/examples/ex6.png)


The former passes all messages it receives unconditionally. The latter passes a message if and only if the function given as its argument applied to the incoming message (`p` in our case) returns a tuple with the first member being atom `just`, and the second member being the term to pass (may be the message itself unchanged, depending on the function). So, in our example `p` returns the `just`-tagged tuple for positive numbers and 0, and `nothing` for negative. Thus, the lower display will not update with negative values which is demonstrated by the screenshot: the upper counter became negative after seceral clicks on the "-" button while the lower remains "0".

## Reversing the Placement Order ##

Normally, subordinates are placed within its parent widget left to right (horizontal) and top to bottom (vertical) direction. A widget which has subordinates has to call either [wxi:comp/2](http://wxi.googlecode.com/hg/doc/wxi.html#comp-2) or [wxi:rcomp/2](http://wxi.googlecode.com/hg/doc/wxi.html#rcomp-2) in order to render its subordinates visible. The `wxi:comp/2` function results in "normal" placement, and `wxi:rcomp/2` in reverse.

The recommended way to implement this in widgets with subordinates is to allow negation of the direction flag, as in the `wxi:panel/2` widget: `?wxVERTICAL` for top to bottom order, and `-?wxVERTICAL` for bottom to top.

It is left as an exercise to the reader to try changing sign of the direction flag on the panel containing buttons in the example program (`ex5` or `ex6`) and see what happens.

Note though that order change does not normally propagate below the level of immediate subordinates of a panel or a similar widget. If [wxi:modSizerFlags/2](http://wxi.googlecode.com/hg/doc/wxi.html#modSizerFlags-2) is applied to the panel subordinates, placement order will be always "normal" as `wxi:modSizerFlags/2` has no idea about such order change. So, the correct way to achieve the reverse placement order is to set sizer flags first, and then combine the widgets whose order needs to be reversed in an additional panel with desired direction and placement order.

# More Examples #

The [wxi calculator](http://wxi.googlecode.com/hg/examples/wxicalc.erl) program contains more examples of widgets creation and composition. Read the code and the comments. The calculator itself looks like shown below:

![http://wxi.googlecode.com/hg/examples/wxicalc.png](http://wxi.googlecode.com/hg/examples/wxicalc.png)