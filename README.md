RaceMan
=======

RaceMan is an open system for publishing rowing results. It is based upon [Erlang](http://erlang.org), [Webmachine](https://github.com/basho/webmachine/wiki) with [ErlyDTL](https://github.com/erlydtl/erlydtl) and [Bootstrap 3](http://getbootstrap.com)

Requirements
------------

[Erlang](http://erlang.org) (R16) with OTP 17 and [rebar](https://github.com/rebar/rebar).
   
   * Mac with homebrew: 
      
        brew install erlang rebar

Installation
------------

1. Clone the repository:
    
        git clone git@github.com:openrow/raceman.git
       
2. Install depenencies:

        rebar get-deps 
        
3. Build all:

        rebar compile 
   
   or 
   
        make
        
4. Run:

        ./dev.sh
        
5. Open [http://localhost:8080/](http://localhost:8080/) in your browser.

Beenden der laufenden Anwendung in der REPL mit `halt().`



Development
-----------

RaceMan is reloading changed `.beam` files in development mode by using [mochiweb's reloader implementation](http://code.google.com/p/mochiweb/source/browse/trunk/src/reloader.erl). This works for `.erl` and `.dtl` files but not for the `dispatch.conf`. You just need to compile the changes you made by running `rebar compile skip_deps=true` or `make quick`.


Example Data
------------
To add some example data to your database use the cypher statement in [example.cypher](example.cypher).