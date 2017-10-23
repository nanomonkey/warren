# warren

![Warren](warren_screen_shot.png)

A maze game where you play as a mouse who travels inside a rabbit warren 
that has been beseiged by a hungry weasel. 

## Overview

This is a game that I'm working on to learn Clojurescript and Reagent. 

## Setup

To get an interactive development environment run:

    lein figwheel

and open your browser at [localhost:3449](http://localhost:3449/).
This will auto compile and send all changes to the browser without the
need to reload. After the compilation process is complete, you will
get a Browser Connected REPL. An easy way to try it is:

    (js/alert "Am I connected?")

and you should see an alert in the browser window.

To clean all compiled files:

    lein clean

To create a production build run:

    lein do clean, cljsbuild once min

And open your browser in `resources/public/index.html`. You will not
get live reloading, nor a REPL. 

## License

Too ill.
