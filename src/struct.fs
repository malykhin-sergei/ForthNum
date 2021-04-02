\ Structures

: structure: create ( name -- ) here 0 0 , does> @ ;
: +field:    create ( name -- ) over , +   does> @ + ;
: (cell)   aligned 1 cells  +field: ;
: (float) faligned 1 floats +field: ;
: structure-end ( addr size -- ) swap ! ;
