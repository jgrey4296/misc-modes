# A Collection of misc emacs modes

## Timeline Notes
Timeline mode is for creating files describing events and periods in history.
To be accessed using a helm or ivy.

* Format
#+NAME: Timeline Format
#+begin_src timeline :results value
# Timeline Format:
# Ext: .timeline

# For file global tags:
:tags ....

# Form One: Event
year       event country person* :desc .... :tags ... :wiki ....

# Form Two: Period:
year->year event country person_surname* :tags ... :wiki .... :desc ....
#+end_src
