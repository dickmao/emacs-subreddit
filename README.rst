|build-status|

Install
=======
::

   git clone https://github.com/dickmao/emacs-subreddit.git
   make -C emacs-subreddit install

Usage
=====
::

   M-x emacs-subreddit
   
   From either summary or article buffers:
   RET                  read comment
   n                    read next comment
   p                    read previous comment
   r                    reply
   g                    pull new posts
   c                    mark all read
   x                    un/show read
   s                    isearch
   q                    dismiss
   SPC                  scroll through
   T T                  un/display threaded
   T H                  un/collapse threads
   

.. |build-status|
   image:: https://github.com/dickmao/emacs-subreddit/workflows/CI/badge.svg
   :target: https://github.com/dickmao/emacs-subreddit/actions
   :alt: Build Status
