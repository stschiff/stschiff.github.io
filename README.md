This is the repository with code and content of my personal webpage hosted via github pages on [https://www.stephanschiffels.de](https://www.stephanschiffels.de).

To build the site you will need the [stack build tool](https://docs.haskellstack.org/en/stable/) for Haskell.

You can then clone the repository, cd into it and run `stack run site -- build`. Together:

    git clone https://github.com/stschiff/stschiff.github.io.git
    cd stschiff.github.io.git
    stack run site -- build

The site is then built into a directory named `docs`, which in turn is read by github pages to host the site.

You can run a local server to view the site using 

    stack run site -- watch

Feel free to use the code here and adapt for your own website.