# dotfiles
This is my dotfiles repo. I hope you find something useful here!

## Literate programming
Literate programming is a concept in which you write prose and embed in it source code. It has the benefit of being written for humans in the first place. I haven't spent enough time with it to know if this is true, but I suspect it also makes updating the documentation with a given change a little easier as well.

This is another attempt of mine for creating a readable and maintainable dotfiles configuration. I'm hopeful that using a literate programming style will help tame the (dis)organization.

## Instructions
Clone the repository in a folder of your preference (here I will clone as `.dotfiles`), either from [GitHub](https://github.com),
```sh
$ git clone https://github.com/padawanphysicist/dotfiles.git .dotfiles
```

or from [GitLab](https://gitlab.com):
```sh
$ git clone https://gitlab.com/padawanphysicist/dotfiles.git .dotfiles
```

then enter the directory,
```sh
$ cd .dotfiles
```

and link the important files to your `$HOME` directory:
```sh
$ ./bootstrap # this will symlink the important files
```