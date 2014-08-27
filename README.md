# dotemacs-rails

Packages & configurations for Ruby on Rails development on Emacs.

Tested on GNU Emacs 23.3 & 24.3, Ruby 1.8.7/1.9.3/2.0.0/2.1.2, on Ubuntu Linux 11.10/12.04, RHEL
5.x.


## Package integrated

| Package             | Description                                                                      | Homepage                                                        | melpa | external      |
| ac-inf-ruby         | Enable auto-complete in inf-ruby sessions                                        | https://github.com/purcell/ac-inf-ruby                          | Y     | auto-complete |
| anything-rails      | Adds useful anything sources for use with Ruby on Rails development              | https://github.com/wolfmanjm/anything-on-rails                  |       | anything      |
| bundler             | Interact with Bundler from Emacs                                                 | https://github.com/tobiassvn/bundler.el                         | Y     |               |
| enh-ruby-mode       | Major mode for editing Ruby files                                                | https://github.com/zenspider/enhanced-ruby-mode                 | Y     |               |
| flymake-ruby        | A flymake handler for ruby-mode files                                            | https://github.com/purcell/flymake-ruby                         | Y     |               |
| geben               | DBGp protocol frontend, a script debugger                                        | http://code.google.com/p/geben-on-emacs/                        | Y     |               |
| haml-mode           | Major mode for editing Haml files                                                | https://github.com/nex3/haml-mode                               | Y     |               |
| inf-ruby            | Run a Ruby process in a buffer                                                   | https://github.com/nonsequitur/inf-ruby                         | Y     |               |
| jquery-doc          | jQuery api documentation interface for emacs                                     | https://github.com/ananthakumaran/jquery-doc.el                 | Y     |               |
| js2-mode            | Improved JavaScript editing mode                                                 | https://github.com/mooz/js2-mode                                | Y     |               |
| know-your-http-well | Look up the meaning of HTTP headers, methods, relations, status codes            | https://github.com/for-GET/know-your-http-well                  | Y     |               |
| know-your-css-well  | CSS properties description in your emacs [Inspired by know-your-http-well]       | https://github.com/0xAX/know-your-css-well                      |       |               |
| milkode             | Command line search and direct jump with Milkode                                 | https://github.com/ongaeshi/emacs-milkode                       | Y     |               |
| php-mode            | Major mode for editing PHP code                                                  | https://github.com/ejmr/php-mode                                | Y     |               |
| projectile-rails    | Minor mode for Rails projects based on projectile-mode                           | https://github.com/asok/projectile-rails                        | Y     | projectile    |
| pry                 | Pry support within Emacs                                                         | https://github.com/jacott/emacs-pry                             |       |               |
| rails-log-mode      | Major mode for viewing Rails log files                                           | https://github.com/ananthakumaran/rails-log-mode                | Y     |               |
| rhtml-mode          | major mode for editing RHTML files                                               | https://github.com/eschulte/rhtml                               | Y     |               |
| rainbow-mode        | a minor mode displays color strings with the color they represent as background. | https://julien.danjou.info/projects/emacs-packages#rainbow-mode |       |               |
| rinari              | Rinari Is Not A Rails IDE                                                        | https://github.com/eschulte/rinari                              | Y     |               |
| robe                | Code navigation, documentation lookup and completion for Ruby                    | https://github.com/dgutov/robe                                  | Y     |               |
| rubocop             | An Emacs interface for RuboCop                                                   | https://github.com/bbatsov/rubocop-emacs                        | Y     |               |
| ruby-block          | highlight matching block                                                         | http://www.emacswiki.org/emacs/ruby-block.el                    | Y     |               |
| ruby-compilation    | run a ruby process in a compilation buffer                                       | https://github.com/eschulte/rinari                              | Y     |               |
| ruby-tools          | Collection of handy functions for ruby-mode.                                     | https://github.com/rejeep/ruby-tools                            | Y     |               |
| web-mode            | major mode for editing html templates                                            | https://github.com/fxbois/web-mode                              | Y     |               |
| yaml-mode           | Major mode for editing YAML files                                                | https://github.com/yoshiki/yaml-mode                            | Y     |               |
| yari                | Yet Another RI interface for Emacs                                               | https://github.com/hron/yari.el                                 | Y     |               |

### some other packages might be useful (but not included here)

| Package | Description                                                                     | Homepage                                                | melpa |
| omniref | Emacs interface for Ruby documentation search engine http://www.omniref.com/    | https://github.com/dotemacs/omniref.el                  | Y     |
| realgud | A modular front-end for interacting with external debuggers (supports `rdebug`) | https://github.com/rocky/emacs-dbgr                     | Y     |
| rdebug  |                                                                                 | https://github.com/cldwalker/debugger/tree/master/emacs |       |

## Usage

`M-x load-file RET /path/to/dotemacs-rails/_init.el RET`

or:

Add this into your dotemacs (`~/.emacs` or `~/.emacs.d/init.el`)

```emacs-lisp
    (load-file "/path/to/dotemacs-rails/_init.el")
```

Tested on GNU Emacs 23.3 & 24.3.


### Major modes

| extension | major mode                 | alternative   |
| .rb       | ruby-mode (emacs built-in) | enh-ruby-mode |
| .erb      | web-mode                   | rhtml-mode    |
| .haml     | haml-mode                  |               |
| .yml      | yaml-mode                  |               |
| .php      | web-mode                   | php-mode      |
| .rake     | ruby-mode (emacs built-in) | enh-ruby-mode |
| .js       | js-mode (emacs built-in)   | js2-mode      |
| .css      | css-mode (emacs built-in)  |               |

To use *alternative* major-mode, you need to activate them manually (`M-x major-mode-name` after
file opened, or customized `auto-mode-alist` by yourself).

#### enh-ruby-mode

Advantages over built-in `ruby-mode`:
 * Dynamic syntax checking
 * `beginning-of-defun` and `end-of-defun` would work (while emacs < 24 would not)
 * `imenu` would work (but emacs < 24 would not)

Note:
 * To use `enh-ruby-mode`, `ruby` intepreter must exist in your `PATH`, and it must be >=1.9.2

### Jumping across files

For Rails project, `rinari-minor-mode` would be activated for ruby & erb buffers:

```
     C-c ; f c	rinari-find-controller
     C-c ; f e	rinari-find-environment
     C-c ; f f	rinari-find-file-in-project
     C-c ; f h	rinari-find-helper
     C-c ; f i	rinari-find-migration
     C-c ; f j	rinari-find-javascript
     C-c ; f l	rinari-find-plugin
     C-c ; f m	rinari-find-model
     C-c ; f n	rinari-find-configuration
     C-c ; f o	rinari-find-log
     C-c ; f p	rinari-find-public
     C-c ; f s	rinari-find-script
     C-c ; f t	rinari-find-test
     C-c ; f v	rinari-find-view
     C-c ; f w	rinari-find-worker
     C-c ; f x	rinari-find-fixture
     C-c ; f y	rinari-find-stylesheet
```

Note: use `C-u M-x info RET dotemacs-rails/info/rinari.info` to view info pages of `rinari`

#### open gems source

`bundle-open` wraps `bundle open`, which opens gem according to Gemfile.

### Jumping to class/symbol definition

 * method 1: use `ctags` (emacs built-in)
  * make sure you've installed exuberant ctags (http://ctags.sourceforge.net/). use `ctags
--version` to check
  * run `ctags -R -e .` in your Rails project
  * call `find-tag` to jump to class/function at point

Note: `rinari-minor-mode` would automatically updating your `tags-file-name` variable whenever
you enter a rails project,

 * method 2: use `robe`
  * Use `M-x rinari-console` or `M-x inf-ruby-console-auto` to launch Rails console
(for non-Rails project, use `M-x inf-ruby-console-default`)
  * Call `M-x robe-jump`

### code completion

in ruby source buffer:
 * `M-x hippie-expand` is always your friend (zero configuration,
zero pain)

 * `ac-source-robe` (from package `robe`) enables `auto-complete` for
ruby buffers

But it seems to make editing too slow, thus disabled by
default. Use `M-x ruby-mode-enable-ac` to activate it for current
buffer. (If you want to activate it each time a ruby buffer opens,
add `(add-hook 'ruby-mode-hook 'ruby-mode-enable-ac)` to your
`~/.emacs` .)

And make sure you've started an `inf-ruby` process (see section
[console](console) below).


in irb shell buffer:
 * [ ] `ac-inf-ruby` (but not enabled in this suite as it won't work
in my machine)
  * [ ] what if you're using `pry` as console (if `ac-inf-ruby`
works for `irb`)?

### Document lookup
 * `yari` from package `[[https://github.com/hron/yari.el][yari]]`
  * use `gem rdoc <gemname> [-v <version>]` to generate RDoc/RI
documentation (generated files live in `ruby/lib/ruby/gems/1.9.x/doc/gemname-1.x.x/`)
  * call `M-x yari` to lookup documentaion

References:
  * [Ubuntu Manpage: ri1.9.1 - Ruby Information at your fingertips](http://manpages.ubuntu.com/manpages/trusty/en/man1/ri.1.html)
  * [Ubuntu Manpage: rdoc1.9.1 - Generate documentation from Ruby script files](http://manpages.ubuntu.com/manpages/trusty/en/man1/rdoc.1.html)
  * [用好ri，轻松查阅ruby/rails文档 - 杨波的专栏](http://blog.csdn.net/yangbo_hr/article/details/2026216)

 * If you're using **pry** as console in emacs (see below [pry-console-in-emacs](pry-console-in-emacs))
  * `ri` command in **pry** console
  * `show-doc` command in **pry** console

References:
  * [Documentation browsing · pry/pry Wiki · GitHub](https://github.com/pry/pry/wiki/Documentation-browsing)

### Syntax checking

For ruby source code:
 * enh-ruby-mode
 * flymake-ruby
 * rubocop

### Debugging
 * method 1: use package `rdebug` in `debugger` gem
(https://github.com/cldwalker/debugger/tree/master/emacs )

Note: on emacs 24, you should change `require 'gud-ui` to `require 'gui-mi`.

steps:
  * abc
  * def
  * ghi

 * method 2: use package `realgud` (https://github.com/rocky/emacs-dbgr)

steps:
  * abc
  * def
  * ghi

### Console

 * For Rails project, you can use
  * `M-x rinari-console` from package `[[https://github.com/eschulte/rinari%20][rinari]]`
  * `M-x projectile-rails-console` from package `[[https://github.com/asok/projectile-rails][projectile-rails]]`
(but you need to install `[[https://github.com/bbatsov/projectile][projectile]]` by yourself, as it is not
include in this suite)

 * `inf-ruby-console-auto` from package `[[https://github.com/nonsequitur/inf-ruby][inf-ruby]]` is recommended
for non-rails project or gem source
  * `M-x inf-ruby-console-rails` runs `rails console` for rails project
  * `M-x inf-ruby-console-default` runs `bundle console` for
bundler-enabled project
  * `M-x inf-ruby-console-gem` runs `bundle exec irb` (if Gemfile
exists) or `irb -I lib`
  * `M-x inf-ruby-console-auto` automatically choose above 3 methods

#### Using `pry` as console in emacs
https://github.com/pry/pry

`run-pry` from package `[[http://http://github.com/jacott/emacs-pry][pry]]` :: `M-x run-pry`

 * if you've add `[[https://github.com/rweng/pry-rails][pry-rails]]` into your Gemfile, then (after `bundle
install` finished ) `rinari-console` or `inf-ruby-console-rails` would be powered
by **pry**

Some notes for emacs:
 * [Why is my emacs shell output showing odd characters?](https://github.com/pry/pry/wiki/FAQ#why-is-my-emacs-shell-output-showing-odd-characters)
 * [It may be necessary to turn paging off if you are running Pry from
within an emacs shell or similar.](https://github.com/pry/pry/wiki/Customization-and-configuration#pager)

```ruby
  if ENV['TERM']=='dump'
     Pry.config.pager = false
     # uncomment the following two lines if your emacs doesn't support ANSI color
     # https://github.com/pry/pry/wiki/FAQ#emacsshell
     #Pry.config.color = false
     #Pry.config.auto_indent = false 
  end
```

> if you want to disable Pry for `rinari-console` or
`inf-ruby-console-rails`, type

```
   M-x setenv RET DISABLE_PRY_RAILS RET 1 RET
```
> or
```
  M-: (setenv "DISABLE_PRY_RAILS" "1") RET
```


### Database

`rinari-sql`

### View logs

As log files could be very big, it is not a good idea to open it
directly into Emacs. You can use the following ways:

 * Call `tail -f log/development.log` in shell/eshell buffer

 * Use `M-x rails-log-view-development` from package
`rails-log-mode`, which wraps `tail -f`. The advantage is that all
the file paths in the log are navigatable, thus you can press
`Enter` to open a file in the stack trace

 * If you've installed package `projectile`, you can use `M-x
projectile-rails-find-log` . It allow you find a log file and
enable `auto-revert-tail-mode` in its buffer.

## Other tips
### debugging php
### http status / http header
### css rainbow
### css property
### jquery-doc
### milkode
