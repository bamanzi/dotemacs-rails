## Synopsis

**Projectile Rails** is a minor mode for working with the Rails project in GNU Emacs.
Internally it based on [Projectile](https://github.com/bbatsov/projectile).

It means that you can use Projectile's commands for greping (or acking) files, run tests, switch between projects, etc.

With Projectile Rails you are able to:

* navigate through rails resources (controllers, views, helpers and so on)
* jump to ruby classes and template files
* run `rake`
* run `rails console`
* run `rails generate`
* open log files with `auto-revert-mode` on
* see rails keywords highlighted
* take advantage of [zeus](https://github.com/burke/zeus) and [spring](https://github.com/jonleighton/spring) preloaders

It can be a replacement for [rinari](https://github.com/eschulte/rinari).

## Setup

### Installation

#### Melpa

Once you have setup [Melpa](http://melpa.milkbox.net/#/getting-started) you can use `package-install` command to install Projectile Rails. The package name is `projectile-rails`.

## Usage

### Hooking up with Projectile

To make it start alongside `projectile-mode`:

```lisp
(add-hook 'projectile-mode-hook 'projectile-rails-on)
```
That will start it only if the current project is a Rails project.

Probably you should read Projectile's [README](https://github.com/bbatsov/projectile) on setting up the completion system, caching and indexing files. Although the default settings are quite sensible and you should be ready to go without much tweaking.

### Customizing

The mode's buffers will have the Rails keywords higlighted. To turn it off:
```lisp
(setq projectile-rails-add-keywords nil)
```

If you are using [yasnippet](https://github.com/capitaomorte/yasnippet) and you open a new file it will be filled with a skeleton class. To turn it off:
```lisp
(setq projectile-rails-expand-snippet nil)
```

### Interactive commands

Command                                  | Keybinding                                 | Description
-----------------------------------------|--------------------------------------------|-------------------------------------------------------
projectile-rails-find-model              | <kbd>C-c r m</kbd>                         | Find a model using `projectile-completion-system`.
projectile-rails-find-current-model      | <kbd>C-c r M</kbd>, <kbd>C-c r g m</kbd>   | Go to a model connected with the current resource.
projectile-rails-find-controller         | <kbd>C-c r c</kbd>                         | Find a controller using `projectile-completion-system`.
projectile-rails-find-current-controller | <kbd>C-c r C</kbd>, <kbd>C-c r g c</kbd>   | Go to a controller connected with the current resource.
projectile-rails-find-view               | <kbd>C-c r v</kbd>                         | Find a template or partial using `projectile-completion-system`.
projectile-rails-find-current-view       | <kbd>C-c r V</kbd>, <kbd>C-c r g v</kbd>   | Go to a view connected with the current resource.
projectile-rails-find-helper             | <kbd>C-c r h</kbd>                         | Find a helper using `projectile-completion-system`.
projectile-rails-find-current-helper     | <kbd>C-c r H</kbd>, <kbd>C-c r g h</kbd>   | Go to a helper connected with the current resource.
projectile-rails-find-lib                | <kbd>C-c r l</kbd>                         | Find a lib using `projectile-completion-system`.
projectile-rails-find-spec               | <kbd>C-c r s</kbd>                         | Find a spec using `projectile-completion-system`.
projectile-rails-find-current-spec       | <kbd>C-c r S</kbd>, <kbd>C-c r g s</kbd>   | Go to a spec connected with the current resource.
projectile-rails-find-migration          | <kbd>C-c r n</kbd>                         | Find a migration using `projectile-completion-system`.
projectile-rails-find-current-migration  | <kbd>C-c r N</kbd>, <kbd>C-c r g n</kbd>   | Go to a migration connected with the current resource.
projectile-rails-find-javascript         | <kbd>C-c r j</kbd>                         | Find a javascript using `projectile-completion-system`.
projectile-rails-find-log                | <kbd>C-c r o</kbd>                         | Find a log file and enable `auto-revert-tail-mode` in its buffer.
projectile-rails-find-initializer        | <kbd>C-c r i</kbd>                         | Find an initializer file using `projectile-completions-system`.
projectile-rails-find-environment        | <kbd>C-c r e</kbd>                         | Find an environment file using `projectile-completions-system`.
projectile-rails-find-locale             | <kbd>C-c r a</kbd>                         | Find a locale file using `projectile-completions-system`.
projectile-rails-find-mailer             | <kbd>C-c r @</kbd>                         | Find a mailer file using `projectile-completions-system`.
projectile-rails-find-layout             | <kbd>C-c r y</kbd>                         | Find a layout file using `projectile-completions-system`.
projectile-rails-console                 | <kbd>C-c r r</kbd>                         | Run `rails console` command in `inf-ruby` buffer.
projectile-rails-rake                    | <kbd>C-c r k</kbd>                         | Select a rake task to run using `projectile-completion-system`.
projectile-rails-generate                | <kbd>C-c r t</kbd>                         | Run `rails generate` command.
projectile-rails-goto-file-at-point      | <kbd>C-c r RET</kbd>, <kbd>C-c r g f</kbd> | Go to a file at point. Depending on the context that might be a constant, template or partial, or a gem.
projectile-rails-goto-gemfile            | <kbd>C-c r g g</kbd>                       | Go to `Gemfile` file.
projectile-rails-goto-routes             | <kbd>C-c r g r</kbd>                       | Go to `config/routes.rb` file.
projectile-rails-goto-schema             | <kbd>C-c r g h</kbd>                       | Go to `db/schema.rb` file.
projectile-rails-goto-spec-helper        | <kbd>C-c r g p</kbd>                       | Go to `spec/spec_helper.rb` file.

You might want to create your own keybinding for your favorite commands. For example:

```lisp
(define-key projectile-rails-mode-map (kbd "s-m")   'projectile-rails-find-model)
(define-key projectile-rails-mode-map (kbd "s-c")   'projectile-rails-find-controller)
(define-key projectile-rails-mode-map (kbd "s-v")   'projectile-rails-find-view)
(define-key projectile-rails-mode-map (kbd "s-RET") 'projectile-rails-goto-file-at-point)
(define-key projectile-rails-mode-map (kbd "C-c g")  projectile-rails-mode-goto-map)
```

## Beyond

Consider installing other Emacs packages that can help you working specifically with Rails projects.

### Templates

Extension   | Alternatives
------------|------------------------------------------------------
 erb        | [web-mode](https://github.com/fxbois/web-mode), [mmm-mode](https://github.com/purcell/mmm-mode), [rhtml-mode](https://github.com/eschulte/rhtml)
 haml		| [haml-mode](https://github.com/nex3/haml-mode)
 slim		| [emacs-slim](https://github.com/slim-template/emacs-slim)
 yaml		| [yaml-mode](https://github.com/yoshiki/yaml-mode)

### Running ruby gems

Some of the Projectile Rails functions run `rake` or `rails` executables. If you are using a ruby version manager you might need to configure your Emacs to play nicely with it.

* [rvm.el](https://github.com/senny/rvm.el)
* [rbenv.el](https://github.com/senny/rbenv.el)

OS X users might want to look at [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell).

### Miscellaneous

* [bundler.el](https://github.com/tobiassvn/bundler.el) to interact with Bundler.
* [rspec-mode](https://github.com/pezra/rspec-mode) to run and edit spec files.
* [feature-mode](https://github.com/michaelklishin/cucumber.el) to edit feature files.
* [robe](https://github.com/dgutov/robe) to view gems documentation and jump to methods and classes definitions.
* [magit](https://github.com/magit/magit) to interact with git.

## Caveat

### Running pry instead of irb
* Pry's paging is not working in emacs. It should be disabled with `Pry.config.pager = false`. [Reference](https://github.com/pry/pry/wiki/Customization-and-configuration#wiki-pager).

* When `projectile-rails-console` runs rails console using a pre-loader (zeus or spring) and pry's indent correction is enabled then pry will insert some ansi codes that are misinterpreted by `comint-mode`. A workaround is to disable the indentation correction with `Pry.config.correct_indent = false`. [Reference](https://github.com/pry/pry/wiki/Customization-and-configuration#wiki-correct-indent). [Issue](https://github.com/asok/projectile-rails/issues/12).

## Contribution

Install [cask](https://github.com/rejeep/cask.el) if you haven't already, then:

```bash
$ cd /path/to/projectile-rails
$ cask
```

Run all tests with:

```bash
$ make test
```

For all of them to pass you will need the `bundle` executable in your path.
