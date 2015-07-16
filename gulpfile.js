'use strict';

var gulp        = require('gulp')
  , purescript  = require('gulp-purescript')
  , jsValidate  = require('gulp-jsvalidate')
  , plumber     = require("gulp-plumber")
  , run         = require("gulp-run")
  ;

var sources = [
  'src/**/*.purs',
  'test/**/*.purs',
  'examples/src/**/*.purs',
  'bower_components/purescript-*/src/**/*.purs'
];

var foreigns = [
  'src/**/*.js',
  'bower_components/purescript-*/src/**/*.js'
];

gulp.task('jsvalidate', function () {
  return gulp.src(foreigns)
    .pipe(plumber())
    .pipe(jsValidate());
});

gulp.task('psc', function() {
  return purescript.psc({
    src: sources,
    ffi: foreigns
  })
});

gulp.task('pscDocs', function() {
  return purescript.pscDocs({
    src: sources,
    docgen: {
      'Control.Monad.Aff': 'docs/Control.Monad.Aff.md',
      'Control.Monad.Aff.AVar': 'docs/Control.Monad.Aff.AVar.md',
      'Control.Monad.Aff.Console': 'docs/Control.Monad.Aff.Console.md',
      'Control.Monad.Aff.Class': 'docs/Control.Monad.Aff.Class.md',
      'Control.Monad.Aff.Par': 'docs/Control.Monad.Aff.Par.md',
      'Control.Monad.Aff.Unsafe': 'docs/Control.Monad.Aff.Unsafe.md'
    }
  })
})

gulp.task('dotPsci', function() {
  return purescript.psci({
    src: sources,
    ffi: foreigns
  })
  .pipe(gulp.dest('.'))
})

gulp.task('examples', ['psc'], function() {
  return purescript.pscBundle({
    src: "output/**/*.js",
    main: "Examples",
    output: "examples/output/examples.js"
  })
});

gulp.task('test', ['psc'], function() {
  return purescript.pscBundle({ src: "output/**/*.js", main: "Test.Main" })
    .pipe(run("node"));
});

gulp.task('make', ['jsvalidate', 'psc', 'dotPsci', 'pscDocs']);
gulp.task('default', ['make', 'examples', 'test']);
