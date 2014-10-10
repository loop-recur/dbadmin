var gulp = require('gulp'),
    purescript = require('gulp-purescript');

var src = [
      'bower_components/purescript-*/src/**/*.purs',
      'src/**/*.purs'
    ];

var psc = function() {
      // We need this hack for now until gulp does something about
      // https://github.com/gulpjs/gulp/issues/71
      var psc = purescript.psc({
            main: "Main",
            output: 'app.js',
            modules: ['Ajax', 'Api', 'Custom',
                      'Dispatcher', 'Form', 'Helper',
                      'List', 'Login', 'Main', 'Nav',
                      'Types', 'Ui'
                     ]
          });

      psc.on('error', function(e) {
          console.error(e.message);
          psc.end();
      });

      return gulp.src(src)
              .pipe(psc)
              .pipe(gulp.dest("js"));
    };

var psci = function() {
      return gulp.src(src).pipe(purescript.dotPsci());
    };

gulp.task('psc', psc);
gulp.task('psci', psci);
gulp.task('build', ['psc', 'psci']);
gulp.task('all', ['psc', 'psci', 'watch']);
gulp.task('default', ['psc', 'watch']);
gulp.task('watch', function() {
  gulp.watch("src/**/*", ['psc']);
});

