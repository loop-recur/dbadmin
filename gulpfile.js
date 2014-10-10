var gulp = require('gulp')
  , purescript = require('gulp-purescript');

var src = [
    'bower_components/purescript-*/src/**/*.purs',
    'src/**/*.purs'
];

var compile = function(options) {
    return function() {
        // We need this hack for now until gulp does something about
        // https://github.com/gulpjs/gulp/issues/71
        //var psc = purescript.psc();
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
};

gulp.task('.psci', purescript.dotPsci)

gulp.task('src', compile({}));

gulp.task('watch', function() {
    gulp.watch("src/**/*", ['src']);
});

gulp.task('default', ['src', 'watch']);
