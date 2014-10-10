module.exports = function(grunt) {
  "use strict";

  grunt.initConfig({

    srcFiles: [
      "src/**/*.purs", 
      "bower_components/**/src/**/*.purs"
    ],

    psc: {
      options: {
        main: "Main",
        modules: ['Ajax', 'Api', 'Custom',
                  'Dispatcher', 'Form', 'Helper',
                  'List', 'Login', 'Main', 'Nav',
                  'Types', 'Ui'
                  ]
        //modules: ["src/app"]
      },
      all: {
	    src: ["<%=srcFiles%>"],
        dest: "js/app.js"
      }
    },

    dotPsci: ["<%=srcFiles%>"]
  });

  grunt.loadNpmTasks("grunt-purescript");
  grunt.registerTask("default", ["psc:all", "dotPsci"]);
};
