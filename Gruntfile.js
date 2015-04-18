module.exports = function(grunt) {
  "use strict";

  grunt.initConfig({ 
    psc: {
      options: {
            main: "Examples",
            modules: ["Examples"]
        },
        example: {
            src: ["<%=libFiles%>", "examples/Prelude.purs"],
            dest: "output/examples.js"
      }
    },
    libFiles: [
      "src/**/*.purs",
      "examples/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs",
    ],
    
    clean: ["output"],
  
    pscMake: ["<%=libFiles%>"],
    dotPsci: ["<%=libFiles%>"],
    pscDocs: {
        readme: {
            src: "src/**/*.purs",
            dest: "MODULES.md"
        }
    },
    jsvalidate: {
      options:{
        globals: {},
        esprimaOptions: {},
        verbose: false
      },
      targetName:{
        files:{
          src:['output/**/*.js']
        }
      }
    }
  });

  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-purescript");
  grunt.loadNpmTasks('grunt-jsvalidate');
  
  grunt.registerTask("make", ["pscMake", "jsvalidate", "dotPsci", "pscDocs"]);
  grunt.registerTask("test", ["jsvalidate", "psc", "pscDocs"]);
  grunt.registerTask("default", ["make"]);
};