const autoprefixer = require('autoprefixer');


module.exports = {
  paths: {
    public: 'public'
  },

  files: {
    stylesheets: {
      joinTo: 'css/app.css'
    }
  },

  plugins: {
    postcss: {
      processors: [autoprefixer()]
    },
  }
}
