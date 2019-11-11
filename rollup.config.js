import elm from 'rollup-plugin-elm'
import postcss from 'rollup-plugin-postcss'
import postcssEnv from 'postcss-preset-env'

const dev = process.env.ROLLUP_WATCH

export default {
  input: 'src/index.js',
  output: { file: `dist/bundle.js`, format: 'iife' },
  plugins: [
    postcss({
      minimize: !dev,
      plugins: [
        postcssEnv({ stage: 3, features: {
          'nesting-rules': true,
        }})
      ],
    }),
    elm({
      exclude: 'elm_stuff/**',
      compiler: { optimize: !dev, debug: dev },
    }),
  ],
}
