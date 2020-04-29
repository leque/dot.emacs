module.exports = {
    'parserOptions': {
        'ecmaVersion': 6,
        'sourceType': 'script'
    },
    'env': {
        'browser': true
    },
    'extends': 'eslint:recommended',
    'rules': {
        'array-bracket-spacing': ['error', 'never'],
        'brace-style': ['error', '1tbs'],
        'comma-spacing': ['error', { 'before': false, 'after': true }],
        'comma-style': ['error', 'last'],
        'eol-last': ['error', 'always'],
        'eqeqeq': ['error', 'always', { 'null': 'ignore' }],
        'func-call-spacing': ['error', 'never'],
        'indent': ['error', 4],
        'key-spacing': ['error', { 'beforeColon': false, 'afterColon': true }],
        'keyword-spacing': ['error', { 'before': true, 'after': true }],
        'linebreak-style': ['error', 'unix'],
        'max-len': ['error', { 'code': 120, 'ignoreUrls': true }],
        'no-eval': 'error',
        'no-extra-semi': 'error',
        'no-fallthrough': 'error',
        'no-throw-literal': 'error',
        'no-trailing-spaces': ['error', { 'ignoreComments': true }],
        'no-unused-vars': ['error', { 'argsIgnorePattern': '^_' }],
        'no-var': ['off'],
        'object-curly-spacing': ['error', 'always'],
        'operator-linebreak': ['error', 'before'],
        'quotes': ['error', 'single'],
        'semi': ['error', 'always'],
        'semi-spacing': ['error', { 'after': true, 'before': false }],
        'semi-style': ['error', 'last'],
        'space-before-blocks': ['error', 'always'],
        'space-before-function-paren': ['error', 'never'],
        'switch-colon-spacing': ['error', { 'before': false, 'after': true }],
    },
    'root': true
};
