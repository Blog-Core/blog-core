Comment form subproject.

## Building

Install dependencies (in comment directory):

    npm install

Build bundle:

    make all

## Testing

Load Blog-Core tests (in main project root):

    swipl -s tests/tests.pl

Run frontend tests (in comment directory):

    make test
