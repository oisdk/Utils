from io import StringIO
import sys

class Capturing(list):
    def __enter__(self):
        self._stdout = sys.stdout
        sys.stdout = self._stringio = StringIO()
        return self
    def __exit__(self, *args):
        self.extend(self._stringio.getvalue().splitlines())
        sys.stdout = self._stdout

def same(iterable):
    try: e = next(iter(iterable))
    except StopIteration: return True
    return all(e == x for x in iterable)

def compose(*funcs):

    # compose(h, g, f)(x) => h(g(f(x)))

    return reduce(lambda g, f: lambda x: g(f(x)), funcs)
