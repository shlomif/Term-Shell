use Test;

BEGIN { plan tests => 2 }

use Term::Shell;

BEGIN { ok(1) }

$shell = Term::Shell->new;

ok($shell);
