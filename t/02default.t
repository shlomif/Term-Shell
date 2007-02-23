use Test;

BEGIN { plan tests => 12 }

package MyShell;
use base qw(Term::Shell);

sub run_command1  { print "command1\n"; }
sub smry_command1 { "what does command1 do?" }
sub help_command1 {
<<'END';
Help on 'command1', whatever that may be...
END
}

sub run_command2 { print "command2\n"; }

package main;
$shell = MyShell->new;

#=============================================================================
# Command completions
#=============================================================================
$cmds = [$shell->possible_actions('e', 'run')];
ok(ref($cmds), 'ARRAY');
ok($#$cmds, 0);
ok($cmds->[0], 'exit');

$cmds = [$shell->possible_actions('h', 'run')];
ok($#$cmds, 0);
ok($cmds->[0], 'help');

$cmds = [$shell->possible_actions('c', 'run')];
ok($#$cmds, 1);

#=============================================================================
# Help completions
#=============================================================================
$cmds = [$shell->possible_actions('e', 'help')];
ok($#$cmds, 0);
ok($cmds->[0], 'exit');

$cmds = [$shell->possible_actions('h', 'help')];
ok($#$cmds, 0);
ok($cmds->[0], 'help');

$cmds = [$shell->possible_actions('c', 'help')];
ok($#$cmds, 0);
ok($cmds->[0], 'command1');
