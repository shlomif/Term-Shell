package Term::Shell;
$VERSION = '0.01';

use strict;
use Data::Dumper;
use Term::ReadLine;

#=============================================================================
# Term::Shell API methods
#=============================================================================
sub new {
    my $cls = shift;
    my $o = bless {
	term	=> do {
	    #local $^W;
	    Term::ReadLine->new('shell');
	},
    }, ref($cls) || $cls;

    # Set up the API hash:
    $o->{command} = {};
    $o->{API} = {
	args		=> \@_,
	case_ignore	=> ($^O eq 'MSWin32' ? 1 : 0),
	check_idle	=> 0,	# changing this isn't supported
	class		=> $cls,
	command		=> $o->{command},
	match_uniq	=> 1,
	readline	=> $o->term->ReadLine,
	script		=> (caller(0))[1],
	version		=> $Term::Shell::VERSION,
    };

    # Note: the rl_completion_function doesn't pass an object as the first
    # argument, so we have to use a closure. This has the unfortunate effect
    # of preventing two instances of Term::ReadLine from coexisting.
    my $completion_handler = sub {
	$o->rl_complete(@_);
    };
    if ($o->term->ReadLine eq 'Term::ReadLine::Gnu') {
	my $attribs = $o->term->Attribs;
	$attribs->{completion_function} = $completion_handler;
    }
    elsif ($o->term->ReadLine eq 'Term::ReadLine::Perl') {
	$readline::rl_completion_function = 
	$readline::rl_completion_function = $completion_handler;
    }

    # Read the namespace and find the action handlers.
    $o->find_handlers;

    $o->init;
    $o;
}

sub DESTROY {
    my $o = shift;
    $o->fini;
}

sub cmd {
    my $o = shift;
    $o->{line} = shift;
    if ($o->line =~ /\S/) {
	my ($cmd, @args) = $o->line_parsed;
	$o->run($cmd, @args);
	unless ($o->{command}{run}{found}) {
	    my @c = sort $o->possible_actions($cmd, 'run', 1);
	    if (@c) {
		print $o->msg_ambiguous_cmd($cmd, @c);
	    }
	    else {
		print $o->msg_unknown_cmd($cmd);
	    }
	}
    }
    else {
	$o->run('');
    }
}

sub cmdloop {
    my $o = shift;
    $o->{stop} = 0;
    $o->preloop;
    while (defined (my $line = $o->readline($o->prompt_str))) {
	$o->cmd($line);
	last if $o->{stop};
    }
    $o->postloop;
}
*mainloop = \&cmdloop;

sub readline {
    my $o = shift;
    my $prompt = shift;
    return $o->term->readline($prompt)
	if $o->{API}{check_idle} == 0
	    or not defined $o->term->IN;

    # They've asked for idle-time running of some user command.
    local $Term::ReadLine::toloop = 1;
    local *Tk::fileevent = sub {
	my $cls = shift;
	my ($file, $boring, $callback) = @_;
	$o->{fh} = $file;	# save the filehandle!
	$o->{cb} = $callback;	# save the callback!
    };
    local *Tk::DoOneEvent = sub {
	# We'll totally cheat and do a select() here -- the timeout will be
	# $o->{API}{check_idle}; if the handle is ready, we'll call &$cb;
	# otherwise we'll call $o->idle(), which can do some processing.
	my $timeout = $o->{API}{check_idle};
	use IO::Select;
	if (IO::Select->new($o->{fh})->can_read($timeout)) {
	    # Input is ready: stop the event loop.
	    $o->{cb}->();
	}
	else {
	    $o->idle;
	}
    };
    return $o->term->readline($prompt);
}

sub page { shift; print @_ }
sub stoploop { $_[0]->{stop}++ }
sub term { $_[0]->{term} }

# These are likely candidates for overriding in subclasses
sub init { }		# called last in the ctor
sub fini { }		# called first in the dtor
sub preloop { }
sub postloop { }
sub precmd { }
sub postcmd { }
sub prompt_str { 'shell> ' }
sub idle { }
sub cmd_prefix { '' }
sub cmd_suffix { '' }

#=============================================================================
# Run actions
#=============================================================================
sub run {
    my $o = shift;
    my $action = shift;
    my @args = @_;
    $o->do_action($action, \@args, 'run')
}

sub complete {
    my $o = shift;
    my $action = shift;
    my @args = @_;
    my @compls = $o->do_action($action, \@args, 'comp');
    return () unless $o->{command}{comp}{found};
    return @compls;
}

sub help {
    my $o = shift;
    my $topic = shift;
    my @subtopics = @_;
    $o->do_action($topic, \@subtopics, 'help')
}

sub summary {
    my $o = shift;
    my $topic = shift;
    $o->do_action($topic, [], 'smry')
}

sub prompt {
    my $o = shift;
    my ($prompt, $default, $completions, $casei) = @_;

    # A closure to read the line.
    my $line;
    my $readline = sub {
	my ($sh, $gh) = @{$o->term->Features}{qw(setHistory getHistory)};
	my @history = $o->term->GetHistory if $gh;
	$o->term->SetHistory() if $sh;
	$line = $o->readline($prompt);
	$line = $default
	    if ((not defined $line or $line =~ /^\s*$/) and defined $default);
	# Restore the history
	$o->term->SetHistory(@history) if $sh;
	$line;
    };
    # A closure to complete the line.
    my $complete = sub {
	my ($word, $line, $start) = @_;
	return $o->completions($word, $completions, $casei);
    };
    if ($o->term->ReadLine eq 'Term::ReadLine::Gnu') {
	my $attribs = $o->term->Attribs;
	local $attribs->{completion_function} = $complete;
	&$readline;
    }
    elsif ($o->term->ReadLine eq 'Term::ReadLine::Perl') {
	local $readline::rl_completion_function = $complete;
	&$readline;
    }
    else {
	&$readline;
    }
    $line;
}

sub print_pairs {
    my $o    = shift;
    my @keys = @{shift(@_)};
    my @vals = @{shift(@_)};
    my $sep  = shift || ": ";
    my $left = shift || 0;
    my $ind  = shift || "";
    my $len  = shift || 0;
    my $wrap = shift || 0;
    if ($wrap) {
	eval { require Text::Autoformat };
	if ($@) {
	    warn (
		"Term::Shell::print_pairs(): Text::Autoformat is required " .
		"for wrapping. Wrapping disabled"
	    ) if $^W;
	    $wrap = 0;
	}
    }
    my $cols = shift || $ENV{COLUMNS} || 78;
    $len < length($_) and $len = length($_) for @keys;
    for my $i (0 .. $#keys) {
	next unless defined $vals[$i];
	my $sz   = ($len - length($keys[$i]));
	my $lpad = $left ? "" : " " x $sz;
	my $rpad = $left ? " " x $sz : "";
	my $l = "$ind$lpad$keys[$i]$rpad$sep";
	my $wrap = $wrap & ($vals[$i] =~ /\s/ and $vals[$i] !~ /\d/);
	my $form = (
	    $wrap
	    ? autoformat(
		$vals[$i],
		{ left => length($l)+1, right => $cols, all => 1 },
	    )
	    : "$l$vals[$i]\n"
	);
	substr($form, 0, length($l), $l);
	print $form;
    }
    return $len;
}

sub line {
    my $o = shift;
    $o->{line}
}

# Handle backslash translation; doesn't do anything complicated yet.
sub process_esc {
    my $o = shift;
    my $c = shift;
    my $q = shift;
    my $n;
    return '\\' if $c eq '\\';
    return $q if $c eq $q;
    return "\\$c";
}

sub line_parsed {
    my $o = shift;
    my $args = shift || $o->line;
    my @args;

    # Parse a quoted string
    my $parse_quoted = sub {
        my $raw = shift;
	my $quote = shift;
	my $i=1;
	my $string = '';
	my $c;
	while($i <= length($raw) and ($c=substr($raw, $i, 1)) ne $quote) {
	    if ($c eq '\\') {
	        $string .= $o->process_esc(substr($raw, $i+1, 1), $quote);
		$i++;
	    }
	    else {
	    	$string .= substr($raw, $i, 1);
	    }
	    $i++;
	}
	return ($string, $i);
    };

    # Parse an array of arguments. Whitespace separates, unless quoted.
    my $arg = undef;
    my $raw = undef;
    for(my $i=0; $i<length($args); $i++) {
	my $c = substr($args, $i, 1);
	if ($c =~ /\S/ and @args == 1) {
	    $raw ||= substr($args, $i);
	}
	if ($c =~ /['"]/) {
	    my ($str, $n) = $parse_quoted->(substr($args,$i),$c);
	    $i += $n;
	    $arg = (defined($arg) ? $arg : '') . $str;
	}
# We do not parse outside of strings
#	elsif ($c eq '\\') {
#	    $arg = (defined($arg) ? $arg : '') 
#	      . $o->process_esc(substr($args,$i+1,1));
#	    $i++;
#	}
	elsif ($c =~ /\s/) {
	    push @args, $arg if defined $arg;
	    $arg = undef
	} 
	else {
	    $arg .= substr($args,$i,1);
	}
    }
    push @args, $arg if defined($arg);
    return @args;
}

#=============================================================================
# Term::Shell error messages
#=============================================================================
sub msg_ambiguous_cmd {
    my ($o, $cmd, @c) = @_;
    local $" = "\n\t";
    <<END;
Ambiguous command '$cmd': possible commands:
	@c
END
}

sub msg_unknown_cmd {
    my ($o, $cmd) = @_;
    <<END;
Unknown command '$cmd'; type 'help' for a list of commands.
END
}

#=============================================================================
# Term::Shell private methods
#=============================================================================
sub do_action {
    my $o = shift;
    my $cmd = shift;
    my $args = shift || [];
    my $type = shift || 'run';
    my $handler = $o->handler($cmd, $type, $args);
    $o->{command}{$type} = {
	name	=> $cmd,
	found	=> defined $handler ? 1 : 0,
	handler	=> $handler,
    };
    if (defined $handler) {
	# We've found a handler. Set up a value which will call the postcmd()
	# action as the subroutine leaves. Then call the precmd(), then return
	# the result of running the handler.
	$o->precmd(\$handler, \$cmd, $args);
	my $postcmd = Term::Shell::OnScopeLeave->new(sub {
	    $o->postcmd(\$handler, \$cmd, $args);
	});
	return $o->$handler(@$args);
    }
}

sub handler {
    my $o = shift;
    my ($command, $type, $args, $preserve_args) = @_;

    # First try finding the standard handler, then fallback to the
    # catch_$type method. The columns represent "action", "type", and "push",
    # which control whether the name of the command should be pushed onto the
    # args.
    my @tries = (
	[$command, $type, 0],
	[$o->cmd_prefix . $type . $o->cmd_suffix, 'catch', 1],
    );

    # The user can control whether or not to search for "unique" matches,
    # which means calling $o->possible_actions(). We always look for exact
    # matches.
    my @matches = qw(exact_action);
    push @matches, qw(possible_actions) if $o->{API}{match_uniq};

    for my $try (@tries) {
	my ($cmd, $type, $add_cmd_name) = @$try;
	for my $match (@matches) {
	    my @handlers = $o->$match($cmd, $type);
	    next unless @handlers;
	    unshift @$args, $command
		if $add_cmd_name and not $preserve_args;
	    return $o->unalias($handlers[0], $type)
	}
    }
    return undef;
}

sub uniq {
    my $o = shift;
    my %seen;
    $seen{$_}++ for @_;
    my @ret;
    for (@_) { push @ret, $_ if $seen{$_}-- == 1 }
    @ret;
}

sub completions {
    my $o = shift;
    my $action = shift;
    my $compls = shift || [];
    my $casei  = shift;
    $casei = $o->{API}{case_ignore} unless defined $casei;
    $casei = $casei ? '(?i)' : '';
    return grep { $_ =~ /$casei^\Q$action\E/ } @$compls;
}

sub possible_actions {
    my $o = shift;
    my $action = shift;
    my $type = shift;
    my $strip = shift || 0;
    my $casei = $o->{API}{case_ignore} ? '(?i)' : '';
    my @keys =	grep { $_ =~ /$casei^\Q$action\E/ } 
		grep { exists $o->{handlers}{$_}{$type} }
		keys %{$o->{handlers}};
    return @keys if $strip;
    return map { "${type}_$_" } @keys;
}

sub exact_action {
    my $o = shift;
    my $action = shift;
    my $type = shift;
    my $strip = shift || 0;
    my $casei = $o->{API}{case_ignore} ? '(?i)' : '';
    my @key = grep { $action =~ /$casei^\Q$_\E$/ } keys %{$o->{handlers}};
    return () unless @key == 1;
    return () unless exists $o->{handlers}{$key[0]}{$type};
    my $handler = $o->{handlers}{$key[0]}{$type};
    $handler =~ s/\Q${type}_\E// if $strip;
    return $handler;
}

sub is_alias {
    my $o = shift;
    my $action = shift;
    exists $o->{handlers}{$action}{alias} ? 1 : 0;
}

sub has_aliases {
    my $o = shift;
    my $action = shift;
    my @a = $o->get_aliases($action);
    @a ? 1 : 0;
}

sub get_aliases {
    my $o = shift;
    my $action = shift;
    my @a = eval {
	my $hndlr = $o->{handlers}{$action}{alias};
	return () unless $hndlr;
	$o->$hndlr();
    };
    $o->{aliases}{$_} = $action for @a;
    @a;
}

sub unalias {
    my $o = shift;
    my $alias = shift;
    my $type  = shift;
    return $alias unless $type;
    my @stuff = split '_', $alias;
    $stuff[1] ||= '';
    return $alias unless $stuff[0] eq $type;
    return $alias unless exists $o->{aliases}{$stuff[1]};
    return $type . '_' . $o->{aliases}{$stuff[1]};
}

sub find_handlers {
    my $o = shift;
    my $pkg = shift || $o->{API}{class};

    # Find the handlers in the given namespace:
    my %handlers;
    {
	no strict 'refs';
	my @r = keys %{ $pkg . "::" };
	$o->add_handlers(@r);
    }

    # Find handlers in its base classes.
    {
	no strict 'refs';
	my @isa = @{ $pkg . "::ISA" };
	for my $pkg (@isa) {
	    $o->find_handlers($pkg);
	}
    }
}

sub rl_complete {
    my $o = shift;
    my ($word, $line, $start) = @_;

    # If it's a command, complete 'run_':
    if ($start == 0 or substr($line, 0, $start) =~ /^\s*$/) {
	my @compls = $o->complete('', $word, $line, $start);
	return @compls if $o->{command}{comp}{found};
    }

    # If it's a subcommand, send it to any custom completion function for the
    # function:
    else {
	my $command = ($o->line_parsed($line))[0];
	my @compls = $o->complete($command, $word, $line, $start);
	return @compls if $o->{command}{comp}{found};
    }

    ()
}

#=============================================================================
# Manually add & remove handlers
#=============================================================================
sub add_handlers {
    my $o = shift;
    for my $hnd (@_) {
	next unless $hnd =~ /^(run|help|smry|comp|catch|alias)_/o;
	my $t = $1;
	my $a = substr($hnd, length($t) + 1);
	# Add on the prefix and suffix if the command is defined
	if (length $a) {
	    substr($a, 0, 0) = $o->cmd_prefix;
	    $a .= $o->cmd_suffix;
	}
	$o->{handlers}{$a}{$t} = $hnd;
	if ($o->has_aliases($a)) {
	    my @a = $o->get_aliases($a);
	    for my $alias (@a) {
		substr($alias, 0, 0) = $o->cmd_prefix;
		$alias .= $o->cmd_suffix;
		$o->{handlers}{$alias}{$t} = $hnd;
	    }
	}
    }
}

sub remove_handlers {
    my $o = shift;
    for my $hnd (@_) {
	next unless $hnd =~ /^(run|help|smry|comp|catch|alias)_/o;
	my $t = $1;
	my $a = substr($hnd, length($t) + 1);
	# Add on the prefix and suffix if the command is defined
	if (length $a) {
	    substr($a, 0, 0) = $o->cmd_prefix;
	    $a .= $o->cmd_suffix;
	}
	delete $o->{handlers}{$a}{$t};
    }
}

sub remove_commands {
    my $o = shift;
    for my $name (@_) {
	delete $o->{handlers}{$name};
    }
}

#=============================================================================
# Two action handlers provided by default: help and exit.
#=============================================================================
sub smry_exit { "exits the program" }
sub help_exit {
    <<'END';
Exits the program.
END
}
sub run_exit {
    my $o = shift;
    $o->stoploop;
}

sub smry_help { "prints this screen, or help on 'command'" }
sub help_help {
    <<'END'
Provides help on commands...
END
}
sub comp_help {
    my ($o, $word, $line, $start) = @_;
    my @words = $o->line_parsed($line);
    return []
      if (@words > 2 or @words == 2 and $start == length($line));
    sort $o->possible_actions($word, 'help', 1);
}
sub run_help {
    my $o = shift;
    my $cmd = shift;
    if ($cmd) {
	my $txt = $o->help($cmd, @_);
	if ($o->{command}{help}{found}) {
	    $o->page($txt)
	}
	else {
	    my @c = sort $o->possible_actions($cmd, 'help', 1);
	    if (@c) {
		local $" = "\n\t";
		print <<END;
Ambiguous help topic '$cmd': possible help topics:
	@c
END
	    }
	    else {
		print <<END;
Unknown help topic '$cmd'; type 'help' for a list of help topics.
END
	    }
	}
    }
    else {
	print "Type 'help command' for more detailed help on a command.\n";
	my (%cmds, %docs);
	my %done;
	my %handlers;
	for my $h (keys %{$o->{handlers}}) {
	    next unless length($h);
	    next unless grep{defined$o->{handlers}{$h}{$_}} qw(run smry help);
	    my $dest = exists $o->{handlers}{$h}{run} ? \%cmds : \%docs;
	    my $smry = exists $o->{handlers}{$h}{smry}
		? $o->summary($h)
		: "undocumented";
	    my $help = exists $o->{handlers}{$h}{help}
		? (exists $o->{handlers}{$h}{smry}
		    ? ""
		    : " - but help available")
		: " - no help available";
	    $dest->{"    $h"} = "$smry$help";
	}
	print "  Commands:\n" if %cmds;
	$o->print_pairs(
	    [sort keys %cmds], [map {$cmds{$_}} sort keys %cmds], ' - ', 1
	);
	print "  Extra Help Topics: (not commands)\n" if %docs;
	$o->print_pairs(
	    [sort keys %docs], [map {$docs{$_}} sort keys %docs], ' - ', 1
	);
    }
}

sub comp_ {
    my ($o, $word, $line, $start) = @_;
    my @comp = grep { length($_) } sort $o->possible_actions($word, 'run', 1);
    return @comp;
}

package Term::Shell::OnScopeLeave;

sub new {
    return bless [@_[1 .. $#_]], ref($_[0]) || $_[0];
}

sub DESTROY {
    my $o = shift;
    for my $c (@$o) {
	&$c;
    }
}

1;
