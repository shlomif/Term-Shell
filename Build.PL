use strict;
use warnings;

use lib "./inc";

use Test::Run::Builder;

my $builder = Test::Run::Builder->new(
    module_name         => 'Term::Shell',
    license             => 'perl',
    dist_author         => q{Shlomi Fish <shlomif@cpan.org>},
    dist_version_from   => 'lib/Term/Shell.pm',
    requires =>
    {
        'Data::Dumper' => 0,
        'Term::ReadLine' => 0,
        'strict' => 0,
        'warnings' => 0,
    },
    build_requires =>
    {
        'Test' => 0,
        'Test::More' => 0,
        'vars' => 0,
    },
    configure_requires =>
    {
        'Module::Build' => 0,
    },
    add_to_cleanup      => [ 'Term-Shell-*' ],
    create_makefile_pl => 'traditional',
    meta_merge =>
    {
        resources =>
        {
            repository => "https://github.com/shlomif/Term-Shell",
        },
        keywords =>
        [
            'console',
            'readline',
            'shell',
            'term',
            'terminal',
        ],
    },
);

$builder->create_build_script();
