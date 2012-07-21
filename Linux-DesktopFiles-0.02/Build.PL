use Module::Build;

my $build = Module::Build->new(
dist_name => 'Linux::DesktopFiles',
dist_version_from => 'lib/Linux/DesktopFiles.pm',
#requires => { 'Some::Module' => 0, },
#recommends => { 'Some::Module_XS' => 0, },
);

$build->create_build_script;
