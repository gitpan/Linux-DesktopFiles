package Linux::DesktopFiles;

# This module is designed to be pretty fast.
# The best uses of this module is to generate real
# time menus, based on the content of .desktop files.

#use 5.010;
#use strict;
#use warnings;

our $VERSION = '0.06';

sub new {
    my ($class, %opts) = @_;

    my $self = bless {}, $class;

    $self->{home_dir} = delete($opts{home_dir}) || $ENV{HOME} || $ENV{LOGDIR};

    my @default_arguments = qw(
      full_icon_paths
      icon_db_filename
      with_icons
      keep_empty_categories
      categories_case_sensitive
      skip_file_name_re
      skip_app_name_re
      skip_app_command_re
      skip_file_content_re
      clean_command_name_re
      skip_svg_icons
      icon_dirs_first
      icon_dirs_second
      icon_dirs_last
      use_only_my_icon_dirs
      terminalize
      );

    @{$self}{@default_arguments} = delete @opts{@default_arguments};

    $self->{desktop_files_paths} =
        ref $opts{desktop_files_paths} eq 'ARRAY' ? delete($opts{desktop_files_paths})
      : defined($opts{desktop_files_paths}) ? [delete($opts{desktop_files_paths})]
      :                                       [qw(/usr/share/applications)];

    my $default_categories = {
                              utility     => undef,
                              development => undef,
                              education   => undef,
                              game        => undef,
                              graphics    => undef,
                              audiovideo  => undef,
                              network     => undef,
                              office      => undef,
                              settings    => undef,
                              system      => undef,
                             };

    $self->{keys_to_keep} =
        ref $opts{keys_to_keep} eq 'ARRAY' ? delete($opts{keys_to_keep})
      : defined($opts{keys_to_keep}) ? [delete($opts{keys_to_keep})]
      :                                [qw(Exec Name), $self->{with_icons} ? qw(Icon) : ()];

    $self->{file_keys_re} = do {
        my @keys = map quotemeta, do {
            my %seen;
            grep !$seen{$_}++, @{$self->{keys_to_keep}}, qw(Hidden NoDisplay Categories),
              $self->{terminalize} ? qw(Terminal) : ();
        };
        local $" = q{|};
        qr/@keys/;
    };

    $self->{categories} =
        ref $opts{categories} eq 'ARRAY' ? {map { $_ => undef } @{delete($opts{categories})}}
      : defined $opts{categories} ? {delete($opts{categories}) => 1}
      :                             $default_categories;

    $self->{true_value} =
        ref $opts{true_value} eq 'ARRAY' ? {map { $_ => 1 } @{delete($opts{true_value})}}
      : defined $opts{true_value} ? {delete($opts{true_value}) => 1}
      : {
         true => 1,
         True => 1,
         1    => 1
        };

    # Normalize categories
    if (not $self->{categories_case_sensitive}) {
        foreach my $key (keys %{$self->{categories}}) {
            my $category = lc $key;
            $category =~ tr/_a-z/_/c;
            $self->{categories}{$category} = delete($self->{categories}{$key});
        }
    }

    $self->{gtk_rc_filename} = delete($opts{gtk_rc_filename}) // "$self->{home_dir}/.gtkrc-2.0";
    $self->{terminal} = delete($opts{terminal}) || $ENV{TERM};

    if (defined $self->{icon_db_filename} and $self->{with_icons} and $self->{full_icon_paths}) {
        $self->init_icon_database() || warn "Can't open/create database '$self->{icon_db_filename}': $!";
    }

    foreach my $key (keys %opts) {
        warn "$0: Invalid option: $key\n";
    }

    return $self;
}

sub iterate_desktop_files {
    my ($self, $code) = @_;

    foreach my $dir (@{$self->{desktop_files_paths}}) {
        opendir(my $dir_h, $dir) or next;
        foreach my $file (readdir $dir_h) {
            $self->$code("$dir/$file") if substr($file, -8) eq '.desktop';
        }
        closedir $dir_h;
    }
}

sub get_desktop_files {
    my ($self) = @_;
    $self->iterate_desktop_files(sub { push @{$self->{DESKTOP_FILES}}, $_[1] });
    wantarray ? @{$self->{DESKTOP_FILES}} : $self->{DESKTOP_FILES};
}

sub get_icon_theme_name {
    my ($self) = @_;

    if (-r $self->{gtk_rc_filename}) {
        if (sysopen my $fh, $self->{gtk_rc_filename}, 0) {
            my $content;
            sysread $fh, $content, -s _;
            if ($content =~ /^\s*gtk-icon-theme-name\s*=\s*["']?([^'"\r\n]+)/im) {
                $self->{icon_theme} = $1;
            }
            close $fh;
        }
    }
    $self->{icon_theme} //= '';
}

sub get_icon_path {
    my ($self, $icon_name) = @_;

    if (defined($icon_name) and $icon_name ne q{}) {
        if (chr ord $icon_name eq '/') {
            return $icon_name if -f $icon_name;
        }
        else {
            $icon_name =~ s/\.\w{3}$//;
            return $icon_name unless $self->{full_icon_paths};
        }
    }
    else {
        return q{};
    }

    my $icon = $self->{icons_db}{$icon_name};

    if (not defined $icon) {

        if (not defined $self->{icon_theme}) {
            $self->get_icon_theme_name() if not $self->{use_only_my_icon_dirs};
        }

        my @icon_dirs;
        if (defined $self->{icon_dirs_first}
            and ref $self->{icon_dirs_first} eq 'ARRAY') {
            push @icon_dirs, grep -d, @{$self->{icon_dirs_first}};
        }

        if (length $self->{icon_theme} and not $self->{use_only_my_icon_dirs}) {
            foreach my $icon_dir (
                                  "/usr/share/icons/$self->{icon_theme}/",
                                  "$self->{home_dir}/.icons/$self->{icon_theme}/",
                                  "$self->{home_dir}/.local/share/icons/$self->{icon_theme}/"
              ) {
                push @icon_dirs, $icon_dir if -d $icon_dir;
            }
        }

        if (defined $self->{icon_dirs_second}
            and ref $self->{icon_dirs_second} eq 'ARRAY') {
            push @icon_dirs, grep -d, @{$self->{icon_dirs_second}};
        }

        if (not $self->{use_only_my_icon_dirs}) {
            push @icon_dirs, grep -d,
              '/usr/share/pixmaps/',                   '/usr/share/icons/hicolor/',
              "$self->{home_dir}/.local/share/icons/", "$self->{home_dir}/.icons/";
        }

        if (defined $self->{icon_dirs_last}
            and ref $self->{icon_dirs_last} eq 'ARRAY') {
            push @icon_dirs, grep -d, @{$self->{icon_dirs_last}};
        }

        s{^.+\K/$}{} for @icon_dirs;    # remove the very last slash

        require File::Find;
        File::Find::find(
            {
             wanted => sub {
                 return if substr($File::Find::name, -4, 1) ne q{.};
                 return if substr($_, -4, 4, q{}) eq '.svg' and $self->{skip_svg_icons};
                 return if exists $self->{icons_db}{$_};
                 $self->{icons_db}{$_} = $File::Find::name;
             },
            } => do { my %seen; grep !$seen{$_}++ => @icon_dirs }
        );
    }

    unless (defined $icon) {
        $icon = $self->{icons_db}{$icon_name};
        unless (defined $icon) {
            $self->{icons_db}{$icon_name} = '';
            $icon = '';
        }
    }

    $icon;    # return the icon
}

sub parse_desktop_file {
    my ($self, $file) = @_;

    $self->_clean_categories();
    $self->_store_info(-f $file ? $file : return);

    foreach my $value (values %{$self->{categories}}) {
        return $value->[0] if ref $value eq 'ARRAY';
    }

    return;
}

sub _store_info {
    my $self = shift;

    foreach my $desktop_file (@_) {

        if (defined $self->{skip_file_name_re}) {
            next if substr($desktop_file, rindex($desktop_file, '/') + 1) =~ /$self->{skip_file_name_re}/o;
        }

        my $file;
        sysopen my $desktop_fh, $desktop_file, 0 or next;
        sysread $desktop_fh, $file, -s $desktop_file;

        if ((my $index = index($file, "]\n", index($file, "[Desktop Entry]") + 15)) != -1) {
            $file = substr($file, 0, $index);
        }

        my %info = $file =~ m{^($self->{file_keys_re})=(.*\S)\s*$}gm;

        if (defined $info{NoDisplay}) {
            next if exists $self->{true_value}{$info{NoDisplay}};
        }

        if (defined $info{Hidden}) {
            next if exists $self->{true_value}{$info{Hidden}};
        }

        my @categories = split(/;/, $info{Categories} // next);

        my $found_cat = 0;
        foreach my $category (@categories) {
            if (not $self->{categories_case_sensitive}) {
                $category = lc $category;
                $category =~ tr/_a-z/_/c;
            }
            elsif ($found_cat) {
                last;
            }
            ++$found_cat if not $found_cat and exists $self->{categories}{$category};
        }
        next if not $found_cat;

        if (defined $self->{skip_file_content_re}) {
            next if $file =~ /$self->{skip_file_content_re}/o;
        }

        if (defined $self->{clean_command_name_re}) {
            $info{Exec} =~ s/$self->{clean_command_name_re}//go;
        }

        if (defined $self->{skip_app_command_re}) {
            next if $info{Exec} =~ /$self->{skip_app_command_re}/o;
        }

        $info{Exec} =~ s/ +%.*//s if index($info{Exec}, q{ %}) != -1;

        if (defined $self->{skip_app_name_re}) {
            next if $info{Name} =~ /$self->{skip_app_name_re}/o;
        }

        if (    $self->{terminalize}
            and defined $info{Terminal}
            and exists $self->{true_value}{$info{Terminal}}) {
            $info{Exec} = qq[$self->{terminal} -e '$info{Exec}'];
        }

        if ($self->{with_icons}) {
            $info{Icon} = $self->get_icon_path($info{Icon});
        }

        foreach my $category (@categories) {
            next unless exists $self->{categories}{$category};
            push @{$self->{categories}{$category}}, {map { $_ => $info{$_} } @{$self->{keys_to_keep}}};
        }
    }
    1;
}

sub _clean_categories {
    my ($self) = @_;
    @{$self->{categories}}{keys %{$self->{categories}}} = ();
}

sub get_categories {
    my ($self) = @_;

    return $self->{keep_empty_categories}
      ? {%{$self->{categories}}}
      : {
         map { $_ => $self->{categories}{$_} }
         grep ref $self->{categories}{$_} eq 'ARRAY', keys %{$self->{categories}}
        };
}

sub parse_desktop_files {
    my ($self) = @_;

    $self->_clean_categories();
    $self->iterate_desktop_files(\&_store_info);
    return $self->get_categories();
}

sub init_icon_database {
    my ($self) = @_;

    require GDBM_File;
    GDBM_File->import;
    tie %{$self->{icons_db}}, 'GDBM_File', $self->{icon_db_filename}, &GDBM_WRCREAT, 0640;
}

1;

__END__

=head1 NAME

Linux::DesktopFiles - Get and parse the Linux .desktop files.

=head1 SYNOPSIS

  use Linux::DesktopFiles;
  my $obj = Linux::DesktopFiles->new( terminalize => 1 );
  print join "\n", $obj->get_desktop_files;
  my $hash_ref = $obj->parse_desktop_files;

=head1 DESCRIPTION

The C<Linux::DesktopFiles> is a very simple module to parse .desktop files.

=head1 CONSTRUCTOR METHODS

The following constructor methods are available:

=over 4

=item $obj = Linux::DesktopFiles->new( %options )

This method constructs a new C<Linux::DesktopFiles> object and returns it.
Key/value pair arguments may be provided to set up the initial state.
The following options correspond to attribute methods described below:

   KEY                         DEFAULT
   -----------                 --------------------
   with_icons                  0
   full_icon_paths             0
   skip_svg_icons              0
   icon_db_filename            undef
   icon_dirs_first             undef
   icon_dirs_second            undef
   icon_dirs_last              undef

   categories_case_sensitive   0
   keep_empty_categories       0
   use_only_my_icon_dirs       0
   terminalize                 0
   terminal                    $ENV{TERM}
   home_dir                    $ENV{HOME}
   gtk_rc_filename             "~/.gtkrc-2.0"
   true_value                  ['true', 'True', '1']

   skip_file_name_re           undef
   skip_app_name_re            undef
   skip_app_command_re         undef
   skip_file_content_re        undef
   clean_command_name_re       undef

   desktop_files_paths         ['/usr/share/applications']
   keys_to_keep                ["Name", "Exec"]
   categories                  [qw( utility
                                    development
                                    education
                                    game
                                    graphics
                                    audiovideo
                                    network
                                    office
                                    settings
                                    system )
                               ]

=back

=head2 Main options

=over 4

=item desktop_files_paths => ['dir1', 'dir2', ...]

Set directories where to find the .desktop files (default: /usr/share/applications)

=item keys_to_keep => [qw(Icon Exec Name Comment ...)]

Any of the valid keys from .desktop files. This keys will be stored in the retured
hash reference when calling C<$obj-E<gt>parse_desktop_files>.

=item categories => [qw(Graphics Network AudioVideo ...)]

Any of the valid categories from the .desktop files. Any category not listed,
will be ignored.

=back

=head2 Other options

=over 4

=item keep_empty_categories => 1

If a category is empty, keep it in the returned hash reference when
I<parse_desktop_files> is called.

=item categories_case_sensitive => 1

Make categories case sensitive. By default, they are case insensitive
in a way that "X-XFCE4" is equivalent to "x_xfce4".

=item terminalize => 1

When B<Terminal> is true, modify the B<Exec> value to something like:
I<terminal -e 'command'>

=item terminal => "xterm"

This terminal will be used when I<terminalize> is set to a true value.

=item home_dir => "/home/dir"

Set the home directory. This value is used to locate icons in the ~/.local/share/icons.

=item gtk_rc_filename => "/path/to/.gtkrc-x.x"

This file is used to get the icon theme name from it. (default: ~/.gtkrc-2.0)
I<NOTE:> It works with Gtk3 as well.

=item true_value => [qw(1 true True)]

This values are used to test for I<true> some values from the .desktop files.

=back

=head2 Icon options

=over 4

=item with_icons => 1

Require icons. Unless I<full_icon_paths> is set to a true value, this
option will return icon names without the extension. If an B<Icon> value
is an absolute path to an icon in the system, it will be returned as it is.

=item full_icon_paths => 1

Full icon paths for B<Icon> values.

=item icon_db_filename => "filename.db"

GDBM database name used to store icon names as keys and icon paths as
values for a faster lookup (used with GDBM_File).
I<NOTE:> Works in combination with B<full_icon_paths> and B<with_icons>

=item skip_svg_icons => 1

Ignore .svg icons when looking for full icon paths.

=item icon_dirs_first => [dir1, dir2, ...]

When looking for full icon paths, look in this directories first, before
looking in the directories of the current icon theme.

=item icon_dirs_second => [dir1, dir2, ...]

When looking for full icon paths, look in this directories as a second
icon theme. (Before I</usr/share/pixmaps>)

=item icon_dirs_last => [dir1, dir2, ...]

Look in this directories at the very last, after looked in I</usr/share/pixmaps>,
I</usr/share/icons/hicolor> and some other directories.

=item use_only_my_icon_dirs => 1

Be very strict and use only the directories specified by you in either
one of I<icon_dirs_first>, I<icon_dirs_second> and/or I<icon_dirs_last>

=back

=head2 Regex options

=over 4

=item skip_file_name_re => qr/regex/

Skip .desktop files if their file names will match the regex.
I<NOTE:> File names are from the last slash to the end.

=item skip_app_name_re => qr/regex/

Skip .desktop files based on the value of B<Name>.

=item skip_app_command_re => qr/regex/

Skip .desktop files based on the value of B<Exec>.

=item skip_file_content_re => qr/regex/

Skip .desktop files if the regex matches anywhere in the [Desktop Entry]
section.

=item clean_command_name_re =>  qr/regex/

Anything matched by this regex in the values of B<Exec> will be replaced
with nothing.

=back

=head1 SUBROUTINES/METHODS

=over 4

=item $obj->iterate_desktop_files(\&code_ref)

Iterate over desktop files, one file at a time.

=item $obj->get_desktop_files()

Get all desktop files. In list context it returns a list, but in scalar context,
it returns an array reference containing the full names of the desktop files.

=item $obj->get_icon_theme_name()

Returns the icon theme name, if any, otherwise it returns an empty string.

=item $obj->get_icon_path("icon_name")

If I<full_icon_paths> is set to a true value, it returns the absolute path of
a icon name located in the system. If it can't found the icon name, it returns
an empty string.
If I<full_icon_paths> is set to a false value, it strips the extension name of
the icon (if any), and returns the icon name. If the icon name is undefined, it
returns an empty string.

=item $obj->parse_desktop_file("filename")

It returns a HASH reference which contains the I<keys_to_keep> and the values
from the desktop file specified as an argument.

=item $obj->parse_desktop_files()

It returns a HASH reference which categories names as keys, and ARRAY references
as values which contains HASH references with the keys specified in the I<keys_to_keep>
option, and values from the .desktop files.

The returned HASH reference might look something like this:

        {
          utility => [ {Exec => "...", Name => "..."}, {Exec => "...", Name => "..."} ],
          network => [ {Exec => "...", Name => "..."}, {Exec => "...", Name => "..."} ],
        }

=back

=head1 AUTHOR

Trizen, E<lt>trizenx@gmail.comE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2012 by Trizen

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.14.2 or,
at your option, any later version of Perl 5 you may have available.

=head1 SEE ALSO

L<File::DesktopEntry> and L<X11::FreeDesktop::DesktopEntry>

=cut
