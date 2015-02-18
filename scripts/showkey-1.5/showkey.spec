Name: showkey
Version: 1.5
Release: 1
URL: http://www.catb.org/~esr/showkey/
Source0: %{name}-%{version}.tar.gz
License: MIT
Group: Utilities/System
Summary: echo raw keystrokes in a readable form
Packager: Eric S. Raymond <esr@thyrsus.com>
BuildRoot: %{_tmppath}/%{name}-root

%description
All this program does is read keystrokes and spit them back at you in
a simple, printable form.  This is one of these silly little utilities
that you never think you'll need until some unexpected situation pops
up and you have to have it.  Then you get to be mugged by the details
of tty-interface code, yippee.  Never again!

%prep
%setup -q

%build
make %{?_smp_mflags} showkey showkey.1

%install
[ "$RPM_BUILD_ROOT" -a "$RPM_BUILD_ROOT" != / ] && rm -rf "$RPM_BUILD_ROOT"
mkdir -p "$RPM_BUILD_ROOT"%{_bindir}
mkdir -p "$RPM_BUILD_ROOT"%{_mandir}/man1/
cp showkey "$RPM_BUILD_ROOT"%{_bindir}
cp showkey.1 "$RPM_BUILD_ROOT"%{_mandir}/man1/

%clean
[ "$RPM_BUILD_ROOT" -a "$RPM_BUILD_ROOT" != / ] && rm -rf "$RPM_BUILD_ROOT"

%files
%defattr(-,root,root,-)
%{_mandir}/man1/showkey.1*
%{_bindir}/showkey
%doc README

%changelog
* Thu Oct 21 2003 Eric S. Raymond <esr@snark.thyrsus.com> 1.5-1
- Feature added: now displays control-key formulas as well as ASCII memonics.
- Documentation has been significantly upgraded.

* Mon Dec 29 2003 Eric S. Raymond <esr@snark.thyrsus.com> 1.4-1
- RPMs can now be built by non-root user.

* Tue Aug 27 2002 Eric S. Raymond <esr@snark.thyrsus.com> 1.3-1
- Change the implementation to use termios(3) for better portability.
- Only reference portable signals.
