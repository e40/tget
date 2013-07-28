Summary: Torrent Get
Name: tget
Version: %{version}
Release: %{release}
License: Mozilla
Group: Applications/Internet
Requires: transmission-common
Source0: %{name}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root

# Don't attempt to make the debuginfo package
%define debug_package %{nil}

%description
Torrent Get.  Your personal torrent downloader.


%prep
%setup -q

%build
LISP="%{mlisp}" make

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT{/usr/bin,/usr/lib/tget}
cp -rp tget/* $RPM_BUILD_ROOT/usr/lib/tget
ln -s /usr/lib/tget/tget $RPM_BUILD_ROOT/usr/bin/tget

%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
/usr/lib/tget/*
/usr/bin/tget
%doc


%changelog
* Sun Jul 28 2013 me - 
- v1.36: fix name

* Sat Jul 08 2013 me -
- Depend on transmission-common for transmission-remote.

* Sat Mar 23 2013 me - 
- Initial build.

