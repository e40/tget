Summary: T0rrent Get
Name: tget
Version: %{version}
Release: %{release}
License: Mozilla
Group: Applications/Internet
#URL: 
Source0: %{name}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root

# Don't attempt to make the debuginfo package
%define debug_package %{nil}

%description
T0rrent Get.  Similar to flexget, but better in some ways.


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
* Sat Mar 23 2013 Kevin Layer <layer@relay> - 
- Initial build.

