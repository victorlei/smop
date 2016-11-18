FCN_FILE_DIRS += pkg

pkg_PRIVATE_FCN_FILES = \
  pkg/private/build.m \
  pkg/private/configure_make.m \
  pkg/private/copy_files.m \
  pkg/private/create_pkgadddel.m \
  pkg/private/default_prefix.m \
  pkg/private/describe.m \
  pkg/private/dirempty.m \
  pkg/private/extract_pkg.m \
  pkg/private/finish_installation.m \
  pkg/private/fix_depends.m \
  pkg/private/fix_version.m \
  pkg/private/generate_lookfor_cache.m \
  pkg/private/get_description.m \
  pkg/private/get_forge_download.m \
  pkg/private/get_forge_pkg.m \
  pkg/private/getarch.m \
  pkg/private/getarchdir.m \
  pkg/private/getarchprefix.m \
  pkg/private/get_unsatisfied_deps.m \
  pkg/private/install.m \
  pkg/private/installed_packages.m \
  pkg/private/is_architecture_dependent.m \
  pkg/private/list_forge_packages.m \
  pkg/private/load_package_dirs.m \
  pkg/private/load_packages.m \
  pkg/private/load_packages_and_dependencies.m \
  pkg/private/packinfo_copy_file.m \
  pkg/private/parse_pkg_idx.m \
  pkg/private/prepare_installation.m \
  pkg/private/print_package_description.m \
  pkg/private/rebuild.m \
  pkg/private/repackage.m \
  pkg/private/save_order.m \
  pkg/private/shell.m \
  pkg/private/uninstall.m \
  pkg/private/unload_packages.m \
  pkg/private/verify_directory.m \
  pkg/private/write_index.m


pkg_FCN_FILES = \
  pkg/pkg.m \
  $(pkg_PRIVATE_FCN_FILES)

FCN_FILES += $(pkg_FCN_FILES)

PKG_ADD_FILES += pkg/PKG_ADD

DIRSTAMP_FILES += pkg/$(octave_dirstamp)
