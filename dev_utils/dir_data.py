ROOT_DIR_EXTENSIONS = {
    "include/"  : [".hpp", ".ipp"],
    "src/"      : [".cpp"],
    "tests/src/": [".cpp"],
}


def is_unit_file_extension(extension):
    for root_dir, unit_file_extensions in ROOT_DIR_EXTENSIONS.items():
        for unit_file_extension in unit_file_extensions:
            if extension == unit_file_extension:
                return True

    return False
