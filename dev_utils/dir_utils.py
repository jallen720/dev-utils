ROOT_DIR_EXTENSIONS = {
    "include/"  : [".hpp", ".ipp"],
    "src/"      : [".cpp"],
    "tests/src/": [".cpp"],
}


def is_source_file_extension(extension):
    for root_dir, source_file_extensions in ROOT_DIR_EXTENSIONS.items():
        for source_file_extension in source_file_extensions:
            if extension == source_file_extension:
                return True

    return False


def directify(directory):
    def is_valid_directory(directory):
        return directory[-1] == "/"

    return directory if is_valid_directory(directory) else directory + "/"
