ROOT_DIR_EXTENSIONS = {
    "include/"   : [".hpp", ".ipp"],
    "src/"       : [".cpp"],
    "tests/src/" : [".cpp"],
}


def isSourceFileExtension(extension):
    for rootDir, sourceFileExtensions in ROOT_DIR_EXTENSIONS.items():
        for sourceFileExtension in sourceFileExtensions:
            if extension == sourceFileExtension:
                return True

    return False


def directify(directory):
    def isValidDirectory(directory):
        return directory[-1] == "/"

    return directory if isValidDirectory(directory) else directory + "/"
