def directify(directory):
    def is_valid_directory(directory):
        return directory[-1] == "/"

    return directory if is_valid_directory(directory) else directory + "/"
