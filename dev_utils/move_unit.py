import sys
import shutil
import os
import fileinput
import dev_utils.dir_data as dir_data


def check_make_to_dirs(unit_name, from_subdir, to_subdir):
    def check_make_to_dir(to_dir):
        if not os.path.isdir(to_dir):
            os.makedirs(to_dir)

    for root_dir, extensions in dir_data.ROOT_DIR_EXTENSIONS.items():
        for extension in extensions:
            if os.path.isfile(root_dir + from_subdir + unit_name + extension):
                check_make_to_dir(root_dir + to_subdir)


def get_header_paths(unit_name, from_subdir, to_subdir):
    header_paths = {
        "from": [],
        "to": []
    }

    for header_extension in dir_data.ROOT_DIR_EXTENSIONS["include/"]:
        file_name        = unit_name + header_extension
        from_header_path = from_subdir + file_name
        to_header_path   = to_subdir + file_name

        if os.path.isfile("include/" + from_header_path):
            header_paths["from"].append(from_header_path)
            header_paths["to"].append(to_header_path)

    return header_paths


def should_check_source_file(file_path, from_header_paths):
    def needs_includes_updated():
        with open(file_path, "r") as file_handle:
            for line in file_handle:
                for from_header_path in from_header_paths:
                    if from_header_path in line:
                        return True

        return False

    return len(from_header_paths) > 0 and needs_includes_updated()


def update_includes(unit_name, from_subdir, to_subdir):
    header_paths = get_header_paths(unit_name, from_subdir, to_subdir)

    def for_each_source_file(file_cb):
        for root_dir in dir_data.ROOT_DIR_EXTENSIONS:
            for root, subdirs, file_names in os.walk(root_dir):
                for file_name in file_names:
                    file_cb(os.path.join(root, file_name))

    def get_updated_line(line):
        updated_line = line

        for i in range(len(header_paths["from"])):
            from_header_path = header_paths["from"][i]
            to_header_path = header_paths["to"][i]

            if from_header_path in line:
                updated_line = line.replace(from_header_path, to_header_path)
                break

        return updated_line

    def check_update_includes(file_path):
        if should_check_source_file(file_path, header_paths["from"]):
            with fileinput.FileInput(file_path, inplace = True) as file_handle:
                for line in file_handle:
                    print(get_updated_line(line), end = "")

    for_each_source_file(check_update_includes)


def move_file(file_name, from_dir, to_dir):
    shutil.move(from_dir + file_name, to_dir + file_name)
    print("moved \"" + from_dir + file_name + "\" -> \"" + to_dir + file_name + "\"")


def move_files(unit_name, from_subdir, to_subdir):
    for root_dir, extensions in dir_data.ROOT_DIR_EXTENSIONS.items():
        for extension in extensions:
            if os.path.isfile(root_dir + from_subdir + unit_name + extension):
                move_file(
                    unit_name + extension,
                    root_dir + from_subdir,
                    root_dir + to_subdir)


def check_clean_empty_from_subdir(from_subdir):
    for root_dir in dir_data.ROOT_DIR_EXTENSIONS:
        for root, subdirs, files in os.walk(root_dir + from_subdir):
            if not files and not subdirs:
                os.rmdir(root_dir + from_subdir)


def move_unit(unit_name, from_subdir, to_subdir):
    check_make_to_dirs(unit_name, from_subdir, to_subdir)

    update_includes(
        unit_name,
        from_subdir,
        to_subdir)

    move_files(
        unit_name,
        from_subdir,
        to_subdir)

    check_clean_empty_from_subdir(from_subdir)
