import jinja2
import yaml
import argparse
import importlib.util
import sys
import os

def load_module_from_file(file_path, module_name):
    spec = importlib.util.spec_from_file_location(module_name, file_path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[module_name] = module
    spec.loader.exec_module(module)
    return module

def warning_comment(defn, template):
    defn = os.path.basename(defn)
    template = os.path.basename(template)
    return f"// Warning: This is a generated file. To edit its contents, modify {defn} and {template}\n\n"

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-d", "--definitions", required=True, type=str, help="YAML file with class definitions")
    parser.add_argument("-t", "--template", required=True, type=str, help="jinja2 template file")
    parser.add_argument("-o", "--output", required=True, type=str, help="Name of the generated file")
    parser.add_argument("-f", "--filters", nargs='+', type=str, help="Python files defining filters")
    args = parser.parse_args()

    template_file = os.path.abspath(args.template)
    template_dir = os.path.dirname(template_file)
    template_filename = os.path.basename(template_file)

    loader = jinja2.FileSystemLoader(template_dir)
    env = jinja2.Environment(autoescape=False, loader=loader)

    if args.filters:
        for idx, filter_file in enumerate(args.filters):
            module_name = f"module_{idx}"
            module = load_module_from_file(filter_file, module_name)
            env.filters.update(module.filters)

    temp = env.get_template(template_filename)

    with open(args.definitions, 'r') as f:
        classes = yaml.safe_load(f)

    rendered_text = warning_comment(args.definitions, args.template) + temp.render(classes)

    with open(args.output, 'w') as f:
        f.write(rendered_text)

main()
