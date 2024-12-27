# Copied from FacetFilters.py
def build_inheritance_tree(symbols):
    name_to_symbol = {symbol['name']: symbol for symbol in symbols}
    tree = {}
    root = None
    for symbol in symbols:
        base_class = symbol.get('base', None)
        if base_class:
            if base_class not in tree:
                tree[base_class] = []
            tree[base_class].append(symbol['name'])
        else:
            if symbol['name'] not in tree:
                tree[symbol['name']] = []
            root = symbol

    def render_tree(class_name, depth=0, is_last=True, prefix=''):
        result = '// '
        if depth > 0:
            result += prefix
            if is_last:
                result += '╰╴'
            else:
                result += '├╴'
        result += class_name + '\n'
        if class_name in tree:
            children = tree[class_name]
            new_prefix = '' if depth == 0 else prefix + ('│ ' if not is_last else '  ')
            for i, child in enumerate(children):
                is_last = (i == len(children) - 1)
                result += render_tree(child, depth + 1, is_last=is_last, prefix=new_prefix)
        return result

    return render_tree(root.get('name', None))

filters = {
    'build_inheritance_tree': build_inheritance_tree
}
