
def collect_all_members(cls, classes):
    members = []
    offset = 0
    first = True
    while cls:
        new_members = cls.get('members', [])
        if not first:
            offset += len(new_members)
        first = False
        members = new_members + members
        cls = get_base(cls, classes)
    return members, offset
    
def build_inheritance_tree(facets):
    name_to_facet = {facet['name']: facet for facet in facets}
    tree = {}
    root = None
    for facet in facets:
        base_class = facet.get('base', None)
        if base_class:
            if base_class not in tree:
                tree[base_class] = []
            tree[base_class].append(facet['name'])
        else:
            if facet['name'] not in tree:
                tree[facet['name']] = []
            root = facet

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
