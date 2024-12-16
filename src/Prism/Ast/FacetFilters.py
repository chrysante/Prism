def interface_type(type):
    return {
        "TerminalFacet": "Token",
        "AstWrapperFacet": "AstNode*"
    }.get(type, f"{type} const*")

def to_interface_type(type):
    return {
        "TerminalFacet": "->token()",
        "AstWrapperFacet": "->get()"
    }.get(type, "")

def to_stored_type(type):
    return {
        "TerminalFacet": "allocate<TerminalFacet>",
        "AstWrapperFacet": "allocate<AstWrapperFacet>"
    }.get(type, "")


filters = {
    "interface_type": interface_type,
    "to_interface_type": to_interface_type,
    "to_stored_type": to_stored_type
}
