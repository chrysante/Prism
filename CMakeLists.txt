cmake_minimum_required(VERSION 3.15)

project(Prism VERSION 1.0)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

include(cmake/CPM.cmake)

function(enable_sanitizers target)
    if(CMAKE_CXX_COMPILER_ID MATCHES "Clang" OR CMAKE_CXX_COMPILER_ID MATCHES "GNU")
        target_compile_options(${target} PRIVATE 
            $<$<CONFIG:Debug>:-fsanitize=address,undefined -fno-omit-frame-pointer>)
        target_link_options(${target} PRIVATE 
            $<$<CONFIG:Debug>:-fsanitize=address,undefined -fno-omit-frame-pointer>)
    endif()
endfunction()

function(create_empty_file filename)
    get_filename_component(dir ${filename} DIRECTORY)
    file(MAKE_DIRECTORY ${dir})
    file(TOUCH ${filename})
endfunction()

function(add_code_generator target definitions template output filters)
    set(output ${CMAKE_BINARY_DIR}/${output})
    create_empty_file(${output})

    target_sources(${target} PRIVATE ${definitions} ${template} ${filters} ${output})
    source_group(TREE ${CMAKE_SOURCE_DIR} FILES ${definitions} ${template} ${filters})
    source_group(TREE ${CMAKE_BINARY_DIR} FILES ${output})

    string(CONCAT command_source
      "${CMAKE_SOURCE_DIR}/generator/source_generator.sh "
      "--definitions ${CMAKE_SOURCE_DIR}/${definitions} "
      "--template ${CMAKE_SOURCE_DIR}/${template} "
      "--output ${output} "
      "--filters ${CMAKE_SOURCE_DIR}/${filters}")

    add_custom_command(
        TARGET ${target}
        PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E env sh -c ${command_source}
        DEPENDS ${definitions} ${template} ${filters}
        VERBATIM)
endfunction()

CPMAddPackage("gh:chrysante/csp#main")
CPMAddPackage("gh:chrysante/utility#main")
CPMAddPackage("gh:chrysante/termfmt#main")
CPMAddPackage("gh:ericniebler/range-v3#0.12.0")
CPMAddPackage("gh:Neargye/magic_enum@0.9.7")
CPMAddPackage("gh:catchorg/Catch2@3.7.1")

set(PRISM_HEADER_FILES 
    include/Prism/Ast/Ast.h
    include/Prism/Ast/AstFwd.h
    include/Prism/Ast/AstDump.h
    include/Prism/Ast/Ast.def
    include/Prism/Ast/Operators.h

    include/Prism/Common/Allocator.h
    include/Prism/Common/Assert.h
    include/Prism/Common/EnumUtil.h
    include/Prism/Common/Functional.h
    include/Prism/Common/IndentingStreambuf.h
    include/Prism/Common/Issue.h
    include/Prism/Common/IssueHandler.h
    include/Prism/Common/MacroUtils.h
    include/Prism/Common/MacroUtilsUndef.h
    include/Prism/Common/NoParent.h
    include/Prism/Common/Rtti.h
    include/Prism/Common/TreeFormatter.h

    include/Prism/Lexer/Lexer.h
    include/Prism/Lexer/LexicalIssue.h
    
    include/Prism/Parser/Parser.h
    include/Prism/Parser/SyntaxIssue.h
    include/Prism/Parser/SyntaxIssue.def

    include/Prism/Ast/Facet.def
    include/Prism/Ast/Facet.h

    include/Prism/Source/SourceContext.h
    include/Prism/Source/SourceLocation.h
    include/Prism/Source/Token.h
    include/Prism/Source/Token.def
)

set(PRISM_SOURCE_FILES
    src/Prism/Ast/Ast.cpp
    src/Prism/Ast/AstDump.cpp

    src/Prism/Common/Allocator.cpp
    src/Prism/Common/IndentingStreambuf.cpp
    src/Prism/Common/Issue.cpp
    src/Prism/Common/IssueHandler.cpp
    src/Prism/Common/SyntaxMacros.h
    src/Prism/Common/TreeFormatter.cpp

    src/Prism/Lexer/Lexer.cpp
    src/Prism/Lexer/LexicalIssue.cpp
    
    src/Prism/Parser/Grammar.txt
    src/Prism/Parser/Parser.cpp
    src/Prism/Parser/SyntaxIssue.cpp

    src/Prism/Ast/Facet.cpp

    src/Prism/Source/SourceContext.cpp
    src/Prism/Source/SourceLocation.cpp
    src/Prism/Source/Token.cpp
)

set(PRISM_TEST_FILES 
    test/Prism/Common/Allocator.t.cpp 
    test/Prism/Lexer/Lexer.t.cpp 
    test/Prism/Parser/Parser.t.cpp 
    test/Prism/Parser/ParserTestUtils.h
    test/Prism/Parser/ParserTestUtils.cpp
)

add_library(Prism SHARED)
enable_sanitizers(Prism)

add_code_generator(Prism
                   src/Prism/Ast/Facets.yaml 
                   src/Prism/Ast/FacetClassDef.jinja 
                   include/Prism/Ast/Facets.h
                   src/Prism/Ast/FacetFilters.py)

target_sources(Prism 
  PRIVATE 
    ${PRISM_HEADER_FILES}
    ${PRISM_SOURCE_FILES}
)

source_group(TREE ${CMAKE_SOURCE_DIR} FILES ${PRISM_SOURCE_FILES} ${PRISM_HEADER_FILES})

target_link_libraries(Prism 
  PUBLIC
    range-v3 
    csp 
    utility
    magic_enum
  PRIVATE
    termfmt
)

target_include_directories(Prism PUBLIC include PRIVATE src)

add_executable(PrismTest)
enable_sanitizers(PrismTest)

target_sources(PrismTest
  PRIVATE  
    ${PRISM_TEST_FILES}
) 

source_group(TREE ${CMAKE_SOURCE_DIR} FILES ${PRISM_TEST_FILES})

target_link_libraries(PrismTest
  PRIVATE
    Catch2::Catch2WithMain
    Prism
    termfmt
)

target_include_directories(PrismTest PRIVATE test)

add_executable(Playground)
enable_sanitizers(Playground)

target_sources(Playground
  PRIVATE  
    src/Playground/Playground.cpp
    examples/Playground.prism
)

target_link_libraries(Playground
  PRIVATE
    Prism
)
