cmake_minimum_required(VERSION 3.15)

project(Prism VERSION 1.0)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

include(cmake/CPM.cmake)

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
    include/Prism/Common/TreeFormatter.h

    include/Prism/Lexer/Lexer.h
    include/Prism/Lexer/LexicalIssue.h
    
    include/Prism/Parser/Parser.h
    include/Prism/Parser/SyntaxIssue.h

    include/Prism/ParseTree/ParseTree.def
    include/Prism/ParseTree/ParseTree.h

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
    src/Prism/Common/TreeFormatter.cpp

    src/Prism/Lexer/Lexer.cpp
    src/Prism/Lexer/LexicalIssue.cpp
    
    src/Prism/Parser/Parser.cpp
    src/Prism/Parser/SyntaxIssue.cpp

    src/Prism/ParseTree/ParseTree.cpp

    src/Prism/Source/SourceContext.cpp
    src/Prism/Source/SourceLocation.cpp
    src/Prism/Source/Token.cpp
)

set(PRISM_TEST_FILES 
    test/Prism/Lexer/Lexer.t.cpp 
)

add_library(Prism SHARED)

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

target_sources(PrismTest
  PRIVATE  
    ${PRISM_TEST_FILES}
) 

source_group(TREE ${CMAKE_SOURCE_DIR} FILES ${PRISM_TEST_FILES})

target_link_libraries(PrismTest
  PRIVATE
    Catch2::Catch2WithMain
    Prism
)

add_executable(Playground)

target_sources(Playground
  PRIVATE  
    src/Playground/Playground.cpp
)

target_link_libraries(Playground
  PRIVATE
    Prism
)