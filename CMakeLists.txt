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

function(enable_warnings target)
  set(warning_flags
      -Wall
      -Wextra
      -Wpedantic
      -Wnon-virtual-dtor
      -Wconversion
      -Wsign-conversion
  )
  target_compile_options(${target} PRIVATE ${warning_flags})
endfunction()

function(create_empty_file filename)
    get_filename_component(dir ${filename} DIRECTORY)
    file(MAKE_DIRECTORY ${dir})
    file(TOUCH ${filename})
endfunction()

function(auto_source_group base_dir file file_abs)
    # Match first two segments: e.g., "foo/bar/" from "foo/bar/baz.cpp"
    string(REGEX MATCH "^([^/]+/[^/]+/)" first_segment "${file}")
    if(first_segment)
        source_group(TREE "${base_dir}/${first_segment}" FILES "${file_abs}")
    endif()
endfunction()

function(add_code_generator target definitions template output filters)
    string(REGEX MATCH "^[^/]+/" output_root "${output}")
    set(output_abs ${CMAKE_BINARY_DIR}/${output})
    create_empty_file(${output_abs})
    target_sources(${target} PRIVATE ${definitions} ${template} ${filters} ${output_abs})
    auto_source_group(${CMAKE_SOURCE_DIR} "${definitions}" "${definitions}")
    auto_source_group(${CMAKE_SOURCE_DIR} "${template}" "${template}")
    auto_source_group(${CMAKE_SOURCE_DIR} "${filters}" "${filters}")
    auto_source_group(${CMAKE_BINARY_DIR} "${output}" "${output_abs}")

    string(CONCAT command_source
      "${CMAKE_SOURCE_DIR}/generator/source_generator.sh "
      "--definitions ${CMAKE_SOURCE_DIR}/${definitions} "
      "--template ${CMAKE_SOURCE_DIR}/${template} "
      "--output ${output_abs} "
      "--filters ${CMAKE_SOURCE_DIR}/${filters}")

    add_custom_command(
        OUTPUT ${output_abs}
        COMMAND ${CMAKE_COMMAND} -E env sh -c ${command_source}
        DEPENDS ${definitions} ${template} ${filters}
        VERBATIM)

    target_include_directories(${target} PUBLIC ${CMAKE_BINARY_DIR}/include PRIVATE ${CMAKE_BINARY_DIR}/src)
endfunction()

CPMAddPackage("gh:chrysante/csp#main")
CPMAddPackage("gh:chrysante/utility#3c24fa7dbdabdc645d2455f0dd6f4656fa15a5e6")
CPMAddPackage("gh:chrysante/APMath#main")
CPMAddPackage("gh:chrysante/termfmt#main")
CPMAddPackage("gh:chrysante/graphgen#main")
CPMAddPackage("gh:ericniebler/range-v3#0.12.0")
CPMAddPackage("gh:Neargye/magic_enum@0.9.7")
CPMAddPackage("gh:catchorg/Catch2@3.7.1")
CPMAddPackage("gh:CLIUtils/CLI11@2.5.0")

set(PRISM_HEADER_FILES
    include/Prism/Common/Allocator.h
    include/Prism/Common/Assert.h
    include/Prism/Common/EnumUtil.h
    include/Prism/Common/DebugGraphGen.h
    include/Prism/Common/Functional.h
    include/Prism/Common/IndentingStreambuf.h
    include/Prism/Common/MacroUtils.h
    include/Prism/Common/MacroUtilsUndef.h
    include/Prism/Common/PrettyName.h
    include/Prism/Common/Ranges.h
    include/Prism/Common/Rtti.h
    include/Prism/Common/TreeFormatter.h
    include/Prism/Common/Typename.h

    include/Prism/Diagnostic/Diagnostic.h
    include/Prism/Diagnostic/DiagnosticEmitter.h
    include/Prism/Diagnostic/DiagnosticFormat.h

    include/Prism/Facet/Facet.h

    include/Prism/Invocation/Invocation.h

    include/Prism/Lexer/Lexer.h
    include/Prism/Lexer/LexicalDiagnostic.h
    
    include/Prism/Parser/Parser.h
    include/Prism/Parser/SyntaxError.h
    include/Prism/Parser/SyntaxError.def

    include/Prism/Sema2/Analysis.h
    include/Prism/Sema2/AnalysisContext.h
    include/Prism/Sema2/ConformanceAnalysis.h
    include/Prism/Sema2/Construction.h
    include/Prism/Sema2/ExprAnalysis.h
    include/Prism/Sema2/FunctionAnalysis.h
    include/Prism/Sema2/FuncSig.h
    include/Prism/Sema2/FunctionOverloadMap.h
    include/Prism/Sema2/GenericMatching.h
    include/Prism/Sema2/NameLookup.h
    include/Prism/Sema2/OverloadResolution.h
    include/Prism/Sema2/Scope.h
    include/Prism/Sema2/SemaContext.h
    include/Prism/Sema2/SemaDiagnostic.h
    include/Prism/Sema2/SemaFwd.h
    include/Prism/Sema2/SemaPrint.h
    include/Prism/Sema2/SubContext.h
    include/Prism/Sema2/Symbol.h
    include/Prism/Sema2/TypeLayout.h
#[[
    include/Prism/Sema/Analysis.h
    include/Prism/Sema/AnalysisBase.h
    include/Prism/Sema/Builtins.def
    include/Prism/Sema/ConformanceAnalysis.h
    include/Prism/Sema/Construction.h
    include/Prism/Sema/Contracts.h
    include/Prism/Sema/DependencyGraph.h
    include/Prism/Sema/ExprAnalysis.h
    include/Prism/Sema/FuncSig.h
    include/Prism/Sema/FunctionAnalysis.h
    include/Prism/Sema/GenericInstantiation.h
    include/Prism/Sema/NameLookup.h
    include/Prism/Sema/Scope.h
    include/Prism/Sema/SemaContext.h
    include/Prism/Sema/SemaDiagnostic.h
    include/Prism/Sema/SemaPrint.h
    include/Prism/Sema/Symbol.h
    include/Prism/Sema/SemaFwd.h
    include/Prism/Sema/TypeLayout.h
    include/Prism/Sema/QualType.h
]]
    include/Prism/Source/SourceContext.h
    include/Prism/Source/SourceLocation.h
    include/Prism/Source/Token.h
    include/Prism/Source/Token.def
)

set(PRISM_SOURCE_FILES
    src/Prism/Common/Allocator.cpp
    src/Prism/Common/DebugGraphGen.cpp
    src/Prism/Common/IndentingStreambuf.cpp
    src/Prism/Common/PrettyName.cpp
    src/Prism/Common/SyntaxMacros.h
    src/Prism/Common/TreeFormatter.cpp
    src/Prism/Common/Typename.cpp
    src/Prism/Common/Filters.py

    src/Prism/Diagnostic/Diagnostic.cpp
    src/Prism/Diagnostic/DiagnosticEmitter.cpp
    src/Prism/Diagnostic/DiagnosticFormat.cpp

    src/Prism/Facet/Facet.cpp

    src/Prism/Invocation/Invocation.cpp

    src/Prism/Lexer/Lexer.cpp
    src/Prism/Lexer/LexicalDiagnostic.cpp
    
    src/Prism/Parser/Grammar.ebnf
    src/Prism/Parser/LinearParser.h
    src/Prism/Parser/LinearParser.cpp
    src/Prism/Parser/Parser.cpp
    src/Prism/Parser/ParserBase.h
    src/Prism/Parser/ParserBase.cpp
    src/Prism/Parser/SyntaxError.cpp

    src/Prism/Sema2/Analysis.cpp
    src/Prism/Sema2/AnalysisContext.cpp
    src/Prism/Sema2/ConformanceAnalysis.cpp
    src/Prism/Sema2/Construction.cpp
    src/Prism/Sema2/ExprAnalysis.cpp
    src/Prism/Sema2/FunctionAnalysis.cpp
    src/Prism/Sema2/FuncSig.cpp
    src/Prism/Sema2/FunctionOverloadMap.cpp
    src/Prism/Sema2/GenericMatching.cpp
    src/Prism/Sema2/NameLookup.cpp
    src/Prism/Sema2/OverloadResolution.cpp
    src/Prism/Sema2/Scope.cpp
    src/Prism/Sema2/SemaContext.cpp
    src/Prism/Sema2/SemaDiagnostic.cpp
    src/Prism/Sema2/SemaFwd.cpp
    src/Prism/Sema2/SemaPrint.cpp
    src/Prism/Sema2/SubContext.cpp
    src/Prism/Sema2/Symbol.cpp
    src/Prism/Sema2/TypeLayout.cpp
#[[
    src/Prism/Sema/Analysis.cpp
    src/Prism/Sema/AnalysisBase.cpp
    src/Prism/Sema/ConformanceAnalysis.cpp
    src/Prism/Sema/Construction.cpp
    src/Prism/Sema/Contracts.cpp
    src/Prism/Sema/DependencyGraph.cpp
    src/Prism/Sema/ExprAnalysis.cpp
    src/Prism/Sema/FuncSig.cpp
    src/Prism/Sema/FunctionAnalysis.cpp
    src/Prism/Sema/GenericInstantiation.cpp
    src/Prism/Sema/NameLookup.cpp
    src/Prism/Sema/SemaContext.cpp
    src/Prism/Sema/SemaDiagnostic.cpp
    src/Prism/Sema/SemaPrint.cpp
    src/Prism/Sema/Scope.cpp
    src/Prism/Sema/Symbol.cpp
    src/Prism/Sema/SemaFwd.cpp
    src/Prism/Sema/TypeLayout.cpp
    src/Prism/Sema/QualType.cpp
]]
    src/Prism/Source/SourceContext.cpp
    src/Prism/Source/SourceLocation.cpp
    src/Prism/Source/Token.cpp
)

set(PRISMC_SOURCE_FILES
    src/Prism/Compiler/Main.cpp
)

set(PRISM_TEST_FILES 
    test/Prism/Common/Allocator.t.cpp 
    
    test/Prism/Lexer/Lexer.t.cpp 

    test/Prism/Parser/Parser.t.cpp 
    test/Prism/Parser/ParserTestUtils.h
    test/Prism/Parser/ParserTestUtils.cpp
#[[
    test/Prism/Sema/ConformanceAnalysis.t.cpp
    ]]
    test/Prism/Sema/SemaDiagnostics.t.cpp
    test/Prism/TestUtils/TestCompiler.h
    test/Prism/TestUtils/TestCompiler.cpp
)

# Core library

add_library(prism SHARED)
enable_sanitizers(prism)
enable_warnings(prism)

add_code_generator(prism
                   src/Prism/Facet/Facets.yaml 
                   src/Prism/Facet/FacetClassDef.jinja 
                   include/Prism/Facet/Facets.inl
                   src/Prism/Facet/FacetFilters.py)

add_code_generator(prism
                   src/Prism/Facet/Facets.yaml 
                   src/Prism/Facet/FacetFwd.jinja 
                   include/Prism/Facet/FacetFwd.h
                   src/Prism/Facet/FacetFilters.py)

add_code_generator(prism
                   src/Prism/Facet/Facets.yaml 
                   src/Prism/Facet/FacetMemberNames.jinja 
                   src/Prism/Facet/FacetMemberNames.inl
                   src/Prism/Facet/FacetFilters.py)

add_code_generator(prism
                   src/Prism/Sema2/Symbols.yaml
                   src/Prism/Sema2/SymbolFwd.jinja
                   include/Prism/Sema2/SymbolFwd.inl
                   src/Prism/Sema2/SemaFilters.py)

add_code_generator(prism
                   src/Prism/Sema2/SemaDiagnostics.yaml
                   src/Prism/Sema2/SemaDiagnosticsDecl.jinja
                   include/Prism/Sema2/SemaDiagnosticsDecl.inl
                   src/Prism/Sema2/SemaFilters.py)

add_code_generator(prism
                   src/Prism/Sema2/SemaDiagnostics.yaml
                   src/Prism/Sema2/SemaDiagnosticsDef.jinja
                   include/Prism/Sema2/SemaDiagnosticsDef.inl
                   src/Prism/Sema2/SemaFilters.py)

target_sources(prism 
  PRIVATE 
    ${PRISM_HEADER_FILES}
    ${PRISM_SOURCE_FILES}
)

source_group(TREE ${CMAKE_SOURCE_DIR}/src/Prism FILES ${PRISM_SOURCE_FILES})
source_group(TREE ${CMAKE_SOURCE_DIR}/include/Prism FILES ${PRISM_HEADER_FILES})

target_link_libraries(prism 
  PUBLIC
    range-v3 
    csp 
    utility
    APMath
    magic_enum
  PRIVATE
    termfmt
    graphgen
)

target_include_directories(prism PUBLIC include PRIVATE src)

# Compiler

add_executable(prismc)
enable_sanitizers(prismc)
enable_warnings(prismc)

target_sources(prismc
  PRIVATE  
    ${PRISMC_SOURCE_FILES}
)

source_group(TREE ${CMAKE_SOURCE_DIR}/src/Prism FILES ${PRISMC_SOURCE_FILES})

target_link_libraries(prismc
  PRIVATE
    prism
    termfmt
    CLI11
)

target_include_directories(prismc PRIVATE src)

# Tests

add_executable(prism-test)
enable_sanitizers(prism-test)
enable_warnings(prism-test)

target_sources(prism-test
  PRIVATE  
    ${PRISM_TEST_FILES}
)

source_group(TREE ${CMAKE_SOURCE_DIR}/test/Prism FILES ${PRISM_TEST_FILES})

target_link_libraries(prism-test
  PRIVATE
    Catch2::Catch2WithMain
    prism
    termfmt
)

target_include_directories(prism-test PRIVATE test)

# Playground

add_executable(playground)
enable_sanitizers(playground)
enable_warnings(playground)

target_sources(playground
  PRIVATE  
    src/Playground/Main.cpp
    src/Playground/ParserPlayground.cpp
    src/Playground/SemaPlayground.cpp
    src/Playground/FuzzParser.cpp
    examples/Playground.prism
)

target_link_libraries(playground
  PRIVATE
    prism
    termfmt
    CLI11
)
