#!/bin/bash

# Generates sample data for one run of DDPA.
# $1 - Description
# $2 - Source File
# $3 - DDPA Level
generate_sample() {
    cat "test-sources/$2" | ./odefa_toploop.byte --analysis=$3 --dotgen
    prefix="sample-output/$1 with $3-level Analysis:"
    cp odefa-ddpa-graph.dot "${prefix} DDPA graph.dot"
    dot -Tpdf <"${prefix} DDPA graph.dot" >"${prefix} DDPA graph.pdf"
    cat odefa-pds-graph.dot | grep -v 'color="blue"' > "${prefix} PDS graph.dot"
    dot -Tpdf <"${prefix} PDS graph.dot" >"${prefix} PDS graph.pdf"
    dot -Tsvg <"${prefix} PDS graph.dot" >"${prefix} PDS graph.svg"
}

generate_sample_by_name() {
    generate_sample 'Overview Example "'"$1"'"' overview_example_$1.odefa $2
}

generate_sample_by_name identity 0
generate_sample_by_name identity 1
generate_sample_by_name identity 2
generate_sample_by_name k_combinator 0
generate_sample_by_name k_combinator 1
generate_sample_by_name recursion 1
generate_sample_by_name higher_order_functions 1
