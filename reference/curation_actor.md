# Build a trusted authenticated actor value.

This constructor is intended for an authenticated deployment resolver or
a test double. Review UI input, display names, and untrusted headers
must never be used to construct the identity passed to a repository.

## Usage

``` r
curation_actor(id, roles = character(0), display_name = NULL)
```

## Arguments

- id:

  Stable authenticated actor identifier.

- roles:

  Character vector of server-assigned roles.

- display_name:

  Optional informational display name; it is never an identity key.

## Value

A `planscanR_curation_actor` object.
