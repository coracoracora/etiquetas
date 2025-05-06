# etiquetas: tag wrangler

- Scans directory hierarchies
- Finds text files with tags matchin patterns
- Suggests tag clusters based upon stem/semantic similarity
- Outputs an index
- allows the user to merge/combine/group tags, and rewrite them in documents.

## Tag Patterns

Each tag pattern has:
- Sigil (e.g. `#`, `@`, etc)
- Body (e.g. `tag-name`, `prj:tag-name`, `prj/type/tag-name`)
- Tag formats match tag bodies to expressions for semantic parsing:
  - `(\#)([^\s,\/])` : Basic tag match (e.g. `#tag1, #tag2,`)
  - `(\#)([^\s,\/])` : Basic tag match (e.g. `#tag1, #tag2,`)
  
## Tag BNF
  ```
  TAG   := (SIGIL)(BODY)
  SIGIL := [@#&%!]                        # Configurable
  BODY  := (ATOM) | (ATOM)(SEP)(BODY) 
  SEP   := '/'
  ATOM  := [^\s$(SEP)]*
  ```
  
## Example Tags

- `#fg::kw::convergence` : Keyword specific to project "fg", with body "convergence"
- `#monkeys` " Generic keyword, "monkeys"

## Scanning

The tool runs in the background, watching a directory for changes. When
a change event is intercepted related to a file that matches a predicate,
an internal event is fired to scan the file for tags.

If tags are found, then they are matched against the configured watched tags.
This fires an "interest" event.

"interest" events are consumed by a state manager. The state manager keeps an internal directory of tag state information; "interest" events are examined and applied as diffs as appropriate. When a diff is applied, then an "update" event is fired.

"update" events are consumed by the updater, which maintains the external index file, and optionally applies changes to the contents of the files.

## Challenges

### Race Conditions

Race conditions. This is the big one, if we're going to rewrite tags in the
content of documents. There is no reliable file locking mechanism that does not
involve implementation by all programs subject to file write races, and we don't
control others' text editors. This would be a nightmare when working with
aggressive autosave systems.


However, we _could_ make organization and indexing automatic, but require
assisted manual reconciliation of tags... for example, the "index" could be a
live web page which allows the user to view tag clusters, and present an
interface for managing them.

  
  
  
  