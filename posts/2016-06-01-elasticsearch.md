# Maintaining Elasticsearch indices and mappings

This post addresses several caveats when working with persistent ES
indices and mappings.

The aims of the post are:

- raise awareness of certain ES features;

- make the maintenance of your ES indices more transparent;

- prevent the «accidentally working» situation;

- make your ES deployments more reproducible;

- (secretly make you give up on ES altogether or at least implement a
  proper typed API to make it edible).

Prerequisites: you should know what Elasticsearch
[indices and types/mappings][index-concept] are.

Examples below use Elasticsearch 5.0.

## tl;dr

- Switch off dynamic mappings.

- Store explicit definitions of your mappings.

- Check mappings when the application starts.

In other words, do what you'd do when using a proper RDBMS. Or just
don't use Elasticsearch instead of a database (although it's actually
quite good «you know, for search»).

## Dynamic mappings

To quote the [reference][dynamic-mapping],

> By default, when a previously unseen field is found in a document,
> Elasticsearch will add the new field to the type mapping.

The type of the new field will be inferred from *the first document*
with this field that you put to the index. This behaviour of ES may be
surprising and undesirable, and here's why. Suppose you write a
document with a new field to your index:

    curl -XPUT http://elasticsearch:9200/articles/rich_article/1 \
        -d '{"text": "2015-05-26", "arbitrary_json": {"key": "value"}}'
```json
{
  "_index": "articles",
  "_type": "rich_article",
  "_id": "1",
  "_version": 1,
  "result": "updated",
  "_shards": {
    "total": 2,
    "successful": 1,
    "failed": 0
  },
  "created": true
}
```

But types inferred for fields by ES is not what you *meant*:

    curl -XGET http://elasticsearch:9200/articles/ \
        | jq '.articles.mappings.rich_article'
```json
{
  "properties": {
    "arbitrary_json": {
      "properties": {
        "key": {
          "type": "text",
          "fields": {
            "keyword": {
              "type": "keyword",
              "ignore_above": 256
            }
          }
        }
      }
    },
    "text": {
      "type": "date"
    }
  }
}
```
Now you're doomed:

    curl -XPUT http://elasticsearch:9200/articles/rich_article/2 \
        -d '{"text": "Kim K “sick of showbiz”, aims to get a PhD"}'

```json
{
  "error": {
    "root_cause": [
      {
        "type": "mapper_parsing_exception",
        "reason": "failed to parse [text]"
      }
    ],
    "type": "mapper_parsing_exception",
    "reason": "failed to parse [text]",
    "caused_by": {
      "type": "illegal_argument_exception",
      "reason": "Invalid format: \"Kim K “sick of showbiz”, aims to...\""
    }
  },
  "status": 400
}
```

    curl -XPUT http://elasticsearch:9200/articles/rich_article/3 \
        -d '{"arbitrary_json": {"key":{"more":"nesting"}}}'

```json
{
  "error": {
    "root_cause": [
      {
        "type": "mapper_parsing_exception",
        "reason": "failed to parse [arbitrary_json.key]"
      }
    ],
    "type": "mapper_parsing_exception",
    "reason": "failed to parse [arbitrary_json.key]",
    "caused_by": {
      "type": "illegal_state_exception",
      "reason": "Can't get text on a START_OBJECT at 1:27"
    }
  },
  "status": 400
}
```

The worst part of it is that there's no way to *alter the type of a
field once it's in the mapping*. You can't even delete that field from
the mapping and create it again.

You have to create a new mapping, explicitly specifying field types
you want. If you mean something, say it. To prevent this kind of
problems in the future, turn off dynamic mappings:

    curl -XPUT http://elasticsearch:9200/articles-v2/ \
        -d '{"mappings":
             {"rich_article":
              {"dynamic": "strict",
               "properties":
                {"arbitrary_json": {"type":"object"},
                 "text":{"type":"string"}}}}}'

```json
{
  "acknowledged": true,
  "shards_acknowledged":true
}
```

Here's why it is better than the default approach:

- by providing field types you set up a contract between your
  application and Elasticsearch you can rely on, instead of relying on
  *implicit* behaviour of Elasticsearch;

- with dynamic mapping turned off and set to `strict`, your
  application will fail early when trying to write a document with an
  unknown field to the index, calling your attention to the lack of an
  explicitly set schema.

Now you only to need move documents from an old mapping to the new
one. [es-reindex][] is a tool for that.

Mapping creation brings us to the next point.

## Manage your mappings

### Write them down

When you do not rely on ES creating mappings, you have to do it
yourself. Keeping the mapping definition in a versioned repository
closer to the code that uses the mapping is a good thing:

- it serves as documentation for your code that works with ES;

- it makes investigations easier and encourages peer review;

- probably you can find a way to reuse it to describe schema or types
  for data structures used on application level (for instance, making
  it impossible to write to a field not mentioned in the mapping
  definition);

- when multiple applications use ES as a common interface layer, it's
  good to have a unified unobscured view on what the mappings should
  look like (instead of having to go through implementation details of
  every specific application).

### Check mappings

Having code that writes certain documents to certain ES mappings means
that your application expects the environment (Elasticsearch) to
fulfill terms of a certain contract. Make sure that all parties (your
app and ES) agree on those terms before things go south!

In addition to dynamic mapping being the default, Elasticsearch also
[automatically creates][auto-create] *indices* and *mappings* too when
you first try to put a document to a non-existent index or mapping,

> The index is created with the default settings, and new fields are
> added to the type mapping by using dynamic mapping.

Use of dynamic mapping in this case may possibly lead to undesired
types being inferred for fields, not to mention that this behaviour
depends on ES cluster configuration.

It's better to know about problems early and check ES setup in run
time on application startup:

- check if your target index exists;

- perhaps you may pick up a [JSON diff][json-diff] library and ensure
  there're no structural differences between the actual mapping and
  what your application expects.

### Create mappings

You can have your application create non-existent indices or have a
separate script handle it. You'll normally want to describe your
mappings once and use them everywhere (impose constraints on your ES
code, check ES setup when the application runs and create non-existent
indices).

It might be a good idea to make it possible to obtain a JSON object
with ES mappings definition from your application:

    app show-index-settings

```json
{
  "mappings": {
    "Article": {
      "dynamic": "strict",
      "properties": {
        "summary": {
          "type": "string",
          "analyzer": "light_english"
        },
        ...
      }
    }
  }
}
```

Here's why: ES «migrations» may easily require re-indexing (if you
need to change field types), which is likely to be done separately from
normal application operation, probably manually. In this case it's
helpful to have your mappings in an ES-friendly format, which you can
use in ES REST API calls to create indices (probably from a migration
handling script):

    curl -XPOST http://elasticsearch:9200/articles-ng/ \
        -d $(app show-index-settings)

## Use aliases

Suppose you need to change the mapping for your documents to use
different types of fields, different analyzers etc. It's much easier
to do so with minimal lag if your application is pointed at
[aliases][index-aliases] instead of targeting indices directly. You
can have different aliases for reading and writing. Switching an alias
from an old index to a new one is atomic.

[auto-create]: https://www.elastic.co/guide/en/elasticsearch/guide/current/_creating_an_index.html
[dynamic-mapping]: https://www.elastic.co/guide/en/elasticsearch/reference/5.0/dynamic-field-mapping.html
[es-reindex]: https://github.com/MailOnline/es-reindex
[index-aliases]: https://www.elastic.co/guide/en/elasticsearch/reference/5.0/indices-aliases.html
[index-concept]: https://www.elastic.co/guide/en/elasticsearch/reference/5.0/_basic_concepts.html#_index
[json-diff]: https://github.com/search?q=json+diff&ref=opensearch&type=Repositories
[mailonline]: https://www.linkedin.com/company/mailonline/
