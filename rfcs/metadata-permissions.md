### Metadata Permissions

Currently, all metadata APIs (`track_table`, `add_remote_relationship etc`.)
are accessible only to `admin` role.

#### Motivation

To Develop the GraphQL APIs via console or CLI on Hasura GraphQL Engine
(hereafter `HGE`) one has to have the admin secret or `admin` role privilage.
The entire HGE metadata is available for edit/delete for the users with `admin`
role. It is unsafe to manage the metadata in large enterprise systems with
multiple teams. The HGE should provide a safe access to manage its metadata for
non-admin roles via RBAC permissions just like the permission system for
GraphQL API. This also influences the console & CLI developer experience.

#### Permission system

The HGE has various entities and sub-entities in its metadata like tables,
tables' relationships, actions, remote schemas etc. The permission system
defines the availibility of each entity and items in the entitiy. The
`export_metadata` API exports the metadata with permissions applied.

For example, let's consider the `action_manager` role is defined and have
only access to actions and nothing else. The `export_metadata` response look
like

```yaml
metadata:
  tables: null
  actions:
  - name: login
    config: some-config
```
