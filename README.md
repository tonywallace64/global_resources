# global_resources

Global resources is intended as a clearing house for data that must be shared around multiple application modules.  Global resources are relied upon by other systems and SHOULD support facilities for on the fly configuration and hot code updates.

This system supports non-stop operation by attempting to validate configuration file changes before they are applied.  This validation includes checking that the erlang terms in the configuration file have the correct structure, and checking referential integrity constraints.  These operations are co-ordinated by a make based system.

TO DO:
UDP based queries.
