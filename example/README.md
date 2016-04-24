In this example, an accounting system has a number of modules, point of sale, stock, general ledger, and a couple of clients are using this software, so each client has its own accounting modules.  For each module, client combination there is a port allocated to communicate with that module.  The ports are allocated in ascending order to help keep the configuration manageable.

For now we are not storing any client data associated with the client entries.  Each accounting module has an entry in the confguration file as well.  To ensure the data is of the correct type there is a config.def file defined.  There is also a data validation program.  When these configuration entries are changed, the make file causes both checks, that the data conforms to definition, and that the relationships between the data items are valid.

There are therefore the following entries:

{module,Atom_module_name,Erlang_Term}
{client,Atom_Client_Name,Erlang_Term}
{tcp_port,Integer_Port,{client,module}}

