{
  auth_enabled = false;
  server = {
    http_listen_port = 3100;
    grpc_listen_port = 9096;
  };
  ingester = {
    wal = {
      enabled = true;
      dir = "/tmp/wal";
    };
    lifecycler = {
      address = "127.0.0.1";
      ring = {
        kvstore.store = "inmemory";
        replication_factor = 1;
      };
      final_sleep = "0s";
    };
    chunk_idle_period = "1h"; # Any chunk not receiving new logs in this time will be flushed
    max_chunk_age = "1h"; # All chunks will be flushed when they hit this age, default is 1h
    chunk_target_size = 1048576; # Loki will attempt to build chunks up to 1.5MB, flushing first if chunk_idle_period or max_chunk_age is reached first
    chunk_retain_period = "30s"; # Must be greater than index read cache TTL if using an index cache (Default index read cache TTL is 5m)
  };
  schema_config.configs = [
    {
      from = "2020-10-24";
      store = "tsdb";
      object_store = "filesystem";
      schema = "v13";
      index = {
        prefix = "index_";
        period = "24h";
      };
    }
  ];
  storage_config = {
    tsdb_shipper = {
      active_index_directory = "/tmp/loki/tsdb-shipper-active";
      cache_location = "/tmp/loki/tsdb-shipper-cache";
      cache_ttl = "24h"; # Can be increased for faster performance over longer query periods, uses more disk space
    };
    filesystem.directory = "/tmp/loki/chunks";
  };
  compactor = {
    working_directory = "/tmp/loki/boltdb-shipper-compactor";
  };
  limits_config = {
    reject_old_samples = true;
    reject_old_samples_max_age = "168h";
    allow_structured_metadata = false;
  };
  table_manager = {
    retention_deletes_enabled = false;
    retention_period = "0s";
  };
}
