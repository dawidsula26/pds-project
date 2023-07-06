use pds

sh.addShard( "rs-shard-a/pds_mongo-shard-a.1:27017,pds_mongo-shard-a.2:27017")
sh.addShard( "rs-shard-b/pds_mongo-shard-b.1:27017,pds_mongo-shard-b.2:27017")
sh.addShard( "rs-shard-c/pds_mongo-shard-c.1:27017,pds_mongo-shard-c.2:27017")

sh.shardCollection(
  "pds.view",
  { _id: "hashed" }
)

sh.shardCollection(
  "pds.view",
  { _id: "hashed" }
)
