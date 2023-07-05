use pds

rs.initiate(
    {
      _id : "rs-shard-b",
      members: [
        { _id : 0, host : "pds_mongo-shard-b.1:27017" },
        { _id : 1, host : "pds_mongo-shard-b.2:27017" },
        { _id : 2, host : "pds_mongo-shard-b.3:27017" }
      ]
    }
  )
