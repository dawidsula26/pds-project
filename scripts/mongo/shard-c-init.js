use pds

rs.initiate(
    {
      _id : "rs-shard-c",
      members: [
        { _id : 0, host : "pds_mongo-shard-c.1:27017" },
        { _id : 1, host : "pds_mongo-shard-c.2:27017" },
        { _id : 2, host : "pds_mongo-shard-c.3:27017" }
      ]
    }
  )
