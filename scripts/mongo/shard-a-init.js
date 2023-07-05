use pds

rs.initiate(
    {
      _id : "rs-shard-a",
      members: [
        { _id : 0, host : "pds_mongo-shard-a.1:27017" },
        { _id : 1, host : "pds_mongo-shard-a.2:27017" },
        { _id : 2, host : "pds_mongo-shard-a.3:27017" }
      ]
    }
  )
