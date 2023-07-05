use pds

rs.initiate(
    {
      _id: "rs-config",
      configsvr: true,
      members: [
        { _id : 0, host : "pds_mongo-config.1:27017" },
        { _id : 1, host : "pds_mongo-config.2:27017" },
        { _id : 2, host : "pds_mongo-config.3:27017" }
      ]
    }
  )
