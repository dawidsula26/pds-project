use pds

rs.initiate(
    {
      _id: "rs-config",
      configsvr: true,
      members: [
        { _id : 0, host : "pds-mongo-config-1:27017" },
        { _id : 1, host : "pds-mongo-config-2:27017" },
        { _id : 2, host : "pds-mongo-config-3:27017" }
      ]
    }
  )
