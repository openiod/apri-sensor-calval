// js communication with R server about timezone
  Shiny.addCustomMessageHandler("getClientTime",
      function(s){
        var d = new Date()
        var clientPosix = parseInt(d.getTime()/1000);
        var clientTimeZone = -(d.getTimezoneOffset() / 60);
        var res =  {
          serverPosix:s.serverPosix,
          serverTimeZone:s.serverTimeZone,
          clientPosix:clientPosix,
          clientTimeZone:clientTimeZone
        }
        Shiny.onInputChange("clientTime",res)
      })
