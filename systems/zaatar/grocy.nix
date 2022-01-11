{
  services.grocy = {
    enable = true;
    hostName = "grocy.kmein.r";
    nginx.enableSSL = false;
    settings.calendar.firstDayOfWeek = 1;
    settings.culture = "de";
    settings.currency = "EUR";
  };
}
