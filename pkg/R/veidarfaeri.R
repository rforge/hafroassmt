veidarfaeri <- function(n=NULL, names=FALSE)
########################################################################################################################
###                                                                                                                    #
### Function: veidarfaeri                                                                                              #
###                                                                                                                    #
### Purpose:  Look up gear codes                                                                                       #
###                                                                                                                    #
### Args:     n is a vector of integer gear codes, or NULL (default)                                                   #
###           names is whether to include gear code in the output                                                      #
###                                                                                                                    #
### Notes:    Current gear table contains 100 gears numbered from 0 to 174, synchronized to orri.veidarfaeri on        #
###           11 Apr 2011                                                                                              #
###                                                                                                                    #
### Returns:  String vector of gear descriptions, or data frame with all gears if n is NULL                            #
###                                                                                                                    #
########################################################################################################################
{
  ## 1  Suppress factor conversion
  osaf <- options(stringsAsFactors=FALSE)
  on.exit(options(osaf))

  ## 2  Prepare key
  key <- c(0,"Stundar ekki veiğar",
           1,"Lína",
           2,"Net",
           3,"Handfæri",
           4,"Şorsknót",
           5,"Dragnót 135 mm",
           6,"Botnvarpa",
           7,"Flotvarpa",
           8,"Spærlingsvarpa",
           9,"Humarvarpa",
           10,"Síldarnót",
           11,"Reknet",
           12,"Loğnunót",
           13,"Loğnuflotvarpa",
           14,"Rækjuvarpa",
           15,"Hörpudiskplógur/Scallop dr.",
           16,"Grálúğulína",
           17,"Şorskgildra",
           18,"Krabbagildra",
           19,"Gulllaxavarpa",
           20,"İmis veiğarfæri",
           21,"Síldar-/kolmunnaflotvarpa",
           22,"Botnvarpa klædd",
           23,"Seiğaflotvarpa",
           24,"Flotvarpa klædd",
           25,"Grásleppunet",
           26,"Dragnót 120 mm",
           27,"Dragnót klædd, 40 mm",
           28,"Kassabotnvarpa",
           29,"Rauğmaganet",
           30,"Rækjuvarpa m/ leggpoka",
           31,"Sandsílisvarpa",
           32,"Lagnet",
           33,"Flotvarpa Gloria óklædd",
           34,"Flotvarpa Gloria klædd",
           35,"Dragnót 155 mm",
           36,"Lófótlína / Flotlína",
           37,"Vatnsşrıstiplógur",
           38,"Kúffisksplógur",
           39,"Beitukóngsgildra",
           40,"Ígulkeraplógur",
           41,"Ígulkerakafari",
           42,"Kræklingalína",
           43,"Veiğistöng",
           44,"Síldar-/kolmunnaflotvarpa meğ skilju",
           45,"Sjóstöng",
           55,"Dragnót meğ leggpoka",
           59,"Botnvarpa, 135 mm m. skilju, 60mm rimlab",
           60,"Botnvarpa 135 mm í poka",
           61,"Fótreipisbotnvarpa",
           62,"Botnvarpa meğ skilju",
           63,"Botnvarpa meğ skilju, 50mm rimlabil",
           64,"Botnvarpa meğ skilju, 52mm rimlabil",
           65,"Botnvarpa meğ skilju, 55mm rimlabil",
           66,"Botnvarpa meğ skilju og yfirpoka",
           68,"Botnvarpa klædd (B2-84), stærri hlerar",
           70,"Botnvarpa meğ leggpoka",
           71,"Landbeitt lína",
           72,"Net - SMN",
           73,"Botnvarpa klædd (SMB)",
           74,"Botnvarpa klædd (SMG)",
           75,"Tveggja báta troll",
           76,"Tvær botnvörpur",
           77,"Botnvarpa klædd (SMH)",
           78,"Botnvarpa klædd (SMH djúpslóğ)",
           79,"Botnvarpa meğ leggglugga",
           80,"Dragnót meğ leggglugga",
           90,"Ádráttarnet",
           91,"Skötuselsnet",
           99,"Óskráğ veiğarfæri",
           130,"Van Veen greip / Van Veen grab 35-40 kg",
           131,"Van Veen greip / Van Veen grab 70-100 kg",
           132,"Myndavél / Camera",
           133,"Petersen greip / Petersen grab",
           134,"Shipek greip / Shipek grab",
           135,"Kjarnataki / Smögen box corer",
           136,"RP sleği / RP sledge",
           137,"Sneli sleği / detr. sledge (Sneli)",
           138,"Skrapa / Triangle dredge",
           139,"Agassiz troll / Agassiz trawl",
           141,"Tucker lirfur háfur 1 fermetra op",
           142,"Tucker lirfur háfur 4 fermetra op",
           143,"WP-80 Eggja háfur sem er 80 cm í şvermál",
           144,"WP-II   Átuháfur",
           150,"Sonda",
           160,"ROTV / Dragsleği meğ myndavél",
           161,"ROV / Fjarstırğ grind meğ myndavélum",
           162,"Litmyndir",
           163,"Svarthvítt",
           164,"Hljóğsjá / Scanning sonar",
           165,"Hljóğnemi / Hydrophone",
           166,"GPS / GPS neğansjávarstağsetning",
           167,"Scanmar nemar",
           168,"Dıptarmælir / Dıptarmælir skips",
           169,"Höfuğlínusónar",
           170,"Skipsstraumsjá / Doppler um borğ í skipi",
           171,"DST mælar / Síritandi hita og dıpisnemar",
           172,"Sandsílaplógur",
           173,"Sandsílaplógur (kúffisksplógur)",
           174,"KC greip / KC Day grab")
  key <- matrix(key, ncol=2, byrow=TRUE)
  key <- data.frame(id=as.integer(key[,1]), lysing=key[,2], row.names=as.integer(key[,1]))

  ## 3  Look up
  if(is.null(n))
  {
    output <- if(names) key else key[-1]
  }
  else
  {
    output <- key$lysing[match(n,key$id)]
    if(names)
      names(output) <- n
  }

  return(output)
}
