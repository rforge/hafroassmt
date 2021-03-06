veidarfaeri <- function(x=NULL, names=FALSE)
{
  ## 1  Suppress factor conversion
  osaf <- options(stringsAsFactors=FALSE)
  on.exit(options(osaf))

  ## 2  Prepare key
  key <- c(0,"Stundar ekki vei�ar",
           1,"L�na",
           2,"Net",
           3,"Handf�ri",
           4,"�orskn�t",
           5,"Dragn�t 135 mm",
           6,"Botnvarpa",
           7,"Flotvarpa",
           8,"Sp�rlingsvarpa",
           9,"Humarvarpa",
           10,"S�ldarn�t",
           11,"Reknet",
           12,"Lo�nun�t",
           13,"Lo�nuflotvarpa",
           14,"R�kjuvarpa",
           15,"H�rpudiskpl�gur/Scallop dr.",
           16,"Gr�l��ul�na",
           17,"�orskgildra",
           18,"Krabbagildra",
           19,"Gulllaxavarpa",
           20,"�mis vei�arf�ri",
           21,"S�ldar-/kolmunnaflotvarpa",
           22,"Botnvarpa kl�dd",
           23,"Sei�aflotvarpa",
           24,"Flotvarpa kl�dd",
           25,"Gr�sleppunet",
           26,"Dragn�t 120 mm",
           27,"Dragn�t kl�dd, 40 mm",
           28,"Kassabotnvarpa",
           29,"Rau�maganet",
           30,"R�kjuvarpa m/ leggpoka",
           31,"Sands�lisvarpa",
           32,"Lagnet",
           33,"Flotvarpa Gloria �kl�dd",
           34,"Flotvarpa Gloria kl�dd",
           35,"Dragn�t 155 mm",
           36,"L�f�tl�na / Flotl�na",
           37,"Vatns�r�stipl�gur",
           38,"K�ffiskspl�gur",
           39,"Beituk�ngsgildra",
           40,"�gulkerapl�gur",
           41,"�gulkerakafari",
           42,"Kr�klingal�na",
           43,"Vei�ist�ng",
           44,"S�ldar-/kolmunnaflotvarpa me� skilju",
           45,"Sj�st�ng",
           55,"Dragn�t me� leggpoka",
           59,"Botnvarpa, 135 mm m. skilju, 60mm rimlab",
           60,"Botnvarpa 135 mm � poka",
           61,"F�treipisbotnvarpa",
           62,"Botnvarpa me� skilju",
           63,"Botnvarpa me� skilju, 50mm rimlabil",
           64,"Botnvarpa me� skilju, 52mm rimlabil",
           65,"Botnvarpa me� skilju, 55mm rimlabil",
           66,"Botnvarpa me� skilju og yfirpoka",
           68,"Botnvarpa kl�dd (B2-84), st�rri hlerar",
           70,"Botnvarpa me� leggpoka",
           71,"Landbeitt l�na",
           72,"Net - SMN",
           73,"Botnvarpa kl�dd (SMB)",
           74,"Botnvarpa kl�dd (SMG)",
           75,"Tveggja b�ta troll",
           76,"Tv�r botnv�rpur",
           77,"Botnvarpa kl�dd (SMH)",
           78,"Botnvarpa kl�dd (SMH dj�psl��)",
           79,"Botnvarpa me� leggglugga",
           80,"Dragn�t me� leggglugga",
           90,"�dr�ttarnet",
           91,"Sk�tuselsnet",
           99,"�skr�� vei�arf�ri",
           130,"Van Veen greip / Van Veen grab 35-40 kg",
           131,"Van Veen greip / Van Veen grab 70-100 kg",
           132,"Myndav�l / Camera",
           133,"Petersen greip / Petersen grab",
           134,"Shipek greip / Shipek grab",
           135,"Kjarnataki / Sm�gen box corer",
           136,"RP sle�i / RP sledge",
           137,"Sneli sle�i / detr. sledge (Sneli)",
           138,"Skrapa / Triangle dredge",
           139,"Agassiz troll / Agassiz trawl",
           141,"Tucker lirfur h�fur 1 fermetra op",
           142,"Tucker lirfur h�fur 4 fermetra op",
           143,"WP-80 Eggja h�fur sem er 80 cm � �verm�l",
           144,"WP-II   �tuh�fur",
           150,"Sonda",
           160,"ROTV / Dragsle�i me� myndav�l",
           161,"ROV / Fjarst�r� grind me� myndav�lum",
           162,"Litmyndir",
           163,"Svarthv�tt",
           164,"Hlj��sj� / Scanning sonar",
           165,"Hlj��nemi / Hydrophone",
           166,"GPS / GPS ne�ansj�varsta�setning",
           167,"Scanmar nemar",
           168,"D�ptarm�lir / D�ptarm�lir skips",
           169,"H�fu�l�nus�nar",
           170,"Skipsstraumsj� / Doppler um bor� � skipi",
           171,"DST m�lar / S�ritandi hita og d�pisnemar",
           172,"Sands�lapl�gur",
           173,"Sands�lapl�gur (k�ffiskspl�gur)",
           174,"KC greip / KC Day grab")
  key <- matrix(key, ncol=2, byrow=TRUE)
  key <- data.frame(id=as.integer(key[,1]), lysing=key[,2], row.names=as.integer(key[,1]))

  ## 3  Look up
  if(is.null(x))
  {
    output <- if(names) key else key[-1]
  }
  else if(is.numeric(x))
  {
    output <- key$lysing[match(x,key$id)]
    if(names)
      names(output) <- x
  }
  else
  {
    output <- key$id[match(x,key$lysing)]
    if(names)
      names(output) <- x
  }

  return(output)
}
