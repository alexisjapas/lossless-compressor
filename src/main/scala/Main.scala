import lz.LZW
import statistic.{Huffman, ShannonFano}

class Test_RLE[T] {
  def test(msg : Seq[T]): Unit = {
    println(s"RLE on $msg:")
    val rle_compressor = new RLE[T]
    val compressed_data = rle_compressor.compress(msg)
    println(s"Compressed data: $compressed_data")
    val uncompressed_data = rle_compressor.uncompress(compressed_data)
    println(s"Uncompressed data: $uncompressed_data")
    if (uncompressed_data.isEmpty) {
      println("Empty entry data")
    } else {
      println(s"${if (uncompressed_data.get.equals(msg)) "OK" else "ko"}")
    }
    println()
  }
}


class Test_LZ {
  def test_lz78(msg : Seq[Char]) : Unit = {
    println(s"LZ78 on $msg:")
    val compressed_data = lz.LZ78.compress(msg)
    println(s"Compressed data: $compressed_data")
    val uncompressed_data = lz.LZ78.uncompress(compressed_data)
    println(s"Uncompressed data: $uncompressed_data")
    if (uncompressed_data.isEmpty) {
      println("Empty entry data")
    } else {
      println(s"${if (uncompressed_data.get.equals(msg)) "OK" else "ko"}")
    }
    println()
  }

  def test_lzw(msg : Seq[Char]) : Unit = {
    println(s"LZW on $msg:")
    val lzw_compressor = new LZW()
    val compressed_data = lzw_compressor.compress(msg)
    println(s"Compressed data: $compressed_data")
    val uncompressed_data = lzw_compressor.uncompress(compressed_data)
    println(s"Uncompressed data: $uncompressed_data")
    if (uncompressed_data.isEmpty) {
      println("Empty entry data")
    } else {
      println(s"${if (uncompressed_data.get.equals(msg)) "OK" else "ko"}")
    }
    println()
  }
}

class Test_Huffman[S] {
  def test(msg : Seq[S]): Unit = {
    println(s"Huffman on $msg:")
    val huffman_compressor = new Huffman[S](msg)
    val compressed_data = huffman_compressor.compress(msg)
    println(s"Compressed data: $compressed_data")
    val uncompressed_data = huffman_compressor.uncompress(compressed_data)
    println(s"Uncompressed data: $uncompressed_data")
    if (uncompressed_data.isEmpty) {
      println("Empty entry data")
    } else {
      println(s"${if (uncompressed_data.get.equals(msg)) "OK" else "ko"}")
    }
    println()
  }
}

class Test_ShannonFano[S] {
  def test(msg : Seq[S]): Unit = {
    println(s"Shannon Fano on $msg:")
    val sf_compressor = new ShannonFano[S](msg)
    val compressed_data = sf_compressor.compress(msg)
    println(s"Compressed data: $compressed_data")
    val uncompressed_data = sf_compressor.uncompress(compressed_data)
    println(s"Uncompressed data: $uncompressed_data")
    if (uncompressed_data.isEmpty) {
      println("Empty entry data")
    } else {
      println(s"${if (uncompressed_data.get.equals(msg)) "OK" else "ko"}")
    }
    println()
  }
}


object Main {
  def compare(msg: Seq[Char]): Unit = {
    val rle_compressor = new RLE[Char]
    val lzw_compressor = new LZW()
    val huffman_compressor = new Huffman[Char](msg)
    val sf_compressor = new ShannonFano[Char](msg)

    println(Seq((rle_compressor.compress(msg).size * 2 * 8, "RLE"),
      (lz.LZ78.compress(msg).size * 2 * 8, "LZ78"),
      (lzw_compressor.compress(msg).size * 9 + 256 * 8, "LZW"),
      (huffman_compressor.compress(msg).size, "Huffman"),
      (sf_compressor.compress(msg).size, "ShannonFano")).sortBy(_._1))
  }


  def main(args: Array[String]): Unit = {
    val empty_int_seq:Seq[Int] = IndexedSeq()
    val char_seq:Seq[Char] = Seq('b', 'e', 'l', 'l', 'e', ' ', 'e', 'c', 'h', 'e', 'l', 'l', 'e', ' ', '!')
    val char_seq1:Seq[Char] = Seq('b', 'e', 'l', 'l', 'e', 's', ' ', 'b', 'e', 'l', 'l', 'e', 's', ' ' , 'c', 'o', 'm', 'm', 'e', ' ', 'l', 'e', ' ', 'j', 'o', 'u', 'r', ' ', '!')
    val char_seq2:Seq[Char] = Seq('C', '\'', 'e', 's', 't', ' ', 'a', 's', 's', 'e', 'z', ',', ' ', 'd' , 'i', 't', ' ', 'l', 'a', ' ', 'b', 'a', 'l', 'e', 'i', 'n', 'e')
    val char_seq3:Seq[Char] = Seq('S', 'o', 'n', 't', '-', 'e', 'l', 'l', 'e', 's', ' ', 's', 'è', 'c' , 'h', 'e', 's', ' ', '?')
    val char_seq4:Seq[Char] = Seq('C', 'e', ' ', 'p', 'a', 'l', 'e', ' ', 'p', 'a', 'l', 'o', 'i', 's' , ' ', 'e', 's', 't', ' ', 'e', 'm', 'p', 'a', 'l', 'e', ' ' , 's', 'u', 'r', ' ', 'u', 'n', ' ', 'p', 'a', 'l', 'e')
    val string_seq:Seq[String] = IndexedSeq("Scalapi", "Scalapi", "Scapali", "Poum", "Poum")
    val int_seq:Seq[Int] = IndexedSeq(1, 1, 1, 2, 4, 4, 5, 5, 3, 2, 3, 3)
    val seq_seq:Seq[Seq[Int]] = IndexedSeq(IndexedSeq(1, 2, 3), IndexedSeq(1, 2, 3), IndexedSeq(2))

    // RLE
    println("########## RLE ##########")
    val test_char = new Test_RLE[Char]
    val test_int = new Test_RLE[Int]
    val test_string = new Test_RLE[String]
    val test_seq_seq = new Test_RLE[Seq[Int]]

    test_int.test(empty_int_seq)
    test_char.test(char_seq)
    test_char.test(char_seq1)
    test_char.test(char_seq2)
    test_char.test(char_seq3)
    test_char.test(char_seq4)
    test_string.test(string_seq)
    test_int.test(int_seq)
    test_seq_seq.test(seq_seq)


    // TEST DICT
    val test_lz = new Test_LZ
    println("########## LZ78 ##########")
    test_lz.test_lz78(char_seq)
    test_lz.test_lz78(char_seq1)
    test_lz.test_lz78(char_seq2)
    test_lz.test_lz78(char_seq3)
    test_lz.test_lz78(char_seq4)

    println("########## LZW ##########")
    test_lz.test_lzw(char_seq)
    test_lz.test_lzw(char_seq1)
    test_lz.test_lzw(char_seq2)
    test_lz.test_lzw(char_seq3)
    test_lz.test_lzw(char_seq4)

    // TESTS STATS
    println("########## HUFFMAN ##########")
    val huffman_empty = new Test_Huffman[Int]
    huffman_empty.test(empty_int_seq)

    val huffman_char = new Test_Huffman[Char]
    huffman_char.test(char_seq)
    huffman_char.test(char_seq1)
    huffman_char.test(char_seq2)
    huffman_char.test(char_seq3)
    huffman_char.test(char_seq4)

    val huffman_string = new Test_Huffman[String]
    huffman_string.test(string_seq)

    val huffman_int = new Test_Huffman[Int]
    huffman_int.test(int_seq)

    val huffman_seq = new Test_Huffman[Seq[Int]]
    huffman_seq.test(seq_seq)

    println("########## SHANNON FANO ##########")
    val sf_empty = new Test_ShannonFano[Int]
    sf_empty.test(empty_int_seq)

    val sf_char = new Test_ShannonFano[Char]
    sf_char.test(char_seq)
    sf_char.test(char_seq1)
    sf_char.test(char_seq2)
    sf_char.test(char_seq3)
    sf_char.test(char_seq4)

    val sf_string = new Test_ShannonFano[String]
    sf_string.test(string_seq)

    val sf_int = new Test_ShannonFano[Int]
    sf_int.test(int_seq)

    val sf_seq = new Test_ShannonFano[Seq[Int]]
    sf_seq.test(seq_seq)


    println("########## COMPARISON ##########")
    compare("Une banane est tombée.")
    compare("Trois anneaux pour les Rois Elfes sous le ciel, Sept pour les Seigneurs Nains dans leurs demeures de pierre, Neuf pour les Hommes Mortels destinés au trépas, Un pour le Seigneur des Ténèbres sur son sombre trône Dans le Pays de Mordor où s\'étendent les Ombres.")
    compare("Tu crois savoir tout ce qui se passe dans ton âme, dès que c'est suffisamment important, parce que ta conscience te l'apprendrait alors. Et quand tu restes sans nouvelles d'une chose qui est dans ton âme, tu admets, avec une parfaite assurance, que cela ne s'y trouve pas. Tu vas même jusqu'à tenir « psychique » pour identique à « conscient », c'est-à-dire connu de toi, et cela malgré les preuves les plus évidentes qu'il doit sans cesse se passer dans ta vie psychique bien plus de choses qu'il ne peut s'en révéler à ta conscience. Tu te comportes comme un monarque absolu qui se contente des informations que lui donnent les hauts dignitaires de la cour et qui ne descend pas vers le peuple pour entendre sa voix. Rentre en toi-même profondément et apprends d'abord à te connaître, alors tu comprendras pourquoi tu vas tomber malade, et peut-être éviteras-tu de le devenir. »C'est de cette manière que la psychanalyse voudrait instruire le moi. Mais les deux clartés qu'elle nous apporte : savoir que la vie instinctive de la sexualité ne saurait être complètement domptée en nous et que les processus psychiques sont en eux-mêmes inconscients, et ne deviennent accessibles et subordonnés au moi que par une perception incomplète et incertaine, équivalent à affirmer que le moi n'est pas maître dans sa propre maison. Freud, Essais de psychanalyse appliquée.")
    compare("Aragorn était le plus grand de la Compagnie, mais Boromir, de taille légèrement moins élevée, était de carrure plus large et plus lourde. Il passa devant et Aragorn le suivit. Lentement, ils se mirent en marche, et bientôt ils peinaient ferme. La neige leur arrivait par endroits à la poitrine, et Boromir paraissait plutôt nager ou creuser avec ses grands bras que marcher.\n\nAprès les avoir observés un moment, Legolas se tourna vers les autres, un sourire aux lèvres :\n\n— Les plus forts doivent chercher un chemin, disiez-vous ? Mais moi je dis : qu’un laboureur laboure, mais choisissez plutôt une loutre pour nager et pour courir légèrement sur l’herbe et les feuilles, ou même la neige – un Elfe.\n\nSur quoi, il s’élança lestement, et Frodon remarqua pour la première fois, bien qu’il le sût depuis longtemps, que l’Elfe n’avait pas de bottes, mais qu’il portait seulement des chaussures légères, comme il faisait toujours ; et ses pieds laissaient à peine de traces dans la neige.\n\n— Adieu ! dit-il à Gandalf. Je vais chercher le soleil.\n\nAlors, avec la rapidité d’un coureur sur du sable ferme, il partit en flèche, et, ayant vite rattrapé les hommes qui peinaient, il les dépassa avec un signe de la main, il poursuivit son chemin à toute vitesse et disparut derrière l’arête de rocher.\n\nLes autres attendirent, serrés les uns contre les autres et observant jusqu’au moment où Boromir et Aragorn ne furent plus que des points noirs dans la blancheur. Enfin, eux aussi disparurent. Le temps se traîna. Les nuages s’abaissèrent, et quelques flocons de neige recommencèrent à tourbillonner.\n\nUne heure peut-être passa, quoique cela parût bien plus long, et enfin ils virent revenir Legolas. En même temps Boromir et Aragorn reparurent au tournant, loin derrière lui, et ils s’avancèrent péniblement sur la pente.\n\n— Après tout, cria Legolas tandis qu’il accourait, je n’ai pas apporté la Soleil. Elle se promène dans les champs bleus du Sud et une légère couronne de neige sur cette petite butte de Rubicorne ne la trouble nullement. Mais j’ai rapporté un rayon de bonne espérance pour ceux qui sont condamnés à aller à pied. Il y a la plus grande de toutes les congères juste au-delà du tournant, et là, nos Hommes forts ont été presque enterrés. Ils désespéraient jusqu’au moment où je suis revenu leur dire que la congère était à peine plus épaisse qu’un mur. De l’autre côté, la neige diminue tout d’un coup pour devenir un peu plus bas une simple courtepointe blanche pour rafraîchir les pieds des Hobbits.")
    compare("http://La-Philosophie.com Le cas d'une dissertation redigée et corrigée La dissertation en philosophie est un exercice difficile car elle suppose la maîtrise d'une méthode et d'une structure déterminée. Nous vous donnons donc un exemple de dissertation redigée et corrigée par un professeur, tant d'un point de vue méthodologique (forme) qu'éditorial (fond). Nous avons volontairement choisi un sujet de dissertation très classique en terminale philo : \"La liberté est-elle une illusion ?\" (fréquent pour les terminales littéraires) La liberté est-elle une illusion ? Travail préparatoire A) L’analyse des termes du sujet : 1) La liberté : Il s’agit de toujours partir de la conception spontanée, immédiate que l’on se fait de la liberté, celle de l’ « homme de la rue » qu’aurait pu interroger Socrate. Ainsi, la liberté, c’est « faire ce que l’on veut », elle correspond, semble-t-il à la toute-puissance de la volonté de chacun. Spontanément, tout individu se sent libre dès lors qu’il peut accomplir tous ses désirs, toutes ses envies. Or l’expérience ordinaire de la vie montre aussi, paradoxalement, l’être humain soumis à de nombreuses contraintes à la fois externes (physiques, sociales, politiques) et internes (instincts, habitudes, passions) qui pèsent sur sa liberté et qu’il lui est difficile voire impossible de surmonter totalement de sa propre initiative. Dès lors, le sentiment de liberté ne serait-il qu’illusoire ? 2) l’illusion : Il s’agit de saisir l’importance de ce terme à distinguer de l’erreur. L’illusion procède certes de l’erreur en ce qu’elle trompe l’individu, mais elle procède également de la mystification. Qu’est-ce à dire ? Tout individu est responsable de ses erreurs et dispose du pouvoir de les corriger. En revanche, dans l’illusion, qui peut être à la fois individuelle et collective, nous serions victimes d’une puissance trompeuse impossible à vaincre. La question qui s’impose est donc la suivante : Quel type de désir proprement humain se trouve à la racine d’une illusion ? Ou bien quel besoin l’homme cherche-t-il à satisfaire dans la pérennité d’une illusion ? B) Repérer les notions du programme en jeu dans le sujet : la liberté, la conscience et l’inconscient, le désir. C) Problématiser le sujet : Si tout individu éprouve un sentiment immédiat de liberté, cette conviction renvoie-t-elle à une croyance illusoire ou à une véritable connaissance de soi ? L’objectif consistera donc à faire la part de ce qui relève d’une liberté réelle, repérable, de ce qui relève d’un désir infondé de liberté, dans un souci de lucidité et de vérité. http://La-Philosophie.com D) Mobiliser des références utilisables : - Platon, dans le Gorgias, dénonce la confusion commune entre la liberté du sage et la réalisation impulsive de tous ses désirs. - Descartes, dans La Méditation quatrième, donne une définition du libre arbitre qui apparente l’homme à Dieu. - Spinoza, dans L’Ethique, montre que la conscience d’exister n’implique pas nécessairement la liberté humaine. E) Elaboration du plan : elle doit obéir à la règle du « plus proche au plus lointain », c’est-à-dire aller de l’explicite à l’implicite, du plus évident au moins évident. Exemple de plan possible : I) La liberté est un sentiment immédiat : la thèse du libre arbitre II) La critique déterministe du libre arbitre III) La liberté est à conquérir : de la libération à la quête d’autonomie Introduction à la dissertation 1) Amorce : Il nous faut partir de ce constat de départ que le sentiment commun et immédiat éprouvé par tout homme est de se sentir libre : en effet, chaque homme peut faire l’expérience, du moins intérieure, d’une liberté de penser et d’agir, indépendamment de toute contrainte extérieure. Cette conviction intérieure est donc profondément ancrée en chacun de nous. 2) Annonce du sujet et problématisation : Cependant, la liberté ne serait-elle pas une illusion ? Ou pour le dire autrement, le fait de se sentir libre n’est-il pas susceptible de ne renvoyer qu’à une croyance illusoire ? Le sentiment immédiat de notre liberté est-il vrai, c’est-à-dire renvoie-t-il à une véritable connaissance de soi-même ? 3) Annonce du plan d’étude : elle doit être suffisamment explicite sans en dire trop, sans être trop « lourde » : Nous tenterons, tout d’abord, d’évaluer la pertinence et les limites du sentiment spontané de liberté, commun à tous les hommes. Puis nous tâcherons de montrer que cette expérience immédiate du libre arbitre est susceptible de camoufler à l’homme une méconnaissance de lui-même. Enfin, une nouvelle tâche se dressera face à nous : la nécessité de reconstruire une nouvelle approche de la liberté humaine, si tant est qu’elle soit possible. Développement de la dissertation : 1ère partie http://La-Philosophie.com I) Le sentiment immédiat de notre liberté : la théorie du libre arbitre a) Tout homme se juge spontanément libre Dans le langage courant, la liberté renvoie au pouvoir que possède tout homme de n’obéir qu’à lui-même, qu’à sa propre volonté, et d’agir uniquement en fonction de ses désirs, indépendamment de toute contrainte ou de toute pression extérieure. Tout homme se sent donc spontanément libre, tout simplement parce qu’il se croit capable de faire des choix de petite ou de grande importance, de prendre des décisions, de petite ou de grande ampleur. Autrement dit, tout homme, lorsqu’il porte un regard réflexif sur lui-même, se juge spontanément libre, c’est-à-dire en mesure d’agir simplement en fonction de sa volonté. La plupart des philosophes qui se sont prononcés en faveur de la liberté humaine, en faveur de l’existence du libre arbitre, ont accordé une grande valeur à l’expérience intime, immédiate que nous aurions, selon eux, de notre liberté : « La liberté de notre volonté, écrit Descartes (Principes de la Philosophie, I, art.39), se connaît sans preuve par la seule expérience que nous en avons ». Transition : Faire le point et formuler une ou plusieurs questions permettant de poursuivre la réflexion : La liberté correspondrait donc à un sentiment intérieur, à une expérience immédiate en chaque homme. Or peut-on se contenter de cette expérience immédiate ou pour reprendre la formulation de Bergson, de cette « donnée immédiate de la conscience » ? Autrement dit, peut-on se contenter du sentiment de notre liberté pour en déduire son existence certaine ? Est-il donc possible de faire une expérience de notre liberté qui puisse justifier ce sentiment ? b) Peut-on prouver l’existence du libre arbitre ? 1) Première tentative de preuve : l’expérience de l’âne de Buridan et la mise à jour de la « liberté d’indifférence » Jean Buridan, philosophe français du quatorzième siècle, aurait, selon la légende, conçu une expérience imaginaire afin de prouver l’existence du libre arbitre : la situation serait celle d’un animal, en l’occurrence un âne, ayant également faim et soif, et qui, placé à égale distance d’une botte de foin et d’un seau d’eau, hésite, se montre incapable de choisir, et finalement se laisse mourir. Ce « protocole expérimental métaphysique » aurait donc pour objectif de prouver l’existence de la « liberté d’indifférence » proprement humaine. En effet, nous avons tous déjà vécu une situation où les mobiles ou motifs en faveur d’un acte ou d’un autre étaient si équivalents, ou aussi contraignants l’un que l’autre, que nous nous sommes retrouvés incapables de faire un choix. En effet, que se passe-t-il lorsqu’un individu se retrouve face à deux possibilités aussi équivalentes l’une que l’autre, lorsque rien ne puisse permettre de déterminer son choix ? Or ce qui permet à l’homme d’échapper à la situation absurde de l’âne mourant de faim et de soif entre une botte de http://La-Philosophie.com foin et un seau d’eau, c’est qu’il dispose de cette liberté d’indifférence, c’est-à-dire de cette liberté par laquelle notre volonté a le pouvoir de choisir spontanément et de sa propre initiative. Cette situation d’indifférence du choix prouve donc que l’homme est doté d’un libre arbitre, c’està-dire d’une capacité de choisir pouvant échapper à tout déterminisme. Pour Descartes, cette liberté d’indifférence, bien que considérée comme « le plus bas degré de la liberté », témoigne en même temps d’un pur libre arbitre qui apparente l’homme à Dieu (Méditation quatrième). 2) Seconde tentative de preuve du libre arbitre : le crime de Lafcadio dans Les Caves du Vatican d’André Gide André Gide, dans Les Caves du Vatican, cherche à illustrer la possibilité pour un être humain de réaliser un acte gratuit, c’est-à-dire un acte accompli sans raison, par le seul effet de sa liberté. Dans le roman, le « héro » Lafcadio se rend à Rome par le train et se retrouve seul dans la nuit, ne partageant son compartiment qu’avec un vieux monsieur. Lafcadio se prend alors d’une idée folle : « Là sous ma main, la poignée. Il suffirait de la tirer et de le pousser en avant. On n’entendrait même pas un cri dans la nuit. Qui le verrait…Un crime immotivé, quel embarras pour la police ». Lafcadio se dit en effet, et à juste titre, que s’il n’a pas de mobiles pour réaliser ce crime, il n’a donc pas de motivations. Le lien entre l’acteur et l’acte commis est inexistant. Lafcadio prend d’ailleurs un soin tout particulier à renforcer la gratuité de son crime : il remet tout au hasard et se met à compter pour soumettre sa décision de passer à l’acte ou de ne pas passer à l’acte à l’apparition d’un feu dans la nuit. Or le hasard, c’est précisément ce qui est fortuit, c’est-à-dire dépourvu de toute intention consciente, donc de motivation intrinsèque… Et le crime a lieu. 3) Peut-on dire que l’acte de Lafcadio est un acte gratuit ? Le mérite du roman d’André Gide est d’aborder la question suivante : Un acte gratuit est-il possible ? Or deux critiques permettent d’être avancées pour remettre en cause cette possibilité : La première critique consistera à remarquer que Lafcadio fait reposer son passage à l’acte sur des signes extérieurs, en l’occurrence l’apparition ou la non apparition d’un feu dans la campagne. Son acte serait donc déterminé par une extériorité. La seconde critique consistera à remarquer que l’absence de motivations dans l’acte de Lafcadio est tout sauf évidente : l’une de ses premières motivations ne serait-elle pas le désir même de se prouver à lui-même sa liberté ? Si bien qu’il est tout-à fait envisageable de soupçonner Lafcadio de prendre pour une absence de motifs ce qui ne serait au fond qu’une ignorance profonde des motifs de son acte. L’ « acte gratuit » est donc une notion philosophiquement problématique : la volonté de prouver sa liberté par un acte supposé sans mobile constitue, par elle-même, un mobile. Transition : Une nouvelle question se pose dès lors : le sentiment de liberté ou la volonté de réaliser un acte non déterminé ne seraient-ils pas qu’une croyance ? Ne semble-t-il pas que ce ne http://La-Philosophie.com soit que de façon illusoire et superficielle que je fasse l’ « expérience » de ma liberté, par ignorance des déterminations qui sont pourtant en jeu ? Développement de la dissertation : 2ème partie II) La critique déterministe du libre arbitre a) L’illusion anthropocentrique du libre arbitre : « L’homme n’est pas un empire dans un empire » (Spinoza) Le projet philosophique de B.Spinoza, dans le sillage des travaux scientifiques de Laplace, est de dénoncer les illusions du libre arbitre. C’est ainsi que dans la troisième partie de l’Ethique, dans la section intitulée De l’origine et de la nature des affections, Spinoza rejette totalement l’idée selon laquelle l’homme occuperait une place privilégiée au sein de la nature. Spinoza critique notamment Descartes qui conçoit l’homme comme « un empire dans un empire », ainsi que tous les philosophes qui croient que « l’homme trouble l’ordre de la Nature plutôt qu’il ne le suit, qu’il a sur ses propres actions un pouvoir absolu et ne tire que de lui-même sa détermination ». Or l’objectif de Spinoza est bel et bien de montrer que l’homme suit les lois communes de la Nature, comme toutes les choses de ce monde. b) L’illusion humaine de la liberté C’est dans sa lettre à Schuller, extraite de sa Correspondance, que Spinoza dénonce l’illusion du libre arbitre. Il défend ainsi une position philosophique déterministe suivant laquelle tous les événements sont absolument nécessaires et le sentiment que nous avons d’être libres ne serait qu’une illusion naturelle : « Telle est cette liberté humaine que tous les hommes se vantent d’avoir et qui consiste en cela seul que les hommes sont conscients de leurs désirs et ignorants des causes qui les déterminent ». Et Spinoza d’ajouter un peu plus loin : « Et comme ce préjugé est inné en tous les hommes, ils ne s’en libèrent pas facilement ». Cette illusion naturelle de l’homme a donc deux causes d’après Spinoza qui justifient que l’homme s’illusionne et qu’il ne fasse pas seulement erreur. Premièrement, la source de l’illusion humaine du libre arbitre est l’ignorance des causes qui nous poussent à agir. Or à prendre les choses rigoureusement, l’homme est tout aussi déterminé à se mouvoir sous l’influence de causes externes qu’une pierre qui reçoit une impulsion. Les hommes se croient libres alors qu’ils sont contraints ou déterminés par leur nature. Deuxièmement, Spinoza précise bien que les hommes « se vantent » d’être libre car le désir d’être libre, même illusoire, est beaucoup plus valorisant pour l’orgueil humain que l’idée d’être totalement déterminé. http://La-Philosophie.com c) La liberté désigne alors la nécessité bien comprise C’est ainsi que Spinoza ne fait pas consister la liberté, dans la lettre à Schuller, dans un libre décret mais dans une libre nécessité ou dans la nécessité bien comprise : « j’appelle libre, quant à moi, une chose qui est et agit par la seule nécessité de sa nature ». Tout comme les comportements des animaux sont déterminés par l’instinct, leur environnement ou des déterminations biologiques, les actes et les pensées des hommes le sont eux-mêmes par de multiples facteurs à la fois internes et externes dont on ignore le plus souvent l’existence et la puissance : facteurs d’origine physiologiques, psychologiques, sociales, etc. Dès lors, l’un des apports essentiels de la critique spinoziste du libre arbitre est de montrer que la croyance en l’existence du libre arbitre est la source d’aliénation de l’homme. En effet, selon Spinoza, non seulement l’homme est déterminé mais cette illusion naturelle du libre arbitre nous déterminent à ne pas savoir que nous sommes déterminés, et ainsi à l’être d’autant plus sûrement. Or il n’y a pas pire esclave que celui qui se croit libre. Transition : Il nous faut donc tirer les enseignements de la critique spinoziste du libre arbitre et reconnaître que l’idée d’une liberté spontanée ou d’un sentiment immédiat de liberté n’est plus tenable. Est-il dès lors possible de reconstruire une approche de la liberté qui soit accessible à l’homme ? Développement de la dissertation ; 3ème et dernière partie III) La liberté est à conquérir : de la libération à la quête d’autonomie a) Être libre, c’est apprendre à se libérer des passions Platon, dans le Gorgias, pose la question suivante : est-ce la vie de l’homme aux désirs insatiables ou celle guidée par la raison qui est la meilleure ? Dans ce dialogue qui met aux prises Socrate et Calliclès, ce dernier défend le droit au désir, comme un droit à être puissant, autrement dit à être capable de mettre les forces de son énergie et de son intelligence au service des passions, pour leur donner la plus grande ampleur possible. C’est ainsi que Calliclès préfère les « tonneaux qui fuient » puisque « ce qui fait l’agrément de la vie, c’est de verser le plus possible ». En revanche, Socrate choisit la vie ordonnée, celle où les tonneaux du sage « seraient en bon état ». Platon cherche ainsi à montrer, dans ce dialogue, l’illusion dans laquelle se trouvent les hommes comme Calliclès, qui croient qu’être libre consiste à faire ce que l’on veut, c’est-à-dire à réaliser tous ses désirs. Or une telle vie, guidée par des désirs multiples, polymorphes et surtout infinis, mène nécessairement au tourment et au malheur. En effet, le risque pour un homme comme http://La-Philosophie.com Calliclès décidant de mener une vie intempérante et désordonnée est de devenir l’esclave de ses propres passions et désirs. A cette vie désordonnée, Platon oppose une vie guidée par la raison, incarnée par la sagesse socratique. Socrate incarne, en effet, le sage qui sait distinguer entre les désirs à poursuivre ou à ne pas poursuivre, qui sait se gouverner lui-même et qui est en mesure d’accéder à une véritable autonomie de la volonté. b) Être libre, c’est être responsable de ses actes Par conséquent, l’entrée dans la liberté authentique, par opposition avec la liberté illusoire des désirs infinis, c’est l’entrée dans une véritable autonomie et c’est pouvoir devenir responsable de ses actes et pouvoir en répondre. L’enjeu de l’entrée dans la liberté authentique est donc celui du rapport à soi-même et à autrui. La liberté entre alors dans le champ de la réflexion morale, sociale et politique. C’est ainsi qu’au sens moral et juridique, être libre, c’est pouvoir être reconnu autonome et responsable de ses actes, de ses choix, à la fois devant soi-même et devant la société à laquelle on appartient. En conséquence, si la liberté est illusoire ou inaccessible, il semble que c’en soit fini de la responsabilité morale et juridique de tout individu, et par là même de la justice. Le fait que nous nous sentions, à tort ou à raison libre, exige donc que l’on agisse comme si on était effectivement libre. c) La liberté comme condition de l’acte éthique C’est ainsi que dans la première note de la préface à la Critique de la raison pratique, Kant affirme que la liberté est la condition de possibilité et l’essence (la ratio essendi) de la vie morale de l’homme, comme la vie morale de l’homme est ce par quoi l’homme connaît la réalité de sa liberté (elle en est la ratio cognoscendi). Et Kant ajoute pour préciser : « (…) si la loi morale n’était pas d’abord clairement conçue dans notre raison, nous ne nous croirions jamais autorisés à admettre une chose telle que la liberté (…). En revanche, s’il n’y avait pas de liberté, la loi morale ne saurait nullement être rencontrée en nous ». Ainsi, pour Kant, pour que l’homme soit moral, il faut qu’il soit libre, car s’il était forcé par une nature intelligible à la bonté, à la justice et à l’altruisme, il ne serait qu’un automate spirituel et s’il était forcé par sa nature sensible à l’égoïsme, il ne serait qu’un mécanisme matériel. Conclusion de de notre exemple sur la dissertation philosophique 1) Faire le bilan de la démarche poursuivie dans le devoir : La liberté humaine est-elle donc possible ? Nous avons pu comprendre, tout au long de notre travail, la difficulté qui existe à http://La-Philosophie.com pouvoir saisir une véritable « expérience » de la liberté et, par conséquent, la difficulté à en prouver véritablement l’existence. 2) Répondre à la question initiale : La liberté est-elle une illusion ? Notre travail a, en tout cas, cherché à démontrer que si la croyance en une liberté immédiate était illusoire, voire naïve, la critique spinoziste nous a permis d’accéder à une approche de la liberté qui puisse permettre d’en préserver l’espoir : en effet, si l’homme n’est pas libre, il lui est, en revanche, donné d’entrer dans un processus, dans une conquête assimilable à une libération par l’usage de la raison et par son entrée dans la morale et la vie sociale. 3) Si possible, proposer une ouverture à une nouvelle réflexion : Comment penser les conséquences d’une authentique libération de l’homme dans ses interactions morales, sociales et politiques ? Vincent Boyer, professeur de philosophie à Paris. Pour aller plus loin sur le bac philosophie : Méthode de la dissertation philosophique Le Commentaire de Philosophie Aide à la dissertation de Philosophie")
  }
}
