(ns backup-uploader.tagmapper
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]))

(declare rosetta-stone)

(defn translate-artist [tag]
  (str/replace tag #"^artist:" "artist: "))

(defn translate-tag [tag]
  (translate-artist
    (match [(rosetta-stone tag)]
      [nil] tag
      [translated-tag] translated-tag)))

(def rosetta-stone
  {"connie" "connie (steven universe)",
   "white diamond" "white diamond (steven universe)",
   "white pearl" "white pearl (steven universe)",
   "pearl" "pearl (steven universe)",
   "lapis lazuli" "lapis lazuli (steven universe)",
   "sapphire" "sapphire (steven universe)",
   "peridot" "peridot (steven universe)",
   "rose quartz" "rose quartz (steven universe)",
   "jasper" "jasper (steven universe)",
   "sugilite" "sugilite (steven universe)",
   "opal" "opal (steven universe)",
   "sardonyx" "sardonyx (steven universe)",
   "amethyst" "amethyst (steven universe)",
   "ruby" "ruby (steven universe)",
   "garnet" "garnet (steven universe)",
   "lion" "lion (steven universe)",
   "steven" "steven (steven universe)",
   "stevonnie" "stevonnie (steven universe)",
   "alexandrite" "alexandrite (steven universe)",
   "malachite" "malachite (steven universe)",
   "amethyst guards" "amethyst guards (steven universe)",
   "crystal temps" "crystal temps (steven universe)",
   "pumpkin" "pumpkin (steven universe)",
   "homeworld" "homeworld (steven universe)",
   "back to the future" "fandom: back to the future",
   "bismuth" "bismuth (steven universe)",
   "greg" "greg (steven universe)",
   "lars" "lars (steven universe)",
   "sadie" "sadie (steven universe)",
   "corrupted gems" "corrupted gems (steven universe)",
   "blue pearl" "blue pearl (steven universe)",
   "blue diamond" "blue diamond (steven universe)",
   "buddy buddwick" "buddy buddwick (steven universe)",
   "palanquin" "palanquin (steven universe)",
   "rainbow quartz" "rainbow quartz (steven universe)",
   "garnet (first form)" "young garnet (steven universe)",
   "amethyst (young)" "young amethyst (steven universe)",
   "rose's battle flag" "rose's battle flag (steven universe)",
   "roaming eye" "roaming eye (steven universe)",
   "kindergarten" "kindergarten (steven universe)",
   "mystery girl" "mystery girl (steven universe)",
   "it's over isn't it" "it's over isn't it (steven universe)",
   "crystal temple" "crystal temple (steven universe)",
   "centipeetle" "centipeetle (steven universe)",
   "yellow diamond" "yellow diamond (steven universe)",
   "what's the use of feeling blue" "what's the use of feeling blue (steven universe)",
   "yellow pearl" "yellow pearl (steven universe)",
   "implied future vision" "implied future vision (steven universe)",
   "the cool kids" "the cool kids (steven universe)",
   "buck dewey" "buck dewey (steven universe)",
   "jenny pizza" "jenny pizza (steven universe)",
   "sour cream" "sour cream (steven universe)",
   "vidalia" "vidalia (steven universe)",
   "aquamarine" "aquamarine (steven universe)",
   "holly blue agate" "holly blue agate (steven universe)",
   "smoky quartz" "smoky quartz (steven universe)",
   "topaz" "topaz (steven universe)",
   "padparadscha" "padparadscha (steven universe)",
   "blue zircon" "blue zircon (steven universe)",
   "yellow zircon" "yellow zircon (steven universe)",
   "pearl point" "pearl point (steven universe)",
   "implied rhodonite" "implied rhodonite (steven universe)"})

