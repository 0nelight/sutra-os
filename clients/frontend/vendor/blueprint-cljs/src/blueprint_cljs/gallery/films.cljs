(ns blueprint-cljs.gallery.films
  "The Top 100 films of Blueprint's select examples, and their renderers."
  (:require [clojure.string :as str]
            [blueprint-cljs.menu :as menu]
            [blueprint-cljs.query-list :as ql]))

(def top-100-films
  (vec (map-indexed
        (fn [i [title year]] {:title title :year year :rank (inc i)})
        [["The Shawshank Redemption" 1994]
         ["The Godfather" 1972]
         ["The Godfather: Part II" 1974]
         ["The Dark Knight" 2008]
         ["12 Angry Men" 1957]
         ["Schindler's List" 1993]
         ["Pulp Fiction" 1994]
         ["The Lord of the Rings: The Return of the King" 2003]
         ["The Good, the Bad and the Ugly" 1966]
         ["Fight Club" 1999]
         ["The Lord of the Rings: The Fellowship of the Ring" 2001]
         ["Star Wars: Episode V - The Empire Strikes Back" 1980]
         ["Forrest Gump" 1994]
         ["Inception" 2010]
         ["The Lord of the Rings: The Two Towers" 2002]
         ["One Flew Over the Cuckoo's Nest" 1975]
         ["Goodfellas" 1990]
         ["The Matrix" 1999]
         ["Seven Samurai" 1954]
         ["Star Wars: Episode IV - A New Hope" 1977]
         ["City of God" 2002]
         ["Se7en" 1995]
         ["The Silence of the Lambs" 1991]
         ["It's a Wonderful Life" 1946]
         ["Life Is Beautiful" 1997]
         ["The Usual Suspects" 1995]
         ["Léon: The Professional" 1994]
         ["Spirited Away" 2001]
         ["Saving Private Ryan" 1998]
         ["Once Upon a Time in the West" 1968]
         ["American History X" 1998]
         ["Interstellar" 2014]
         ["Casablanca" 1942]
         ["City Lights" 1931]
         ["Psycho" 1960]
         ["The Green Mile" 1999]
         ["The Intouchables" 2011]
         ["Modern Times" 1936]
         ["Raiders of the Lost Ark" 1981]
         ["Rear Window" 1954]
         ["The Pianist" 2002]
         ["The Departed" 2006]
         ["Terminator 2: Judgment Day" 1991]
         ["Back to the Future" 1985]
         ["Whiplash" 2014]
         ["Gladiator" 2000]
         ["Memento" 2000]
         ["The Prestige" 2006]
         ["The Lion King" 1994]
         ["Apocalypse Now" 1979]
         ["Alien" 1979]
         ["Sunset Boulevard" 1950]
         ["Dr. Strangelove or: How I Learned to Stop Worrying and Love the Bomb" 1964]
         ["The Great Dictator" 1940]
         ["Cinema Paradiso" 1988]
         ["The Lives of Others" 2006]
         ["Grave of the Fireflies" 1988]
         ["Paths of Glory" 1957]
         ["Django Unchained" 2012]
         ["The Shining" 1980]
         ["WALL·E" 2008]
         ["American Beauty" 1999]
         ["The Dark Knight Rises" 2012]
         ["Princess Mononoke" 1997]
         ["Aliens" 1986]
         ["Oldboy" 2003]
         ["Once Upon a Time in America" 1984]
         ["Witness for the Prosecution" 1957]
         ["Das Boot" 1981]
         ["Citizen Kane" 1941]
         ["North by Northwest" 1959]
         ["Vertigo" 1958]
         ["Star Wars: Episode VI - Return of the Jedi" 1983]
         ["Reservoir Dogs" 1992]
         ["Braveheart" 1995]
         ["M" 1931]
         ["Requiem for a Dream" 2000]
         ["Amélie" 2001]
         ["A Clockwork Orange" 1971]
         ["Like Stars on Earth" 2007]
         ["Taxi Driver" 1976]
         ["Lawrence of Arabia" 1962]
         ["Double Indemnity" 1944]
         ["Eternal Sunshine of the Spotless Mind" 2004]
         ["Amadeus" 1984]
         ["To Kill a Mockingbird" 1962]
         ["Toy Story 3" 2010]
         ["Logan" 2017]
         ["Full Metal Jacket" 1987]
         ["Dangal" 2016]
         ["The Sting" 1973]
         ["2001: A Space Odyssey" 1968]
         ["Singin' in the Rain" 1952]
         ["Toy Story" 1995]
         ["Bicycle Thieves" 1948]
         ["The Kid" 1921]
         ["Inglourious Basterds" 2009]
         ["Snatch" 2000]
         ["3 Idiots" 2009]
         ["Monty Python and the Holy Grail" 1975]])))

(defn film-item-props
  [film {:keys [handle-click handle-focus modifiers query ref id]}]
  {:active   (:active modifiers)
   :disabled (:disabled modifiers)
   :id       id
   :label    (str (:year film))
   :on-click handle-click
   :on-focus handle-focus
   :ref      ref
   :text     (ql/highlight-text (str (:rank film) ". " (:title film)) query)})

(defn render-film
  [film item-props]
  (when (get-in item-props [:modifiers :matches-predicate])
    ^{:key (:rank film)}
    [menu/menu-item (assoc (film-item-props film item-props) :role-structure :listoption)]))

(defn render-create-film
  [query active? handle-click]
  [menu/menu-item {:icon                   :add
                   :text                   (str "Create \"" query "\"")
                   :role-structure         :listoption
                   :active                 active?
                   :on-click               handle-click
                   :should-dismiss-popover false}])

(defn filter-film
  [query film _index exact-match?]
  (let [title (str/lower-case (:title film))
        q     (str/lower-case query)]
    (if exact-match?
      (= title q)
      (str/includes? (str (:rank film) ". " title " " (:year film)) q))))

(defn create-film
  [title]
  {:rank  (+ 100 (rand-int 100) 1)
   :title title
   :year  (.getFullYear (js/Date.))})

(defn films-equal?
  [a b]
  (= (str/lower-case (:title a)) (str/lower-case (:title b))))
