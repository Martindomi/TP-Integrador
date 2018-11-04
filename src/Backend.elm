module Backend exposing(..)
import Models exposing(Movie, Preferences)
import List exposing( member )

completaAca = identity

-- **************
-- Requerimiento: filtrar películas por su título a medida que se escribe en el buscador;
-- **************

filtrarPeliculasPorPalabrasClave : String -> List Movie -> List Movie
filtrarPeliculasPorPalabrasClave palabras = List.filter (peliculaTienePalabrasClave palabras)

--
-- Además tiene dos problemas, que también deberías corregir:
--
-- * busca una coincidencia exacta, pero si escribís "Avengers Ultron" debería encontrar a "Avengers: Age Of Ultron"
--

peliculaTienePalabrasClave : String -> Movie -> Bool
peliculaTienePalabrasClave palabras pelicula = String.foldr contains (String.toUpper palabras) (String.toUpper pelicula.title)


-- **************
-- Requerimiento: visualizar las películas según el género elegido en un selector;
-- **************

-- los generos de la pagina (terror y suspenso) fueron modificados para poder compararlos con los de la pelicula

filtrarPeliculasPorGenero : String -> List Movie -> List Movie
filtrarPeliculasPorGenero genero = List.filter (peliculaTieneGenero genero)

peliculaTieneGenero : String -> Movie -> Bool
peliculaTieneGenero genero pelicula = List.member genero pelicula.genre

-- **************
-- Requerimiento: filtrar las películas que sean aptas para menores de edad,
--                usando un checkbox;
-- **************

filtrarPeliculasPorMenoresDeEdad : Bool -> List Movie -> List Movie
filtrarPeliculasPorMenoresDeEdad mostrarSoloMenores peliculas = if mostrarSoloMenores then listaMenores peliculas else peliculas

listaMenores : List Movie -> List Movie
listaMenores = List.filter ( esMenor ) 

esMenor : Movie -> Bool
esMenor pelicula = pelicula.forKids 



-- **************
-- Requerimiento: ordenar las películas por su rating;
-- **************

ordenarPeliculasPorRating : List Movie -> List Movie
ordenarPeliculasPorRating = orden

orden : List Movie -> List Movie
orden peliculas =
 case peliculas of
  [] -> []
  (x::ys) -> orden (List.filter (tieneMasRating x) ys) ++ [x] ++ orden (List.filter(not << tieneMasRating x) ys)  

tieneMasRating : Movie -> Movie -> Bool
tieneMasRating pelicula1 pelicula2 = pelicula1.rating <= pelicula2.rating

-- **************
-- Requerimiento: dar like a una película
-- **************

darLikeAPelicula : Int -> List Movie -> List Movie
darLikeAPelicula id peliculas = List.map (aumentarLikes id)  peliculas

aumentarLikes : Int -> Movie -> Movie
aumentarLikes id pelicula = if pelicula.id == id then { pelicula | likes = pelicula.likes +1 } else  pelicula 

-- **************
-- Requerimiento: cargar preferencias a través de un popup modal,
--                calcular índice de coincidencia de cada película y
--                mostrarlo junto a la misma;
-- **************
-- type alias Preferences = {keywords: String,genre: String,favoriteActor: String}

calcularPorcentajeDeCoincidencia : Preferences -> List Movie -> List Movie
calcularPorcentajeDeCoincidencia preferencias peliculas = List.map (noMasde100 << calcularIndice preferencias) peliculas

calcularIndice : Preferences -> Movie -> Movie
calcularIndice preferencia pelicula =  (coincideActor preferencia (coincideGenero preferencia (coincidePalabra preferencia (coincide preferencia pelicula)))) 
--habria que aplicar composicion.

sumaPuntos : Int -> Movie -> Movie
sumaPuntos pts pelicula = { pelicula | matchPercentage = pelicula.matchPercentage + pts }

coincidePalabra : Preferences -> Movie -> Movie
coincidePalabra preferencia pelicula = if peliculaTienePalabrasClave preferencia.keywords pelicula then sumaPuntos 20 pelicula else pelicula

coincideActor : Preferences -> Movie -> Movie
coincideActor preferencia pelicula = if List.member preferencia.favoriteActor pelicula.actors then sumaPuntos 50 pelicula else pelicula

coincideGenero : Preferences -> Movie -> Movie
coincideGenero preferencia pelicula = if peliculaTieneGenero preferencia.genre pelicula then sumaPuntos 60 pelicula else pelicula

coincide : Preferences -> Movie -> Movie
coincide preferencia pelicula = if (String.contains "terror" preferencia.genre && peliculaTieneGenero "suspenso" pelicula) then sumaPuntos 15 pelicula else pelicula
  
noMasde100 : Movie -> Movie
noMasde100 pelicula =  { pelicula | matchPercentage = min 100 pelicula.matchPercentage } 
