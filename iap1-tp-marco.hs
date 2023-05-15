-- Completar con los datos del grupo
--
-- Nombre de Grupo: xx
-- Integrante 1: Nombre Apellido, email, LU
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us




-- Ejercicios

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red = proyectarNombres (usuarios red)

proyectarNombres :: [Usuario] -> [String]
proyectarNombres us = eliminarRepetidos (proyectarNombresAux us)

proyectarNombresAux :: [Usuario] -> [String]
proyectarNombresAux [] = []
proyectarNombresAux (x:xs) = nombreDeUsuario x : proyectarNombresAux xs



-- describir qué hace la función: .....

amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe rs u = eliminarRepetidos (amigosDeAux (relaciones rs) u)

amigosDeAux :: [Relacion] -> Usuario -> [Usuario]
amigosDeAux [] u = []
amigosDeAux ((u1, u2):us) u | u == u1 = u2 : amigosDeAux us u
                            | u == u2 = u1 : amigosDeAux us u
                            | otherwise = amigosDeAux us u
                     


-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos rs u = longitud (amigosDe rs u)



-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos ([u], x, y) = u
usuarioConMasAmigos ((u1:u2:us), x, y) | cantidadDeAmigos rs u1 > cantidadDeAmigos rs u2 = usuarioConMasAmigos ((u1:us), x, y)
                                       | otherwise = usuarioConMasAmigos ((u2:us), x, y)
                                       where rs = ((u1:u2:us), x, y)



-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos rs | cantidadDeAmigos rs (usuarioConMasAmigos rs) >= 1000000000 = True
                     | otherwise =  False



-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe rs u = eliminarRepetidos (auxiliarPublicacionesDe rs u)

auxiliarPublicacionesDe :: RedSocial -> Usuario -> [Publicacion]
auxiliarPublicacionesDe (x, y, []) u = []
auxiliarPublicacionesDe (x, y, (us, s, l):pbs) u | us == u = (us, s, l) : auxiliarPublicacionesDe (x, y, pbs) u
                                                 | otherwise = auxiliarPublicacionesDe (x, y, pbs) u


-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA rs u = eliminarRepetidos (auxiliarPublicacionesQueLeGustanA rs u)

auxiliarPublicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]    
auxiliarPublicacionesQueLeGustanA (x, y, []) u = []
auxiliarPublicacionesQueLeGustanA (x, y, pb:pbs) u | pertenece (likesDePublicacion pb) u = pb : auxiliarPublicacionesQueLeGustanA (x, y, pbs) u
                                                   | otherwise = auxiliarPublicacionesQueLeGustanA (x, y, pbs) u                                                



-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones rs u1 u2 | publicacionesQueLeGustanA rs u1 == publicacionesQueLeGustanA rs u2 = True
                                         | otherwise = False



-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel rs u | auxiliarTieneUnSeguidorFiel rs u (likesDePublicacion (head (publicacionesDe rs u))) == [] = False
                         | auxiliarTieneUnSeguidorFiel rs u (likesDePublicacion (head (publicacionesDe rs u))) == [u] = False
                         | otherwise = True


auxiliarTieneUnSeguidorFiel :: RedSocial -> Usuario -> [Usuario] -> [Usuario]
auxiliarTieneUnSeguidorFiel rs u1 [] = []
auxiliarTieneUnSeguidorFiel rs u1 (u:us) | leGustanTodasLasPublicacionesDe rs u1 u = u : (auxiliarTieneUnSeguidorFiel rs u1 us)
                                         | otherwise = auxiliarTieneUnSeguidorFiel rs u1 (us)
                                         

leGustanTodasLasPublicacionesDe :: RedSocial -> Usuario -> Usuario -> Bool
leGustanTodasLasPublicacionesDe rs u1 u2 | estaContenidaEn (publicacionesDe rs u1) (publicacionesQueLeGustanA rs u2) = True
                                         | otherwise = False



-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos rs u1 u2 | pertenece (aplanarRedDeAmigos rs (amigosDe rs u1)) u2 = True
                                 | otherwise = False

aplanarRedDeAmigos :: RedSocial -> [Usuario] -> [Usuario]
aplanarRedDeAmigos rs us | mismosElementos (us ++ auxiliarAplanarRedDeAmigos rs us) us = us
                         | otherwise = aplanarRedDeAmigos rs (us ++ auxiliarAplanarRedDeAmigos rs us) 


auxiliarAplanarRedDeAmigos :: RedSocial -> [Usuario] -> [Usuario]
auxiliarAplanarRedDeAmigos rs [] = []
auxiliarAplanarRedDeAmigos rs [u] = amigosDe rs u
auxiliarAplanarRedDeAmigos rs (u:us) = amigosDe rs u ++ auxiliarAplanarRedDeAmigos rs us



-- Funciones auxiliares de las guias

-- Funcion "longitud" que dada una lista devuelve su longitud
longitud :: [t] -> Int
longitud l | null l = 0
           | otherwise = 1 + longitud (tail l)


-- Funcion "pertenece" que devuelve True si x es un elemento de l y False si no
pertenece :: [t] -> (Eq t) => t -> Bool
pertenece l x | null l = False
              | head l == x = True
              | otherwise = pertenece (tail l) x 


-- quita a la primera aparicion de x de la lista xs 
quitar :: (Eq t) => t -> [t] -> [t]
quitar x xs | not (pertenece xs x) = xs 
            | x == head xs = tail xs 
            | otherwise = head xs : quitar x (tail xs)
           

-- Funcion "quitarTodos" que elimina todas las apariciones de x de la lista xs
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos x xs | not (pertenece (quitar x xs) x) = quitar x xs
                 | otherwise = quitarTodos x (quitar x xs)


-- Funcion "eliminarRepetidos" que elimina las apariciones repetidas de elementos de la lista de entrada
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos [x] = [x]
eliminarRepetidos (x:xs) | pertenece xs x = x : (eliminarRepetidos (quitarTodos x xs))
                         | otherwise = x : eliminarRepetidos xs


-- Funcion "mismosElementos" que devuelve True sii dos listas tienen los mismos elementos, sin contar repeticiones
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos xs ys | longitud (eliminarRepetidos xs) /= longitud (eliminarRepetidos ys) = False
                      | otherwise = estaContenidaEn (eliminarRepetidos xs) (eliminarRepetidos ys)


-- Funcion que devuelve True sii todos los elementos de la primera lista son parte de la segunda, y devuelve False si no
estaContenidaEn :: (Eq t) => [t] -> [t] -> Bool
estaContenidaEn [] ys = True
estaContenidaEn (x:xs) ys | not (pertenece ys x) = False
                          | pertenece ys x = estaContenidaEn xs ys
                          | otherwise = True














































