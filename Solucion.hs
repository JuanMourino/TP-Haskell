module Solucion where
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
nombresDeUsuarios ([], _, _) = []
nombresDeUsuarios (((_, nombre):xs), _, _) | nombrePertenece nombre xs = nombresDeUsuarios (xs, [], [])
                                           | otherwise = nombre : nombresDeUsuarios (xs, [], [])
--Indica si un nombre de usuario esta contenido en una lista de usuarios
nombrePertenece :: String -> [Usuario] -> Bool
nombrePertenece _ [] = False
nombrePertenece nombre ((_,y):xs) = nombre == y || nombrePertenece nombre xs

-- describir qué hace la función: Dada una Red Social, lee la lista de los usuarios y devuelve los nombres
-- Si hay algun nombre repetido, lo devuelve una unica vez en la lista.
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (_, [], _) _ = []
amigosDe (_, (pu, su) : xs, _) u | pu == u = su : amigosDe ([], xs, []) u
                                 | su == u = pu : amigosDe ([], xs, []) u
                                 | otherwise = amigosDe ([], xs, []) u

-- describir qué hace la función: Dada una Red Social y un usuario, lee la lista de las relaciones y devuelve
-- todos los usuarios que estan en una relacion con el usuario ingresado.
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos (_, [], _) _ = 0
cantidadDeAmigos (_, (pu, su) : xs, _) u | pu == u || su == u = 1 + cantidadDeAmigos ([], xs, []) u
                                         | otherwise = cantidadDeAmigos ([], xs, []) u

-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos ([usuario], _, _) = usuario
usuarioConMasAmigos ((u1 : u2 : us), relaciones, _) | (cantidadDeAmigos ((u1: u2: us), relaciones, []) u1) > (cantidadDeAmigos ((u1: u2: us), relaciones, []) u2) = usuarioConMasAmigos ((u1 : us), relaciones, [])
                                                    | otherwise = usuarioConMasAmigos ((u2 : us), relaciones, [])

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos (_, [], _) = False
estaRobertoCarlos ([], _, _) = False
estaRobertoCarlos (u:us, relaciones, _) = cantidadDeAmigos ([], relaciones, []) u >= 1000000 || estaRobertoCarlos (us, relaciones, [])

-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (_, _, []) _ = []
publicacionesDe (_, _, ((u, p, us) : ps)) usuario | u == usuario = (u, p, us) : publicacionesDe ([], [], ps) usuario
                                                  | otherwise = publicacionesDe ([], [], ps) usuario

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (_, _, []) _ = []
publicacionesQueLeGustanA (_, _, (publicacion : ps)) u | gustaPublicacion u publicacion = publicacion : publicacionesQueLeGustanA ([], [], ps) u
                                                       | otherwise = publicacionesQueLeGustanA ([], [], ps) u

--Ve la lista de usuarios a los que les gusta una publicacion e indica si un usuario dado esta dentro de esa lista
gustaPublicacion :: Usuario -> Publicacion -> Bool
gustaPublicacion _ (_, _, []) = False
gustaPublicacion u (poster, _, (u1 : us)) = u == u1 || gustaPublicacion u (poster , [], us)

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = (publicacionesQueLeGustanA red u1) == (publicacionesQueLeGustanA red u2)

-- describir qué hace la función: .....
--Use us para representar la secuencia de usuarios en vez de (x : xs) para poder contemplar el caso de que sea vacia
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel (_, _, []) _ = False
tieneUnSeguidorFiel (_, _, ((u1, _, us) : ps)) u | u1 == u && us == [] = False
                                                 | u1 == u && publicacionesDe ([], [], (u1, [], us) : ps) u1 == publicacionesQueLeGustanA ([], [], (u1, [], us) : ps) (head us) = True
                                                 | otherwise = tieneUnSeguidorFiel ([], [], ((u1, [], (tail us)) : ps)) u1

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined
