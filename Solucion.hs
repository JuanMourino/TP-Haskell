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
nombresDeUsuarios (u : us, _, _) = nombreDeUsuario u : nombresDeUsuarios (quitar (nombreDeUsuario u) us, [], [])

--quitar: Dados un String y una lista de Usuarios, devuelve una lista de usuarios cuyos nombres sean diferentes al String
quitar :: String -> [Usuario] -> [Usuario]
quitar _ [] = []
quitar nombre (u : us) | nombre == nombreDeUsuario u = quitar nombre us
                       | otherwise = u : (quitar nombre us)

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
publicacionesDe (_, _, publicacion : ps) usuario | (usuarioDePublicacion publicacion) == usuario = publicacion : publicacionesDe ([], [], ps) usuario
                                                 | otherwise = publicacionesDe ([], [], ps) usuario

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (_, _, []) _ = []
publicacionesQueLeGustanA (_, _, (publicacion : ps)) u | pertenece u (likesDePublicacion publicacion) = publicacion : publicacionesQueLeGustanA ([], [], ps) u
                                                       | otherwise = publicacionesQueLeGustanA ([], [], ps) u

--Dados un usuario y una lista de usuarios, indica si el usuario esta dentro de la lista
pertenece :: Usuario -> [Usuario] -> Bool
pertenece _ [] = False
pertenece u (u1 : us) = u == u1 || pertenece u us

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = (publicacionesQueLeGustanA red u1) == (publicacionesQueLeGustanA red u2)

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel (_, _, []) _ = False
tieneUnSeguidorFiel (_, _, (u1, _, us) : ps) u | u1 == u && us == [] = False
                                               | u1 == u && (head us) == u1 = tieneUnSeguidorFiel ([], [], (u1, [], (tail us)) : ps) u
                                               | u1 == u && publicacionesDe ([], [], (u1, [], us) : ps) u1 == esSeguidorFiel  = True
                                               | otherwise = tieneUnSeguidorFiel ([], [], (u1, [], (tail us)) : ps) u1
                                               where esSeguidorFiel = publicacionesQueLeGustanA ([], [], publicacionesDe ([], [], ((u1, [], us) : ps)) u) (head us)

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos (_, [], _) _ _ = False
existeSecuenciaDeAmigos (_, ((u1, u2) : rs), _) inu outu | (u1, u2) == (inu, outu) || (u1, u2) == (outu, inu) = True
                                                         | u1 == inu && existeSecuenciaDeAmigos ([], rs, []) u2 outu = True
                                                         | u2 == inu && existeSecuenciaDeAmigos ([], rs, []) u1 outu = True
                                                         | otherwise = existeSecuenciaDeAmigos ([], rs ++ [(u1, u2)], []) inu outu