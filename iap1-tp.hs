-- Completar con los datos del grupo
--
-- Nombre de Grupo: Algo1Francia2
-- Integrante 1: Juan Ignacio Mouriño Atela, juanimou@gmail.com, 423/23
-- Integrante 2: Marco Salcedo, salsommarco@gmail.com, 426/21
-- Integrante 3: Nicolas Fabio Huamani Llallire, nicohuamani6470@gmail.com, 488/22
-- Integrante 4: Tomas Rodriguez Nadin, tomyrn23@gmail.com, 431/23

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

--EJERCICIO 1
--Dada una red social, devuelve una lista con los nombres de usuarios de la red social sin repetir
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios ([], _, _) = []
nombresDeUsuarios (u : us, _, _) = nombreDeUsuario u : nombresDeUsuarios (quitar (nombreDeUsuario u) us, [], [])

--quitar: Dados un String y una lista de Usuarios, devuelve una lista de usuarios cuyos nombres sean diferentes al String
quitar :: String -> [Usuario] -> [Usuario]
quitar _ [] = []
quitar nombre (u : us) | nombre == nombreDeUsuario u = quitar nombre us
                       | otherwise = u : (quitar nombre us)

--EJERCICIO 2
--Dada una red social y un usuario devuelve una lista con los usuarios que tienen una relacion con el usuario ingresado
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (_, [], _) _ = []
amigosDe (_, (pu, su) : xs, _) u | pu == u = su : amigosDe ([], xs, []) u
                                 | su == u = pu : amigosDe ([], xs, []) u
                                 | otherwise = amigosDe ([], xs, []) u

--EJERCICIO 3
--Dada una red social y un usuario, devuelve la cantidad de usuarios que estan en una relacion con en usuario ingresado
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos (_, [], _) _ = 0
cantidadDeAmigos (_, (pu, su) : xs, _) u | pu == u || su == u = 1 + cantidadDeAmigos ([], xs, []) u
                                         | otherwise = cantidadDeAmigos ([], xs, []) u

-- EJERCICIO 4
-- Dada una red social, devuelve al usuario que este en mas relaciones dentro de esta red
--Si hay mas de un usuario con la mayor cantidad de amigos, devuelve el primero
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos ([usuario], _, _) = usuario
usuarioConMasAmigos ((u1 : u2 : us), relaciones, _) | (cantidadDeAmigos ((u1: u2: us), relaciones, []) u1) >= (cantidadDeAmigos ((u1: u2: us), relaciones, []) u2) = usuarioConMasAmigos ((u1 : us), relaciones, [])
                                                    | otherwise = usuarioConMasAmigos ((u2 : us), relaciones, [])

-- EJERCICIO 5
--Dada una red social indica verdadero o falso segun si hay una persona con 5 o mas amigos o no, respectivamente
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos (_, [], _) = False
estaRobertoCarlos ([], _, _) = False
estaRobertoCarlos (u:us, relaciones, _) = cantidadDeAmigos ([], relaciones, []) u >= 1000000 || estaRobertoCarlos (us, relaciones, [])

-- EJERCICIO 6
-- Dada una red social y un usuario devuelve una lista con todas las publicaciones cuyo usuario de publicacion sea el usuario ingresado
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (_, _, []) _ = []
publicacionesDe (_, _, publicacion : ps) usuario | (usuarioDePublicacion publicacion) == usuario = publicacion : publicacionesDe ([], [], ps) usuario
                                                 | otherwise = publicacionesDe ([], [], ps) usuario

-- EJERCICIO 7
-- Dada una red social y un usuario, devuelve todas las publicaciones en las que el usuario este en la lista de quienes le dieron like
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (_, _, []) _ = []
publicacionesQueLeGustanA (_, _, (publicacion : ps)) u | pertenece u (likesDePublicacion publicacion) = publicacion : publicacionesQueLeGustanA ([], [], ps) u
                                                       | otherwise = publicacionesQueLeGustanA ([], [], ps) u

--Funcion auxiliar 'pertenece': Dados un usuario y una lista de usuarios, indica si el usuario esta dentro de la lista
pertenece :: Usuario -> [Usuario] -> Bool
pertenece _ [] = False
pertenece u (u1 : us) = u == u1 || pertenece u us

-- EJERCICIO 8
-- Dada una red social y 2 usuarios, devuelve verdadero si ambos usuarios estan en las mismas listas de likes de las mismas publicaciones
-- y falso si no es el caso
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = (publicacionesQueLeGustanA red u1) == (publicacionesQueLeGustanA red u2)

-- EJERCICIO 9
-- Dada una red social y un usuario, devuelve verdadero si hay un usuario que este en todas las listas de likes de las publicaciones
-- de este primer usuario y falso si no es el caso
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel (_, _, []) _ = False
tieneUnSeguidorFiel (_, _, (u1, _, us) : ps) u | u1 == u && us == [] = False
                                               | u1 == u && (head us) == u1 = tieneUnSeguidorFiel ([], [], (u1, [], (tail us)) : ps) u
                                               | u1 == u && publicacionesDe ([], [], (u1, [], us) : ps) u1 == esSeguidorFiel  = True
                                               | otherwise = tieneUnSeguidorFiel ([], [], (u1, [], (tail us)) : ps) u1
                                               where esSeguidorFiel = publicacionesQueLeGustanA ([], [], publicacionesDe ([], [], ((u1, [], us) : ps)) u) (head us)

-- EJERCICIO 10
-- Dada una red social y dos usuarios, devuelve verdadero si se puede hacer una lista de usuarios que empiece con uno y termine con
-- el otro, agregando un usuario solo si esta en una relacion con el anterior
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos (_, [], _) _ _ = False
existeSecuenciaDeAmigos (_, ((u1, u2) : rs), _) inu outu | (u1, u2) == (inu, outu) || (u1, u2) == (outu, inu) = True
                                                         | amigosDe ([], ((u1, u2) : rs), []) inu == [] = False
                                                         | u1 == inu && existeSecuenciaDeAmigos ([], rs, []) u2 outu = True
                                                         | u2 == inu && existeSecuenciaDeAmigos ([], rs, []) u1 outu = True
                                                         | inu == u1 || inu == u2 = existeSecuenciaDeAmigos ([], rs, []) inu outu
                                                         | otherwise = existeSecuenciaDeAmigos ([], rs ++ [(u1, u2)], []) inu outu