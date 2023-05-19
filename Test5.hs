module Test5 where

import Test.HUnit
import Solucion

run = runTestTT tests

tests = test [
    "nombresDeUsuario con nombres repetidos"   ~: (nombresDeUsuarios red1)          ~?= ["Juan", "Sofia", "Jazmin", "Nico", "Marco", "Sin amigos"],
    "nombresDeUsuario sin nombres repetidos"   ~: (nombresDeUsuarios red2)          ~?= ["Juan", "Sofia", "Jazmin", "Nico", "Marco"],
    "nombresDeUsuario de red sin usuarios"     ~: (nombresDeUsuarios red0)          ~?= [],

    "amigosDe, red con relaciones y el usuario sin amigos"        ~: (amigosDe red1 u7)           ~?= [],
    "amigosDe, red con relaciones y el usuario con amigos"        ~: (amigosDe red1 u1)           ~?= [u2, u3, u4, u5, u6],
    "amigosDe, red sin relaciones"                                ~: (amigosDe red7 u3)           ~?= [],

    "cantAmigos, red con relaciones y el usuario sin amigos"      ~: (cantidadDeAmigos red1 u7)   ~?= 0,
    "cantAmigos, red con relaciones y el usuario con amigos"      ~: (cantidadDeAmigos red1 u1)   ~?= 5,
    "cantAmigos, red sin relaciones"                              ~: (cantidadDeAmigos red7 u3)   ~?= 0,

    "usuarioConMasAmigos, varios usuarios con la maxima cantidad"      ~: expectAny (usuarioConMasAmigos red3)    [u1, u2, u3],
    "usuarioConMasAmigos, un solo usuario con la maxima cantidad"      ~: (usuarioConMasAmigos red1)              ~?= u1,
    "usuarioConMasAmigos, red con un solo usuario"                     ~: (usuarioConMasAmigos red8)              ~?= u5,
    "usuarioConMasAmigos, red con usuarios pero sin relaciones"        ~: expectAny (usuarioConMasAmigos red7)    [u1, u2, u3, u4],

    "estaRobertoCarlos, hay un usuario con mas de 10 amigos"           ~: (estaRobertoCarlos red6)     ~?= True,
    "estaRobertoCarlos, no hay usuario con mas de 10 amigos"           ~: (estaRobertoCarlos red2)     ~?= False,
    "estaRobertoCarlos, red sin usuarios"                              ~: (estaRobertoCarlos red0)     ~?= False,

    "publicacionesDe, red con publicaciones del usuario"                   ~: (publicacionesDe red1 u1)            ~?= [p1d1, p1d2, p1d3],
    "publicacionesDe, red con publicaciones pero ninguna del usuario"      ~: (publicacionesDe red1 u3)            ~?= [],
    "publicacionesDe, red sin publicaciones"                               ~: (publicacionesDe red7 u2)            ~?= [],

    "red con publicaciones likeadas por el usuario"                    ~: (publicacionesQueLeGustanA red1 u2)      ~?= [p1d1, p1d3],
    "red con publicaciones pero ninguna likeada por el usuario"        ~: (publicacionesQueLeGustanA red1 u6)      ~?= [],
    "red sin publicaciones"                                            ~: (publicacionesQueLeGustanA red7 u4)      ~?= [],

    "la red tiene publicaciones y les gustan las mismas"               ~: (lesGustanLasMismasPublicaciones red1 u1 u3) ~?= True,
    "la red tiene publicaciones pero no les gustan las mismas"         ~: (lesGustanLasMismasPublicaciones red2 u1 u3) ~?= False,
    "la red no tiene publicaciones"                                    ~: (lesGustanLasMismasPublicaciones red7 u2 u3) ~?= True,
    "los usuarios ingresados son iguales"                              ~: (lesGustanLasMismasPublicaciones red1 u6 u6) ~?= True,

    "la red no tiene publicaciones"                              ~: (tieneUnSeguidorFiel red7 u1)                ~?= False,
    "la red tiene un solo usuario"                               ~: (tieneUnSeguidorFiel red8 u5)                ~?= False,
    "el usuario tiene un seguidor fiel"                          ~: (tieneUnSeguidorFiel red1 u1)                ~?= True,
    "el usuario es su unico y propio seguidor fiel"              ~: (tieneUnSeguidorFiel red2 u1)                ~?= False,
    "el usuario no tiene un seguidor fiel"                       ~: (tieneUnSeguidorFiel red3 u1)                ~?= False,

    "la red no tiene relaciones"                                 ~: (existeSecuenciaDeAmigos red7 u1 u4)         ~?= False,         
    "existe secuencia de amigos"                                 ~: (existeSecuenciaDeAmigos red4 u1 u5)         ~?= True,
    "no existe secuencia de amigos"                              ~: (existeSecuenciaDeAmigos red5 u1 u5)         ~?= False]

expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)
--Usuarios:
u1 = (1, "Juan")
u2 = (2, "Sofia")
u3 = (3, "Jazmin")
u4 = (4, "Nico")
u5 = (5, "Marco")
u6 = (6, "Juan")
u7 = (7, "Sin amigos")
u8 = (8, "Mati")
u9 = (9, "Santi")
u10 = (10, "More")
u11 = (11, "Esteban")
u12 = (12, "Maira")


--Relaciones:
r1 = (u1, u2)
r2 = (u3, u1)
r3 = (u1, u4)
r4 = (u2, u3)
r5 = (u1, u5)
r6 = (u1, u6)
r7 = (u3, u2)
r8 = (u3, u4)
r9 = (u1, u7)
r10 = (u5, u4)
r11 = (u1, u8)
r12 = (u9, u1)
r13 = (u10, u1)
r14 = (u1, u11)
r15 = (u12, u1)

--Publicaciones:
p1d1 = (u1, "1ra publicacion", [u1, u2, u3, u4])
p1d2 = (u1, "2da publicacion", [u1, u3])
p1d3 = (u1, "3ra publicacion", [u1, u3, u2])
p1df = (u1, "Falso positivo", [u1])
p1d4 = (u1, "Sin seguidor fiel", [])
p2d1 = (u2, "1ra pub2",  [])

--Redes:
red0 = ([], [], [])
red1 = ([u1, u2, u3, u4, u5, u6, u7], [r1, r2, r3, r4, r5, r6], [p1d1, p1d2, p1d3])
--Deberia dar True Roberto Carlos con 5 amigos, seguidor fiel para u1 y le gustan mismas publicaciones con (u1, u3)
red2 = ([u1, u2, u3, u4, u5], [r1, r2, r3, r4, r5], [p1d1, p1d2, p1df])
--Deberia dar False Roberto Carlos, seguidor fiel para u1 y le gustan mismas publicaciones con (u1, u3)
--Deberia dar True para le gustan mismas publicaciones con (u2, u3)
red3 = ([u1, u2 ,u3 ,u4], [r1, r2, r4],[p1d4])
red4 = ([u1, u2, u3, u4, u5, u6, u7], [r10, r1, r8, r7, r9], [])
red5 = ([u1, u2, u3, u4, u5, u6, u7], [r10, r1, r7, r9], [])
red6 = ([u1, u2, u3, u4, u5, u6, u8, u9, u10, u11, u12], [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15], [])
red7 = ([u1, u2, u3, u4],[],[])
red8 = ([u5],[],[])
